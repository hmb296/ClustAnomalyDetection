set.seed(123)
###### IMPORTS #######
library(ggplot2)
library(gridExtra)
library(cluster)
library(clustMixType)
library(kamila)
library(caret)
library(cvAUC)
library(kmed)
library(dplyr)

###### PARAMETERS FOR EXPERIMENTS #######
target_prop <- 0.05
sample_size <- 150
anomaly_dist_method <- "MAD"

###### DATA LOADING ######
data <- read.csv("preprocessed data/cirrhosis.csv", sep = ",")
cat_cols <- seq(1,7)
num_cols <- seq(8,ncol(data)-1)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

source("adjust_prop.R")
data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns
train_val_data <- adjust_prop(target_prop, data$target, data, sample_size)
target_index <- length(train_val_data) # target column is the last one
train_val_data[, cat_cols] <- lapply(train_val_data[, cat_cols], as.factor) # adjust the type of categorical columns
train_val_data[, num_cols] <- lapply(train_val_data[, num_cols], as.numeric) # adjust the type of numerical columns

target <- as.factor(train_val_data$target) # adjust the type of the target
levels(target) <- c(0,1)

# initialising variables for storing results
results_list <- list()
row_idx <- 1

set.seed(123)
train_idx <- createDataPartition(train_val_data[,target_index], p = 0.8, list = FALSE)

test_data  <- train_val_data[-train_idx, ]
train_data <- train_val_data[train_idx, ]

if (!any(test_data[, target_index] == 1)) {
  # find one positive in train_data
  pos_in_train <- which(train_data[, target_index] == 1)[1]
  # move it to test_data
  test_data  <- rbind(test_data, train_data[pos_in_train, , drop = FALSE])
  train_data <- train_data[-pos_in_train, , drop = FALSE]
}

# removing constant columns from the training data
train_data <- train_data[, sapply(train_data, function(col) length(unique(col)) > 1)] ## drop constant columns after adj
test_data  <- test_data[,colnames(train_data)]

# separate numeric/categorical in train/test
categorical_data <- train_data[, cat_cols, drop = FALSE]
numeric_data <- train_data[, num_cols, drop = FALSE]

categorical_test <- test_data[, cat_cols, drop = FALSE]
numeric_test <- test_data[, num_cols, drop = FALSE]

####### DISTANCES ######
# Gower distance matrix:
gower_matrix <- daisy(train_data[, -target_index], metric = "gower")

# DAFI-Gower distance matrix:
# based on the implementation by Liu et al., the DAFI Gower matrix is obtained as:
source("self_adaptive_distance.R")
source("gower.dist.modify.R")
source("normalized_MI.R")
feature_importance1 = self_adaptive_distance(continuous_feature=numeric_data, 
                                             categorical_feature=categorical_data)
feature_importance2 = normalized_MI(continuous_feature=numeric_data, 
                                    categorical_feature=categorical_data)
feature_importance <- feature_importance1*feature_importance2
feature_importance <- feature_importance/sum(feature_importance)
dafi_gower_matrix <- gower.dist.modify(train_data, var.weights = feature_importance, 
                                       robcb = "iqr")

###### TUNING k ######
source("tune_k_silhouette.R")
optimal_k_kamila <- tune_k_sil(method = "kamila", distance_matrix=gower_matrix, numeric_data = numeric_data, 
                               categorical_data = categorical_data, k_range = 2:10, plot=FALSE)
cat("Optimal k for KAMILA:", optimal_k_kamila, "\n")
optimal_k_kproto <- tune_k_sil(method = "kproto", distance_matrix=gower_matrix, data=train_data, k_range = 2:10, plot=FALSE)
cat("Optimal k for K-Prototypes:", optimal_k_kproto, "\n")
optimal_k_pam <- tune_k_sil(method = "pam", distance_matrix=gower_matrix, k_range = 2:10, plot=FALSE)
cat("Optimal k for PAM:", optimal_k_pam, "\n")

###### CLUSTERING ######
# k-Prototypes:
kproto_res <- kproto(train_data, k = optimal_k_kproto, verbose = FALSE, type="gower")
train_data$cluster_kproto <- kproto_res$cluster

# PAM with Gower distance:
pam_gower <- pam(gower_matrix, k = optimal_k_pam)
train_data$cluster_pam_gower <- pam_gower$clustering

# PAM with DAFI-Gower distance:
pam_dafi <- pam(dafi_gower_matrix, k = optimal_k_pam)
train_data$cluster_pam_dafi_gower <- pam_dafi$clustering

kamila_result <- kamila(numeric_data, categorical_data, numClust = optimal_k_kamila, 
                        numInit = 10)
train_data$cluster_kamila <- kamila_result$finalMemb

###### FLAGGING ANOMALIES ######
source("flag_anomalies.R") # function for anomaly thresholding and flagging

cat("Dataset:", dataset,
    "\n Target prop:", target_prop,
    "\n Sample size:", sample_size)

###### ORDERING COMBINATIONS BASED ON AUC-ROC ######
set.seed(123)
# clusterings to be tested:
cluster_vars <- list(
  cluster_kproto = train_data$cluster_kproto,
  cluster_pam_gower = train_data$cluster_pam_gower,
  cluster_pam_dafi_gower = train_data$cluster_pam_dafi_gower,
  cluster_kamila = train_data$cluster_kamila
)

# distance matrices to be tested:
distance_matrices <- list(
  gower_matrix = gower_matrix,
  dafi_gower_matrix = dafi_gower_matrix
)

source("get_auc.R") # function for obtaining AUC-ROC score (on train set/full data)

# evaluation procedure for each combination:
results <- list()
for (cluster_name in names(cluster_vars)) {
  for (dist_name in names(distance_matrices)) {
    res <- get_auc_train(cluster_vars[[cluster_name]], distance_matrices[[dist_name]])
    results[[paste(cluster_name, dist_name, sep = "_x_")]] <- c("Cluster" = cluster_name, 
                                                                "Distance" = dist_name, res)
  }
}

# combining the results into a sorted data frame
results_df <- do.call(rbind, results)
results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)
results_df$AUCROC <- as.numeric(results_df$AUCROC)
results_df <- results_df[order(results_df$AUCROC, decreasing = TRUE), ]
print(head(results_df, n=3), row.names = FALSE, sep="\t\t")

source("test_eval.R") # function for evaluation on validation set

###### TUNING LAMBDA ######
best_ba <- -Inf # store best balanced accuracy score
best_lambda <- NA # store best lambda
best_cm <- NA # store confusion matrix for the best lambda

for(lambda in c(0.2, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 3)){
  for (i in 1:3) { # checking the best 3 combinations
    cluster_name <- results_df$Cluster[i]
    dist_name <- results_df$Distance[i]
    anomaly_output <- flag_relative_distance_anomalies(train_data, cluster_vars[[cluster_name]], 
                                                       distance_matrices[[dist_name]],
                                                       method = anomaly_dist_method, 
                                                       lambda = lambda)
    
    thresholds <- anomaly_output$thresholds
    medoids_ind <- anomaly_output$medoids_ind
    medoids <- train_data[medoids_ind,]
    anomaly_flags <- anomaly_output$anomaly_flags
    
    anomaly_pred <- as.factor(ifelse(anomaly_flags == "TRUE", 1, 0))
    levels(anomaly_pred) <- c(0,1)
    train_target <- as.factor(train_data$target)
    levels(train_target) <- c(0,1)
    
    scores_on_test <- score_on_test(medoids, thresholds, distance_metric = dist_name)
    ba_test_value <- scores_on_test$cm_test$byClass["Balanced Accuracy"]
    cm_test <- scores_on_test$cm_test
    
    if (ba_test_value > best_ba) {
      best_ba <- ba_test_value
      best_lambda <- lambda
      best_cm <- cm_test
    }
  }}
cat("Best lambda based on BA:", best_lambda, "with BA =", round(best_ba,4), "\n")
print(best_cm$table)

# presenting results
results_dataframe <- do.call(rbind, results_list)
print(results_dataframe)

# 3 compared configurations:
anomaly_output <- flag_relative_distance_anomalies(train_data, train_data$cluster_kamila,
                                                  gower_matrix,
                                                  method = anomaly_dist_method,
                                                  lambda = best_lambda)

# anomaly_output <- flag_relative_distance_anomalies(train_data, train_data$cluster_pam_gower,
#                                                    gower_matrix,
#                                                    method = anomaly_dist_method,
#                                                    lambda = best_lambda)

# anomaly_output <- flag_relative_distance_anomalies(train_data, train_data$cluster_kproto,
#                                                    dafi_gower_matrix,
#                                                    method = anomaly_dist_method,
#                                                    lambda = best_lambda)

thresholds <- anomaly_output$thresholds
medoids_ind <- anomaly_output$medoids_ind
medoids <- train_data[medoids_ind,]

###### CONFIRMATORY EVALUATION ###### 
library(dplyr)

df_out <- anti_join(data, rbind(train_data[,colnames(test_data)],test_data))
eval_sample_size <- 15
target_prop_eval <- 0.3

set.seed(123)
rows_pos <- sample(which(df_out$target == 1), size=floor(target_prop_eval*eval_sample_size))
rows_neg <- sample(which(df_out$target != 1), size=floor((1-target_prop_eval)*eval_sample_size))
rows_sample <- sort(c(rows_pos, rows_neg))
data_sample <- df_out[rows_sample, ]

scores_eval <- score_on_eval(medoids, thresholds, distance_metric = "gower_matrix") # or "dafi_gower_matrix"
scores_eval$cm_test$table

# ###### ADDITIONAL: VISUALISATION ######
set.seed(123)

combined_data <- rbind(train_data[1:target_index], medoids[1:target_index], data_sample)
n_train <- nrow(train_data)
n_medoids <- nrow(medoids)
n_test <- nrow(data_sample)
test_target <- as.factor(data_sample$target)
levels(test_target) <- c(0,1)
matrix_test <- as.matrix(daisy(combined_data[, -target_index], metric = "gower"))
matrix_test <- matrix_test[(nrow(train_data)+1):nrow(matrix_test),(nrow(train_data)+1):nrow(matrix_test)]
dist_test_to_medoids <- matrix_test[(n_medoids + 1):(n_medoids + n_test), 1:n_medoids] # distances to medoids
closest_medoid_idx <- apply(dist_test_to_medoids, 1, which.min) # find the closest medoid for each test point
dist_test_to_closest <- apply(dist_test_to_medoids, 1, min) # find the distance to closest medoid for each test point
anomaly_flags_test <- dist_test_to_closest > thresholds[closest_medoid_idx]
anomaly_pred <- as.factor(ifelse(anomaly_flags_test == "TRUE", 1, 0))
levels(anomaly_pred) <- c(0,1)

mds_coords <- cmdscale(matrix_test, k = 2)
mds_df <- as.data.frame(mds_coords)
colnames(mds_df) <- c("Dim1", "Dim2")

mds_df$anomaly_pred <- c(rep(0,n_medoids), ifelse(anomaly_flags_test == "TRUE", 1, 0))
mds_df$anomaly_pred <- as.factor(mds_df$anomaly_pred)
mds_df$target <- c(medoids$target, data_sample$target)
mds_df$target <- as.factor(mds_df$target)

ggplot(mds_df, aes(x = Dim1, y = Dim2)) +
  geom_point(aes(shape = anomaly_pred, color = target, fill = anomaly_pred), alpha=0.6, stroke = 1.5) +
  scale_color_manual(values = c('0' = "lightblue", '1' = "hotpink")) +
  scale_shape_manual(values = c('0' = 1, '1' = 4)) +
  # scale_fill_manual(values = c('0' = "lightblue", '1' = "hotpink")) +
  labs(title = "MDS Plot with Highlighted Outliers and Targets",
       x = "Dimension 1", y = "Dimension 2",
       shape = "anomaly_pred", color = "target", fill="anomaly_pred") +
  theme_minimal()

print(scores_eval$cm_test$table)
print(best_lambda)

###### ADDITIONAL: VISUALISATION ######
anomaly_output <- flag_relative_distance_anomalies(train_data, train_data$cluster_kamila,
                                                   gower_matrix,
                                                   method = anomaly_dist_method,
                                                   lambda = 1.5)

thresholds <- anomaly_output$thresholds
medoids_ind <- anomaly_output$medoids_ind
medoids <- train_data[medoids_ind,]

set.seed(123)

combined_data <- rbind(medoids[1:target_index], train_data[1:target_index], data_sample)
n_train <- nrow(train_data)
n_medoids <- nrow(medoids)
n_test <- nrow(data_sample)
test_target <- as.factor(c(train_data$target, data_sample$target))
levels(test_target) <- c(0,1)
matrix_test <- as.matrix(daisy(combined_data[, -target_index], metric = "gower"))
dist_test_to_medoids <- matrix_test[(n_medoids + 1):(n_medoids + n_test + n_train), 1:n_medoids] # distances to medoids
closest_medoid_idx <- apply(dist_test_to_medoids, 1, which.min) # find the closest medoid for each test point
dist_test_to_closest <- apply(dist_test_to_medoids, 1, min) # find the distance to closest medoid for each test point
anomaly_flags_test <- dist_test_to_closest > thresholds[closest_medoid_idx]
anomaly_pred <- as.factor(ifelse(anomaly_flags_test == "TRUE", 1, 0))
levels(anomaly_pred) <- c(0,1)

# dimensionality reduction for visualisation
library(Rtsne)
tsne_out <- Rtsne(matrix_test, dims = 2, perplexity = 35, verbose = TRUE, check_duplicates = FALSE)
mds_df <- as.data.frame(tsne_out$Y)
colnames(mds_df) <- c("Dim1", "Dim2")

mds_df$anomaly_pred <- c(rep(0,n_medoids), ifelse(anomaly_flags_test == "TRUE", 1, 0))
mds_df$anomaly_pred <- as.factor(mds_df$anomaly_pred)
mds_df$medoid <- c(1,2,rep(0,nrow(mds_df)-2))
mds_df$medoid <- as.factor(mds_df$medoid)
mds_df$target <- c(medoids$target, train_data$target, data_sample$target)
mds_df$target <- as.factor(mds_df$target)
mds_df$cluster <- c(medoids$cluster_kamila, train_data$cluster_kamila, closest_medoid_idx[rownames(data_sample)])
mds_df$cluster <- as.factor(mds_df$cluster)

ggplot(mds_df, aes(x = Dim1, y = Dim2)) +
  geom_point(data = ~ filter(.x, medoid == 1), # marking medoids
             shape = 1, size = 5, stroke = 2, color = "darkslateblue", fill = NA) +
  geom_point(data = ~ filter(.x, medoid == 2), # marking medoids
             shape = 1, size = 5, stroke = 2, color = "darkcyan", fill = NA) +
  geom_point(aes(color = cluster), alpha=0.5, stroke = 1.5) +
  scale_color_manual(values = c('1' = "darkslateblue", '2' = "darkcyan")) +
  geom_text(data = filter(mds_df, medoid == 1),
            aes(label = "Medoid 1"), fontface = "bold", vjust = -1.2, color = "darkslateblue") +
  geom_text(data = filter(mds_df, medoid == 2), aes(label = "Medoid 2"), fontface = "bold",
            vjust = +2.2, color = "darkcyan") +
  labs(title = "MDS plot showing clustering result", x = "Dimension 1", y = "Dimension 2", color = "Cluster") +
  theme_minimal()

ggplot(mds_df, aes(x = Dim1, y = Dim2)) +
  geom_point(data = ~ filter(.x, medoid == 1), # marking medoids
             shape = 1, size = 5, stroke = 2, color = "darkslateblue", fill = NA) +
  geom_point(data = ~ filter(.x, medoid == 2), # marking medoids
             shape = 1, size = 5, stroke = 2, color = "darkgreen", fill = NA) +
  geom_point(aes(shape = anomaly_pred, color = target, alpha = target), stroke = 2) +
  scale_color_manual(values = c('0' = "darkcyan", '1' = "lightcoral")) +
  scale_alpha_manual(values = c('0' = 0.4, '1' = 0.8)) +
  scale_shape_manual(values = c('0' = 1, '1' = 4)) +
  geom_text(data = filter(mds_df, medoid == 1), aes(label = "Medoid 1"), fontface = "bold",family = "Times New Roman", vjust = -1.2, color = "darkslateblue") +
  geom_text(data = filter(mds_df, medoid == 2), aes(label = "Medoid 2"), fontface = "bold",family = "Times New Roman",
            vjust = 2.2, color = "darkgreen") +
  labs(title = "MDS plot showing anomalies and targets", x = "Dimension 1", y = "Dimension 2",
       shape = "Anomaly flag", color = "Target",alpha = "Target") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", hjust = 0.5),
      text = element_text(family = "Times New Roman"))

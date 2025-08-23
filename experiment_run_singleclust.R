set.seed(123)
source("adjust_prop.R")

####### DATA LOADING ######
if (dataset == "obesity"){
  data <- read.csv("preprocessed data/obesity.csv", sep=",")
  cat_cols <- seq(1,9)
  num_cols <- seq(10,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "Obesity Dataset"
}
if(dataset == "arrhythmia"){
  data <- read.csv("preprocessed data/arrhythmia.csv", sep = ",")
  cat_cols <- seq(1,2)
  num_cols <- seq(3,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "Arrhythmia Dataset"
}
if(dataset == "cirrhosis"){
  data <- read.csv("preprocessed data/cirrhosis.csv", sep = ",")
  cat_cols <- seq(1,7)
  num_cols <- seq(8,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "Cirrhosis Dataset"
}
if(dataset == "statlog_heart"){
  data <- read.csv("preprocessed data/statlog_heart.csv", sep = ",")
  cat_cols <- seq(1,7)
  num_cols <- seq(8,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "Statlog Heart Disease Dataset"
}
if(dataset == "aids"){
  data <- read.csv("preprocessed data/aids.csv", sep = ",")
  cat_cols <- seq(1,14)
  num_cols <- seq(15,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "AIDS Dataset"
}
if(dataset == "diabetes_indicators"){
  data <- read.csv("preprocessed data/diabetes_indicators.csv", sep = ",")
  cat_cols <- seq(1,15)
  num_cols <- seq(16,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "Diabetes Indicators Dataset"
}
if(dataset == "heart_failure"){
  data <- read.csv("preprocessed data/heart_failure.csv", sep = ",")
  colnames(data)[ncol(data)] <- "target"
  cat_cols <- seq(1,6)
  num_cols <- seq(7,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "Heart Failure Dataset"
}
if(dataset == "liver_patient"){
  data <- read.csv("preprocessed data/liver_patient.csv", sep = ",")
  colnames(data)[ncol(data)] <- "target"
  cat_cols <- seq(1,2)
  num_cols <- seq(3,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "Liver Patient Dataset"
}
if(dataset == "mimic2"){
  data <- read.csv("preprocessed data/mimic2.csv", sep = ",")
  colnames(data)[ncol(data)] <- "target"
  cat_cols <- seq(1,15)
  num_cols <- seq(16,ncol(data)-1)
  cat_cols <- colnames(data)[cat_cols]
  num_cols <- colnames(data)[num_cols]
  dataset_name <- "MIMIC-II Dataset"
}

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

if (!is.na(target_prop) | !is.na(sample_size)){ # adjust target proportion and sample size
  data <- adjust_prop(target_prop, data$target, data, sample_size)
}

target_index <- length(data) # target column is the last one
data[, cat_cols] <- lapply(data[, cat_cols], as.factor) # adjust the type of categorical columns
data[, num_cols] <- lapply(data[, num_cols], as.numeric) # adjust the type of numerical columns

categorical_data <- data[, cat_cols] # separate categorical columns
numeric_data <- data[, num_cols] # separate numerical columns

target <- as.factor(data$target) # adjust the type of the target
levels(target) <- c(0,1)

####### DISTANCES ######
# Huang distance matrix:
huang_matrix <- distmix(data, method = "huang", idnum = num_cols, idcat = cat_cols)

# Gower distance matrix:
gower_matrix <- daisy(data[, -target_index], metric = "gower")

# DAFI-Gower matrix
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
dafi_gower_matrix <- gower.dist.modify(data, var.weights = feature_importance, 
                                       robcb = "iqr")

###### CLUSTERING ######
data$cluster <- rep(1, nrow(data))

###### OUTLIERS
source("flag_anomalies.R")

cat("Dataset:", dataset,
    "\n Target prop:", target_prop,
    "\n Sample size:", sample_size)

###### ORDERING COMBINATIONS BASED ON AUC-ROC ######
# testing only the single cluster case:
cluster_vars <- list(one_cluster = data$cluster)  

# distance matrices to be tested
distance_matrices <- list(
  gower_matrix = gower_matrix,
  dafi_gower_matrix = dafi_gower_matrix
)

source("get_auc.R")

# evaluation procedure for each combination:
results <- list()
for (cluster_name in names(cluster_vars)) {
  for (dist_name in names(distance_matrices)) {
    res <- get_auc(cluster_vars[[cluster_name]], distance_matrices[[dist_name]])
    results[[paste(cluster_name, dist_name, sep = "_x_")]] <- c("Cluster" = cluster_name, 
                                                                "Distance" = dist_name, res)
  }
}

# combining the results into a sorted data frame
results_df <- do.call(rbind, results)
results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)
results_df$AUCROC <- as.numeric(results_df$AUCROC)
results_df <- results_df[order(results_df$AUCROC, decreasing = TRUE), ]
# print(head(results_df, n=3), row.names = FALSE, sep="\t\t")

###### TUNING LAMBDA ######
best_ba <- -Inf # store best balanced accuracy score
best_lambda <- NA # store best lambda
best_cm <- NA # store confusion matrix for the best lambda

for(lambda in c(0.2, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 3)){
  for (i in 1:2) { # checking the best 3 combinations
    cluster_name <- results_df$Cluster[i]
    dist_name <- results_df$Distance[i]
    anomaly_output <- flag_relative_distance_anomalies(data, cluster_vars[[cluster_name]], 
                                                       distance_matrices[[dist_name]],
                                                       method = anomaly_dist_method, 
                                                       lambda = lambda)
    anomaly_flags <- anomaly_output$anomaly_flags
    anomaly_pred <- as.factor(ifelse(anomaly_flags == "TRUE", 1, 0))
    levels(anomaly_pred) <- c(0,1)
    
    # evaluation: confusion matrix and balanced accuracy
    cm <- confusionMatrix(data=anomaly_pred, reference = target, positive = "1")
    ba <- cm$byClass["Balanced Accuracy"]
    
    if (is.na(ba)){
      ba <- -Inf
    }
    
    if (ba > best_ba) {
      best_ba <- ba
      best_lambda <- lambda
      best_cm <- cm
    }
  }}
cat("\nBest lambda based on BA:", best_lambda, "with BA =", round(best_ba,4), "\n")
# print(best_cm$table)

###### EVALUATING PERFORMANCE ######
for (i in 1:2) {
  cluster_name <- results_df$Cluster[i]
  dist_name <- results_df$Distance[i]
  aucroc <- results_df$AUCROC[i]
  
  anomaly_output <- flag_relative_distance_anomalies(data, 
                                                     cluster_vars[[cluster_name]], 
                                                     distance_matrices[[dist_name]], 
                                                     method = anomaly_dist_method, 
                                                     lambda = best_lambda)
  thresholds <- anomaly_output$thresholds
  medoids_ind <- anomaly_output$medoids_ind
  medoids <- data[medoids_ind,]
  anomaly_flags <- anomaly_output$anomaly_flags
  anomaly_pred <- as.factor(ifelse(anomaly_flags == "TRUE", 1, 0))
  levels(anomaly_pred) <- c(0,1)
  
  # evaluation: confusion matrix and performance metrics
  cm <- confusionMatrix(data=anomaly_pred, reference = target, positive = "1")
  acc <- cm$overall["Accuracy"]
  spec <- cm$byClass["Specificity"]
  f1 <- cm[["byClass"]]["F1"]
  dr <- cm[["byClass"]]["Detection Rate"]
  ba <- cm$byClass["Balanced Accuracy"]
  
  if(is.na(f1)){
    f1 <- 0
  }
  
  # saving results:
  results_list[[row_idx]] <- data.frame(
    dataset      = dataset,
    target_prop  = target_prop,
    sample_size  = sample_size,
    cluster      = cluster_name,
    distance     = dist_name,
    aucroc       = aucroc,
    best_lambda  = best_lambda,
    anomaly_dist_method = anomaly_dist_method,
    ba           = ba,
    f1           = f1,
    TN = cm$table[1],
    FP = cm$table[2],
    FN = cm$table[3],
    TP = cm$table[4]
  )
  row_idx <- row_idx + 1
}

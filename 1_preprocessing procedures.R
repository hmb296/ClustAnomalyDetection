###### PREPROCESSING PROCEDURES ######
#------------------------ Cirrhosis ---------------------------
data <- read.csv("initial data/cirrhosis.csv", sep = ",")
target_name <- "target"
data$target <- ifelse(data$target == "D", 1, 0)

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

# AGE --> ordered variable, as the other categorical are: Drug (nominal), EDEMA - 3 cat, rest are binary
cat_cols <- c(1,2,3,4,5,6,7) # age is made into ordered
num_cols <- c(8,9,10,11,12,13,14,15,16,17) #c(8,9,15,16)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$Age <- cut(
  data$Age/365, # divide for cirrhosis,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/cirrhosis.csv", row.names = FALSE)

#------------------------ Statlog Heart ------------------------
data <- read.csv("initial data/statlog_heart.csv", sep = ",")
target_name <- "target"
data$target <- ifelse(data$target==1,0,1)

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

# VARIABLES - STATLOG_HEART
# Real: 1,4,5,8,10,12
# Ordered:11 - ST segment measured in terms of slope during peak exercise; 0: up sloping; 1: flat; 2: down sloping - treated as categorical
# Binary: 2,6,9
# Nominal:7,3,13

cat_cols <- c(1,2,3,6,7,9,11,13) # age is made into ordered
num_cols <- c(4,5,8,10,12)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$age <- cut(
  data$age,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/statlog_heart.csv", row.names = FALSE)

#------------------------ Obesity ------------------------
data <- read.csv("initial data/obesity.csv", sep = ",")
target_name <- "target"
data$target <- ifelse(data$target %in% c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II"), 0, 1) # only obesity

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

cat_cols <- c(1,2,5,6,9,10,12,15,16) # age is made into ordered
num_cols <- c(3,4,7,8,11,13,14)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$Age <- cut(
  data$Age,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/obesity.csv", row.names = FALSE)
#------------------------ AIDS ------------------------
data <- read.csv("initial data/aids.csv", sep = ",")
target_name <- "target"

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

cat_cols <- c(2,3,5,6,7,9,10,12,13,14,15,16,17,18) # age is made into ordered
num_cols <- c(1,4,8,11,19,20,21,22)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$age <- cut(
  data$age,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/aids.csv", row.names = FALSE)

#------------------------ Diabetes indicators ------------------------
data <- read.csv("initial data/diabetes_indicators.csv", sep = ",")
target_name <- "target"

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

cat_cols <- c(1,2,3,5,6,7,8,9,10,11,12,13,17,18,19)
num_cols <- c(4,14,15,16,20,21)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)
data <- data[,cols]

data$Age <- cut(
  data$Age,
  breaks = c(0, 4, 7, 10, Inf),
  labels = c("young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/diabetes_indicators.csv", row.names = FALSE)

#------------------------ [not done - ambiguous target] CCRF ----------------------
# data <- read.csv("cervical_cancer_risk_factors.csv", sep = ",")
# target_name <- "target"
# 
# data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns
# 
# cat_cols <- c(2,3,5,6,7,9,10,12,13,14,15,16,17,18) # age is made into ordered
# num_cols <- c(1,4,8,11,19,20,21,22)
# cat_cols <- colnames(data)[cat_cols]
# num_cols <- colnames(data)[num_cols]
# 
# cols <- c(cat_cols, num_cols, target_name)
# 
# data <- data[,cols]
# 
# data$age <- cut(
#   data$age,
#   breaks = c(0, 20, 35, 50, 65, Inf),
#   labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
#   right = FALSE
# )
# 
# data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
# data[, num_cols] <- lapply(data[, num_cols], as.numeric)
# 
# data <- na.omit(data)
# 
# # normalisation of numerical features
# normalize <- function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
# 
# data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))
# 
# write.csv(data,"preprocessed data/......", row.names = FALSE)

#------------------------ Heart failure ------------------------
data <- read.csv("initial data/heart_failure.csv", sep = ",")
target_name <- "DEATH_EVENT"

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

cat_cols <- c(1,2,4,6,10,11)
num_cols <- c(3,5,7,8,9,12)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$age <- cut(
  data$age,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/heart_failure.csv", row.names = FALSE)

#------------------------ Liver patient ------------------------
data <- read.csv("initial data/liver_patient.csv", sep = ",")
target_name <- "Dataset"
data$Dataset <- ifelse(data$Dataset==1,0,1)

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

cat_cols <- c(1,2)
num_cols <- c(3,4,5,6,7,8,9,10)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$Age <- cut(
  data$Age,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/liver_patient.csv", row.names = FALSE)

#------------------------ MIMIC-II ------------------------
data <- read.csv("initial data/mimic2.csv", sep = ",")
target_name <- "day_28_flg" # consider target change -- death??

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns
cat_cols <- c(1,4,5,10,12,19,20,21,22,23,24,25,26,27,28)
num_cols <- c(2,3,6,7,8,9,14,29,30,31,32,33,34,35,36,37,38,39,40,41,42)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$age <- cut(
  data$age,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/mimic2.csv", row.names = FALSE)


#------------------------ Arrhythmia ------------------------
data <- read.csv("initial data/arrhythmia.csv", sep = ",")
target_name <- "target"
data$target <- ifelse(data$Class.type=="1",0,1)

library(naniar)
data <- data %>% replace_with_na_all(condition = ~.x == "?")
data <- as.data.frame(data)

data <- data[, sapply(data, function(col) length(unique(col)) > 1)] # drop constant columns

cat_cols <- c(2,3)
num_cols <- c(4,5,6,7,8,9,10,11,12,13,14,16)
cat_cols <- colnames(data)[cat_cols]
num_cols <- colnames(data)[num_cols]

cols <- c(cat_cols, num_cols, target_name)

data <- data[,cols]

data$Age <- cut(
  data$Age,
  breaks = c(0, 20, 35, 50, 65, Inf),
  labels = c("child", "young_adult", "mid_adult", "adult", "senior"),
  right = FALSE
)

data[, cat_cols] <- lapply(data[, cat_cols], as.factor)
data[, num_cols] <- lapply(data[, num_cols], as.numeric)

data <- na.omit(data)

# normalisation of numerical features
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data[, num_cols] <- as.data.frame(lapply(data[, num_cols], normalize))

write.csv(data,"preprocessed data/arrhythmia.csv", row.names = FALSE)

################################
data <- adjust_prop(target_prop, data$target, data)
data <- data[, sapply(data, function(col) length(unique(col)) > 1)] ## drop constant columns
################################
categorical_data <- data[, cat_cols]
numeric_data <- data[, num_cols]

target_index <- length(cols) # target column is the last one
################################
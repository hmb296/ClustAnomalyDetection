###### IMPORTS #######
library(ggplot2)
library(gridExtra)
library(cluster)
library(clustMixType)
library(kamila)
library(caret)
library(cvAUC)
library(kmed)

###### ALL EXPERIMENT RUNS ######
datasets <- c("heart_failure","liver_patient","mimic2","obesity","arrhythmia",
              "cirrhosis","statlog_heart","aids","diabetes_indicators")
sample_size <- NA
plots <- FALSE

results_list <- list() # saving results for each combination
row_idx <- 1

for(anomaly_dist_method in c("SD","MAD","IQR")){
  for(dataset in datasets){
    print(dataset)
    for(target_prop in c(0.01, 0.05, 0.1, 0.2)){
      if(dataset == "diabetes_indicators"){
        sample_size <- 1000 # downsampling
      }
      source("experiment_run_singleclust.R")
      # source("experiment_run.R")
      sample_size <- NA # restarting sample size
    }
  }
}

# saving results
results_dataframe <- do.call(rbind, results_list)
write.csv(results_dataframe,"results_experiments_single_aug.csv", row.names = FALSE)
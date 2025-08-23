get_auc <- function(cluster_vec, dist_matrix){
  anomaly_dists <- flag_relative_distance_anomalies(data, cluster_vec, dist_matrix, 
                                                    method="SD", lambda=1)$distances 
  # method can be ignored here, as we only need the distances for AUC
  
  roc_score <- AUC(anomaly_dists, data$target) # obtaining AUC scores
  names(roc_score) <- "AUCROC" # assigning a name to the result
  return(roc_score)
}

get_auc_train <- function(cluster_vec, dist_matrix){
  anomaly_dists <- flag_relative_distance_anomalies(train_data, cluster_vec, dist_matrix, 
                                                    method="SD", lambda=1)$distances 
  # method can be ignored here, as we only need the distances for AUC
  
  roc_score <- AUC(anomaly_dists, train_data$target) # obtaining AUC scores
  names(roc_score) <- "AUCROC" # assigning a name to the result
  return(roc_score)
}
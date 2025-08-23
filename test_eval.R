score_on_test <- function(medoids_def, thresholds_def, distance_metric = c("gower_matrix", "dafi_gower_matrix")){
  combined_data <- rbind(train_data[1:target_index], medoids_def[1:target_index], test_data)
  n_medoids <- nrow(medoids)
  n_test <- nrow(test_data)
  
  test_target <- as.factor(test_data$target)
  levels(test_target) <- c(0,1)
  
  if(distance_metric == "gower_matrix"){
    matrix_test <- as.matrix(daisy(combined_data[, -target_index], metric = "gower"))
    matrix_test <- matrix_test[(nrow(train_data)+1):nrow(matrix_test),(nrow(train_data)+1):nrow(matrix_test)]
    dist_test_to_medoids <- matrix_test[(n_medoids + 1):(n_medoids + n_test), 1:n_medoids] # distances to medoids
    closest_medoid_idx <- apply(dist_test_to_medoids, 1, which.min) # find the closest medoid for each test point
    dist_test_to_closest <- apply(dist_test_to_medoids, 1, min) # find the distance to closest medoid for each test point
    anomaly_flags_test <- dist_test_to_closest > thresholds_def[closest_medoid_idx]
    
    anomaly_pred <- as.factor(ifelse(anomaly_flags_test == "TRUE", 1, 0))
    levels(anomaly_pred) <- c(0,1)
    
    cm_test <- confusionMatrix(data=anomaly_pred, reference = test_target, positive="1")
    
    f1_test <- cm_test[["byClass"]]["F1"]
    ba_test <- cm_test$byClass["Balanced Accuracy"]
  }
  
  else if(distance_metric == "dafi_gower_matrix"){
    matrix_test <- gower.dist.modify(combined_data[, -target_index], var.weights = feature_importance, robcb = "iqr")
    matrix_test <- matrix_test[(nrow(train_data)+1):nrow(matrix_test),(nrow(train_data)+1):nrow(matrix_test)]
    dist_test_to_medoids <- matrix_test[(n_medoids + 1):(n_medoids + n_test), 1:n_medoids]
    closest_medoid_idx <- apply(dist_test_to_medoids, 1, which.min)
    dist_test_to_closest <- apply(dist_test_to_medoids, 1, min)
    anomaly_flags_test <- dist_test_to_closest > thresholds_def[closest_medoid_idx]
    
    anomaly_pred <- as.factor(ifelse(anomaly_flags_test == "TRUE", 1, 0))
    levels(anomaly_pred) <- c(0,1)
    
    cm_test <- confusionMatrix(data=anomaly_pred, reference = test_target, positive = "1")
    f1_test <- cm_test[["byClass"]]["F1"]
    ba_test <- cm_test$byClass["Balanced Accuracy"]
  }
  
  return(list(anomaly_flags_test = anomaly_flags_test, cm_test = cm_test, f1_test = f1_test, ba_test = ba_test))
}

score_on_eval <- function(medoids_def, thresholds_def, distance_metric = c("gower_matrix", "dafi_gower_matrix")){
  combined_data <- rbind(train_data[1:target_index], medoids_def[1:target_index], data_sample)
  n_train <- nrow(train_data)
  n_medoids <- nrow(medoids)
  n_test <- nrow(data_sample)
  
  test_target <- as.factor(data_sample$target)
  levels(test_target) <- c(0,1)
  
  if(distance_metric == "gower_matrix"){
    matrix_test <- as.matrix(daisy(combined_data[, -target_index], metric = "gower"))
    dist_test_to_medoids <- matrix_test[(n_train + n_medoids + 1):(n_train + n_medoids + n_test), (n_train+1):(n_train+n_medoids)] # distances to medoids
    closest_medoid_idx <- apply(dist_test_to_medoids, 1, which.min) # find the closest medoid for each test point
    dist_test_to_closest <- apply(dist_test_to_medoids, 1, min) # find the distance to closest medoid for each test point
    anomaly_flags_test <- dist_test_to_closest > thresholds_def[closest_medoid_idx]
    
    anomaly_pred <- as.factor(ifelse(anomaly_flags_test == "TRUE", 1, 0))
    levels(anomaly_pred) <- c(0,1)
    
    cm_test <- confusionMatrix(data=anomaly_pred, reference = test_target, positive= "1")
    
    f1_test <- cm_test[["byClass"]]["F1"]
    ba_test <- cm_test$byClass["Balanced Accuracy"]
  }
  
  else if(distance_metric == "dafi_gower_matrix"){
    matrix_test <- gower.dist.modify(combined_data[, -target_index], var.weights = feature_importance, robcb = "iqr")
    matrix_test <- matrix_test[(nrow(train_data)+1):nrow(matrix_test),(nrow(train_data)+1):nrow(matrix_test)]
    dist_test_to_medoids <- matrix_test[(n_medoids + 1):(n_medoids + n_test), 1:n_medoids]
    closest_medoid_idx <- apply(dist_test_to_medoids, 1, which.min)
    dist_test_to_closest <- apply(dist_test_to_medoids, 1, min)
    anomaly_flags_test <- dist_test_to_closest > thresholds_def[closest_medoid_idx]
    
    anomaly_pred <- as.factor(ifelse(anomaly_flags_test == "TRUE", 1, 0))
    levels(anomaly_pred) <- c(0,1)
    
    cm_test <- confusionMatrix(data=anomaly_pred, reference = test_target, positive = "1")
    f1_test <- cm_test[["byClass"]]["F1"]
    ba_test <- cm_test$byClass["Balanced Accuracy"]
  }
  
  return(list(anomaly_flags_test = anomaly_flags_test, cm_test = cm_test, f1_test = f1_test, ba_test = ba_test))
}

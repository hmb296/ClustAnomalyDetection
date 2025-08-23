flag_relative_distance_anomalies <- function(data, cluster_vector, distance_matrix,
                                             method=c("SD","MAD","IQR"), lambda=NULL) {
  distance_matrix <- as.matrix(distance_matrix)
  
  n <- nrow(data)
  distances <- rep(NA, n)
  anomaly_flags <- rep(FALSE, n)
  
  unique_clusters <- sort(unique(cluster_vector)) # saving unique cluster names (in case not numbered 1, 2, 3, ...)
  thresholds <- rep(NA, length(unique_clusters)) # initialising a vector for thresholds per cluster
  medoids_ind <- rep(NA, length(unique_clusters)) # initialising a vector for medoid indices per cluster
  
  for (i in seq_along(unique_clusters)) {
    ind_clust <- which(cluster_vector==unique_clusters[i]) # extracting samples in the current cluster
    
    if (length(ind_clust) <= 1) {# case when cluster has only 1 observation
      cluster_dists <- 0
      distances[ind_clust] <- 0
      medoids_ind[i] <- ind_clust
    }
    else{
      # finding medoid:
      sub_dist <- distance_matrix[ind_clust, ind_clust] # sub-matrix - distances in the current cluster
      medoid_local_ind <- which.min(rowSums(sub_dist)) # find medoid in the sub-matrix
      medoid_global_ind <- ind_clust[medoid_local_ind] # locate the medoid in the full matrix
      
      # saving medoid information:
      medoids_ind[i] <- medoid_global_ind # saving medoid index
      cluster_dists <- distance_matrix[ind_clust, medoid_global_ind] # extracting distances to this medoid
      distances[ind_clust] <- cluster_dists # saving cluster-specific distances
    }
    
    # compute cluster-specific threshold using the specified method
    if (method == "SD") {
      threshold <- mean(cluster_dists) + lambda * sd(cluster_dists)
    } 
    
    else if (method == "MAD") {
      med <- median(cluster_dists)
      mad_val <- median(abs(cluster_dists - med))
      threshold <- med + lambda * 1.4826 * mad_val
    } 
    
    else if (method == "IQR") {
      q1 <- quantile(cluster_dists, 0.25)
      q3 <- quantile(cluster_dists, 0.75)
      iqr_val <- q3 - q1
      threshold <- q3 + lambda * iqr_val
    }
    
    anomaly_flags[ind_clust] <- cluster_dists > threshold
    thresholds[i] <- threshold
  }
  
  # naming output lists
  names(medoids_ind) <- unique_clusters
  names(thresholds) <- unique_clusters
  
  return(list(distances = distances, anomaly_flags = anomaly_flags, 
              thresholds = thresholds, medoids_ind = medoids_ind))
}

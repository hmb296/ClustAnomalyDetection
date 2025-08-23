set.seed(123)
tune_k_sil <- function(method = c("pam", "kproto", "kamila"), distance_matrix, data=NULL, 
                       numeric_data=NULL, categorical_data=NULL, k_range = 2:10, plot=TRUE) {
  # a function for calculating silhouette scores for a range of k values and 
  # identifying the optimal k for one of the three methods: "pam", "kproto", "kamila"
  sil_scores <- numeric(length(k_range)) # initialising a vector for silhouette scores
  names(sil_scores) <- k_range

  for (i in seq_along(k_range)) {
    k <- k_range[i]
    if (method == "pam") {
      pam_res <- pam(distance_matrix, k = k, diss = TRUE)
      sil <- silhouette(pam_res$clustering, distance_matrix, full=1)
      sil_scores[i] <- mean(sil[, 3])
      
    } else if (method == "kproto") {
      kproto_res <- kproto(data, k = k, verbose = FALSE, type="gower")
      sil <- silhouette(kproto_res$cluster, distance_matrix)
      sil_scores[i] <- mean(sil[, 3])
      
    } else if (method == "kamila") {
      set.seed(123)
      kamila_res <- kamila(conVar = numeric_data, catFactor = categorical_data, numClust = k, numInit = 10)
      sil <- silhouette(kamila_res$finalMemb, distance_matrix)
      sil_scores[i] <- mean(sil[, 3])
    }
  }
  
  optimal_k <- k_range[which.max(sil_scores)]
  
  if(plot==TRUE){
    plot(k_range, sil_scores, type = "b", pch =19,
         xlab = "Number of clusters (k)", ylab = "Avg. silhouette width",
         main = paste("Cluster quality for", toupper(method)))
  }
  return(optimal_k)
}
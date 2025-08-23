adjust_prop <- function(target_prop, target_col, data, sample_size=NA){
  if(is.na(target_prop)){ # calculating initial target proportion
    target_prop <- length(which(target_col==1))/length(target_col)
  }
  if((!is.na(sample_size)) & (nrow(data) >= sample_size)){
    # stratified sampling of n samples:
    set.seed(123)
    rows_pos <- sample(which(target_col==1), size=floor(target_prop*sample_size))
    rows_neg <- sample(which(target_col!=1), size=floor((1-target_prop)*sample_size))
    rows_sample <- sort(c(rows_pos, rows_neg))
    data_sample <- data[rows_sample, ]
    return(data_sample)
  }
  else{
    n <- nrow(data) # initial total count
    n_pos <- sum(target_col==1) # initial number of positive examples
    n_pos_keep <- target_prop * n # target number of positive examples
    
    if(n_pos_keep <= n_pos){
      set.seed(123)
      rows_pos <- sample(which(target_col==1), n_pos_keep)
      rows_neg <- which(target_col!=1)
    }
    else{
      n_neg_keep <- n_pos/target_prop - n_pos
      set.seed(123)
      rows_neg <- sample(which(target_col!=1), n_neg_keep)
      rows_pos <- which(target_col==1)
    }
    sample_keep <- sort(c(rows_pos, rows_neg))
    data_sample <- data[sample_keep,]
    return(data_sample)
  }
}
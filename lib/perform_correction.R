do_correction <- function(left_token, cur_token, right_token){
  # pc, ptc, plc, prc
  if (is.na(left_token)){
    plc <- 1
  }
  if (is.na(right_token)){
    prc <- 1
  }
  
  

  return(corrected)
}

do_correction_on_vec <- function(vec, clean_ind){
  n <- length(vec)
  lr_mat <- matrix(NA, ncol = 3, nrow = sum(!clean_ind))
  for (i in 1:n){
    if (!clean_ind[i]){
        lr_mat[i,] <- c(ifelse(i==1,NA,vec[i-1]), vec[i], ifelse(i==n, NA,vec[i+1]))
    }
  }
  corrected_errors <- apply(lr_mat, 1, do_correction)
  output_vec <- vec
  output_vec[!clean_ind] <- corrected_errors
  return(output_vec)
}
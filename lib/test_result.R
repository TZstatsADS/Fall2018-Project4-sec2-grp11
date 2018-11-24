get_result <- function(file_id, tokens_list, labels_list, predict_list){
  tp_err_list <- list()
  tn_err_list <- list()
  fp_err_list <- list()
  fn_err_list <- list()
  # predicted_err_list <- list()
  
  for(i in 1:length(test_tokens_list)){
    tp_err_list[[i]] <- tokens_list[[i]][(!predict_list[[i]]) & (!labels_list[[i]])]
    tn_err_list[[i]] <- tokens_list[[i]][predict_list[[i]] & labels_list[[i]]]
    fp_err_list[[i]] <- tokens_list[[i]][(!predict_list[[i]]) & labels_list[[i]]]
    fn_err_list[[i]] <- tokens_list[[i]][predict_list[[i]] & (!labels_list[[i]])]
    # predicted_err_list[[i]] <- tokens_list[[i]][!predict_list[[i]]]
  }
  return(list(tp_err_list, tn_err_list, fp_err_list, fn_err_list))
}
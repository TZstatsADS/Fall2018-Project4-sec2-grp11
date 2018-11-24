doc_match <- function(tesseract_doc, truth_doc){
  
  l_tess <- length(tesseract_doc)
  l_truth <- length(truth_doc)
  
  doc_clean_ind_list <- list()
  line_diff <- 0
  if(l_tess == l_truth){
    for (i in 1:l_tess){
      line_clean_ind <- ifelse(tesseract_doc[[i]] %in% truth_doc[[i]],TRUE,FALSE)
      doc_clean_ind_list[[i]] <- line_clean_ind
    }
  }
  else{
    line_diff <- l_tess - l_truth
    for (i in 1:l_tess){
      line_clean_ind <- ifelse(tesseract_doc[[i]] %in% unlist(truth_doc[i:(i+abs(line_diff))]),TRUE,FALSE)
      doc_clean_ind_list[[i]] <- line_clean_ind
    }
  }
  return(list("doc_clean_ind" = doc_clean_ind_list, "line_diff" = line_diff))
}
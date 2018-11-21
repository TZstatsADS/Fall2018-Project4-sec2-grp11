doc_match <- function(tesseract_doc, truth_doc){
  doc_clean_ind <- c()
  line_equal <- TRUE
  if(length(tesseract_doc)==length(truth_doc)){
    for (i in 1:length(tesseract_doc)){
      line_clean_ind <- ifelse(tesseract_doc[[i]] %in% truth_doc[[i]],TRUE,FALSE)
      doc_clean_ind <- c(doc_clean_ind,line_clean_ind)
    }
  }
  else{
    tesseract_vec <- unlist(tesseract_doc)
    truth_vec <- unlist(truth_doc)
    doc_clean_ind <- ifelse(tesseract_vec %in% truth_vec, TRUE, FALSE)
    line_equal <- FALSE
  }
  return(list("doc_clean_ind" = doc_clean_ind, "line_equal" = line_equal))
}
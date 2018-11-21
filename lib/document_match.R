doc_match <- function(tesseract_doc, truth_doc){
  doc_clean_ind <- c()
  line_equal <- TRUE
  line_diff <- 0
  if(length(tesseract_doc)==length(truth_doc)){
    for (i in 1:length(tesseract_doc)){
      line_clean_ind <- ifelse(tesseract_doc[[i]] %in% truth_doc[[i]],TRUE,FALSE)
      doc_clean_ind <- c(doc_clean_ind,line_clean_ind)
    }
  }
  else{
    line_equal <- FALSE
    line_diff <- length(tesseract_doc) - length(truth_doc)
    for (i in 1:length(tesseract_doc)){
      if(i>1 & i<length(tesseract_doc)){
        line_clean_ind <- ifelse(tesseract_doc[[i]] %in% c(truth_doc[[i-1]],truth_doc[[i]],truth_doc[[i+1]]),TRUE,FALSE)
        
      }
      else if(i==1){
        line_clean_ind <- ifelse(tesseract_doc[[i]] %in% c(truth_doc[[i]],truth_doc[[i+1]]),TRUE,FALSE)
      }
      else{
        line_clean_ind <- ifelse(tesseract_doc[[i]] %in% c(truth_doc[[i-1]], truth_doc[[i]]),TRUE,FALSE)
      }
      doc_clean_ind <- c(doc_clean_ind,line_clean_ind)
    }
  }
  return(list("doc_clean_ind" = doc_clean_ind, "line_equal" = line_equal, "line_diff" = line_diff))
}
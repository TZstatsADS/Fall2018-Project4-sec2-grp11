count_char <- function(vec){
  string = paste(vec, collapse = " ")
  chars <- strsplit(string, "")[[1]]
  chars_useful <- chars[grepl("[a-zA-Z]|[[:digit:]]|[[:space:]]", chars)]
  chars_useful <- chars_useful[chars_useful != "\f"]
  chars_useful <- ifelse(chars_useful==" ", "space", chars_useful)
  chars_useful <- factor(chars_useful, levels = c(letters, LETTERS, "space", "0","1","2","3","4","5","6","7","8","9"))
  count_table <- table(chars_useful)
  cnts <- as.vector(count_table)
  names(cnts) <- c(letters, LETTERS, "space", "0","1","2","3","4","5","6","7","8","9")
  return(cnts)
}

# test_ground_truth_char_table <- count_char(unlist(test_ground_truth_list_vec))
# test_tesseract_char_table <- count_char(unlist(test_tesseract_list_vec))
# old_char_ratio <- test_tesseract_char_table/test_ground_truth_char_table
# old_char_ratio_sorted <- sort(old_char_ratio, decreasing = TRUE)
# old_char_ratio_sorted
# old_char_ratio_sorted_mat <- matrix(old_char_ratio_sorted, ncol = 9, nrow = 7, byrow = TRUE)
# barplot(old_char_ratio_sorted)
# old_char_ratio_sorted_df <- as.data.frame(old_char_ratio_sorted)
# names(old_char_ratio_sorted_df) <-"ratio"
# old_char_ratio_sorted_df$character <- factor(rownames(old_char_ratio_sorted_df), levels = names(old_char_ratio_sorted))
# library(ggplot2)
# ggplot(data = old_char_ratio_sorted_df)+
#   geom_bar(stat = "identity", aes(x=character,y = ratio))+
#   ggtitle("Ratio of tesseract to ground truth for number of each alphanumeric character")

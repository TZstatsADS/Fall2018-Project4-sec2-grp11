stringsplit_1st <- function(line){
  return(strsplit(line, " ")[[1]])
}

cut_bigram <- function(start_pos, token){
  return(substr(token, start = start_pos, stop = start_pos+1))
}

bigram_from_token <- function(input_string){
  nb <- nchar(input_string)-1
  if(nb >= 2) { bigram <- sapply(1:nb, cut_bigram, input_string) }
  else { bigram <- input_string }
  return(bigram)
}

num_pattern <- function(candidate, cur_token){
  ldist <- adist(cur_token, candidate, counts = TRUE)
  return(sum(as.vector(attr(ldist, "counts")) != 0))
}

extract_feature <- function(cur_token, freq_bigrams, truth_set, include_dist = TRUE){

  feature_list <- rep(list(NULL),13)
  
  #feature 1
  l <- nchar(cur_token)
  feature_list[[1]] <- l
  
  #feature 2
  vowels <- '[aeiouAEIOU]'
  consonants <- '[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]'
  v <- str_count(cur_token, pattern = vowels)
  c <- str_count(cur_token, pattern = consonants)
  feature_list[[2]] <- c(v,c,v/l,c/l,ifelse(c>0,v/c,100))
  
  #feature 3
  s <- str_count(cur_token, pattern = '[^[:alnum:]]')
  feature_list[[3]] <- c(s, s/l)
  
  #feature 4
  d <- str_count(cur_token, pattern = '[0-9]')
  feature_list[[4]] <- c(d, d/l)
  
  #feature 5
  low <- str_count(cur_token, pattern = '[:lower:]')
  upp <- str_count(cur_token, pattern = '[:upper:]')
  feature_list[[5]] <- c(low, upp, low/l, upp/l)

  #feature 6
  string_split <- strsplit(cur_token, "")[[1]]
  consecutive_cnt <- rle(string_split)
  max_seq <- max(consecutive_cnt[[1]])
  feature_list[[6]] <- ifelse(max_seq>=3, max_seq/l, 0)

  #feature 7
  la <- str_count(cur_token, pattern = '[:alnum:]')
  feature_list[[7]] <- ifelse(s>la, 1, 0)

  #feature 8
  conso_token <- gsub(consonants, "b", cur_token)
  consecutive_conso <- rle(strsplit(conso_token, "")[[1]])
  max_conso_seq <- max(c(0, consecutive_conso[[1]][consecutive_conso[[2]] == "b"]))
  feature_list[[8]] <- ifelse(max_conso_seq>=6, 1, 0)

  #feature 9 
  token_infix <- substr(cur_token, start = 2, stop = l-1)
  n <- str_count(token_infix, pattern = '[^[:alnum:]]')
  feature_list[[9]] <- ifelse(n>=2, 1, 0)

  #feature 10 bigram
  cur_bigrams <- bigram_from_token(tolower(cur_token))
  n <- length(cur_bigrams)
  if(n == 1) { if(nchar(cur_bigrams) == 1) n <- 0.5 }
  feature_list[[10]] <- (sum(freq_bigrams[cur_bigrams],na.rm = TRUE)/10000)/n

  #feature 11
  max_freq <- max(table(string_split))
  feature_list[[11]] <- ifelse(max_freq>=3, max_freq/l, 0)

  #feature 12
  l1 <- str_count(cur_token, pattern = '[:alpha:]')
  l2 <- l - l1
  feature_list[[12]] <- ifelse(l1>0, l2/l1, 100)
  
  #feature 13 leveshtein distance
  if(include_dist){
    all.words <- unique(c(english.words, truth_set))
    nv <- levenshtein.distance(tolower(cur_token), all.words)
    min_dist <- min(nv)
    if(min_dist > 2) { f13 <- l }
    else {
      candidate.words <- all.words[which.min(nv)]
      p <- min(sapply(candidate.words, num_pattern, cur_token))
      f13 <- (min_dist+(p/2)+1)/l
    }
    feature_list[[13]] <- f13
  }
  ft <- unlist(feature_list)
  return(ft)
}

token_to_input_format <- function(vec, freq_bigrams, truth_set){
  input_format <- matrix(unlist(lapply(vec,extract_feature,freq_bigrams,truth_set)), byrow = TRUE, nrow = length(vec))
  return(input_format)
}

get_line_input <- function(token, input_matrix){
  return(input_matrix[which(rownames(input_matrix) == token),])
}
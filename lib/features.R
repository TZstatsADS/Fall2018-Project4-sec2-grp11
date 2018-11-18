bigram_from_token <- function(input_string){
  nb <- nchar(input_string)-1
  if(nb>0){
    bigram <- rep(NA, nb)
    for(i in 1:nb){
      bigram[i]<-substr(input_string,start=i,stop=i+1)
    }
  }
  else{
    bigram = input_string
  }
  return(bigram)
}

bigram_freq<-function(bigram, all_bigrams){
  return(sum(all_bigrams == bigram))
}

extract_feature <- function(cur_token, all_correct_bigrams){
  
  feature_list <- rep(list(NULL),13)
  
  #feature 1
  l <- nchar(cur_token)
  feature_list[[1]] <- l
  
  #feature 2
  vowels <- '[aeiouAEIOU]'
  consonants <- '[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]'
  v <- str_count(cur_token, pattern = vowels)
  c <- str_count(cur_token, pattern = consonants)
  feature_list[[2]] <- c(v,c,v/l,c/l,ifelse(c>0,v/c,10000))
  
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
  count <- 0
  prev_symbol <- NULL
  max_seq <- 0
  for(i in 1:l){
    cur_symbol <- string_split[i]
    if(is.null(prev_symbol)){
      count <- count + 1
      if (count >= 3 & count > max_seq){
        max_seq <- count
      }
    }
    else if(cur_symbol == prev_symbol){
      count <- count + 1
      if (count >= 3 & count > max_seq){
        max_seq <- count
      }
    }
    else{
      count <- 0
    }
    
    prev_symbol <- cur_symbol
  }
  feature_list[[6]] <- (max_seq/l)
  
  #feature 7
  la <- str_count(cur_token, pattern = '[:alnum:]')
  k <- s
  feature_list[[7]] <- ifelse(k>la, 1, 0)
  
  #feature 8
  count <- 0
  f8 <- 0
  conso_vec <- strsplit(consonants, "")[[1]][-c(1, nchar(consonants))]
  for(i in 1:l){
    cur_symbol <- string_split[i]
    
    if(cur_symbol %in% conso_vec){
      count <- count + 1
      if(count >= 6){
        f8 <- 1
        break
      }
    }
    else{
      count <- 0
    }
  }
  feature_list[[8]] <- f8
  
  #feature 9 
  token_infix <- substr(cur_token, start = 2, stop = l-1)
  n <- str_count(token_infix, pattern = '[^[:alnum:]]')
  f9 <- ifelse(n>=2, 1, 0)
  feature_list[[9]] <- f9
  
  #feature 10 bigram
  # N<-length(all_correct_tokens)
  cur_bigrams <- bigram_from_token(cur_token)
  
  bf <- unlist(lapply(cur_bigrams, bigram_freq, all_correct_bigrams))
  n <- length(cur_bigrams)
  bigr <- sum(bf)/n/10000
  # bf <- rep(0,n)
  # for(i in 1:n){
  #     bf[i]<- sum(all_correct_bigrams == cur_bigrams[i])
  # }
  
  feature_list[[10]] <- bigr
  
  #feature 11
  symbol_list <- list()
  for(j in 1:l){
    cur_symbol <- string_split[j]
    if (cur_symbol %in% names(symbol_list)){
      symbol_list[[cur_symbol]] <- symbol_list[[cur_symbol]]+1
    }
    else{
      symbol_list[[cur_symbol]] <- 1
    }
  }
  i <- max(unlist(symbol_list))
  feature_list[[11]] <- ifelse(i>=3, i/l, 0)
  
  #feature 12
  l1 <- str_count(cur_token, pattern = '[:alpha:]')
  l2 <- l - l1
  feature_list[[12]] <- ifelse(l1>0, l2/l1, 10000)
  
  #feature 13 leveshtein distance
  nv <- min(levenshtein.distance(cur_token, ground_truth_set))
  feature_list[[13]] <-(nv+1)/l
  
  ft = unlist(feature_list)
  return(ft)
}
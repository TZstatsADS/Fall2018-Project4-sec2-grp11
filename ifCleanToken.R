##############################
## Garbage detection
## Ref: first three rules in the paper
##      'On Retrieving Legal Files: Shortening Documents and Weeding Out Garbage'
## Input: one word -- token
## Output: bool -- if the token is clean or not
##############################
ifCleanToken <- function(cur_token){
  
  feature_list <- rep(list(NULL),12)
  
  #feature 1
  l <- nchar(cur_token)
  feature_list[[1]] <- l
  
  #feature 2
  vowels <- '[aeiouAEIOU]'
  consonants <- '[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]'
  v <- str_count(cur_token, pattern = vowels)
  c <- str_count(cur_token, pattern = consonants)
  feature_list[[2]] <- c(v,c,v/l,c/l, ifelse(c > 0, v/c, NULL))
  
  #feature 3
  s <- str_count(cur_token, pattern = '[^[:alnum:]]')
  s/l
  feature_list[[3]] <- c(s, s/l)
  
  #feature 4
  d <- str_count(cur_token, pattern = '[0-9]')
  d/l
  feature_list[[4]] <- c(d, d/l)
  
  #feature 5
  low <- str_count(cur_token, pattern = '[:lower:]')
  upp <- str_count(cur_token, pattern = '[:upper:]')
  feature_list[[5]] <- c(low, upp, low/l, upp/l)
  
  #feature 6
  string_split <- strsplit(curtoken, "")[[1]]
  count <- 0
  prev_symbol <- NULL
  max_seq <- 0
  for(i in 1:l){
    cur_symbol <- string_split[i]
    if(is.null(prev_symbol) | cur_symbol == prev_symbol){
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

  #feature 10

  #feature 11
  symbol_list <- list()
  for(j in 1:l){
    cur_symbol <- string_split[j]
    if (cur_symbol %in% names(list)){
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
  feature_list[[12]] <- l2/l1
}

# ifCleanToken <- function(cur_token){
#   now <- 1
#   if_clean <- TRUE
#   
#   ## in order to accelerate the computation, conduct ealy stopping
#   rule_list <- c("str_count(cur_token, pattern = '[A-Za-z0-9]') <= 0.5*nchar(cur_token)", # If the number of punctuation characters in a string is greater than the number of alphanumeric characters, it is garbage
#                  "length(unique(strsplit(gsub('[A-Za-z0-9]','',substr(cur_token, 2, nchar(cur_token)-1)),'')[[1]]))>1", #Ignoring the first and last characters in a string, if there are two or more different punctuation characters in thestring, it is garbage
#                  "nchar(cur_token)>20") #A string composed of more than 20 symbols is garbage 
#   while((if_clean == TRUE)&now<=length(rule_list)){
#     if(eval(parse(text = rule_list[now]))){
#       if_clean <- FALSE
#     }
#     now <- now + 1
#   }
#   return(if_clean)
# }
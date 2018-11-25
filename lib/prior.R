# Pr(c)
#freq(c)
################################################
#use train_truth_unique to make deletion tables
################################################
del_char <- function(pos, token){
  nc <- nchar(token)
  if(pos == 1){
    deleted <- substr(token, start = 2, stop = nc)
  } else if (pos == nc){
    deleted <- substr(token, start = 1, stop = nc-1)
  } else{
    deleted <- paste(substr(token, start = 1, stop = pos-1), substr(token, start = pos+1, stop = nc), sep = "")
  }
  grab_letter <- substr(token, start = pos, stop = pos)
  return(c(deleted,grab_letter))
}

make_deltable <- function(cur_token){
  l <- nchar(cur_token)
  deltable <- NULL
  if (l > 1){
    deltable <- as.data.frame(matrix(NA, ncol = 3, nrow = l))
    names(deltable) <- c("key", "letter", "position")
    deltable[,3] <- 0:(l-1)
    for (i in l:1){
      deltable[i,1:2] <- del_char(i, cur_token)
    }
    
  }
  return(deltable)
}

# bind_table<- function(word, old_table){
#   new_table <- rbind(old_table, make_deltable)
#   return(new_table)
# }

all_deltable <- function(all_words){
  whole_table <- NULL
  for (i in 1:length(all_words)){
    cur_word <- all_words[i]
    whole_table <- rbind(whole_table, make_deltable(cur_word))
  }
  return(whole_table)
}

hash_time <- system.time((deletion_table <- all_deltable(unique(english.words))))
write.csv(deletion_table, file = "../output/deletion_table.csv")

#search
deltable <- read.csv("../output/deletion_table.csv")[,-1]

check_del <- function(cur_token){
  candidates_table <- as.matrix(deltable[deltable$key == cur_token,])
  candidates <- rep(NA, nrow(candidates_table))
  for(i in 1:nrow(candidates_table)){
    key <- candidates_table[i,1]
    letter <- candidates_table[i,2]
    pos <- as.numeric(candidates_table[i,3])
    l <- nchar(key)
    if (pos == 0){
      candidates[i] <- paste(letter, key, sep = "")
    } else if (pos == l){
      candidates[i] <- paste(key, letter, sep = "")
    } else {
      former <- substr(key, start = 1, stop = pos)
      latter <- substr(key, start = (pos+1), stop = l)
      candidates[i] <- paste(former, letter, latter, sep = "")
    }
  }
  return(candidates)
}

check_del("appy")
################################################
#add function
################################################
dict <- unique(english.words)

check_add <- function(cur_token){
  nc <- nchar(cur_token)
  candidates <- rep(NA, nc)
  for(i in 1:nc){
    candidates[i] <- del_char(i, cur_token)[1]
  }
  candidates <- candidates[candidates %in% dict]
  return(candidates)
}
################################################
#reverse function
################################################
check_rev <- function(cur_token){
  nc <- nchar(cur_token)
  if (nc < 2) {
    return(NULL)
  } 
  else {
    candidates <- rep(NA, nc - 1)
    chars <- strsplit(cur_token, "")[[1]]
    if(nc==2){
      candidates <- paste(chars[2], chars[1], sep = "")
    }
    else{ 
      for(i in 1: (nc-1)){
        if(i==1){
          candidates[i] <- paste(chars[2], chars[1], substr(cur_token, 3, nc), sep = "")
        }
        else if (i== nc-1){
          candidates[i] <- paste(substr(cur_token, 1, nc-2), chars[nc], chars[nc-1], sep = "")
        }
        else{
          candidates[i] <- paste(substr(cur_token, 1, i-1), chars[i+1], chars[i], substr(cur_token, i+2, nc), sep = "")
        }
      }
    }
  }
  candidates <- candidates[candidates %in% dict]
  return(candidates)
}
################################################
#substitution function
################################################
sub_char <- function(char, pos, token){
  
}

check_sub <- function(cur_token){
  nc <- nchar(cur_token)
  candidates <- rep(NA, nc*25)
  for(i in 1:nc)
}





# rebuild_word <- function(items){
#   wd <- NULL
#   key <- items$
#   letter <- items[2]
#   pos <- as.numeric(items[3])
#   l <- nchar(key)
#   if (pos == 0){
#     wd <- paste(letter, key, sep = "")
#   } else if (pos == l){
#     wd <- paste(key, letter, sep = "")
#   } else {
#     former <- substr(key, start = 1, stop = pos)
#     latter <- substr(key, start = (pos+1), stop = l)
#     wd <- paste(former, letter, latter, sep = "")
#   }
#   return(wd)
# }
# 
# search_deletion <- function(cur_token){
#   candidates_table <- as.matrix(deltable[deltable$key == cur_token,])
#   candidates <- apply(candidates_table, 2, rebuild_word)
#   return(candidates)
# }
# Pr(c)
#freq(c)
################################################

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
tim_deltable <- hash_time
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

# check_del("appy")
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
#reverse table
do_reverse <- function(pos, nc, token){
  if(nc < 2 | pos >= nc | pos <= 0){
    return(NULL)
  } else if(nc==2){
    rev <- paste(substr(token, 2, 2), substr(token, 1, 1), sep = "")
  } else {
    if(pos==1){
      rev <- paste(substr(token, 2, 2), substr(token, 1, 1), substr(token, 3, nc), sep = "")
    } else if (pos== nc-1){
      rev <- paste(substr(token, 1, nc-2), substr(token, nc, nc), substr(token, nc-1, nc-1), sep = "")
    } else{
      rev <- paste(substr(token, 1, pos-1), substr(token, pos+1, pos+1), substr(token, pos, pos), substr(token, pos+2, nc), sep = "")
    }
  }
  return(rev)
}

make_revtable <- function(cur_token){
  nc <- nchar(cur_token)
  if (nc < 2) {
    return(NULL)
  } 
  else {
    reversed <- unlist(lapply(1:(nc-1), do_reverse, nc, cur_token))
  }
  revtable <- data.frame(reversed_token = reversed, start_position = 1:(nc-1))
  return(revtable)
}

all_revtable <- function(all_words){
  whole_table <- NULL
  for (i in 1:length(all_words)){
    cur_word <- all_words[i]
    whole_table <- rbind(whole_table, make_revtable(cur_word))
  }
  return(whole_table)
}

time_revtable <- system.time((reversion_table <- all_revtable(dict)))
write.csv(reversion_table, file = "../output/reversion_table.csv")

check_rev <- function(cur_token){
  candidates_table <- as.matrix(revtable[revtable$reversed_token == cur_token,])
  candidates <- rep(NA, nrow(candidates_table))
  for(i in 1:nrow(candidates_table)){
    wd <- candidates_table[i,1]
    p <- as.numeric(candidates_table[i,2])
    n <- nchar(wd)
    candidates[i] <- do_reverse(p, n, wd)
  }
  return(candidates)
}
#########################################################################################
#directly check reverse
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
do_sub <- function(letter, pos, token, nc){
  subed <- NULL
  
  if(pos<= 0 | pos > nc){
    return(NULL)
  }
  
  if (nc==1){
    subed <- letter
  } else if (nc >= 2){
    if (pos == 1){
      subed <- paste(letter, substr(token, 2, nc), sep = "")
    } else if (pos == nc){
      subed <- paste(substr(token, 1, nc-1), letter, sep = "")
    } else {
      subed <- paste(substr(token, 1, pos-1),letter, substr(token, pos+1, nc), sep = "")
    }
  }
  return(subed)
}

make_subtable <- function(cur_token){
  nc <- nchar(cur_token)
  if(nc == 1){
    subs <- letters[letters != cur_token]
    posi <- rep(1, 25)
    true_letter <- rep(cur_token,25)
  }
  else{
    subs <- matrix(NA, ncol = 25, nrow = nc)
    posi <- matrix(rep(1:nc, 25), ncol = 25, nrow = nc)
    true_letter <- matrix(NA, ncol = 25, nrow = nc)
    for(i in 1:nc){
      cur_char <- substr(cur_token, i, i)
      true_letter[i,] <- rep(cur_char, 25)
      choices <- letters[letters != cur_char]
      subs[i,] <- unlist(lapply(choices, do_sub, i, cur_token, nc))
    }
    subs <- as.vector(subs)
    posi <- as.vector(posi)
    true_letter <- as.vector(true_letter)
  }
  subst_table <- data.frame(substituted = subs, position = posi, true_letter = true_letter)
  return(subst_table)
}

check_sub <- function(cur_token, subtable){
  candidates_table <- as.matrix(subtable[subtable$substituted == cur_token,])
  candidates <- rep(NA, nrow(candidates_table))
  for(i in 1:nrow(candidates_table)){
    subed <- candidates_table[i,1]
    p <- as.numeric(candidates_table[i,2])
    c <- candidates_table[i,3]
    l <- nchar(subed)
    candidates[i] <- do_sub(c, p, subed, l)
  }
  return(candidates)
}
# make_subtable <- function(cur_token){
#   nc <- nchar(cur_token)
#   if(nc == 1){
#     subs <- letters[letters != cur_token]
#     posi <- rep(1, 25)
#     true_letter <- rep(cur_token,25)
#   }
#   else{
#     subs <- matrix(NA, ncol = 25, nrow = nc)
#     posi <- matrix(rep(1:nc, 25), ncol = 25, nrow = nc)
#     true_letter <- matrix(NA, ncol = 25, nrow = nc)
#     for(i in 1:nc){
#       cur_char <- substr(cur_token, i, i)
#       true_letter[i,] <- rep(cur_char, 25)
#       choices <- letters[letters != cur_char]
#       for (j in 1:25){
#         if (i == 1){
#           subs[i,j] <- paste(choices[j], substr(cur_token, 2, nc), sep = "")
#         } else if (i == nc){
#           subs[i,j] <- paste(substr(cur_token, 1, nc-1), choices[j], sep = "")
#         } else {
#           subs[i,j] <- paste(substr(cur_token, 1, i-1), choices[j], substr(cur_token, i+1, nc), sep = "")
#         }
#       }
#     }
#   subs <- as.vector(subs)
#   posi <- as.vector(posi)
#   true_letter <- as.vector(true_letter)
#   }
#   subst_table <- data.frame(substituted = subs, position = posi, true_letter = true_letter)
#   return(subst_table)
# }

all_subtable <- function(all_words){
  whole_table <- NULL
  for (i in 1:length(all_words)){
    cur_word <- all_words[i]
    whole_table <- rbind(whole_table, make_subtable(cur_word))
  }
  return(whole_table)
}

dict_alpha <- dict[!(1:length(dict) %in% grep('[^[:alpha:]]',dict))]

hash_subtable <- system.time((substitution_table <- all_subtable(dict_alpha)))
write.csv(substitution_table, file = "../output/substitution_table.csv")

# sub_char <- function(char, pos, token){
#   
# }
# 
# check_sub <- function(cur_token){
#   nc <- nchar(cur_token)
#   candidates <- rep(NA, nc*25)
#   for(i in 1:nc)
# }





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


#############################################################################
#levenshtein distance
library(vwr)

levenshtein.neighbors("1nvolve", english.words)[1:2]
levenshtein.neighbors("nvolve", english.words)[1:2]
levenshtein.neighbors("nivolve", english.words)[1:2]

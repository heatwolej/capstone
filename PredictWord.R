## Predict the Next Word

#limit <- 10

findMatches <- function(len, phrase, limit) {
  if (len == 0 || len > 3) {
    num=0
  }
  if (len == 3) {
    result <- quad_dat[gram==phrase]
    if (nrow(result) > limit) {
      result <- head(result, limit)
    }
    num=nrow(result)
    if (num > 0) {
      suggest <- result$quadgram
    }
  } else if(len==2) {
    result <- tri_dat[gram==phrase]
    if (nrow(result) > limit) {
      result <- head(result, limit)
    }
    num=nrow(result)
    if (num > 0) {
      suggest <- result$trigram
    }
  } else if(len==1) {
    result <- bi_dat[gram==phrase]
    if (nrow(result) > limit) {
      result <- head(result, limit)
    }
    num=nrow(result)
    if (num > 0) {
      suggest <- as.character(levels(result$bigram))[result$bigram]
    }
  }
  if (num==0) {
    suggest="No Matches Found"
  } else {
    for (i in 1:num) {
      suggest[i] <- stripLastWord(suggest[i])
    }
  }
  return(suggest)
}

stripFirstWord <- function(phrase) {
  tokens = unlist(strsplit(phrase, '\\s+'))
  ntokens = length(tokens)
  return(paste(tokens[1:(ntokens-1)]), collapse = ' ')
}

stripLastWord <- function(phrase) {
  tokens = unlist(strsplit(phrase, '\\s+'))
  ntokens = length(tokens)
  return(tokens[ntokens])
}

predictWord <- function(phrase, limit) {
  topten <- head(uni_dat, limit)
  toplist <- rep(NA, limit)
  for (i in 1:limit) {
    toplist[i] <-topten[i]$gram
  }
  tokens = unlist(strsplit(phrase, '\\s+'))
  ntokens = length(tokens)
  if (ntokens > 2) {
    endPhrase <- paste(tokens[(ntokens-2):ntokens], collapse = ' ')
    counter = 3
  } else {
    endPhrase <- phrase
    counter = ntokens
  }
  found = FALSE
  wordList <- rep(NA, limit)
  for (i in counter:1) {
    endPhrase <- paste(tokens[(ntokens-i+1):ntokens], collapse = ' ')
    results <- findMatches(i, endPhrase, limit)
    if (results[1] != "No Matches Found") {
      if (found == FALSE) {
        found = TRUE
        wordList <- results
      } else {
        wordList <- append(wordList, results, after = length(wordList))
      }
    }
  }
  if (found == FALSE) {
    for (i in 1:limit) {
      wordList[i] <- toplist[i]
    }
  }
  else if (length(wordList) > limit) {
    wordList <- head(wordList, limit)
  } else if(length(wordList) < limit) {
    
    numFound = length(wordList)
    for (i in (numFound+1):limit) {
      wordList[i] <- toplist[i-numFound]
    }
  }
  return(wordList)
}

## test harness
setwd("C:/dev/r/capstone/SwiftKey/en_US")

require(tm)
require(dplyr)
require(ggplot2)
require(RWeka)
require(data.table)
require(stringr)

uni_dat <- readRDS("unigram_data.RDS")
bi_dat <- readRDS("bigram_data.RDS")
tri_dat <- readRDS("trigram_data.RDS")
quad_dat <- readRDS("quadgram_data.RDS")
testdf <- readRDS("test_fixture.RDS")

#testdf columns
#1 - string
#2 - #words in string
#3 - n-Gram, i.e. first n words of phrase to test
#4 - final word of phrase to test
#5 - starting position in string (how many words into entire string do we start?)
#6 - length of n-Gram
#7 - Success indicator (1=success, 0=failure)

iter <- nrow(testdf)
#iter <- 100
for (i in 1:iter) {
  if (testdf[i,2] > 3 && testdf[i,5] > 0) {
    phrase = testdf[i,3]
    predict <- predictWord(phrase)
    ##predict <- predictWithStupidBackoff(model=model, sentence=phrase)
    vec <- predict$`Predicted Word`
    if (testdf[i,4] %in% predict) {
      #if (testdf[i,4] %in% vec) {
      testdf[i,7] <- 1
    } else {
      testdf[i,7] <- 0
    }
  }
}
result_df <- testdf[complete.cases(testdf),]
success <- sum(result_df[1:iter,7])
trials <- iter - sum(is.na(testdf[1:iter,7]))
rate <- success/trials

blogSuccess <- sum(result_df[1:8101,7])
blograte <- blogSuccess / 8101
newsSuccess <- sum(result_df[8102:17777,7])
newsrate <- newsSuccess / (17777-8101)
twitSuccess <- sum(result_df[17778:39370,7])
twitrate <- twitSuccess / (39370-17777)



## prediction code below
limit <- 10
topten <- head(uni_dat, limit)
toplist <- rep(NA, limit)
for (i in 1:limit) {
  toplist[i] <-topten[i]$gram
}

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

predictWord <- function(phrase) {
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
    for (i in 1:10) {
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

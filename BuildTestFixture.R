## test harness
setwd("C:/dev/r/capstone/SwiftKey/en_US")

require(tm)
require(dplyr)
require(ggplot2)
require(RWeka)
require(data.table)
require(stringr)

blogfile <- file("en_US.blogs.txt")
blogLines <- readLines(blogfile)
close (blogfile)
blogWords<- sum((nchar(blogLines) - nchar(gsub(' ','',blogLines))) + 1)
blogRows <- nrow(as.matrix(blogLines))

newsfile <- file("en_US.news.txt")
newsLines <- readLines(newsfile)
close(newsfile)
newsWords<- sum((nchar(newsLines) - nchar(gsub(' ','',newsLines))) + 1)
newsRows <- nrow(as.matrix(newsLines))

twitfile <- file("en_US.twitter.txt")
twitLines <- readLines(twitfile)
close(twitfile)
twitWords<- sum((nchar(twitLines) - nchar(gsub(' ','',twitLines))) + 1)
twitRows <- nrow(as.matrix(twitLines))

# create a small sample for performance improvements
blogSlice <- sample(blogLines,blogRows * 0.01)
newsSlice <- sample(newsLines,newsRows * 0.01)
twitSlice <- sample(twitLines,twitRows * 0.01)

# remove non-ascii characters
for (i in 1:length(blogSlice)) {
  blogSlice[i] <- gsub("[^a-zA-Z0-9 ]","",blogSlice[i]) 
}
for (i in 1:length(blogSlice)) {
  newsSlice[i] <- gsub("[^a-zA-Z0-9 ]","",newsSlice[i]) 
}
for (i in 1:length(blogSlice)) {
  twitSlice[i] <- gsub("[^a-zA-Z0-9 ]","",twitSlice[i]) 
}

#write cleaned data samples to staging directory
test_path <- "C:/dev/r/capstone/SwiftKey/en_US/test"
blogtestfile <- file(paste(test_path, "/blogtest.txt", sep=""), open="wt")
newstestfile <- file(paste(test_path, "/newstest.txt", sep=""), open="wt")
twittestfile <- file(paste(test_path, "/twittest.txt", sep=""), open="wt")
writeLines(blogSlice, blogtestfile)
writeLines(newsSlice, newstestfile)
writeLines(twitSlice, twittestfile)
close(blogtestfile)
close(newstestfile)
close(twittestfile)

testCorpus <- Corpus(DirSource(test_path))

testCorpus <- tm_map(testCorpus, stripWhitespace)
testCorpus <- tm_map(testCorpus, content_transformer(tolower))
testCorpus <- tm_map(testCorpus, content_transformer(removePunctuation))
testCorpus <- tm_map(testCorpus, content_transformer(removeNumbers))
offensive <- read.table("offensive.txt", header=FALSE, sep="\n", strip.white=TRUE)
testCorpus <- tm_map(testCorpus, removeWords, offensive[,1])

testdf<-data.frame(text=unlist(sapply(testCorpus, `[`, "content")), stringsAsFactors=F)

for(i in 1:nrow(testdf)) {
  count = testdf[i, 2]
  if (count > 3) {
    typeRand <- runif(1, 0.0 ,1.0)
    if (typeRand < 0.6) {
      words <- 3
    } else if (typeRand < 0.9) {
      words <- 2
    } else  {
      words <- 1
    }
    start <- sample(1:(count-1-words),1)
    testdf[i,3] <- word(testdf[i, 1], start, start+words-1, sep=" ")
    testdf[i,4] <- word(testdf[i, 1], start+words, start+words, sep=" ")
    testdf[i,5] <- start
    testdf[i,6] <- words
  }
}


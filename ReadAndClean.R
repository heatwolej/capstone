# Setup - read and clean files

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

docstats <- matrix(nrow = 3, ncol = 3)
rownames(docstats) <- c("Lines", "Words", "Ratio")
colnames(docstats) <- c("Blogs", "News", "Tweets")
docstats[1,] <- c(blogRows, newsRows, twitRows)
docstats[2,] <- c(blogWords, newsWords, twitWords)
docstats[3,] <- docstats[2,] / docstats[1,]
docstats

# create a small sample for performance improvements
blogSlice <- sample(blogLines,blogRows * 0.08)
newsSlice <- sample(newsLines,newsRows * 0.08)
twitSlice <- sample(twitLines,twitRows * 0.08)

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
sample_path <- "C:/dev/r/capstone/SwiftKey/en_US/Samples"
blogsampfile <- file(paste(sample_path, "/blogsamp.txt", sep=""), open="wt")
newssampfile <- file(paste(sample_path, "/newssamp.txt", sep=""), open="wt")
twitsampfile <- file(paste(sample_path, "/twitsamp.txt", sep=""), open="wt")
writeLines(blogSlice, blogsampfile)
writeLines(newsSlice, newssampfile)
writeLines(twitSlice, twitsampfile)
close(blogsampfile)
close(newssampfile)
close(twitsampfile)
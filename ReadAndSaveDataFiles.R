## Read and Save Data Files

setwd("C:/dev/r/capstone/SwiftKey/en_US")

require(tm)
require(dplyr)
require(ggplot2)
require(RWeka)
require(data.table)
require(stringr)

saveRDS(cleanCorpus, file="cleanCorpus08.RDS")
saveRDS(dtm, "dtm.RDS")
saveRDS(unigram_trim, "trimmedUnigram.RDS")
saveRDS(unigram_sort, "sortedUnigram.RDS")
saveRDS(bigram_trim, "trimmedBigram.RDS")
saveRDS(merged_bigram, "bigramMerged.RDS")
saveRDS(trigram_trim, "trimmedTrigram.RDS")
saveRDS(trigram, "trigram.RDS")
saveRDS(merged_trigram, "trigramMerged.RDS")
saveRDS(trigram2merge, "trigram2merge.RDS")
saveRDS(merged_quadgram, "QuadgramMerged.RDS")
saveRDS(uni_dat, "unigram_data.RDS")
saveRDS(bi_dat, "bigram_data.RDS")
saveRDS(tri_dat, "trigram_data.RDS")
saveRDS(quad_dat, "quadgram_data.RDS")
saveRDS(testdf, "test_fixture.RDS")

cleanCorpus <- readRDS("cleanCorpus08.RDS")
dtm <- readRDS("dtm.RDS")
unigram_trim <- readRDS('trimmedUnigram.RDS')
#bigram <- readRDS("newBigram.RDS")
#trigram <- readRDS("trigram_trim.RDS")
sorted_unigram <- readRDS("sorted_unigram.RDS")
merged_bigram <- readRDS("bigramMerged.RDS")
bigram2merge <- readRDS("bigram2Merge.RDS")
trigram_trim <- readRDS("trimmedTrigram.RDS")
merged_trigram <- readRDS("trigramMerged.RDS")
quadgram_trim <- readRDS("trimmedQuadgram.RDS")
merged_quadgram <- readRDS("QuadgramMerged.RDS")

uni_dat <- readRDS("unigram_data.RDS")
bi_dat <- readRDS("bigram_data.RDS")
tri_dat <- readRDS("trigram_data.RDS")
quad_dat <- readRDS("quadgram_data.RDS")
testdf <- readRDS("test_fixture.RDS")

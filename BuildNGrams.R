## Build N-Grams and Data Tables

unigram = as.data.frame( as.matrix(  tdm_1_gram ) ) 
unigram$sum <- freq_1_gram
tots <- colSums(unigram)
unigram$prob <- unigram[,4]/tots[4]
unigram <- unigram[-3]
unigram <- unigram[-2]
unigram <- unigram[-1]

unigram$first <- rownames(unigram)
unigram_trim <- unigram[unigram$sum > 1, ]
unigram_sort <- arrange(unigram_trim, desc(sum))

bigram = as.data.frame( as.matrix(  tdm_2_gram ) ) 
bigram$sum <- freq_2_gram
bigram_trim <- bigram[bigram$sum > 1, ]
bitots <- colSums(bigram_trim)
bigram_trim <- bigram_trim[-3]
bigram_trim <- bigram_trim[-2]
bigram_trim <- bigram_trim[-1]

bigram_trim$bigram <- rownames(bigram_trim)
bigram_trim$first <- gsub("([A-Za-z]+).*", "\\1", bigram_trim$bigram)
bigram_trim <- as.data.frame(as.matrix(bigram_trim))
bigram_trim <- arrange(bigram_trim, first, desc(sum))
bigram_trim$sum <- as.numeric(levels(bigram_trim$sum))[bigram_trim$sum]
bigram_trim$first <- as.character(levels(bigram_trim$first))[bigram_trim$first]

#uni_dat = data.table (
#  gram = unigram_trim$first,
#  count = unigram_trim$sum,
#  prob = unigram_trim$prob,
#  key = "gram")
#bigram_merge <- mutate(bigram_trim, uni_sum =NA)
#for(i in 1:nrow(bigram_trim)) {
#  bigram_trim[i,4] <- uni_dat[gram==bigram_trim[i,3],2]
#}



merged_bigram <- merge(x=unigram_trim, y=bigram_trim, by="first", all.y=TRUE)
merged_bigram <- merged_bigram[!is.na(merged_bigram$sum.x), ]
merged_bigram <- merged_bigram[!is.na(merged_bigram$sum.y), ]
merged_bigram <- mutate(merged_bigram, cond_prob = sum.y / sum.x)
#new_bigram <- merged_bigram[1187:471450, ]
bigram2merge <- merged_bigram[,4:5]
names(bigram2merge)[1] <- "bi_sum"
bigram2merge$bigram <- as.character(levels(bigram2merge$bigram))[bigram2merge$bigram]
names(merged_bigram)[2] <- "uni_sum"
names(merged_bigram)[4] <- "bi_sum"

first2 <- function(s) {
  tokens = unlist(strsplit(s, split = "\\s+"))[1:2]
  paste(tokens[1], tokens[2], collapse = " ")
}

trigram = as.data.frame(as.matrix(tdm_3_gram))
trigram$sum <- freq_3_gram
trigram_trim <- trigram[trigram$sum > 1, ]
trigram_trim <- trigram_trim[-3]
trigram_trim <- trigram_trim[-2]
trigram_trim <- trigram_trim[-1]
tritots <- colSums(trigram_trim)
trigram_trim$trigram <- rownames(trigram_trim)
#trigram_trim$sum <- as.numeric(levels(trigram_trim$sum))[trigram_trim$sum]
for (i in 1:nrow(trigram_trim)) {
  trigram_trim[i,3] <- word(trigram_trim[i, 2], 1, 2, sep=" ")
}
trigram_trim$bigram <- as.character(levels(trigram_trim$bigram))[trigram_trim$bigram]

bi_dat = data.table (
  count = bigram2merge$bi_sum,
  gram = bigram2merge$bigram
)

merged_trigram <- merge(x=bigram2merge, y=trigram_trim, by="bigram", all.y=TRUE)
merged_trigram <- arrange(merged_trigram, bigram, desc(tri_sum))
merged_trigram <- mutate(merged_trigram, cond_prob = tri_sum / bi_sum)

quadgram = as.data.frame(as.matrix(tdm_4_gram))
quadgram$sum <- freq_4_gram
quadgram_trim <- quadgram[quadgram$sum > 1, ]
quadgram_trim <- quadgram_trim[-3]
quadgram_trim <- quadgram_trim[-2]
quadgram_trim <- quadgram_trim[-1]
quadtots <- colSums(quadgram_trim)
quadgram_trim$quadgram <- rownames(quadgram_trim)
for (i in 1:nrow(quadgram_trim)) {
  quadgram_trim[i,3] <- word(quadgram_trim[i, 2], 1, 3, sep=" ")
}
names(quadgram_trim)[3] <- "trigram"
names(quadgram_trim)[1] <- "quad_sum"
trigram2merge <- merged_trigram[,3:4]
merged_quadgram <- merge(x=trigram2merge, y=quadgram_trim, by="trigram", all.y=TRUE)
merged_quadgram <- mutate(merged_quadgram, cond_prob = quad_sum / tri_sum)

quad_dat = data.table (
  count = merged_quadgram$quad_sum,
  gram = merged_quadgram$trigram,
  quadgram = merged_quadgram$quadgram,
  key="gram"
)
result <- quad_dat[gram=="like many of"]

tri_dat = data.table (
  count = merged_trigram$tri_sum,
  gram = merged_trigram$bigram,
  trigram = merged_trigram$trigram,
  key="gram"
)

bi_dat = data.table (
  count = merged_bigram$bi_sum,
  gram = merged_bigram$first,
  bigram = merged_bigram$bigram,
  key="gram"
)

uni_dat = data.table (
  count = unigram_sort$sum,
  gram = unigram_sort$first
)
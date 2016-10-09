##Build Corpus, Tokenize, Sort, and Count

offensive <- read.table("offensive.txt", header=FALSE, sep="\n", strip.white=TRUE)

# combine into a sample
origCorpus <- Corpus(DirSource(sample_path))

cleanCorpus <- tm_map(origCorpus, stripWhitespace)
cleanCorpus <- tm_map(cleanCorpus, content_transformer(tolower))
cleanCorpus <- tm_map(cleanCorpus, content_transformer(removePunctuation))
cleanCorpus <- tm_map(cleanCorpus, content_transformer(removeNumbers))
cleanCorpus <- tm_map(cleanCorpus, removeWords, offensive[,1])

##typical operations for some applications, but not word predictor
#cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords("english"))
#cleanCorpus <- tm_map(cleanCorpus, stemDocument)

dtm <- DocumentTermMatrix(cleanCorpus) 
sdtm <- removeSparseTerms(dtm, 0.995)
freq <- colSums(as.matrix(dtm))  
sortWords <- order(freq, decreasing=TRUE)
freq[head(sortWords, 20)]

Tokenize_1_gram <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) }
Tokenize_2_gram <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) }
Tokenize_3_gram <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)) }
Tokenize_4_gram <- function(x) { RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4)) }

tdm_1_gram <- TermDocumentMatrix(cleanCorpus, control = list(wordLengths=c(1,Inf), tokenize = Tokenize_1_gram))
tdm_2_gram <- TermDocumentMatrix(cleanCorpus, control = list(tokenize = Tokenize_2_gram))
tdm_3_gram <- TermDocumentMatrix(cleanCorpus, control = list(tokenize = Tokenize_3_gram))
tdm_4_gram <- TermDocumentMatrix(cleanCorpus, control = list(tokenize = Tokenize_4_gram))

#stdm_1_gram <- removeSparseTerms(tdm_1_gram, 0.995)
freq_1_gram <- rowSums(as.matrix(tdm_1_gram)) 
sortWords_1_gram <- order(freq_1_gram, decreasing=TRUE)
freq_1_gram[head(sortWords_1_gram, 20)]

#stdm_2_gram <- removeSparseTerms(tdm_2_gram, 0.995)
freq_2_gram <- rowSums(as.matrix(tdm_2_gram)) 
sortWords_2_gram <- order(freq_2_gram, decreasing=TRUE)
freq_2_gram[head(sortWords_2_gram, 20)]
#stdm_2_gram

#stdm_3_gram <- removeSparseTerms(tdm_3_gram, 0.995)
freq_3_gram <- rowSums(as.matrix(tdm_3_gram)) 
sortWords_3_gram <- order(freq_3_gram, decreasing=TRUE)
freq_3_gram[head(sortWords_3_gram, 20)]

freq_4_gram <- rowSums(as.matrix(tdm_4_gram)) 
sortWords_4_gram <- order(freq_4_gram, decreasing=TRUE)
freq_4_gram[head(sortWords_4_gram, 20)]

library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(RWeka)
library(knitr)
library(quanteda)




en_blogs <- 
  readLines("./../course-data/en_US/en_US.blogs.txt",skipNul = TRUE,warn=FALSE)
en_news <- 
  readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_twitter <- 
  readLines("./../course-data/en_US/en_US.twitter.txt",skipNul = TRUE,warn=FALSE)

str(en_blogs)
#chr [1:899288]
str(en_news)
#chr [1:77259]
str(en_twitter)
#chr [1:2360148]

skim_blogs_vsmall <- en_blogs[1:10]
skim_news_vsmall <- en_news[1:10]
skim_twitter_vsmall <- en_twitter[1:10]

skim_blogs_vsmall
skim_news_vsmall
skim_twitter_vsmall

en_blogs <- iconv(en_blogs, "latin1", "ASCII", sub="")
en_news <- iconv(en_news, "latin1", "ASCII", sub="")
en_twitter <- iconv(en_twitter, "latin1", "ASCII", sub="")

class(en_blogs)
class(en_news)
class(en_twitter)

skim_blogs_01 <- en_blogs[1:50000]
skim_blogs_02 <- en_blogs[50001:100000]
skim_blogs_03 <- en_blogs[100001:150000]
skim_blogs_04 <- en_blogs[150001:200000]
skim_blogs_05 <- en_blogs[200001:250000]
skim_blogs_06 <- en_blogs[250001:300000]
skim_blogs_07 <- en_blogs[300001:350000]
skim_blogs_08 <- en_blogs[350001:400000]
skim_blogs_09 <- en_blogs[400001:450000]
skim_blogs_10 <- en_blogs[450001:500000]
skim_blogs_11 <- en_blogs[500001:550000]
skim_blogs_12 <- en_blogs[550001:600000]
skim_blogs_13 <- en_blogs[600001:650000]
skim_blogs_14 <- en_blogs[650001:700000]
skim_blogs_15 <- en_blogs[700001:750000]
skim_blogs_16 <- en_blogs[750001:800000]
skim_blogs_17 <- en_blogs[800001:850000]
skim_blogs_18 <- en_blogs[850001:899288]

skim_news_01 <- en_news[1:40000]
skim_news_02 <- en_news[40001:77259]

skim_twitter_01 <- en_twitter[1:100000]
skim_twitter_02 <- en_twitter[100001:200000]
skim_twitter_03 <- en_twitter[200001:300000]
skim_twitter_04 <- en_twitter[300001:400000]
skim_twitter_05 <- en_twitter[400001:500000]
skim_twitter_06 <- en_twitter[500001:600000]
skim_twitter_07 <- en_twitter[600001:700000]
skim_twitter_08 <- en_twitter[700001:800000]
skim_twitter_09 <- en_twitter[800001:900000]
skim_twitter_10 <- en_twitter[900001:1000000]
skim_twitter_11 <- en_twitter[1000001:1100000]
skim_twitter_12 <- en_twitter[1100001:1200000]
skim_twitter_13 <- en_twitter[1200001:1300000]
skim_twitter_14 <- en_twitter[1300001:1400000]
skim_twitter_15 <- en_twitter[1400001:1500000]
skim_twitter_16 <- en_twitter[1500001:1600000]
skim_twitter_17 <- en_twitter[1600001:1700000]
skim_twitter_18 <- en_twitter[1700001:1800000]
skim_twitter_19 <- en_twitter[1800001:1900000]
skim_twitter_20 <- en_twitter[1900001:2000000]
skim_twitter_21 <- en_twitter[2000001:2100000]
skim_twitter_22 <- en_twitter[2100001:2200000]
skim_twitter_23 <- en_twitter[2200001:2300000]
skim_twitter_24 <- en_twitter[2300001:2360148]


skim_blogs_small <- en_blogs[1:1000]
skim_news_small <- en_news[1:1000]
skim_twitter_small <- en_twitter[1:1000]

skim_blogs_vsmall <- en_blogs[1:10]
skim_news_vsmall <- en_news[1:10]
skim_twitter_vsmall <- en_twitter[1:10]

### corpora medium

skim_blogs_01_medium <- en_blogs[1:10000]
corpus_blogs_01 <- VCorpus(VectorSource(skim_blogs_01_medium))

######  blogs large (don't do any other large)

myCorpus_blogs_01 = tm_map(corpus_blogs_01, content_transformer(tolower))
myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removePunctuation)
myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removeNumbers)
myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, stemDocument)
myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, 
                           removeWords,c(stopwords(source = "smart"),"english"))
myDTM_blogs_01 = DocumentTermMatrix(myCorpus_blogs_01,
                                    control = list(minWordLength = 1))

inspect(myDTM_blogs_01)

#plotting
myTDM_blogs_01 = TermDocumentMatrix(myCorpus_blogs_01,
                                    control = list(minWordLength = 1))

inspect(myTDM_blogs_01)
findFreqTerms(myTDM_blogs_01,100)

# blogs 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_blogs_2gram <- TermDocumentMatrix(myCorpus_blogs_01, 
                                control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_blogs_2gram)
findFreqTerms(txtTdmBi_blogs_2gram,10)



#findAssocs(txtTdmBi_blogs_2gram, "high school", 0.5)

# blogs 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_blogs_3gram<- TermDocumentMatrix(myCorpus_blogs_01, 
                                          control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_blogs_3gram[500:505, 10:15])
inspect(txtTdmBi_blogs_3gram)
findFreqTerms(txtTdmBi_blogs_3gram,3)



#findAssocs(txtTdmBi_blogs_3gram, "cricket world cup", 0.5)



#I need a much smaller corpus to generate any plots....

matrix_myTDM_blogs_01 <- as.matrix(myTDM_blogs_01)
str(matrix_myTDM_blogs_01)
dimnames(matrix_myTDM_blogs_01)$Terms[1:10]

matrixsums_myTDM_blogs_01  <- sort(rowSums(matrix_myTDM_blogs_01),decreasing=TRUE)
matrixDF_myTDM_blogs_01  <- data.frame(word = names(matrixsums_myTDM_blogs_01),
                                       freq=matrixsums_myTDM_blogs_01)

filtermatrixDF_myTDM_blogs_01 <- filter(matrixDF_myTDM_blogs_01, freq >= 400)

filtermatrixDF_myTDM_blogs_01 %>%
mutate(word = reorder(word, freq)) %>%
ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(filtermatrixDF_myTDM_blogs_01)

matrixDF_myTDM_blogs_01$word[1:100]

##########################
#end of medium EDA
##########################

### corpora small

corpus_blogs_small <- VCorpus(VectorSource(skim_blogs_small))
corpus_news_small <- VCorpus(VectorSource(skim_news_small))
corpus_twitter_small <- VCorpus(VectorSource(skim_twitter_small))

######  blogs small 

myCorpus_blogs_small = tm_map(corpus_blogs_small, content_transformer(tolower))
myCorpus_blogs_small = tm_map(myCorpus_blogs_small, removePunctuation)
myCorpus_blogs_small = tm_map(myCorpus_blogs_small, removeNumbers)
myCorpus_blogs_small = tm_map(myCorpus_blogs_small, stemDocument)
myCorpus_blogs_small = tm_map(myCorpus_blogs_small, 
                              removeWords,c(stopwords(source = "smart"),"english"))
myDTM_blogs_small = DocumentTermMatrix(myCorpus_blogs_small,
                                       control = list(minWordLength = 1))

inspect(myDTM_blogs_small)

#plotting
myTDM_blogs_small = TermDocumentMatrix(myCorpus_blogs_small,
                                       control = list(minWordLength = 1))

inspect(myTDM_blogs_small)

# blogs 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_blogs_2gram <- TermDocumentMatrix(myCorpus_blogs_small, 
                                           control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_blogs_2gram)
findFreqTerms(txtTdmBi_blogs_2gram,10)



#findAssocs(txtTdmBi_blogs_2gram, "high school", 0.5)

# blogs 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_blogs_3gram<- TermDocumentMatrix(myCorpus_blogs_small, 
                                          control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_blogs_3gram[500:505, 10:15])
inspect(txtTdmBi_blogs_3gram)
findFreqTerms(txtTdmBi_blogs_3gram,3)

## blogs plot and table

matrix_myTDM_blogs_small <- as.matrix(myTDM_blogs_small)
str(matrix_myTDM_blogs_small)
dimnames(matrix_myTDM_blogs_small)$Terms[1:10]

matrixsums_myTDM_blogs_small  <- sort(rowSums(matrix_myTDM_blogs_small),decreasing=TRUE)
matrixDF_myTDM_blogs_small  <- data.frame(word = names(matrixsums_myTDM_blogs_small),
                                          freq=matrixsums_myTDM_blogs_small)

filtermatrixDF_myTDM_blogs_small <- filter(matrixDF_myTDM_blogs_small, freq >= 400)

filtermatrixDF_myTDM_blogs_small %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(filtermatrixDF_myTDM_blogs_small)



### taken out of the markdown

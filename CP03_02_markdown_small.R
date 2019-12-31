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


en_blogs <- iconv(en_blogs, "latin1", "ASCII", sub="")
en_news <- iconv(en_news, "latin1", "ASCII", sub="")
en_twitter <- iconv(en_twitter, "latin1", "ASCII", sub="")

skim_blogs_small <- en_blogs[1:100]
skim_news_small <- en_news[1:100]
skim_twitter_small <- en_twitter[1:100]


### corpora 1000

corpus_blogs_01 <- VCorpus(VectorSource(skim_blogs_small))

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
findFreqTerms(myTDM_blogs_01,5)
# blogs 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_blogs_2gram <- TermDocumentMatrix(myCorpus_blogs_01, 
                                control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_blogs_2gram)
findFreqTerms(txtTdmBi_blogs_2gram,2)

#findAssocs(txtTdmBi_blogs_2gram, "high school", 0.5)

# blogs 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_blogs_3gram<- TermDocumentMatrix(myCorpus_blogs_01, 
                                          control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_blogs_3gram)
findFreqTerms(txtTdmBi_blogs_3gram,1)[1:20]


#findAssocs(txtTdmBi_blogs_3gram, "cricket world cup", 0.5)



#I need a much smaller corpus to generate any plots....

matrix_myTDM_blogs_01 <- as.matrix(myTDM_blogs_01)
str(matrix_myTDM_blogs_01)
dimnames(matrix_myTDM_blogs_01)$Terms[1:20]

matrixsums_myTDM_blogs_01  <- sort(rowSums(matrix_myTDM_blogs_01),decreasing=TRUE)
matrixDF_myTDM_blogs_01  <- data.frame(word = names(matrixsums_myTDM_blogs_01),
                                       freq=matrixsums_myTDM_blogs_01)

filtermatrixDF_myTDM_blogs_01 <- filter(matrixDF_myTDM_blogs_01, freq >= 5)

filtermatrixDF_myTDM_blogs_01 %>%
mutate(word = reorder(word, freq)) %>%
ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(filtermatrixDF_myTDM_blogs_01)

matrixDF_myTDM_blogs_01$word[1:40]

##########################
#end of medium EDA
##########################


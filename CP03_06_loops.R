library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(RWeka)
library(knitr)
library(quanteda)

###########################
### section 1
###########################


### turn this entire list of tasks into a loop over really small matrices.

#We load the data and segment it into small chunks that we analyze individually. This 'divide and conquer' method is a quick way to analyze the data given limited computing resources (a laptop).

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


#Our first step is to remove symbols. We show an example of a sentence with symbols that we need to remove.

en_blogs[1]


#Here is the step in which we remove all of these non-Latin or non-ASCII symbols.


en_blogs <- iconv(en_blogs, "latin1", "ASCII", sub="")
en_news <- iconv(en_news, "latin1", "ASCII", sub="")
en_twitter <- iconv(en_twitter, "latin1", "ASCII", sub="")

str(en_blogs)
#chr [1:899288]
str(en_news)
#chr [1:77259]
str(en_twitter)
#chr [1:2360148]

#Here is an example of symbol removal.

en_blogs[1]

#We next segment the data into small chunks. We will set aside some of these chunks for later analysis, effectively using them as validation and test data. Incrementally adding each chunk will model how a real world application will be expoded to additional words phrases not previously encountered.
#> 899288*0.7
#[1] 629501.6
#> 77259*0.7
#[1] 54081.3
#> 2360148*0.7
#[1] 1652104

en_blogs <- en_blogs[1:600000]
en_news <- en_news[1:50000]
en_twitter <- en_twitter[1:1500000]

str(en_blogs)
str(en_news)
str(en_twitter)

class(en_blogs)
class(en_news)
class(en_twitter)

head(en_blogs)
head(en_news)
head(en_twitter)

en_blogs_small <- en_blogs[1:1000]
en_news_small <- en_news[1:1000]
en_twitter_small <- en_twitter[1:1000]

#Here is a sample of sentences from the three data sources (blogs, news, and twitter).

head(en_blogs,3)
head(en_news,10)
head(en_twitter,10)

###########################
### section 2
###########################

## (Task 2) Exploratory Data Analysis of a Medium-sized Sample (10,000 blog sentences)

# 1:1000
# 1001:1200

## loop starts here
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:600000) {
  print("i")
  print(i) 
  #corpus to TDM
  corpus_blogs_01 <- VCorpus(VectorSource(en_blogs[i]))
  myCorpus_blogs_01 = tm_map(corpus_blogs_01, content_transformer(tolower))
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removePunctuation)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removeNumbers)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, stemDocument)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, 
                             removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_blogs_01 = TermDocumentMatrix(myCorpus_blogs_01,
                                      control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_blogs_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 10)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)



##### news 200

## loop starts here
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:50000) {
  print("i")
  print(i) 
  #corpus to TDM
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_news_01 = TermDocumentMatrix(myCorpus_news_01,
                                     control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_news_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 50)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)



#### news and twitter need some love

# news


matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:2000) {
  #corpus to TDM
  print("i")
  print(i) 
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_news_01 = TermDocumentMatrix(myCorpus_news_01,
                                     control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_news_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#1 gram

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 50)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)



#Next 400 for consistency

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 2001:2400) {
  #corpus to TDM
  print("i")
  print(i) 
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_news_01 = TermDocumentMatrix(myCorpus_news_01,
                                     control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_news_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (1-grams).

#1 gram

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 15)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# twitter

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:3000) {
  #corpus to TDM
  print("i")
  print(i) 
  corpus_twitter_01 <- VCorpus(VectorSource(en_twitter[i]))
  myCorpus_twitter_01 = tm_map(corpus_twitter_01, content_transformer(tolower))
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removePunctuation)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removeNumbers)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, stemDocument)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, 
                               removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_twitter_01 = TermDocumentMatrix(myCorpus_twitter_01,
                                        control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_twitter_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (1-grams).

#1 gram

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 50)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)



#Next 200 for consistency


matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 3001:3600) {
  #corpus to TDM
  print("i")
  print(i) 
  corpus_twitter_01 <- VCorpus(VectorSource(en_twitter[i]))
  myCorpus_twitter_01 = tm_map(corpus_twitter_01, content_transformer(tolower))
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removePunctuation)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removeNumbers)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, stemDocument)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, 
                               removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_twitter_01 = TermDocumentMatrix(myCorpus_twitter_01,
                                        control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_twitter_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (1-grams).

#1 gram

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 12)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#########################
## big runs script only  1 gram
#########################

## loop starts here
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:100000) {
  print("i")
  print(i) 
  #corpus to TDM
  corpus_blogs_01 <- VCorpus(VectorSource(en_blogs[i]))
  myCorpus_blogs_01 = tm_map(corpus_blogs_01, content_transformer(tolower))
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removePunctuation)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removeNumbers)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, stemDocument)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, 
                             removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_blogs_01 = TermDocumentMatrix(myCorpus_blogs_01,
                                      control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_blogs_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 10)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)


##### news 200

## loop starts here
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:50000) {
  print("i")
  print(i) 
  #corpus to TDM
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_news_01 = TermDocumentMatrix(myCorpus_news_01,
                                     control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_news_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 50)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)




# twitter

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:1500000) {
  #corpus to TDM
  print("i")
  print(i) 
  corpus_twitter_01 <- VCorpus(VectorSource(en_twitter[i]))
  myCorpus_twitter_01 = tm_map(corpus_twitter_01, content_transformer(tolower))
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removePunctuation)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removeNumbers)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, stemDocument)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, 
                               removeWords,c(stopwords(source = "smart"),"english"))
  myTDM_twitter_01 = TermDocumentMatrix(myCorpus_twitter_01,
                                        control = list(minWordLength = 1))
  matrix_TDM <- as.matrix(myTDM_twitter_01)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (1-grams).

#1 gram

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 50)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)






### end of big runs  for 1 gram






###########################
### section 3 2 gram and 3 gram
###########################

## (Task 2) Exploratory Data Analysis of a Medium-sized Sample (10,000 blog sentences)

# 1:1000
# 1001:1200

### all 2 grams
## (Task 2) Exploratory Data Analysis 2-gram

#We will first look at a large sample of the blog data to get a feel for the frequencies of specific words. We use the tm package to perform the following sequential steps:
#2-gram blogs big 


BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:10) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_blogs_01 <- VCorpus(VectorSource(en_blogs[i]))
  myCorpus_blogs_01 = tm_map(corpus_blogs_01, content_transformer(tolower))
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removePunctuation)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removeNumbers)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, stemDocument)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, 
                             removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_blogs_2gram <- TermDocumentMatrix(myCorpus_blogs_01, 
                                             control = list(tokenize = BigramTokenizer_2gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_blogs_2gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}

#- Distributions of word frequencies (2-grams).

#2-gram blogs big 

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 6)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#2-gram blogs big 

kable(matrixDF_TDM_toplot_filter)


#2-gram blogs small
#Next 200 for consistency

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 2001:3000) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_blogs_01 <- VCorpus(VectorSource(en_blogs[i]))
  myCorpus_blogs_01 = tm_map(corpus_blogs_01, content_transformer(tolower))
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removePunctuation)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removeNumbers)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, stemDocument)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, 
                             removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_blogs_2gram <- TermDocumentMatrix(myCorpus_blogs_01, 
                                             control = list(tokenize = BigramTokenizer_2gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_blogs_2gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (2-grams).

#2-gram blogs small

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 4)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#2-gram news large

en_news[1:10]

BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:1500) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_news_2gram <- TermDocumentMatrix(myCorpus_news_01, 
                                             control = list(tokenize = BigramTokenizer_2gram))
  matrix_TDM <- as.matrix(txtTdmBi_news_2gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (2-grams).

#2-gram news large


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 4)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#2-gram news large

kable(matrixDF_TDM_toplot_filter)


2-gram news small
Next 200 for consistency

```{r 2-gram-news-loop-next, warning=FALSE, message=FALSE}
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1501:2000) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_news_2gram <- TermDocumentMatrix(myCorpus_news_01, 
                                             control = list(tokenize = BigramTokenizer_2gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_news_2gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}
```

- Distributions of word frequencies (2-grams).

2-gram news small

```{r 2-gram-news-plot-next, warning=FALSE, message=FALSE}
matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 3)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```

twitter

2-gram twitter big


```{r 2-gram-twitter-loop, warning=FALSE, message=FALSE}
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:3000) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_twitter_01 <- VCorpus(VectorSource(en_twitter[i]))
  myCorpus_twitter_01 = tm_map(corpus_twitter_01, content_transformer(tolower))
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removePunctuation)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removeNumbers)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, stemDocument)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, 
                               removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_twitter_2gram <- TermDocumentMatrix(myCorpus_twitter_01, 
                                             control = list(tokenize = BigramTokenizer_2gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_twitter_2gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (2-grams).

#2-gram twitter big

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 5)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#2-gram twitter big

kable(matrixDF_TDM_toplot_filter)



#2-gram twitter small

#Next 200 for consistency

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 3001:3600) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_twitter_01 <- VCorpus(VectorSource(en_twitter[i]))
  myCorpus_twitter_01 = tm_map(corpus_twitter_01, content_transformer(tolower))
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removePunctuation)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removeNumbers)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, stemDocument)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, 
                               removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_twitter_2gram <- TermDocumentMatrix(myCorpus_twitter_01, 
                                             control = list(tokenize = BigramTokenizer_2gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_twitter_2gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (2-grams).

#2-gram twitter small

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 2)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



## 2-gram in R for a much larger sample

We ran the full test sample (excludes validation and test set-aside data) for :
  
  - "blogs": R scripts, 600,000.
- "news": R scripts, 50,000.
- "twitter": R scripts, 1,500,000.

Read in the csv files for these



## (Task 2) Exploratory Data Analysis 3-gram

We will first look at a large sample of the blog data to get a feel for the frequencies of specific words. We use the tm package to perform the following sequential steps:
  
  3-gram blogs big 

```{r 3-gram-blogs-loop, warning=FALSE, message=FALSE}

BigramTokenizer_3gram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:1000) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_blogs_01 <- VCorpus(VectorSource(en_blogs[i]))
  myCorpus_blogs_01 = tm_map(corpus_blogs_01, content_transformer(tolower))
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removePunctuation)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removeNumbers)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, stemDocument)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, 
                             removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_blogs_3gram <- TermDocumentMatrix(myCorpus_blogs_01, 
                                             control = list(tokenize = BigramTokenizer_3gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_blogs_3gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (3-grams).

#3-gram blogs big 

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 2)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#3-gram blogs big 

kable(matrixDF_TDM_toplot_filter)

```
#3-gram blogs small
#Next 200 for consistency

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1001:2000) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_blogs_01 <- VCorpus(VectorSource(en_blogs[i]))
  myCorpus_blogs_01 = tm_map(corpus_blogs_01, content_transformer(tolower))
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removePunctuation)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, removeNumbers)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, stemDocument)
  myCorpus_blogs_01 = tm_map(myCorpus_blogs_01, 
                             removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_blogs_3gram <- TermDocumentMatrix(myCorpus_blogs_01, 
                                             control = list(tokenize = BigramTokenizer_3gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_blogs_3gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (3-grams).

#3-gram blogs small


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 2)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#3-gram news large

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:1500) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_news_3gram <- TermDocumentMatrix(myCorpus_news_01, 
                                             control = list(tokenize = BigramTokenizer_3gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_news_3gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (3-grams).

#3-gram news large


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 2)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#3-gram news large

kable(matrixDF_TDM_toplot_filter)



#3-gram news small
#Next 200 for consistency

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1501:2500) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_news_01 <- VCorpus(VectorSource(en_news[i]))
  myCorpus_news_01 = tm_map(corpus_news_01, content_transformer(tolower))
  myCorpus_news_01 = tm_map(myCorpus_news_01, removePunctuation)
  myCorpus_news_01 = tm_map(myCorpus_news_01, removeNumbers)
  myCorpus_news_01 = tm_map(myCorpus_news_01, stemDocument)
  myCorpus_news_01 = tm_map(myCorpus_news_01, 
                            removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_news_3gram <- TermDocumentMatrix(myCorpus_news_01, 
                                             control = list(tokenize = BigramTokenizer_3gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_news_3gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (3-grams).

#3-gram news small


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 2)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#twitter

#3-gram twitter big


matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:4000) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_twitter_01 <- VCorpus(VectorSource(en_twitter[i]))
  myCorpus_twitter_01 = tm_map(corpus_twitter_01, content_transformer(tolower))
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removePunctuation)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removeNumbers)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, stemDocument)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, 
                               removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_twitter_3gram <- TermDocumentMatrix(myCorpus_twitter_01, 
                                             control = list(tokenize = BigramTokenizer_3gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_twitter_3gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (3-grams).

#3-gram twitter big


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 2)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#3-gram twitter big


kable(matrixDF_TDM_toplot_filter)



#3-gram twitter small

#Next 200 for consistency


matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 4001:8000) {
  #corpus to TDM
  print("i")
  print(i)
  corpus_twitter_01 <- VCorpus(VectorSource(en_twitter[i]))
  myCorpus_twitter_01 = tm_map(corpus_twitter_01, content_transformer(tolower))
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removePunctuation)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, removeNumbers)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, stemDocument)
  myCorpus_twitter_01 = tm_map(myCorpus_twitter_01, 
                               removeWords,c(stopwords(source = "smart"),"english"))
  txtTdmBi_twitter_3gram <- TermDocumentMatrix(myCorpus_twitter_01, 
                                             control = list(tokenize = BigramTokenizer_3gram))
  
  matrix_TDM <- as.matrix(txtTdmBi_twitter_3gram)
  matrixsums_TDM  <- rowSums(matrix_TDM)
  matrixDF_TDM  <- data.frame(word=names(matrixsums_TDM),freq=matrixsums_TDM)
  matrixDF_TDM_all <- rbind(matrixDF_TDM,matrixDF_TDM_save)
  matrixDF_TDM_save <- matrixDF_TDM_all
}


#- Distributions of word frequencies (3-grams).

#3-gram twitter small


matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 3)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```


## 3-gram in R for a much larger sample

We ran the full test sample (excludes validation and test set-aside data) for :
  
  - "blogs": R scripts, 600,000.
- "news": R scripts, 50,000.
- "twitter": R scripts, 1,500,000.

Read in the csv files for these



#####

## (Task 2) Questions to Consider

#- 2-grams and 3-grams word frequencies.


BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

txtTdmBi_blogs_3gram<- TermDocumentMatrix(myCorpus_blogs_01, 
                                          control = list(tokenize = BigramTokenizer_3gram))














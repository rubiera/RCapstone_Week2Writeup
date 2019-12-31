library(tm)
library(textreadr)
library(tidyverse)
library(RWeka)
library(knitr)


en_tm_blogs <- readLines("./../course-data/en_US/en_US.blogs.txt",skipNul = TRUE)
en_tm_news <- readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_tm_twitter <- readLines("./../course-data/en_US/en_US.twitter.txt",skipNul = TRUE)

#from test below
en_tm_blogs <- iconv(en_tm_blogs, "latin1", "ASCII", sub="")
en_tm_news <- iconv(en_tm_news, "latin1", "ASCII", sub="")
en_tm_twitter <- iconv(en_tm_twitter, "latin1", "ASCII", sub="")


#######################

#for fast work
en_blogs_skim <- en_tm_blogs[1:1000]
en_news_skim <- en_tm_news[1:1000]
en_twitter_skim <- en_tm_twitter[1:1000]

#I need a larger sample  .... enter en_blogs_skim

corpus_blogs <- VCorpus(VectorSource(en_blogs_skim))
corpus_news <- VCorpus(VectorSource(en_news_skim))
corpus_twitter <- VCorpus(VectorSource(en_twitter_skim))

######  blogs

myCorpus_blogs = tm_map(corpus_blogs, content_transformer(tolower))
myCorpus_blogs = tm_map(myCorpus_blogs, removePunctuation)
myCorpus_blogs = tm_map(myCorpus_blogs, removeNumbers)
myCorpus_blogs = tm_map(myCorpus_blogs, removeWords,c(stopwords("SMART"),"english"))
myDTM_blogs = DocumentTermMatrix(myCorpus_blogs,
                                 control = list(minWordLength = 1))

inspect(myDTM_blogs)
findFreqTerms(myDTM_blogs,50)

#plotting
myTDM_blogs = TermDocumentMatrix(myCorpus_blogs,
                                 control = list(minWordLength = 1))



# 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_blogs_2gram <- TermDocumentMatrix(myCorpus_blogs, 
                                     control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_blogs_2gram[500:505, 10:15])
inspect(txtTdmBi_blogs_2gram)
findFreqTerms(txtTdmBi_blogs_2gram,5)
#findAssocs(txtTdmBi_blogs_2gram, "high school", 0.5)

# 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_blogs_3gram<- TermDocumentMatrix(myCorpus_blogs, 
                                          control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_blogs_3gram[500:505, 10:15])
inspect(txtTdmBi_blogs_3gram)
findFreqTerms(txtTdmBi_blogs_3gram,3)
#findAssocs(txtTdmBi_blogs_3gram, "cricket world cup", 0.5)

##################### blog tables

###blog1gram
mblog1gram <- as.matrix(myTDM_blogs)
vblog1gram <- sort(rowSums(mblog1gram),decreasing=TRUE)
dblog1gram <- data.frame(word = names(v),freq=v)
head(dblog1gram, 10)
class(dblog1gram)
str(dblog1gram)
dblog1gramfilter <- filter(dblog1gram, freq >= 40)
kable(dblog1gramfilter)

kable(findAssocs(myTDM_blogs,"time", 0.3)$time)
kable(findAssocs(myTDM_blogs,"day", 0.3)$day)
kable(findAssocs(myTDM_blogs,"good", 0.3)$good)

##### pick up here
str(mblog2gram)
mblog2gram$terms

###blog2gram
mblog2gram <- as.matrix(txtTdmBi_blogs_2gram)
vblog2gram <- sort(rowSums(mblog2gram),decreasing=TRUE)
dblog2gram <- data.frame(word = names(v),freq=v)
head(dblog2gram, 10)
class(dblog2gram)
str(dblog2gram)
dblog2gramfilter <- filter(dblog2gram, freq >= 20)
kable(dblog2gramfilter)

######  news

myCorpus_news = tm_map(corpus_news, content_transformer(tolower))
myCorpus_news = tm_map(myCorpus_news, removePunctuation)
myCorpus_news = tm_map(myCorpus_news, removeNumbers)
myCorpus_news = tm_map(myCorpus_news, removeWords,c(stopwords("SMART"),"english"))
myDTM_news = DocumentTermMatrix(myCorpus_news,
                                control = list(minWordLength = 1))
inspect(myDTM_news)
findFreqTerms(myDTM_news,30)

# 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_news_2gram <- TermDocumentMatrix(myCorpus_news, 
                                          control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_news_2gram[500:505, 10:15])
inspect(txtTdmBi_news_2gram)
findFreqTerms(txtTdmBi_news_2gram,5)
#findAssocs(txtTdmBi_news_2gram, "high school", 0.5)

# 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_news_3gram<- TermDocumentMatrix(myCorpus_news, 
                                         control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_news_3gram[500:505, 10:15])
inspect(txtTdmBi_news_3gram)
findFreqTerms(txtTdmBi_news_3gram,3)
#findAssocs(txtTdmBi_news_3gram, "osama bin laden", 0.5)

######  twitter

myCorpus_twitter = tm_map(corpus_twitter, content_transformer(tolower))
myCorpus_twitter = tm_map(myCorpus_twitter, removePunctuation)
myCorpus_twitter = tm_map(myCorpus_twitter, removeNumbers)
myCorpus_twitter = tm_map(myCorpus_twitter, removeWords,c(stopwords("SMART"),"english"))
myDTM_twitter = DocumentTermMatrix(myCorpus_twitter,
                                   control = list(minWordLength = 1))
inspect(myDTM_twitter)
findFreqTerms(myDTM_twitter,15)

# 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_twitter_2gram <- TermDocumentMatrix(myCorpus_twitter, 
                                             control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_twitter_2gram[500:505, 10:15])
inspect(txtTdmBi_twitter_2gram)
findFreqTerms(txtTdmBi_twitter_2gram,5)
#findAssocs(txtTdmBi_twitter_2gram, "dont care", 0.4)

# 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_twitter_3gram<- TermDocumentMatrix(myCorpus_twitter, 
                                            control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_twitter_3gram[500:505, 10:15])
inspect(txtTdmBi_twitter_3gram)
findFreqTerms(txtTdmBi_twitter_3gram,2)
#findAssocs(txtTdmBi_twitter_3gram, "make money money", 0.5)




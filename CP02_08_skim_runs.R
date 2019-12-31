library(tm)
library(textreadr)
library(tidyverse)
library(RWeka)
library(textmineR)

#Java problem at census solved by clearing JAVA_HOME.... who knew?
Sys.setenv(JAVA_HOME='')
#Sys.getenv("JAVA_HOME")
#[1] "C:\\Program Files (x86)\\Java\\jre1.8.0_231"

number = "01"

c9_blogs <- readLines(paste("./../Capstone/skims/skim_blogs_",number,".txt",sep=""))
c9_news <- readLines(paste("./../Capstone/skims/skim_news_",number,".txt",sep=""))
c9_twitter <- readLines(paste("./../Capstone/skims/skim_twitter_",number,".txt",sep=""))

c9_blogs <- iconv(c9_blogs, "latin1", "ASCII", sub="")
c9_news <- iconv(c9_news, "latin1", "ASCII", sub="")
c9_twitter <- iconv(c9_twitter, "latin1", "ASCII", sub="")


corpus_blogs_shiny <- VCorpus(VectorSource(c9_blogs))
corpus_news_shiny <- VCorpus(VectorSource(c9_news))
corpus_twitter_shiny <- VCorpus(VectorSource(c9_twitter))

###### census shiny blogs

myCorpus_blogs = tm_map(corpus_blogs_shiny, content_transformer(tolower))
myCorpus_blogs = tm_map(myCorpus_blogs, removePunctuation)
myCorpus_blogs = tm_map(myCorpus_blogs, removeNumbers)
myCorpus_blogs = tm_map(myCorpus_blogs, removeWords,c(stopwords("SMART"),"english"))
myDTM_blogs = DocumentTermMatrix(myCorpus_blogs,
                                 control = list(minWordLength = 1))
myTDM_blogs = TermDocumentMatrix(myCorpus_blogs,
                                 control = list(minWordLength = 1))
inspect(myDTM_blogs)
findFreqTerms(myDTM_blogs,1000)

# 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_blogs_2gram <- TermDocumentMatrix(myCorpus_blogs, 
                                           control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_blogs_2gram)
findFreqTerms(txtTdmBi_blogs_2gram,50)
#findAssocs(txtTdmBi_blogs_2gram, "high school", 0.5)

# 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_blogs_3gram<- TermDocumentMatrix(myCorpus_blogs, 
                                          control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_blogs_3gram)
findFreqTerms(txtTdmBi_blogs_3gram,8)
#findAssocs(txtTdmBi_blogs_3gram, "cricket world cup", 0.5)



###### census shiny news

myCorpus_news = tm_map(corpus_news_shiny, content_transformer(tolower))
myCorpus_news = tm_map(myCorpus_news, removePunctuation)
myCorpus_news = tm_map(myCorpus_news, removeNumbers)
myCorpus_news = tm_map(myCorpus_news, removeWords,c(stopwords("SMART"),"english"))
myDTM_news = DocumentTermMatrix(myCorpus_news,
                                control = list(minWordLength = 1))
myTDM_news = TermDocumentMatrix(myCorpus_news,
                                control = list(minWordLength = 1))
inspect(myDTM_news)
findFreqTerms(myDTM_news,200)

# 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_news_2gram <- TermDocumentMatrix(myCorpus_news, 
                                          control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_news_2gram)
findFreqTerms(txtTdmBi_news_2gram,50)

#findAssocs(txtTdmBi_news_2gram, "high school", 0.5)

# 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_news_3gram<- TermDocumentMatrix(myCorpus_news, 
                                         control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_news_3gram)
findFreqTerms(txtTdmBi_news_3gram,10)

#findAssocs(txtTdmBi_news_3gram, "cricket world cup", 0.5)



###### census shiny twitter

myCorpus_twitter = tm_map(corpus_twitter_shiny, content_transformer(tolower))
myCorpus_twitter = tm_map(myCorpus_twitter, removePunctuation)
myCorpus_twitter = tm_map(myCorpus_twitter, removeNumbers)
myCorpus_twitter = tm_map(myCorpus_twitter, removeWords,c(stopwords("SMART"),"english"))
myDTM_twitter = DocumentTermMatrix(myCorpus_twitter,
                                   control = list(minWordLength = 1))
myTDM_twitter = TermDocumentMatrix(myCorpus_twitter,
                                   control = list(minWordLength = 1))
inspect(myDTM_twitter)
findFreqTerms(myDTM_twitter,200)

# 2 grams
BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_twitter_2gram <- TermDocumentMatrix(myCorpus_twitter, 
                                             control = list(tokenize = BigramTokenizer_2gram))
inspect(txtTdmBi_twitter_2gram)
findFreqTerms(txtTdmBi_twitter_2gram,50)
#findAssocs(txtTdmBi_twitter_2gram, "high school", 0.5)

# 3 grams
BigramTokenizer_3gram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
txtTdmBi_twitter_3gram<- TermDocumentMatrix(myCorpus_twitter, 
                                            control = list(tokenize = BigramTokenizer_3gram))
inspect(txtTdmBi_twitter_3gram)
findFreqTerms(txtTdmBi_twitter_3gram,10)
#findAssocs(txtTdmBi_twitter_3gram, "cricket world cup", 0.5)


library(tm)
library(textreadr)
library(tidyverse)

txt <- system.file("texts","txt",package = "tm")
ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"))

en_tm_blogs <- readLines("./../course-data/en_US/en_US.blogs.txt",skipNul = TRUE)
en_tm_news <- readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_tm_twitter <- readLines("./../course-data/en_US/en_US.twitter.txt",skipNul = TRUE)

#from test below
en_tm_blogs <- iconv(en_tm_blogs, "latin1", "ASCII", sub="")
en_tm_news <- iconv(en_tm_news, "latin1", "ASCII", sub="")
en_tm_twitter <- iconv(en_tm_twitter, "latin1", "ASCII", sub="")

#remove non-english words earliest
# remove words with non-ASCII characters
# assuming you read your txt file in as a vector, eg. 
# dat <- readLines('~/temp/dat.txt')
dat <- "Special,  satisfação, Happy, Sad, Potential, für"
# convert string to vector of words
dat2 <- unlist(strsplit(dat, split=", "))
# find indices of words with non-ASCII characters
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
# subset original vector of words to exclude words with non-ASCII char
dat4 <- dat2[-dat3]
# convert vector back to a string
dat5 <- paste(dat4, collapse = ", ")
# make corpus
require(tm)
words1 <- Corpus(VectorSource(dat5))
inspect(words1)

########################

en_blogs_remove_test  <- en_blogs_skim_20 
en_blogs_remove_test[1]
#this test splits and pastes.... worth trying
dat2 <- unlist(strsplit(en_blogs_remove_test[1], split=", "))
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
dat4 <- dat2[-dat3]
dat5 <- paste(dat4, collapse = ", ")
#I need da5 to be a character vector, not just one line of text
#it needs to be done in a for loop
#this way, I get all of the text in a ball, instead of 20 sentences
#I need to preserve the original structure
#works though
words1 <- Corpus(VectorSource(dat5))
inspect(words1)

#splitting needs to change
en_blogs_remove_test[1]
en_blogs_remove_test_clean <- 
  iconv(en_blogs_remove_test[1], "latin1", "ASCII", sub="")
en_blogs_remove_test_clean
  
#does it work on a character vector
#let's finalize this
en_blogs_skim_20
en_blogs_remove_test_clean_20 <- en_blogs_skim_20
en_blogs_remove_test_clean_20 <-
  iconv(en_blogs_remove_test_clean_20, "latin1", "ASCII", sub="")
en_blogs_remove_test_clean_20

#######################

#for fast work
en_blogs_skim <- en_tm_blogs[1:100000]
en_news_skim <- en_tm_news
en_twitter_skim <- en_tm_twitter[1:200000]

#for even faster work
en_blogs_skim_1000 <- en_blogs_skim[1:1000]
en_news_skim_1000 <- en_news_skim[1:1000]
en_twitter_skim_1000 <- en_twitter_skim[1:1000]

#for code checks
en_blogs_skim_20 <- en_blogs_skim[1:20]
en_news_skim_20 <- en_news_skim[1:20]
en_twitter_skim_20 <- en_twitter_skim[1:20]

corpus_blogs_20 <- VCorpus(VectorSource(en_blogs_skim_20))
corpus_news_20 <- VCorpus(VectorSource(en_news_skim_20))
corpus_twitter_20 <- VCorpus(VectorSource(en_twitter_skim_20))

inspect(corpus_blogs_20)
inspect(corpus_news_20)
inspect(corpus_twitter_20)

lapply(corpus_blogs_20,as.character)
lapply(corpus_news_20,as.character)
lapply(corpus_twitter_20,as.character)

# blogs corpus and DTM sample of 20

myCorpus_blogs = tm_map(corpus_blogs_20, content_transformer(tolower))
myCorpus_blogs = tm_map(myCorpus_blogs, removePunctuation)
myCorpus_blogs = tm_map(myCorpus_blogs, removeNumbers)
myCorpus_blogs = tm_map(myCorpus_blogs, removeWords,c(stopwords("SMART"),"english"))

lapply(myCorpus_blogs[1:4],as.character)

myTDM_blogs = TermDocumentMatrix(myCorpus_blogs,
                                 control = list(minWordLength = 1))
myDTM_blogs = DocumentTermMatrix(myCorpus_blogs,
                                 control = list(minWordLength = 1))
inspect(myTDM_blogs)
inspect(myDTM_blogs)
#use DTM only
findMostFreqTerms(myDTM_blogs,3)
#use this one
findFreqTerms(myDTM_blogs,3)

#associations
#"bad"      "images"   "make"     "people"   
#"problems" "thing"    "time"     "winter" 

findAssocs(myDTM_blogs, "bad", 0.8)
#$bad
#thing 
#0.8 

#I need a larger sample  .... enter en_blogs_skim_1000

corpus_blogs_1000 <- VCorpus(VectorSource(en_blogs_skim_1000))
corpus_news_1000 <- VCorpus(VectorSource(en_news_skim_1000))
corpus_twitter_1000 <- VCorpus(VectorSource(en_twitter_skim_1000))

###### _1000 blogs

myCorpus_blogs = tm_map(corpus_blogs_1000, content_transformer(tolower))
myCorpus_blogs = tm_map(myCorpus_blogs, removePunctuation)
myCorpus_blogs = tm_map(myCorpus_blogs, removeNumbers)
myCorpus_blogs = tm_map(myCorpus_blogs, removeWords,c(stopwords("SMART"),"english"))
myDTM_blogs = DocumentTermMatrix(myCorpus_blogs,
                                 control = list(minWordLength = 1))
inspect(myDTM_blogs)
findFreqTerms(myDTM_blogs,50)

###### _1000 news

myCorpus_news = tm_map(corpus_news_1000, content_transformer(tolower))
myCorpus_news = tm_map(myCorpus_news, removePunctuation)
myCorpus_news = tm_map(myCorpus_news, removeNumbers)
myCorpus_news = tm_map(myCorpus_news, removeWords,c(stopwords("SMART"),"english"))
myDTM_news = DocumentTermMatrix(myCorpus_news,
                                control = list(minWordLength = 1))
inspect(myDTM_news)
findFreqTerms(myDTM_news,30)

###### _1000 twitter

myCorpus_twitter = tm_map(corpus_twitter_1000, content_transformer(tolower))
myCorpus_twitter = tm_map(myCorpus_twitter, removePunctuation)
myCorpus_twitter = tm_map(myCorpus_twitter, removeNumbers)
myCorpus_twitter = tm_map(myCorpus_twitter, removeWords,c(stopwords("SMART"),"english"))
myDTM_twitter = DocumentTermMatrix(myCorpus_twitter,
                                   control = list(minWordLength = 1))
inspect(myDTM_twitter)
findFreqTerms(myDTM_twitter,15)







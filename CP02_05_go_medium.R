library(tm)
library(textreadr)
library(tidyverse)


en_tm_blogs <- readLines("./../course-data/en_US/en_US.blogs.txt",skipNul = TRUE)
en_tm_news <- readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_tm_twitter <- readLines("./../course-data/en_US/en_US.twitter.txt",skipNul = TRUE)

#from test below
en_tm_blogs <- iconv(en_tm_blogs, "latin1", "ASCII", sub="")
en_tm_news <- iconv(en_tm_news, "latin1", "ASCII", sub="")
en_tm_twitter <- iconv(en_tm_twitter, "latin1", "ASCII", sub="")


#######################

#for fast work
en_blogs_skim <- en_tm_blogs[1:50000]
en_news_skim <- en_tm_news[1:50000]
en_twitter_skim <- en_tm_twitter[1:50000]

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

###not working a 200 MB corpus to matrix to plot blew up in size....
#plotting has to go back to the 1000 sample
m <- as.matrix(myTDM_blogs)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

plot(myTDM_blogs, terms = 
       findFreqTerms(myTDM_blogs, lowfreq = 50)[1:25], corThreshold = 0.9)

plot(myTDM_blogs,
     terms = sample(Terms(myTDM_blogs), 20),
     corThreshold = 0.95,
     weighting = FALSE,
     attrs = list(graph = list(rankdir = "BT"),
                  node = list(shape = "rectangle",
                              fixedsize = FALSE)))

plot(myTDM_blogs, corThreshold = 0.95, weighting = TRUE)




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





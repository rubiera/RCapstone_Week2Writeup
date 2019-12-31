library(tm)
library(wordcloud)
library(textreadr)
library(tidyverse)



en_blogs <- 
  readLines("./../course-data/en_US/en_US.blogs.txt",skipNul = TRUE)
en_news <- 
  readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_twitter <- 
  readLines("./../course-data/en_US/en_US.twitter.txt",skipNul = TRUE)

str(en_blogs)
#chr [1:899288]
str(en_news)
#chr [1:77259]
str(en_twitter)
#chr [1:2360148]


#for fast work
en_blogs_skim <- en_blogs[1:100000]
en_news_skim <- en_news
en_twitter_skim <- en_twitter[1:200000]


#for even faster work
en_blogs_skim_1000 <- en_blogs_skim[1:1000]
en_news_skim_1000 <- en_news_skim[1:1000]
en_twitter_skim_1000 <- en_twitter_skim[1:1000]

#for code checks
en_blogs_skim_20 <- en_blogs_skim[1:20]
en_news_skim_20 <- en_news_skim[1:20]
en_twitter_skim_20 <- en_twitter_skim[1:20]

en_blogs_skim_20[1]
en_blogs_skim_20[2]
en_blogs_skim_20[3]
en_blogs_skim_20[4]
en_blogs_skim_20[5]

df_en_blogs_skim_20 <- as.tibble(en_blogs_skim_20)
class(df_en_blogs_skim_20)
str(df_en_blogs_skim_20)
df_en_blogs_skim_20$value[1]

### report 1

myCorpus01av = Corpus(VectorSource(df_en_blogs_skim_20))

myCorpus01a = Corpus(DataframeSource(df_en_blogs_skim_20))
class(myCorpus01a)
str(myCorpus01a)

#tm intro

writeCorpus(myCorpus01a)
inspect(myCorpus01a[1:2])
#2nd member is NA
inspect(myCorpus01a[1])

#using vector instead of data frame
#same as data frame
inspect(myCorpus01av[1:2])

meta(myCorpus01a[1],"id")
#[1] "1"

lapply(myCorpus01a[1],as.character)
#same
lapply(myCorpus01a,as.character)

myCorpus01b = tm_map(myCorpus01a, content_transformer(tolower))
lapply(myCorpus01b[1],as.character)

myCorpus01c = tm_map(myCorpus01b, removePunctuation)
lapply(myCorpus01c[1],as.character)

myCorpus01d = tm_map(myCorpus01c, removeNumbers)
lapply(myCorpus01d[1],as.character)

myCorpus01e = tm_map(myCorpus01d, removeWords,c(stopwords("SMART")))
lapply(myCorpus01e[1],as.character)

myDTM01 = TermDocumentMatrix(myCorpus01e,
                           control = list(minWordLength = 1))
myDTM01 = DocumentTermMatrix(myCorpus01e,
                             control = list(minWordLength = 1))

inspect(myDTM01)
# same output for TDM and DTM
#<<DocumentTermMatrix (documents: 1, terms: 365)>>
#  Non-/sparse entries: 365/0
#Sparsity           : 0%
#Error in nchar(Terms(x), type = "chars") : 
#  invalid multibyte string, element 40

m01 = as.matrix(myDTM01)
inspect(m01)
#Error in UseMethod("inspect", x) : 
#  no applicable method for 'inspect' 
#applied to an object of class "c('matrix', 'double', 'numeric')"

print(m01)

mresult01 <- sort(rowSums(m01), decreasing = TRUE)

#wordcloud(df_en_blogs_skim_20,mresult01,scale=c(4,0.5),min.freq = 5, max.words=50,
#                colors=brewer.pal(8, "Dark2"))


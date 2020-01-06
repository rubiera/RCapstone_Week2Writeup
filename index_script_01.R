library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(RWeka)
library(knitr)
library(quanteda)
library(tidytext)
library(data.table)

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

str(en_blogs)
#chr [1:899288]
str(en_news)
#chr [1:77259]
str(en_twitter)
#chr [1:2360148]



#We next select approximately 70 percent of each of the three data sources (blogs, news, twitter), 
#leaving 20 percent for later validation, and 10 percent as a testing dataset. 
#Once we start using the validation data, we plan to incrementally add data 
#in small chunks to model how a real world application will be exposed to 
#additional words and phrases not previously encountered. Due to the 
#limitations of running within R Markdown with large files, 
#we run on very small selections within this markdown file, 
#and run on much larger samples outside of markdown. 
#The number of sentences used to train our model are:
  
#- "blogs": 600,000 out of 899,288 sentences.
#- "news": 25,000 out of 77,259 sentences.
#- "twitter": 1,500,000 out of 2,360,148 sentences. 

## (Task 2) Exploratory Data Analysis Part 1

#First, we address the following:
  
#- Understand the distribution of words and relationship between the words in the corpora.
#- Understand frequencies of words and word pairs.

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:100) {
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

#- Distributions of word frequencies (1-grams).

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 7)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)

###### 2-gram example

#2-gram blogs big 


BigramTokenizer_2gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:100) {
  #corpus to TDM
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


#2-gram blogs big 

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


#2-gram blogs big 

kable(matrixDF_TDM_toplot_filter)


#############  3-gram example

#3-gram blogs example 


BigramTokenizer_3gram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:100) {
  #corpus to TDM
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

#3-gram blogs example

matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, str_detect(word, "^a[rst]")) 
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#3-gram blogs big 

kable(matrixDF_TDM_toplot_filter)


##### correlations

### twitter

gram_1_twitter_01 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_1.txt")
gram_1_twitter_02 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_2.txt")

gram_1_twitter_01 <- gram_1_twitter_01 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_02 <- gram_1_twitter_02 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))



############## blogs

gram_1_blogs_01 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_1.txt")
gram_1_blogs_02 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_2.txt")


gram_1_blogs_01 <- gram_1_blogs_01 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_02 <- gram_1_blogs_02 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))


############## news

gram_1_news <- read.csv("./../skims/gram_all_news/gram_1_news.txt")

gram_1_news_corr <- gram_1_news %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                term_freq = freq/sum(freq), 
                                                logtf = log10(term_freq), logrank = log10(rank))


par(mfrow=c(1,2))

# words found in both files twitter my way

freq_join_gram_1_twitter <- merge(gram_1_twitter_01, gram_1_twitter_02, by = "word")
freq_join_gram_1_twitter <- filter(freq_join_gram_1_twitter,  freq.x != 0)
head(freq_join_gram_1_twitter)

plot(freq_join_gram_1_twitter$freq.x,freq_join_gram_1_twitter$freq.y, 
     type = "p", col = "black", lwd = 1, 
     xlab = "Selection 1",
     ylab = "Selection 2")
#     main = "Correlation between words found in two twitter selections")

cor.test(freq_join_gram_1_twitter$freq.x,freq_join_gram_1_twitter$freq.y)

# words found in both files blogs my way

freq_join_gram_1_blogs <- merge(gram_1_blogs_01, gram_1_blogs_02, by = "word")
freq_join_gram_1_blogs <- filter(freq_join_gram_1_blogs,  freq.x != 0)
head(freq_join_gram_1_blogs)

plot(freq_join_gram_1_blogs$freq.x,freq_join_gram_1_blogs$freq.y)

cor.test(freq_join_gram_1_blogs$freq.x,freq_join_gram_1_blogs$freq.y)

###### cross correlations

par(mfrow=c(1,3))

#### twitter and blogs

freq_join_gram_1_twitter_blogs <- merge(gram_1_twitter_01, gram_1_blogs_01, by = "word")
freq_join_gram_1_twitter_blogs <- filter(freq_join_gram_1_twitter_blogs,  freq.x != 0)
head(freq_join_gram_1_twitter_blogs)

plot(freq_join_gram_1_twitter_blogs$freq.x,freq_join_gram_1_twitter_blogs$freq.y)

cor.test(freq_join_gram_1_twitter_blogs$freq.x,freq_join_gram_1_twitter_blogs$freq.y)

#### twitter and news

freq_join_gram_1_twitter_news <- merge(gram_1_twitter_01, gram_1_news_corr, by = "word")
freq_join_gram_1_twitter_news <- filter(freq_join_gram_1_twitter_news,  freq.x != 0)
head(freq_join_gram_1_twitter_news)

plot(freq_join_gram_1_twitter_news$freq.x,freq_join_gram_1_twitter_news$freq.y)

cor.test(freq_join_gram_1_twitter_news$freq.x,freq_join_gram_1_twitter_news$freq.y)

#### blogs and news

freq_join_gram_1_blogs_news <- merge(gram_1_blogs_01, gram_1_news_corr, by = "word")
freq_join_gram_1_blogs_news <- filter(freq_join_gram_1_blogs_news,  freq.x != 0)
head(freq_join_gram_1_blogs_news)

plot(freq_join_gram_1_blogs_news$freq.x,freq_join_gram_1_blogs_news$freq.y)

cor.test(freq_join_gram_1_blogs_news$freq.x,freq_join_gram_1_blogs_news$freq.y)


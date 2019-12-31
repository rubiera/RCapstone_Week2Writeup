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

en_news <- 
  readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_news <- iconv(en_news, "latin1", "ASCII", sub="")
en_news <- en_news[1:25000]

#Here is an example of symbol removal.

###########################
### section 2
###########################

## (Task 2) Exploratory Data Analysis of a Medium-sized Sample (10,000 blog sentences)

# 1:1000
# 1001:1200

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
for (i in 1:25000) {
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
head(matrixDF_TDM_toplot)

write.csv(matrixDF_TDM_toplot,"./../skims/gram_2_news.txt")

matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]




matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 4)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



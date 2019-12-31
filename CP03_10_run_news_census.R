library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(stopwords)


en_news <- 
  readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_news <- iconv(en_news, "latin1", "ASCII", sub="")

str(en_news)
#chr [1:77259]


#> 77259*0.7
#[1] 54081.3





## loop starts here
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in 1:25000) {
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

write.csv(matrixDF_TDM_toplot,"./../skims/gram_1_news.txt")

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 50)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)





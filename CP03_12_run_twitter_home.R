library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(knitr)
library(stopwords)


###########################
### section 1
###########################


# CHANGE FILE NUMBER TWICE 

en_twitter <- 
  readLines(paste("./../skims/skim_twitter_02.txt",sep=""),skipNul = TRUE,warn=FALSE)

en_twitter <- iconv(en_twitter, "latin1", "ASCII", sub="")
str(en_twitter)


for (fp in 1:5) {
  ibegin = 1+20000*(fp-1)
  print("ibegin")
  print(ibegin)
  iend = fp*20000
  print("iend") 
  print(iend) 
matrixDF_TDM_save <- data.frame(word = "", freq = 0 )
matrixDF_TDM_all <- data.frame(word = "", freq = 0 )
for (i in ibegin:iend) {
  print("i")
  print(i) 
  #corpus to TDM
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
matrixDF_TDM_toplot <- aggregate(freq ~ word, 
                                 data=matrixDF_TDM_all,FUN=sum, 
                                 na.rm = TRUE) 
matrixDF_TDM_toplot <- matrixDF_TDM_toplot[order(-matrixDF_TDM_toplot$freq),]
write.csv(matrixDF_TDM_toplot,paste("./../skims/gram_1_twitter_02_",fp,".txt",sep=""))

}


head(matrixDF_TDM_toplot)

matrixDF_TDM_toplot_filter <- filter(matrixDF_TDM_toplot, freq >= 10)
matrixDF_TDM_toplot_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

kable(matrixDF_TDM_toplot_filter)

  
  
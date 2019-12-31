library(tm)
library(RWeka)
data(crude)

library(plyr)
library(gamlr)

#crude is a corpus
lapply(crude,as.character)

#maybe I don't need to this...
crude_clean <- iconv(crude, "latin1", "ASCII", sub="")
crude_clean[1:4]

crude_test = tm_map(crude, content_transformer(tolower))
crude_test = tm_map(crude_test, removePunctuation)
crude_test = tm_map(crude_test, removeNumbers)
crude_test = tm_map(crude_test, removeWords,c(stopwords("SMART"),"english"))
myDTM_crude_test = DocumentTermMatrix(crude_test,
                                 control = list(minWordLength = 1))
inspect(myDTM_crude_test)
findFreqTerms(myDTM_crude_test,50)

#use crude or crude_test?
inspect(crude)
inspect(crude_test)

lapply(crude[1:4],as.character)
lapply(crude_test[1:4],as.character)

#Tokenizer for n-grams and passed on to the term-document matrix constructor
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
inspect(txtTdmBi[1000:1005, 10:15])
inspect(txtTdmBi)
findFreqTerms(txtTdmBi,10)
findAssocs(txtTdmBi, "we are", 0.9)

dat <- llply(txtTdmBi$dimnames$Terms, 
             function(i) findAssocs(txtTdmBi, i, 0.5), .progress = "text" )

dat[1:2]

###### apply it to our 1000 sample

###### _1000 blogs


#Tokenizer for n-grams and passed on to the term-document matrix constructor
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi_blogs <- TermDocumentMatrix(myCorpus_blogs, 
                               control = list(tokenize = BigramTokenizer))
inspect(txtTdmBi_blogs[500:505, 10:15])
inspect(txtTdmBi_blogs)
findFreqTerms(txtTdmBi_blogs,5)
findAssocs(txtTdmBi_blogs, "high school", 0.5)

#takes too long
dat_blogs <- llply(txtTdmBi_blogs$dimnames$Terms, 
             function(i) findAssocs(txtTdmBi_blogs, i, 0.7), .progress = "text" )


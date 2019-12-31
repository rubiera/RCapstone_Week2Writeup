library(textreadr)
library(tm)
library(tidyverse)
library(tidytext)
library(knitr)
library(stopwords)


dataset_gram_2_twitter <- read.csv("./../model/test_01/dataset_gram_2_twitter.txt", stringsAsFactors = FALSE)
dataset_gram_3_twitter <- read.csv("./../model/test_01/dataset_gram_3_twitter.txt", stringsAsFactors = FALSE)

#unique eevrything
dataset_gram_2_twitter[9187,]
dataset_gram_2_twitter[10254,]
dataset_gram_2_twitter <- select(dataset_gram_2_twitter,c("word","freq"))
dataset_gram_2_twitter <- unique(dataset_gram_2_twitter)
dataset_gram_2_twitter <- aggregate(freq ~ word, 
                                    data=dataset_gram_2_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_gram_2_twitter)
head(dataset_gram_2_twitter)

dataset_gram_2_twitter[1132,]
dataset_gram_2_twitter[5654,]


dataset_gram_3_twitter <- select(dataset_gram_3_twitter,c("word","freq"))
dataset_gram_3_twitter <- unique(dataset_gram_3_twitter)
dataset_gram_3_twitter <- aggregate(freq ~ word, 
                                    data=dataset_gram_3_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_gram_3_twitter)
head(dataset_gram_3_twitter)

gram_2_twitter_split <- dataset_gram_2_twitter %>%
  separate(word, c("word1", "word2"), sep = " ")

head(gram_2_twitter_split)

gram_2_twitter_split[1132,]
gram_2_twitter_split[5654,]

gram_3_twitter_split <- dataset_gram_3_twitter %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

head(gram_3_twitter_split)

gram_2_twitter_freq <- gram_2_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_2_twitter_freq <- gram_2_twitter_freq %>% 
  mutate(fake_freq = dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_2_twitter_freq)

gram_3_twitter_freq <- gram_3_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_3_twitter_freq <- gram_3_twitter_freq %>% 
  mutate(fake_freq = dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_3_twitter_freq)


#this is a list of input words to develop the model in steps of (hopefully) 
#increasing predictive accuracy

input_gram_1 <- select(gram_2_twitter_freq, word1)
head(input_gram_1)
str(input_gram_1)
input_gram_1 <- unique(input_gram_1)
head(input_gram_1)
str(input_gram_1)

#naive prediction, no smoothing

#start with an easy one
input_gram_1[200:220,]
gram_2_twitter_freq$word1[9187]
gram_2_twitter_freq$word2[9187]
gram_2_twitter_freq$word1[10254]
gram_2_twitter_freq$word2[10254]

gram_2_twitter_freq[1132,]
gram_2_twitter_freq[5654,]

head(gram_2_twitter_freq)

for (i in 1:3) {
  #print("i")
  #print(i)
  #print(input_gram_1[i,])
  for (j in 1:nrow(gram_2_twitter_freq)){
      if(input_gram_1[i,] == gram_2_twitter_freq$word1[j]) {
        print("j")
        print(j)
        print("we are in gram_2 word1")
        print(input_gram_1[i,])
        print(gram_2_twitter_freq$word1[j])
        print("next word")
        print(gram_2_twitter_freq$word2[j])
        print("freq")
        print(gram_2_twitter_freq$freq[j])
        print("term freq")
        print(gram_2_twitter_freq$fake_freq[j])        
      }
  }
}

for (i in 1:nrow(input_gram_1)) {
  #print("i")
  #print(i)
  #print(input_gram_1[i,])
  for (j in 1:nrow(gram_2_twitter_freq)){
    if(input_gram_1[i,] == gram_2_twitter_freq$word1[j]) {
      print("we are in gram_2 word1")
      print(input_gram_1[i,])
      print(gram_2_twitter_freq$word1[j])
      print("next word")
      print(gram_2_twitter_freq$word2[j])
    }
  }
}








library(textreadr)
library(tm)
library(tidyverse)
library(tidytext)
library(knitr)
library(stopwords)


dataset_gram_2_twitter <- read.csv("./../model/test_01/dataset_gram_2_twitter.txt", stringsAsFactors = FALSE)
dataset_gram_3_twitter <- read.csv("./../model/test_01/dataset_gram_3_twitter.txt", stringsAsFactors = FALSE)

#unique eevrything
dataset_gram_2_twitter <- select(dataset_gram_2_twitter,c("word","freq"))
dataset_gram_2_twitter <- unique(dataset_gram_2_twitter)
dataset_gram_2_twitter <- aggregate(freq ~ word, 
                                    data=dataset_gram_2_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_gram_2_twitter)
head(dataset_gram_2_twitter)

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

gram_3_twitter_split <- dataset_gram_3_twitter %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

head(gram_3_twitter_split)

gram_2_twitter_freq <- gram_2_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_2_twitter_freq <- gram_2_twitter_freq %>% 
  mutate(fake_freq = term_freq + 0.1*dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_2_twitter_freq)

gram_2_twitter_freq <- gram_2_twitter_freq %>% arrange(desc(fake_freq))

gram_3_twitter_freq <- gram_3_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_3_twitter_freq <- gram_3_twitter_freq %>% 
  mutate(fake_freq = term_freq + 0.1*dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_3_twitter_freq)

gram_3_twitter_freq <- gram_3_twitter_freq %>% arrange(desc(fake_freq))


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
head(gram_2_twitter_freq,10)



for (i in 1:1) {
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

#1:nrow(input_gram_1)

# will print all of them
# I need to print only the first one
for (i in 3:3) {
  #print("i")
  #print(i)
  #print(input_gram_1[i,])
  for (j in 1:nrow(gram_2_twitter_freq)){
    if(input_gram_1[i,] == gram_2_twitter_freq$word1[j]) {
      print("j")
      print(j)
      print("we are in gram_2 word1")
      print(input_gram_1[i,])
      #print(gram_2_twitter_freq$word1[j])
      print("next word")
      print(gram_2_twitter_freq$word2[j])
      #print("freq")
      #print(gram_2_twitter_freq$freq[j])
      print("term freq")
      print(gram_2_twitter_freq$fake_freq[j])        
    }
  }
}

# first one

for (i in 1:3) {
  #print("i")
  #print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_2_twitter_freq)){
    if(input_gram_1[i,] == gram_2_twitter_freq$word1[j]) {
        #print("count")
        #print(count)
        if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_2 word1")
        print(input_gram_1[i,])
        #print(gram_2_twitter_freq$word1[j])
        print("next word")
        print(gram_2_twitter_freq$word2[j])
        #print("freq")
        #print(gram_2_twitter_freq$freq[j])
        print("term freq")
        print(gram_2_twitter_freq$fake_freq[j]) 
        }
        if (count == 2) {
          print("next best word")
          print("j")
          print(j)
          print("we are in gram_2 word1")
          print(input_gram_1[i,])
          #print(gram_2_twitter_freq$word1[j])
          print("next word")
          print(gram_2_twitter_freq$word2[j])
          #print("freq")
          #print(gram_2_twitter_freq$freq[j])
          print("term freq")
          print(gram_2_twitter_freq$fake_freq[j]) 
        }
        count <- count + 1
        
    }
  }
}

### no we feed data 2 grams, not just a list of words





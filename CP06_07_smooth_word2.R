library(textreadr)
library(tm)
library(tidyverse)
library(tidytext)
library(knitr)
library(stopwords)

# this is my dictionary
dataset_widenet_gram_2_twitter <- read.csv("./../skims/gram_2_blogs/gram_2_blogs_01_1.txt", stringsAsFactors = FALSE)
dataset_widenet_gram_3_twitter <- read.csv("./../skims/gram_3_blogs/gram_3_blogs_01_1.txt", stringsAsFactors = FALSE)

str(dataset_widenet_gram_2_twitter)
str(dataset_widenet_gram_3_twitter)

dataset_widenet_gram_2_twitter <- dataset_widenet_gram_2_twitter[1:1000,]
dataset_widenet_gram_3_twitter <- dataset_widenet_gram_3_twitter[1:1000,]

#unique eevrything
dataset_gram_2_twitter <- select(dataset_widenet_gram_2_twitter,c("word","freq"))
dataset_gram_2_twitter <- unique(dataset_gram_2_twitter)
dataset_gram_2_twitter <- aggregate(freq ~ word, 
                                    data=dataset_gram_2_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_gram_2_twitter)
head(dataset_gram_2_twitter)

dataset_gram_3_twitter <- select(dataset_widenet_gram_3_twitter,c("word","freq"))
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

##################################
########################runs begin
##################################
### no we feed data 2 grams, not just a list of words

gram_2_twitter_input <- read.csv("./../model/test_01/dataset_gram_2_twitter.txt", 
                                 stringsAsFactors = FALSE)
gram_2_twitter_input <- select(gram_2_twitter_input,word)
head(gram_2_twitter_input)
str(gram_2_twitter_input)
gram_2_twitter_input_split <- gram_2_twitter_input %>%
  separate(word, c("word1", "word2"), sep = " ")
head(gram_2_twitter_input_split)

gram_3_twitter_input <- read.csv("./../model/test_01/dataset_gram_3_twitter.txt", 
                                 stringsAsFactors = FALSE)
gram_3_twitter_input <- select(gram_3_twitter_input,word)
head(gram_3_twitter_input)
str(gram_3_twitter_input)
gram_3_twitter_input_split <- gram_3_twitter_input %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")
head(gram_3_twitter_input_split)

#build test output data frame gram_2
output_dfsmooth_gram_2_2 <- data.frame(input_word1=character(), input_word2=character(),
           predict_word2_first=character(),predict_word2_first_freq=numeric(),
           predict_word2_second=character(), predict_word2_second_freq=numeric(),
           stringsAsFactors=FALSE)

str(output_dfsmooth_gram_2_2)
head(output_dfsmooth_gram_2_2)
nrow(output_dfsmooth_gram_2_2)

for (i in 1:nrow(gram_2_twitter_input_split)) {
  print("i ############################################################")
  print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_2_twitter_freq)){
    if(gram_2_twitter_input_split$word1[i] == gram_2_twitter_freq$word1[j]) {
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_2 input word1")
        output_dfsmooth_gram_2_2[i,1] <- gram_2_twitter_input_split$word1[i]
        print("we are in gram_2 input word2")
        output_dfsmooth_gram_2_2[i,2] <- gram_2_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfsmooth_gram_2_2[i,3] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_2_2[i,4] <- gram_2_twitter_freq$fake_freq[j] 
      }
      if (count == 2) {
        print("next best word")        
        print("j")
        print(j)
        print("we are in gram_2 input word1")
        print(gram_2_twitter_input_split$word1[i])
        print("we are in gram_2 input word2")
        print(gram_2_twitter_input_split$word2[i])
        print("next word we predict")
        output_dfsmooth_gram_2_2[i,5] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_2_2[i,6] <- gram_2_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    } #major if loop else {
    #we want to know which word was not in our vocabulary
        output_dfsmooth_gram_2_2[i,1] <- gram_2_twitter_input_split$word1[i]
    }
  }
}

write.csv(output_dfsmooth_gram_2_2,"./../model/test_01/output_dfsmooth_gram_2_2.csv")


#build test output data frame gram_3
output_dfsmooth_gram_3_3 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfsmooth_gram_3_3)
head(output_dfsmooth_gram_3_3)
nrow(output_dfsmooth_gram_3_3)

for (i in 1:nrow(gram_3_twitter_input_split)) {
  print("i ############################################################")
  print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_3_twitter_freq)){
    if(gram_3_twitter_input_split$word1[i] == gram_3_twitter_freq$word1[j]) {
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        output_dfsmooth_gram_3_3[i,1] <- gram_3_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfsmooth_gram_3_3[i,2] <- gram_3_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfsmooth_gram_3_3[i,3] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_3_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
      }
      if (count == 2) {
        print("next best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        print(gram_3_twitter_input_split$word1[i])
        print("we are in gram_3 input word2")
        print(gram_3_twitter_input_split$word2[i])
        print("next word we predict")
        output_dfsmooth_gram_3_3[i,5] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_3_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfsmooth_gram_3_3,"./../model/test_01/output_dfsmooth_gram_3_3.csv")

###########################################################################################
#build test output data frame gram_2  but using gram_3 as input 

output_dfsmooth_gram_3_2 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfsmooth_gram_3_2)
head(output_dfsmooth_gram_3_2)
nrow(output_dfsmooth_gram_3_2)

for (i in 1:nrow(gram_3_twitter_input_split)) {
  print("i ############################################################")
  print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_2_twitter_freq)){
    if(gram_3_twitter_input_split$word1[i] == gram_2_twitter_freq$word1[j]) {
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        output_dfsmooth_gram_3_2[i,1] <- gram_3_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfsmooth_gram_3_2[i,2] <- gram_3_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfsmooth_gram_3_2[i,3] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_3_2[i,4] <- gram_2_twitter_freq$fake_freq[j] 
      }
      if (count == 2) {
        print("next best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        print(gram_3_twitter_input_split$word1[i])
        print("we are in gram_3 input word2")
        print(gram_3_twitter_input_split$word2[i])
        print("next word we predict")
        output_dfsmooth_gram_3_2[i,5] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_3_2[i,6] <- gram_2_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfsmooth_gram_3_2,"./../model/test_01/output_dfsmooth_gram_3_2.csv")


#build test output data frame gram_3 from gram_2 input

output_dfsmooth_gram_2_3 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfsmooth_gram_2_3)
head(output_dfsmooth_gram_2_3)
nrow(output_dfsmooth_gram_2_3)

for (i in 1:nrow(gram_2_twitter_input_split)) {
  print("i ############################################################")
  print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_3_twitter_freq)){
    if(gram_2_twitter_input_split$word1[i] == gram_3_twitter_freq$word1[j]) {
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        output_dfsmooth_gram_2_3[i,1] <- gram_2_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfsmooth_gram_2_3[i,2] <- gram_2_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfsmooth_gram_2_3[i,3] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_2_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
      }
      if (count == 2) {
        print("next best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        print(gram_3_twitter_input_split$word1[i])
        print("we are in gram_3 input word2")
        print(gram_3_twitter_input_split$word2[i])
        print("next word we predict")
        output_dfsmooth_gram_2_3[i,5] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfsmooth_gram_2_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfsmooth_gram_2_3,"./../model/test_01/output_dfsmooth_gram_2_3.csv")

########################
##### word3

#build test output data frame gram_3
output3_dfsmooth_gram_3_3 <- data.frame(input_word2=character(), input_word3=character(),
                                 predict_word3_first=character(),predict_word3_first_freq=numeric(),
                                 predict_word3_second=character(), predict_word3_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output3_dfsmooth_gram_3_3)
head(output3_dfsmooth_gram_3_3)
nrow(output3_dfsmooth_gram_3_3)

for (i in 1:nrow(gram_3_twitter_input_split)) {
  print("i ############################################################")
  print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_3_twitter_freq)){
    if(gram_3_twitter_input_split$word2[i] == gram_3_twitter_freq$word2[j]) {
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        output3_dfsmooth_gram_3_3[i,1] <- gram_3_twitter_input_split$word2[i]
        print("we are in gram_3 input word2")
        output3_dfsmooth_gram_3_3[i,2] <- gram_3_twitter_input_split$word3[i]
        print("next word we predict")
        output3_dfsmooth_gram_3_3[i,3] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_dfsmooth_gram_3_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
      }
      if (count == 2) {
        print("next best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        print(gram_3_twitter_input_split$word2[i])
        print("we are in gram_3 input word2")
        print(gram_3_twitter_input_split$word3[i])
        print("next word we predict")
        output3_dfsmooth_gram_3_3[i,5] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_dfsmooth_gram_3_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      } 
      count <- count + 1
      
    }
  }
}

write.csv(output3_dfsmooth_gram_3_3,"./../model/test_01/output3_dfsmooth_gram_3_3.csv")




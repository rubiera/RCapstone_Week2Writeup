library(textreadr)
library(tm)
library(tidyverse)
library(tidytext)
library(knitr)
library(stopwords)

# this is my dictionary
dataset_widenet_gram_2_twitter <- read.csv("./../skims/gram_2_blogs/gram_2_blogs_01_1.txt", stringsAsFactors = FALSE)
dataset_widenet_gram_3_twitter <- read.csv("./../skims/gram_3_blogs/gram_3_blogs_01_1.txt", stringsAsFactors = FALSE)

#unique eevrything
dataset_widenet_gram_2_twitter <- select(dataset_widenet_gram_2_twitter,c("word","freq"))
dataset_widenet_gram_2_twitter <- unique(dataset_widenet_gram_2_twitter)
dataset_widenet_gram_2_twitter <- aggregate(freq ~ word, 
                                    data=dataset_widenet_gram_2_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_widenet_gram_2_twitter)
head(dataset_widenet_gram_2_twitter)

dataset_widenet_gram_3_twitter <- select(dataset_widenet_gram_3_twitter,c("word","freq"))
dataset_widenet_gram_3_twitter <- unique(dataset_widenet_gram_3_twitter)
dataset_widenet_gram_3_twitter <- aggregate(freq ~ word, 
                                    data=dataset_widenet_gram_3_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_widenet_gram_3_twitter)
head(dataset_widenet_gram_3_twitter)

gram_2_twitter_split <- dataset_widenet_gram_2_twitter %>%
  separate(word, c("word1", "word2"), sep = " ")

head(gram_2_twitter_split)

gram_3_twitter_split <- dataset_widenet_gram_3_twitter %>%
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

gram_2_twitter_input <- read.csv("./../barrel/gram_2/gram_2_twitter_happi_01_1.txt", 
                                 stringsAsFactors = FALSE)
gram_2_twitter_input <- select(gram_2_twitter_input,word)
head(gram_2_twitter_input)
str(gram_2_twitter_input)
gram_2_twitter_input_split <- gram_2_twitter_input %>%
  separate(word, c("word1", "word2"), sep = " ")
head(gram_2_twitter_input_split)

gram_3_twitter_input <- read.csv("./../barrel/gram_3/gram_3_twitter_happi_01_1.txt", 
                                 stringsAsFactors = FALSE)
gram_3_twitter_input <- select(gram_3_twitter_input,word)
head(gram_3_twitter_input)
str(gram_3_twitter_input)
gram_3_twitter_input_split <- gram_3_twitter_input %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")
head(gram_3_twitter_input_split)


#########

gram_2_twitter_input_split$word1[1]
gram_2_twitter_freq$word1[1]

head(gram_2_twitter_input_split,3)

for (i in 1:3) {
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
        print(gram_2_twitter_input_split$word1[i])
        print("we are in gram_2 input word2")
        print(gram_2_twitter_input_split$word2[i])
        print("next word we predict")
        print(gram_2_twitter_freq$word2[j])
        print("term freq")
        print(gram_2_twitter_freq$fake_freq[j]) 
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
        print(gram_2_twitter_freq$word2[j])
        print("term freq")
        print(gram_2_twitter_freq$fake_freq[j]) 
      }
      count <- count + 1
      
    }
  }
}

#build test output data frame gram_2
output_dfwidenet_gram_2_2 <- data.frame(input_word1=character(), input_word2=character(),
           predict_word2_first=character(),predict_word2_first_freq=numeric(),
           predict_word2_second=character(), predict_word2_second_freq=numeric(),
           stringsAsFactors=FALSE)

str(output_dfwidenet_gram_2_2)
head(output_dfwidenet_gram_2_2)
nrow(output_dfwidenet_gram_2_2)

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
        output_dfwidenet_gram_2_2[i,1] <- gram_2_twitter_input_split$word1[i]
        print("we are in gram_2 input word2")
        output_dfwidenet_gram_2_2[i,2] <- gram_2_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfwidenet_gram_2_2[i,3] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_2_2[i,4] <- gram_2_twitter_freq$fake_freq[j] 
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
        output_dfwidenet_gram_2_2[i,5] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_2_2[i,6] <- gram_2_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfwidenet_gram_2_2,"./../model/test_01/output_dfwidenet_gram_2_2.csv")


#build test output data frame gram_3
output_dfwidenet_gram_3_3 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfwidenet_gram_3_3)
head(output_dfwidenet_gram_3_3)
nrow(output_dfwidenet_gram_3_3)

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
        output_dfwidenet_gram_3_3[i,1] <- gram_3_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfwidenet_gram_3_3[i,2] <- gram_3_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfwidenet_gram_3_3[i,3] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_3_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output_dfwidenet_gram_3_3[i,5] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_3_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfwidenet_gram_3_3,"./../model/test_01/output_dfwidenet_gram_3_3.csv")

###########################################################################################
#build test output data frame gram_2  but using gram_3 as input 

output_dfwidenet_gram_3_2 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfwidenet_gram_3_2)
head(output_dfwidenet_gram_3_2)
nrow(output_dfwidenet_gram_3_2)

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
        output_dfwidenet_gram_3_2[i,1] <- gram_3_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfwidenet_gram_3_2[i,2] <- gram_3_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfwidenet_gram_3_2[i,3] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_3_2[i,4] <- gram_2_twitter_freq$fake_freq[j] 
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
        output_dfwidenet_gram_3_2[i,5] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_3_2[i,6] <- gram_2_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfwidenet_gram_3_2,"./../model/test_01/output_dfwidenet_gram_3_2.csv")


#build test output data frame gram_3 from gram_2 input

output_dfwidenet_gram_2_3 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfwidenet_gram_2_3)
head(output_dfwidenet_gram_2_3)
nrow(output_dfwidenet_gram_2_3)

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
        output_dfwidenet_gram_2_3[i,1] <- gram_2_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfwidenet_gram_2_3[i,2] <- gram_2_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfwidenet_gram_2_3[i,3] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_2_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output_dfwidenet_gram_2_3[i,5] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfwidenet_gram_2_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfwidenet_gram_2_3,"./../model/test_01/output_dfwidenet_gram_2_3.csv")

########################
##### word3

#build test output data frame gram_3
output3_dfwidenet_gram_3_3 <- data.frame(input_word2=character(), input_word3=character(),
                                 predict_word3_first=character(),predict_word3_first_freq=numeric(),
                                 predict_word3_second=character(), predict_word3_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output3_dfwidenet_gram_3_3)
head(output3_dfwidenet_gram_3_3)
nrow(output3_dfwidenet_gram_3_3)

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
        output3_dfwidenet_gram_3_3[i,1] <- gram_3_twitter_input_split$word2[i]
        print("we are in gram_3 input word2")
        output3_dfwidenet_gram_3_3[i,2] <- gram_3_twitter_input_split$word3[i]
        print("next word we predict")
        output3_dfwidenet_gram_3_3[i,3] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_dfwidenet_gram_3_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output3_dfwidenet_gram_3_3[i,5] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_dfwidenet_gram_3_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output3_dfwidenet_gram_3_3,"./../model/test_01/output3_dfwidenet_gram_3_3.csv")




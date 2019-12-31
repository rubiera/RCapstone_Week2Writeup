library(textreadr)
library(tm)
library(tidyverse)
library(tidytext)
library(knitr)
library(stopwords)

# this is my dictionary
dataset_gram_3_twitter <- read.csv("./../model/test_01/dataset_gram_3_twitter.txt", stringsAsFactors = FALSE)

dataset_gram_3_twitter <- select(dataset_gram_3_twitter,c("word","freq"))
dataset_gram_3_twitter <- unique(dataset_gram_3_twitter)
dataset_gram_3_twitter <- aggregate(freq ~ word, 
                                    data=dataset_gram_3_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_gram_3_twitter)
head(dataset_gram_3_twitter)

gram_3_twitter_split <- dataset_gram_3_twitter %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

head(gram_3_twitter_split)


gram_3_twitter_freq <- gram_3_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_3_twitter_freq <- gram_3_twitter_freq %>% 
  mutate(fake_freq = term_freq + 0.1*dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_3_twitter_freq)

gram_3_twitter_freq <- gram_3_twitter_freq %>% arrange(desc(fake_freq))

### now we feed data 2 grams, not just a list of words


gram_3_twitter_input <- read.csv("./../barrel/gram_3/gram_3_twitter_happi_01_1.txt", 
                                 stringsAsFactors = FALSE)
gram_3_twitter_input <- select(gram_3_twitter_input,word)
head(gram_3_twitter_input)
str(gram_3_twitter_input)
gram_3_twitter_input_split <- gram_3_twitter_input %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")
head(gram_3_twitter_input_split)



########################
##### word3

#build test output data frame gram_3
output3_df_gram_3_3_word3 <- data.frame(input_word2=character(), input_word3=character(),
                                 predict_word3_first=character(),predict_word3_first_freq=numeric(),
                                 predict_word3_second=character(), predict_word3_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output3_df_gram_3_3_word3)
head(output3_df_gram_3_3_word3)
nrow(output3_df_gram_3_3_word3)

head(gram_3_twitter_freq)

for (i in 1:nrow(gram_3_twitter_input_split)) {
  print("i ############################################################")
  print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_3_twitter_freq)){
    if( (gram_3_twitter_input_split$word1[i] == gram_3_twitter_freq$word1[j])
        && 
        (gram_3_twitter_input_split$word2[i] == gram_3_twitter_freq$word2[j])    ){
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        output3_df_gram_3_3_word3[i,1] <- gram_3_twitter_input_split$word2[i]
        print("we are in gram_3 input word2")
        output3_df_gram_3_3_word3[i,2] <- gram_3_twitter_input_split$word3[i]
        print("next word we predict")
        output3_df_gram_3_3_word3[i,3] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_df_gram_3_3_word3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output3_df_gram_3_3_word3[i,5] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_df_gram_3_3_word3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}




write.csv(output3_df_gram_3_3_word3,"./../model/test_01/output3_df_gram_3_3_word3.csv")

#############################
#############################
#############################
#### use a wider vocabulary that may not have the barrel n grams

dataset_widenet_gram_3_twitter <- read.csv("./../skims/gram_3_blogs/gram_3_blogs_01_1.txt", stringsAsFactors = FALSE)
dataset_gram_3_twitter <- select(dataset_gram_3_twitter,c("word","freq"))
dataset_gram_3_twitter <- unique(dataset_gram_3_twitter)
dataset_gram_3_twitter <- aggregate(freq ~ word, 
                                    data=dataset_gram_3_twitter,FUN=sum, 
                                    na.rm = TRUE) 

str(dataset_gram_3_twitter)
head(dataset_gram_3_twitter)

gram_3_twitter_split <- dataset_gram_3_twitter %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

head(gram_3_twitter_split)


gram_3_twitter_freq <- gram_3_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_3_twitter_freq <- gram_3_twitter_freq %>% 
  mutate(fake_freq = term_freq + 0.1*dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_3_twitter_freq)

gram_3_twitter_freq <- gram_3_twitter_freq %>% arrange(desc(fake_freq))



#build test output data frame gram_3
output3_df_gram_3_3_word3 <- data.frame(input_word2=character(), input_word3=character(),
                                        predict_word3_first=character(),predict_word3_first_freq=numeric(),
                                        predict_word3_second=character(), predict_word3_second_freq=numeric(),
                                        stringsAsFactors=FALSE)

str(output3_df_gram_3_3_word3)
head(output3_df_gram_3_3_word3)
nrow(output3_df_gram_3_3_word3)

head(gram_3_twitter_freq)


for (i in 1:nrow(gram_3_twitter_input_split)) {
  print("i ############################################################")
  print(i)
  #print(input_gram_1[i,])
  count <- 1
  for (j in 1:nrow(gram_3_twitter_freq)){
    if( (gram_3_twitter_input_split$word1[i] == gram_3_twitter_freq$word1[j])
        && 
        (gram_3_twitter_input_split$word2[i] == gram_3_twitter_freq$word2[j])    ){
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        print("we are in gram_3 input word1")
        output3_df_gram_3_3_word3[i,1] <- gram_3_twitter_input_split$word2[i]
        print("we are in gram_3 input word2")
        output3_df_gram_3_3_word3[i,2] <- gram_3_twitter_input_split$word3[i]
        print("next word we predict")
        output3_df_gram_3_3_word3[i,3] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_df_gram_3_3_word3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output3_df_gram_3_3_word3[i,5] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_df_gram_3_3_word3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}


#############################

write.csv(output3_df_gram_3_3_word3,"./../model/test_01/output3_df_gram_3_3_word3_wrong.csv")


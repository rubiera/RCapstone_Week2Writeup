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

dataset_widenet_gram_2_twitter <- filter(dataset_widenet_gram_2_twitter, freq != 0)

str(dataset_widenet_gram_2_twitter)
head(dataset_widenet_gram_2_twitter)

dataset_widenet_gram_3_twitter <- select(dataset_widenet_gram_3_twitter,c("word","freq"))
dataset_widenet_gram_3_twitter <- unique(dataset_widenet_gram_3_twitter)
dataset_widenet_gram_3_twitter <- aggregate(freq ~ word, 
                                    data=dataset_widenet_gram_3_twitter,FUN=sum, 
                                    na.rm = TRUE) 

dataset_widenet_gram_3_twitter <- filter(dataset_widenet_gram_3_twitter, freq != 0)

str(dataset_widenet_gram_3_twitter)
head(dataset_widenet_gram_3_twitter)

gram_2_twitter_split <- dataset_widenet_gram_2_twitter %>%
  separate(word, c("word1", "word2"), sep = " ")

gram_2_twitter_split <- filter(gram_2_twitter_split, freq != 0)
head(gram_2_twitter_split)

gram_3_twitter_split <- dataset_widenet_gram_3_twitter %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

gram_3_twitter_split <- filter(gram_3_twitter_split, freq != 0)
head(gram_3_twitter_split)

gram_2_twitter_freq <- gram_2_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_2_twitter_freq <- gram_2_twitter_freq %>% 
  mutate(fake_freq = term_freq + 0.1*dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_2_twitter_freq)

gram_2_twitter_freq <- gram_2_twitter_freq %>% arrange(desc(fake_freq))
head(gram_2_twitter_freq)

gram_3_twitter_freq <- gram_3_twitter_split %>% mutate(term_freq = freq/sum(freq))
gram_3_twitter_freq <- gram_3_twitter_freq %>% 
  mutate(fake_freq = term_freq + 0.1*dnorm(rnorm(freq))/sum(dnorm(rnorm(freq))))
head(gram_3_twitter_freq)

gram_3_twitter_freq <- gram_3_twitter_freq %>% arrange(desc(fake_freq))
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
head(gram_2_twitter_freq,10)


### no we feed data 2 grams, not just a list of words

gram_2_twitter_input <- read.csv("./../skims/gram_2_blogs/gram_2_blogs_01_1.txt", 
                                 stringsAsFactors = FALSE)
gram_2_twitter_input <- select(gram_2_twitter_input,word)
head(gram_2_twitter_input)
str(gram_2_twitter_input)
gram_2_twitter_input_split <- gram_2_twitter_input %>%
  separate(word, c("word1", "word2"), sep = " ")
head(gram_2_twitter_input_split)

gram_2_twitter_input_split<- na.omit(gram_2_twitter_input_split)

gram_3_twitter_input <- read.csv("./../skims/gram_3_blogs/gram_3_blogs_01_1.txt", 
                                 stringsAsFactors = FALSE)
gram_3_twitter_input <- select(gram_3_twitter_input,word)
head(gram_3_twitter_input)
str(gram_3_twitter_input)
gram_3_twitter_input_split <- gram_3_twitter_input %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")
head(gram_3_twitter_input_split)

gram_3_twitter_input_split <- na.omit(gram_3_twitter_input_split)

########



#  chunk gram_2_twitter_input_split
head(gram_2_twitter_freq)

gram_2_twitter_freq <- arrange(gram_2_twitter_freq,word1)
gram_3_twitter_freq <- arrange(gram_3_twitter_freq,word1)
gram_2_twitter_input_split <- arrange(gram_2_twitter_input_split,word1)
gram_3_twitter_input_split <- arrange(gram_3_twitter_input_split,word1)

nrow(gram_2_twitter_freq)
nrow(gram_2_twitter_input_split)

nrow(gram_3_twitter_freq)
nrow(gram_3_twitter_input_split)

nrow(gram_2_twitter_freq)

sumcheck = filter(gram_2_twitter_freq, str_detect(word1, "^a")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^b")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^c")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^d")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^e")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^f")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^g")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^h")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^i")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^j")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, ("^k|^l"))) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^m")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, ("^n|^o"))) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^p")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, ("^q|^r"))) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, "^s")) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, ("^t|^u|^v"))) %>% nrow() +
filter(gram_2_twitter_freq, str_detect(word1, ("^w|^x|^y|^z"))) %>% nrow()
#[1] 152156

##############################


gram_2_twitter_freq_1   <- filter(gram_2_twitter_freq, str_detect(word1, "^a"))
gram_2_twitter_freq_2   <- filter(gram_2_twitter_freq, str_detect(word1, "^b"))
gram_2_twitter_freq_3   <- filter(gram_2_twitter_freq, str_detect(word1, "^c"))
gram_2_twitter_freq_4  <- filter(gram_2_twitter_freq, str_detect(word1, "^d"))
gram_2_twitter_freq_5   <- filter(gram_2_twitter_freq, str_detect(word1, "^e"))
gram_2_twitter_freq_6   <- filter(gram_2_twitter_freq, str_detect(word1, "^f"))
gram_2_twitter_freq_7   <- filter(gram_2_twitter_freq, str_detect(word1, "^g"))
gram_2_twitter_freq_8   <- filter(gram_2_twitter_freq, str_detect(word1, "^h"))
gram_2_twitter_freq_9   <- filter(gram_2_twitter_freq, str_detect(word1, "^i"))
gram_2_twitter_freq_10   <- filter(gram_2_twitter_freq, str_detect(word1, "^j"))
gram_2_twitter_freq_11   <- filter(gram_2_twitter_freq, str_detect(word1, ("^k|^l")))
gram_2_twitter_freq_12   <- filter(gram_2_twitter_freq, str_detect(word1, "^m"))
gram_2_twitter_freq_13   <- filter(gram_2_twitter_freq, str_detect(word1, ("^n|^o")))
gram_2_twitter_freq_14   <- filter(gram_2_twitter_freq, str_detect(word1, "^p"))
gram_2_twitter_freq_15   <- filter(gram_2_twitter_freq, str_detect(word1, ("^q|^r")))
gram_2_twitter_freq_16  <- filter(gram_2_twitter_freq, str_detect(word1, "^s"))
gram_2_twitter_freq_17   <- filter(gram_2_twitter_freq, str_detect(word1, ("^t|^u|^v")))
gram_2_twitter_freq_18   <- filter(gram_2_twitter_freq, str_detect(word1, ("^w|^x|^y|^z")))


gram_2_twitter_input_split_1   <- filter(gram_2_twitter_input_split, str_detect(word1, "^a"))
gram_2_twitter_input_split_2   <- filter(gram_2_twitter_input_split, str_detect(word1, "^b"))
gram_2_twitter_input_split_3   <- filter(gram_2_twitter_input_split, str_detect(word1, "^c"))
gram_2_twitter_input_split_4  <- filter(gram_2_twitter_input_split, str_detect(word1, "^d"))
gram_2_twitter_input_split_5   <- filter(gram_2_twitter_input_split, str_detect(word1, "^e"))
gram_2_twitter_input_split_6   <- filter(gram_2_twitter_input_split, str_detect(word1, "^f"))
gram_2_twitter_input_split_7   <- filter(gram_2_twitter_input_split, str_detect(word1, "^g"))
gram_2_twitter_input_split_8   <- filter(gram_2_twitter_input_split, str_detect(word1, "^h"))
gram_2_twitter_input_split_9   <- filter(gram_2_twitter_input_split, str_detect(word1, "^i"))
gram_2_twitter_input_split_10   <- filter(gram_2_twitter_input_split, str_detect(word1, "^j"))
gram_2_twitter_input_split_11   <- filter(gram_2_twitter_input_split, str_detect(word1, ("^k|^l")))
gram_2_twitter_input_split_12   <- filter(gram_2_twitter_input_split, str_detect(word1, "^m"))
gram_2_twitter_input_split_13   <- filter(gram_2_twitter_input_split, str_detect(word1, ("^n|^o")))
gram_2_twitter_input_split_14   <- filter(gram_2_twitter_input_split, str_detect(word1, "^p"))
gram_2_twitter_input_split_15   <- filter(gram_2_twitter_input_split, str_detect(word1, ("^q|^r")))
gram_2_twitter_input_split_16  <- filter(gram_2_twitter_input_split, str_detect(word1, "^s"))
gram_2_twitter_input_split_17   <- filter(gram_2_twitter_input_split, str_detect(word1, ("^t|^u|^v")))
gram_2_twitter_input_split_18   <- filter(gram_2_twitter_input_split, str_detect(word1, ("^w|^x|^y|^z")))



paste("gram_2_twitter_freq_",index,sep="")
paste("gram_2_twitter_input_split_",index,sep="")


get(paste("gram_2_twitter_input_split_",1,sep=""))

#quick test (things are working)
gram_2_twitter_freq_1   <- filter(gram_2_twitter_freq, str_detect(word1, "^aa"))
gram_2_twitter_input_split_1   <- filter(gram_2_twitter_input_split, str_detect(word1, "^aa"))


##############################

#build test output data frame gram_2
output_dfmedium_gram_2_2 <- data.frame(input_word1=character(), input_word2=character(),
                                       predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                       predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                       stringsAsFactors=FALSE)

str(output_dfmedium_gram_2_2)
head(output_dfmedium_gram_2_2)
nrow(output_dfmedium_gram_2_2)


for (index in 1:1) {

  gram_2_twitter_input_current <- get(paste("gram_2_twitter_input_split_",index,sep=""))
  gram_2_twitter_freq_current <- get(paste("gram_2_twitter_freq_",index,sep=""))
#these for loops take way too long for a medium to large vocabulary
#breaking up the vocabulary into 18 files

for (i in 1:nrow(gram_2_twitter_input_current)) {
  count <- 1
  for (j in 1:nrow(gram_2_twitter_freq_current)){
    
    print("we are running on file")
    print(index)
    
    if(gram_2_twitter_input_current$word1[i] == gram_2_twitter_freq_current$word1[j]) {
      if (count == 1) {
        print("best word")        
        print("j")
        print(j)
        output_dfmedium_gram_2_2[i,1] <- gram_2_twitter_input_current$word1[i]
        output_dfmedium_gram_2_2[i,2] <- gram_2_twitter_input_current$word2[i]
        output_dfmedium_gram_2_2[i,3] <- gram_2_twitter_freq_current$word2[j]
        output_dfmedium_gram_2_2[i,4] <- gram_2_twitter_freq_current$fake_freq[j] 
      }
      if (count == 2) {
        print("next best word")        
        print("j")
       print(j)
        output_dfmedium_gram_2_2[i,5] <- gram_2_twitter_freq_current$word2[j]
        output_dfmedium_gram_2_2[i,6] <- gram_2_twitter_freq_current$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}
  
}  # end of 2_2 18 files
write.csv(output_dfmedium_gram_2_2,paste("./../model/test_01/output_dfmedium_gram_2_2_",index,".csv",sep=""))

############################


#build test output data frame gram_3
output_dfmedium_gram_3_3 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfmedium_gram_3_3)
head(output_dfmedium_gram_3_3)
nrow(output_dfmedium_gram_3_3)

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
        output_dfmedium_gram_3_3[i,1] <- gram_3_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfmedium_gram_3_3[i,2] <- gram_3_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfmedium_gram_3_3[i,3] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfmedium_gram_3_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output_dfmedium_gram_3_3[i,5] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfmedium_gram_3_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfmedium_gram_3_3,"./../model/test_01/output_dfmedium_gram_3_3.csv")

###########################################################################################
#build test output data frame gram_2  but using gram_3 as input 

output_dfmedium_gram_3_2 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfmedium_gram_3_2)
head(output_dfmedium_gram_3_2)
nrow(output_dfmedium_gram_3_2)

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
        output_dfmedium_gram_3_2[i,1] <- gram_3_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfmedium_gram_3_2[i,2] <- gram_3_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfmedium_gram_3_2[i,3] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfmedium_gram_3_2[i,4] <- gram_2_twitter_freq$fake_freq[j] 
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
        output_dfmedium_gram_3_2[i,5] <- gram_2_twitter_freq$word2[j]
        print("term freq")
        output_dfmedium_gram_3_2[i,6] <- gram_2_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfmedium_gram_3_2,"./../model/test_01/output_dfmedium_gram_3_2.csv")


#build test output data frame gram_3 from gram_2 input

output_dfmedium_gram_2_3 <- data.frame(input_word1=character(), input_word2=character(),
                                 predict_word2_first=character(),predict_word2_first_freq=numeric(),
                                 predict_word2_second=character(), predict_word2_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output_dfmedium_gram_2_3)
head(output_dfmedium_gram_2_3)
nrow(output_dfmedium_gram_2_3)

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
        output_dfmedium_gram_2_3[i,1] <- gram_2_twitter_input_split$word1[i]
        print("we are in gram_3 input word2")
        output_dfmedium_gram_2_3[i,2] <- gram_2_twitter_input_split$word2[i]
        print("next word we predict")
        output_dfmedium_gram_2_3[i,3] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfmedium_gram_2_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output_dfmedium_gram_2_3[i,5] <- gram_3_twitter_freq$word2[j]
        print("term freq")
        output_dfmedium_gram_2_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output_dfmedium_gram_2_3,"./../model/test_01/output_dfmedium_gram_2_3.csv")

########################
##### word3

#build test output data frame gram_3
output3_dfmedium_gram_3_3 <- data.frame(input_word2=character(), input_word3=character(),
                                 predict_word3_first=character(),predict_word3_first_freq=numeric(),
                                 predict_word3_second=character(), predict_word3_second_freq=numeric(),
                                 stringsAsFactors=FALSE)

str(output3_dfmedium_gram_3_3)
head(output3_dfmedium_gram_3_3)
nrow(output3_dfmedium_gram_3_3)

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
        output3_dfmedium_gram_3_3[i,1] <- gram_3_twitter_input_split$word2[i]
        print("we are in gram_3 input word2")
        output3_dfmedium_gram_3_3[i,2] <- gram_3_twitter_input_split$word3[i]
        print("next word we predict")
        output3_dfmedium_gram_3_3[i,3] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_dfmedium_gram_3_3[i,4] <- gram_3_twitter_freq$fake_freq[j] 
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
        output3_dfmedium_gram_3_3[i,5] <- gram_3_twitter_freq$word3[j]
        print("term freq")
        output3_dfmedium_gram_3_3[i,6] <- gram_3_twitter_freq$fake_freq[j]
      }
      count <- count + 1
      
    }
  }
}

write.csv(output3_dfmedium_gram_3_3,"./../model/test_01/output3_dfmedium_gram_3_3.csv")




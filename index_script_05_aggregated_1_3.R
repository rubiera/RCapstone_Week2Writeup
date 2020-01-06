Sys.setenv(JAVA_HOME="")
Sys.setenv(WNHOME="C:\\Program Files (x86)\\WordNet\\2.1\\dict")
library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(rJava)
library(RWeka)
library(knitr)
library(quanteda)
library(tidytext)
library(data.table)
library(wordnet)
setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")
library(wordnet)


gram_1_aggr <- fread("./../skims/merged_grams/dataset_gram_1_aggregated.txt")
gram_2_aggr <- fread("./../skims/merged_grams/dataset_gram_2_aggregated.txt")
gram_3_aggr <- fread("./../skims/merged_grams/dataset_gram_3_aggregated.txt")

gram_1_aggr <- gram_1_aggr[freq >= 2]
gram_2_aggr <- gram_2_aggr[freq >= 20]
gram_3_aggr <- gram_3_aggr[freq >= 4]

gram_2_aggr <- filter(gram_2_aggr, word != "")
gram_3_aggr <- filter(gram_3_aggr, word != "")

table(gram_1_aggr$freq)
table(gram_2_aggr$freq)
table(gram_3_aggr$freq)

#### we are building a whole model that goes to n=3 wish me luck
str(gram_1_aggr)
#160415
str(gram_2_aggr)
#88756
str(gram_3_aggr)
#96429

write.csv(gram_1_aggr, "./../model/training/gram_1_aggr_freq2plus.csv")
write.csv(gram_2_aggr, "./../model/training/gram_2_aggr_freq20plus.csv")
write.csv(gram_3_aggr, "./../model/training/gram_3_aggr_freq4plus.csv")

head(gram_1_aggr$word,100)

##### we start in markdown here

gram_1_aggr <- read.csv("./../model/training/gram_1_aggr_freq2plus.csv", stringsAsFactors = FALSE)
gram_2_aggr <- read.csv("./../model/training/gram_2_aggr_freq20plus.csv", stringsAsFactors = FALSE)
gram_3_aggr <- read.csv("./../model/training/gram_3_aggr_freq4plus.csv", stringsAsFactors = FALSE)
str(gram_1_aggr)
str(gram_2_aggr)
str(gram_3_aggr)


head(gram_1_aggr$word,100)



########################################
########################################
########################################
#### simple model minus wordnet
########################################
#CPW2A model is just one word = time

gram_2_aggr_split <- gram_2_aggr %>% separate(word, c("word1", "word2"), sep = " ")
gram_3_aggr_split <- gram_3_aggr %>% separate(word, c("word1", "word2", "word3"), sep = " ")
head(gram_2_aggr_split)
head(gram_3_aggr_split)

########################################
#CPW2A model is just one word = time

gram_2_aggr_time <- filter(gram_2_aggr_split, word1 == "time")
head(gram_2_aggr_time)
tail(gram_2_aggr_time)
str(gram_2_aggr_time)
table(gram_2_aggr_time$word1)
#time 
#1031

#nrow(gram_2_aggr_time)
head(gram_2_aggr_time$word1,100)
head(gram_3_aggr_split$word1,100)

str(gram_3_aggr_split)

gram_3_aggr_split %>% filter(word1 == "time")

#gram_3_aggr_split %>% filter(word1 == "time" && word2 == "year")

gram_2_aggr_time[1,]
gram_3_aggr_split[1,]

gram_3_aggr_split %>% filter(word1 == "amazon") %>%  filter(word2 == "servic")
gram_3_aggr_split %>% filter(word1 == "time") %>%  filter(word2 == "year")

gram_2_aggr_time <- filter(gram_2_aggr_split, word1 == "time")

gram_2_aggr_time_freqs <- gram_2_aggr_time %>% mutate(term_freq2 = freq/sum(freq))
gram_2_aggr_time_freqs <- gram_2_aggr_time_freqs %>% 
  mutate(freq2 = freq) %>% select(-freq)
head(gram_2_aggr_time_freqs)

length(gram_2_aggr_time_freqs)
gram_2_aggr_time_freqs[1,1:5]
#V1 word1 word2 term_freq2 freq2
#1 50  time  year 0.02193453  1757

write.csv(gram_2_aggr_time_freqs,"./../model/training/test_01_time/gram_2_aggr_time_freqs.csv")
write.csv(gram_3_aggr_split,"./../model/training/test_01_time/gram_3_aggr_split.csv")

filter(gram_3_aggr_split, word1 == "true")

################################################
################################################

#test with freq >= 300 for gram_2_aggr_time
gram_2_aggr_split <- gram_2_aggr %>% separate(word, c("word1", "word2"), sep = " ")
gram_3_aggr_split <- gram_3_aggr %>% separate(word, c("word1", "word2", "word3"), sep = " ")
head(gram_2_aggr_split)
head(gram_3_aggr_split)
gram_2_aggr_split <- filter(gram_2_aggr_split, word1 == "time")
head(gram_2_aggr_split,20)
str(gram_2_aggr_split)

gram_2_aggr_time_freqs <- gram_2_aggr_split %>% mutate(term_freq2 = freq/sum(freq))
gram_2_aggr_time_freqs <- gram_2_aggr_time_freqs %>% 
  mutate(freq2 = freq) %>% select(-freq)
gram_2_aggr_time_freqs <- gram_2_aggr_time_freqs %>% filter(freq2 >= 300)
gram_2_aggr_time_freqs


gram_2_aggr_split_backtrack <- gram_2_aggr %>% separate(word, c("word1", "word2"), sep = " ")

gram_2_aggr_time_freqs[1,4:5]
################################################
################################################
################################################
gram_2_aggr_split_backtrack <- gram_2_aggr %>% separate(word, c("word1", "word2"), sep = " ")
head(gram_2_aggr_split_backtrack)
gram_2_aggr_split_word_001 <- count(gram_2_aggr_split_backtrack, word1) %>% filter(n == 1)
head(gram_2_aggr_split_word_001)
nrow(gram_2_aggr_split_word_001)
gram_2_aggr_split_backtrack %>% filter(word1 == "abbey")
gram_2_aggr_split_word_002 <- count(gram_2_aggr_split_backtrack, word1) %>% filter(n == 2)
head(gram_2_aggr_split_word_002)
nrow(gram_2_aggr_split_word_002)
gram_2_aggr_split_backtrack %>% filter(word1 == "advantag")
gram_2_aggr_split_word_005 <- count(gram_2_aggr_split_backtrack, word1) %>% filter(n == 5)
head(gram_2_aggr_split_word_005)
nrow(gram_2_aggr_split_word_005)
gram_2_aggr_split_backtrack %>% filter(word1 == "agent")
gram_2_aggr_split_word_010 <- count(gram_2_aggr_split_backtrack, word1) %>% filter(n == 10)
head(gram_2_aggr_split_word_010)
nrow(gram_2_aggr_split_word_010)
gram_2_aggr_split_backtrack %>% filter(word1 == "amount")
gram_2_aggr_split_word_020 <- count(gram_2_aggr_split_backtrack, word1) %>% filter(n == 20)
head(gram_2_aggr_split_word_020)
nrow(gram_2_aggr_split_word_020)
gram_2_aggr_split_backtrack %>% filter(word1 == "collect")




################################################
################################################
# nrow(gram_2_aggr_time
gram_3_aggr_time <- 0
gram_2_aggr_time_backout <- 0
for (i in 1:nrow(gram_2_aggr_time_freqs)) {

  print("number")
  print(i)
  print("word2")
  print(gram_2_aggr_time_freqs$word2[i])
  
  gram_3_aggr_time <- 0
  
  for (j in 1:nrow(gram_3_aggr_split)) {
    

        if(  (gram_2_aggr_time_freqs$word1[i] == gram_3_aggr_split$word1[j] &&
              gram_2_aggr_time_freqs$word2[i] == gram_3_aggr_split$word2[j]) ){
                  
                  if (!exists("gram_3_aggr_time")){
                    gram_3_aggr_time <- 0
                  }
                  
                  # if the merged dataset does exist, append to it
                  if (exists("gram_3_aggr_time")){
                    
                    gram_2_aggr_time_backout <- 0
                    for (k in 1:nrow(gram_2_aggr_split_backtrack)) {
                      #back track to 2 gram for word2 and word3 and get it from gram_2_aggr_split
                      if(  (gram_2_aggr_split_backtrack$word1[k] == gram_3_aggr_split$word2[j] &&
                            gram_2_aggr_split_backtrack$word2[k] == gram_3_aggr_split$word3[j])  ){
                        
                        gram_2_aggr_time_backout <- gram_2_aggr_split_backtrack[k,]
                        print("backout")
                        print(gram_2_aggr_time_backout)
                        print(gram_2_aggr_time_backout$freq)
                        gram_2_aggr_time_backout <- gram_2_aggr_time_backout %>% 
                          rename(
                            freq23 = freq
                          )
                        freq23 <- gram_2_aggr_time_backout$freq23
                        print("freq23")
                        print(freq23)
                        
                      }#back track
                    }#k for loop
                    
                    freq2 <- gram_2_aggr_time_freqs[i,5] 
#                    temp_dataset <- gram_3_aggr_split[j,]
                    temp_dataset <- cbind(freq2,freq23, gram_3_aggr_split[j,])
                    gram_3_aggr_time <- rbind(gram_3_aggr_time, temp_dataset)
                    rm(temp_dataset)
                    
                  }#gram_3_aggr_time exists
                
        }#top if loop
  }#j for loop
  
  #head(gram_3_aggr_time$word1,100)
  gram_3_aggr_time <- filter(gram_3_aggr_time, freq != 0)
  gram_3_aggr_time <- gram_3_aggr_time %>% arrange(desc(freq))
  gram_3_aggr_time <- gram_3_aggr_time %>% mutate(freq3 = freq) %>% select(-freq)

  
  ## put in frequency as a fraction with a minor disturbance term
  
  gram_3_aggr_time_freqs <- gram_3_aggr_time %>% mutate(term_freq3 = freq3/sum(freq3))
  gram_3_aggr_time_freqs <- gram_3_aggr_time_freqs %>% 
    mutate(term_freq23 = freq23/sum(freq23)) 
  write.csv(gram_3_aggr_time_freqs,paste0("./../model/training/test_01_time/gram_3_aggr_time_",gram_2_aggr_time_freqs$word2[i],"_freqs.csv"))

}#i for loop

################################################
################################################
################################################
################################################
################################################
################################################
################################################



time_day <- read.csv("./../model/training/test_01_time/gram_3_aggr_time_day_freqs.csv")
time_day


time_write <- read.csv("./../model/training/test_01_time/gram_3_aggr_time_write_freqs.csv")
time_write


## n = 3 low perplexity examples


20

```{r draft-model-3-20, warning=FALSE, message=FALSE}
gram_3_aggr_split_word_020 <- count(gram_3_aggr_split, word2) %>% filter(n == 20)
head(gram_3_aggr_split_word_020)
nrow(gram_3_aggr_split_word_020)
gram_3_aggr_split %>% filter(word2 == "comfort") %>% arrange(word3)
```
gram_3_aggr_split %>% filter(word2 == "comfort") %>% arrange(word1) %>% arrange(word2) %>% arrange(word3)




################################

#look up every word in wordnet for synonyms and come up with an additional list to add to gram_1_aggr

checkWordnet <- function(checkword,found) {
  word <- checkword
  filter <- getTermFilter("ExactMatchFilter", word, TRUE)
  terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 1, filter)
  terms_ADVERB <- getIndexTerms("ADVERB", 1, filter)
  terms_NOUN <- getIndexTerms("NOUN", 1, filter)
  terms_VERB <- getIndexTerms("VERB", 1, filter)
  if(is.null(terms_ADJECTIVE) == TRUE &&
     is.null(terms_ADVERB) == TRUE &&
     is.null(terms_NOUN) == TRUE &&
     is.null(terms_VERB) == TRUE){
    print(paste0(checkword," is not in wordnet"))
    found <- FALSE
  } 
  if(!is.null(terms_ADJECTIVE) == TRUE ||
     !is.null(terms_ADVERB) == TRUE ||
     !is.null(terms_NOUN) == TRUE ||
     !is.null(terms_VERB) == TRUE){
    print(paste0(checkword," is in wordnet"))
    found <- TRUE    
  } 
  return(found)
}

checkWordnet_synonyms <- function(checkword) {
  word <- checkword
  filter <- getTermFilter("ExactMatchFilter", word, TRUE)
  terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 1, filter)
  terms_ADVERB <- getIndexTerms("ADVERB", 1, filter)
  terms_NOUN <- getIndexTerms("NOUN", 1, filter)
  terms_VERB <- getIndexTerms("VERB", 1, filter)
  
  if(!is.null(terms_ADJECTIVE) == TRUE)
  {
    print(paste0(word," is an ADJECTIVE"))
    terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 10, filter)
    print(getSynonyms(terms_ADJECTIVE[[1]]))
  }
  if(!is.null(terms_ADVERB) == TRUE)
  {
    print(paste0(word," is an ADVERB"))
    terms_ADVERB <- getIndexTerms("ADVERB", 10, filter)
    print(getSynonyms(terms_ADVERB[[1]]))
  }
  if(!is.null(terms_NOUN) == TRUE)
  {
    print(paste0(word," is a NOUN"))
    terms_NOUN <- getIndexTerms("NOUN", 10, filter)
    print(getSynonyms(terms_NOUN[[1]]))
  }
  if(!is.null(terms_VERB) == TRUE)
  {
    print(paste0(word," is a VERB"))
    terms_VERB <- getIndexTerms("VERB", 10, filter)
    print(getSynonyms(terms_VERB[[1]]))
  }
  
}

#gram_1_aggr_100

head(gram_2_aggr_time_freqs)
nrow(gram_2_aggr_time_freqs)

for (i in 1:nrow(gram_2_aggr_time_freqs)){
  outcome <- checkWordnet(gram_2_aggr_time_freqs$word2[i])
  #word is in wordnet
  if (outcome == TRUE) {
    checkWordnet_synonyms(gram_1_aggr$word[i])
  }
}


checkWordnet_synonyms("before")
checkWordnet_synonyms("try")
checkWordnet_synonyms("really")

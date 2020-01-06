library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(RWeka)
library(knitr)
library(quanteda)
library(tidytext)
library(data.table)
library(wordnet)
library(rJava)

Sys.setenv(JAVA_HOME="")
Sys.setenv(WNHOME="C:\\Program Files (x86)\\WordNet\\2.1\\dict")
setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")


gram_1_blogs <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_1.txt", stringsAsFactors = FALSE)
gram_2_blogs <- read.csv("./../skims/gram_2_blogs/gram_2_blogs_01_1.txt", stringsAsFactors = FALSE)
gram_3_blogs <- read.csv("./../skims/gram_3_blogs/gram_3_blogs_01_1.txt", stringsAsFactors = FALSE)
gram_4_blogs <- read.csv("./../skims/gram_4_blogs/gram_4_blogs_01_1.txt", stringsAsFactors = FALSE)
gram_5_blogs <- read.csv("./../skims/gram_5_blogs/gram_5_blogs_01_1.txt", stringsAsFactors = FALSE)


str(gram_1_blogs)
str(gram_2_blogs)
str(gram_3_blogs)
str(gram_4_blogs)
str(gram_5_blogs)

gram_2_blogs <- filter(gram_2_blogs, word != "")
gram_3_blogs <- filter(gram_3_blogs, word != "")
gram_4_blogs <- filter(gram_4_blogs, word != "")
gram_5_blogs <- filter(gram_5_blogs, word != "")

#'data.frame':	21127 obs. of  3 variables:
#'data.frame':	152157 obs. of  3 variables:
#'data.frame':	159496 obs. of  3 variables:
#'data.frame':	151501 obs. of  3 variables:

head(gram_1_blogs$word,100)

gram_1_blogs_100 <- gram_1_blogs[1:100,]

head(gram_1_blogs_100$word,100)

#### simple model minus wordnet

gram_2_blogs_split <- gram_2_blogs %>% separate(word, c("word1", "word2"), sep = " ")
gram_3_blogs_split <- gram_3_blogs %>% separate(word, c("word1", "word2", "word3"), sep = " ")
gram_4_blogs_split <- gram_4_blogs %>% separate(word, 
                                        c("word1", "word2", "word3", "word4"), sep = " ")
gram_5_blogs_split <- gram_5_blogs %>% separate(word, 
                                        c("word1", "word2", "word3", "word4", "word5"), sep = " ")

head(gram_2_blogs_split)
head(gram_3_blogs_split)
head(gram_4_blogs_split)
head(gram_5_blogs_split)

gram_2_blogs_time <- filter(gram_2_blogs_split, word1 == "time")
head(gram_2_blogs_time)
tail(gram_2_blogs_time)
str(gram_2_blogs_time)
table(gram_2_blogs_time$word1)

#nrow(gram_2_blogs_time)
head(gram_2_blogs_time$word1,100)
head(gram_3_blogs_split$word1,100)

str(gram_3_blogs_split)

gram_3_blogs_split %>% filter(word1 == "time")

#gram_3_blogs_split %>% filter(word1 == "time" && word2 == "year")

gram_2_blogs_time[1,]
gram_3_blogs_split[1,]

gram_3_blogs_split %>% filter(word1 == "amazon") %>%  filter(word2 == "servic")
gram_3_blogs_split %>% filter(word1 == "time") %>%  filter(word2 == "year")

gram_2_blogs_time <- filter(gram_2_blogs_split, word1 == "time")

gram_2_blogs_time_freqs <- gram_2_blogs_time %>% mutate(term_freq2 = freq/sum(freq))
gram_2_blogs_time_freqs <- gram_2_blogs_time_freqs %>% 
  mutate(fake_freq2 = term_freq2 + 0.1*dnorm(rnorm(freq))/sum(dnorm(rnorm(freq)))) %>%
  mutate(freq2 = freq) %>% select(-freq)
head(gram_2_blogs_time_freqs)

length(gram_2_blogs_time_freqs)
gram_2_blogs_time_freqs[1,1:6]

write.csv(gram_2_blogs_time_freqs,"./../model/test_04/gram_2_blogs_time_freqs.csv")
write.csv(gram_3_blogs_split,"./../model/test_04/gram_3_blogs_split.csv")

filter(gram_3_blogs_split, word1 == "true")

################################################
################################################
################################################
################################################
################################################
################################################
################################################

# nrow(gram_2_blogs_time
gram_3_blogs_time <- 0
gram_2_blogs_time_backout <- 0
for (i in 1:nrow(gram_2_blogs_time)) {

  print("number")
  print(i)
  print("word2")
  print(gram_2_blogs_time$word2[i])
  
  gram_3_blogs_time <- 0
  
  for (j in 1:nrow(gram_3_blogs_split)) {
    

        if(  (gram_2_blogs_time_freqs$word1[i] == gram_3_blogs_split$word1[j] &&
              gram_2_blogs_time_freqs$word2[i] == gram_3_blogs_split$word2[j]) ){
                  
                  if (!exists("gram_3_blogs_time")){
                    gram_3_blogs_time <- 0
                  }
                  
                  # if the merged dataset does exist, append to it
                  if (exists("gram_3_blogs_time")){
                    
                    gram_2_blogs_time_backout <- 0
                    for (k in 1:nrow(gram_2_blogs_split)) {
                      #back track to 2 gram for word2 and word3 and get it from gram_2_blogs_split
                      if(  (gram_2_blogs_split$word1[k] == gram_3_blogs_split$word2[j] &&
                            gram_2_blogs_split$word2[k] == gram_3_blogs_split$word3[j])  ){
                        
                        gram_2_blogs_time_backout <- gram_2_blogs_split[k,]
                        print("backout")
                        print(gram_2_blogs_time_backout)
                        print(gram_2_blogs_time_backout$freq)
                        gram_2_blogs_time_backout <- gram_2_blogs_time_backout %>% 
                          rename(
                            freq23 = freq
                          )
                        freq23 <- gram_2_blogs_time_backout$freq23
                      

                        
                      }#back track
                    }#k for loop
                    
                    
                    temp_dataset <- cbind(gram_2_blogs_time_freqs[i,4:6], freq23, gram_3_blogs_split[j,])
                    gram_3_blogs_time <- rbind(gram_3_blogs_time, temp_dataset)
                    rm(temp_dataset)
                    
                    
                  }#gram_3_blogs_time exists
                
        }#top if loop
  }#j for loop
  
  #head(gram_3_blogs_time$word1,100)
  gram_3_blogs_time <- filter(gram_3_blogs_time, freq2 != 0)
  gram_3_blogs_time <- gram_3_blogs_time %>% arrange(desc(freq))
  gram_3_blogs_time <- gram_3_blogs_time %>% mutate(freq3 = freq) %>% select(-freq) %>% select(-X)

  
  ## put in frequency as a fraction with a minor disturbance term
  
  gram_3_blogs_time_freqs <- gram_3_blogs_time %>% mutate(term_freq3 = freq3/sum(freq3))
  gram_3_blogs_time_freqs <- gram_3_blogs_time_freqs %>% 
    mutate(fake_freq3 = term_freq3 + 0.1*dnorm(rnorm(freq3))/sum(dnorm(rnorm(freq3))))
  gram_3_blogs_time_freqs <- gram_3_blogs_time_freqs %>% 
    mutate(term_freq23 = freq23/sum(freq23)) %>% 
    mutate(fake_freq23 = term_freq23 + 0.1*dnorm(rnorm(freq23))/sum(dnorm(rnorm(freq23))))
  write.csv(gram_3_blogs_time_freqs,paste0("./../model/test_04/gram_3_blogs_time_",gram_2_blogs_time_freqs$word2[i],"_freqs.csv"))

}#i for loop

################################################
################################################
################################################
################################################
################################################
################################################
################################################

head(gram_3_blogs_time)
str(gram_3_blogs_time)
head(gram_3_blogs_time_freqs)
str(gram_3_blogs_time_freqs)
gram_3_blogs_time_freqs


time_day <- read.csv("./../model/test_04/gram_3_blogs_time_day_freqs.csv")
time_made <- read.csv("./../model/test_04/gram_3_blogs_time_made_freqs.csv")
time_year <- read.csv("./../model/test_04/gram_3_blogs_time_year_freqs.csv")

time_day
time_made
time_year


################################

#look up every word in wordnet for synonyms and come up with an additional list to add to gram_1_blogs

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

#gram_1_blogs_100



for (i in 1:nrow(gram_1_blogs_100)){
  outcome <- checkWordnet(gram_1_blogs$word[i])
  #word is in wordnet
  if (outcome == TRUE) {
    checkWordnet_synonyms(gram_1_blogs$word[i])
  }
}

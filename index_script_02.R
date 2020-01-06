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

en_gram_1 <- 
  fread("./../skims/merged_grams/dataset_gram_1_aggregated.txt")
en_gram_2 <- 
  fread("./../skims/merged_grams/dataset_gram_2_aggregated.txt")
en_gram_3 <- 
  fread("./../skims/merged_grams/dataset_gram_3_aggregated.txt")

str(en_gram_1)
str(en_gram_2)
str(en_gram_3)

head(en_gram_1,20)

en_gram_1_plot <- filter(en_gram_1, freq > 50000) 
par(mfrow=c(1,1))
en_gram_1_plot %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
kable(en_gram_1_plot) 

head(en_gram_2,20)

en_gram_2_plot <- filter(en_gram_2, freq > 2400) 
par(mfrow=c(1,1))
en_gram_2_plot %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
kable(en_gram_2_plot) 

head(en_gram_3,20)

en_gram_3_plot <- filter(en_gram_3, freq > 275) 
par(mfrow=c(1,1))
en_gram_3_plot %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
kable(en_gram_3_plot) 

write.csv(en_gram_1_plot, "./../skims/merged_grams/en_gram_1_plot.csv")
write.csv(en_gram_2_plot, "./../skims/merged_grams/en_gram_2_plot.csv")
write.csv(en_gram_3_plot, "./../skims/merged_grams/en_gram_3_plot.csv")

#### 50 and 90 percent question
str(en_gram_1)

sum(en_gram_1$freq)

#####useful examples

select_http_www <- en_gram_1 %>% filter(str_detect(word, "(^http|^www)"))
head(select_http_www,15)
select_http_www <- select_http_www[1:15,]

select_long_words <- en_gram_1 %>% filter(str_detect(word, "[a-z]{18,}"))
head(select_long_words,15)
select_long_words <- select_long_words[1:15,]

select_many_vowels <- en_gram_1 %>% filter(str_detect(word, "[aeiou]{5,}"))
head(select_many_vowels,15)
select_many_vowels <- select_many_vowels[1:15,]

select_many_consonants <- en_gram_1 %>% filter(str_detect(word, "[b-df-gj-np-tv-xz]{6,}"))
head(select_many_consonants,15)
select_many_consonants <- select_many_consonants[1:15,]

found_once <- en_gram_1 %>% 
  filter(str_detect(word, "^[a-z]{1,10}$")) %>% filter(freq == 1) %>% select(word)
found_once <- found_once[70:100,]

write.csv(select_http_www, "./../skims/merged_grams/select_http_www.csv")
write.csv(select_long_words, "./../skims/merged_grams/select_long_words.csv")
write.csv(select_many_vowels, "./../skims/merged_grams/select_many_vowels.csv")
write.csv(select_many_consonants, "./../skims/merged_grams/select_many_consonants.csv")
write.csv(found_once, "./../skims/merged_grams/found_once.csv")

select_http_www <- read.csv("./../skims/merged_grams/select_http_www.csv", stringsAsFactors = FALSE)
select_long_words <- read.csv("./../skims/merged_grams/select_long_words.csv", stringsAsFactors = FALSE)
select_many_vowels <- read.csv("./../skims/merged_grams/select_many_vowels.csv", stringsAsFactors = FALSE)
select_many_consonants <- read.csv("./../skims/merged_grams/select_many_consonants.csv", stringsAsFactors = FALSE)
found_once <- read.csv("./../skims/merged_grams/found_once.csv", stringsAsFactors = FALSE)

select_http_www
select_long_words
select_many_vowels
select_many_consonants
found_once

#first round of outs
select_round_1  <- en_gram_1 %>% filter(str_detect(word, 
                              "(^http|^www|^[a-z]{18,}$|[aeiouyh]{5,}|[b-df-gj-np-tv-xz]{6,})")) 
str(select_round_1)
#'data.frame':	36623 obs. of  3 variables:

en_gram_1_round_2 <- en_gram_1 %>% filter(!word %in% (select_round_1$word))
str(en_gram_1_round_2)
#Classes ‘data.table’ and 'data.frame':	477305 obs. of  3 variables:
str(merged_gram_1_round_2)

en_gram_1_round_2_2plus <- merged_gram_1_round_2 %>% filter(freq > 1)
str(en_gram_1_round_2_2plus)

write.csv(en_gram_1_round_2_2plus, 
         "./../skims/merged_grams/en_gram_1_round_2_2plus.csv")

en_gram_1_round_2_2plus <- 
  read.csv("./../skims/merged_grams/en_gram_1_round_2_2plus.csv", stringsAsFactors = FALSE)

str(en_gram_1_round_2_2plus)

table(merged_gram_1_round_2$freq)
#1       2      3      4       5      6      7      8      9      10      
#285317  52248  21897  12543   8392   5930   4600   3681   2915   2369 

str(en_gram_1_round_2_2plus)
19624213*0.5
#[1] 9812107
19624213*0.9
#[1] 17661792
sum(en_gram_1_round_2_2plus[1:600,]$freq)
#[1] 9776698
sum(en_gram_1_round_2_2plus[1:7500,]$freq)
#[1] 17679382

write.csv(en_gram_1_round_2_2plus[1:600,],"./../skims/merged_grams/en_gram_1_round_2_2plus_0600.csv")
write.csv(en_gram_1_round_2_2plus[1:7500,],"./../skims/merged_grams/en_gram_1_round_2_2plus_7500.csv")


merged_gram_1_highfreq <- merged_gram_1_round_2 %>% filter(freq > 20)
str(merged_gram_1_highfreq)
plot(merged_gram_1_highfreq$freq)

merged_gram_1_vhighfreq <- merged_gram_1_round_2 %>% filter(freq > 1000)
str(merged_gram_1_vhighfreq)
#2921
plot(merged_gram_1_vhighfreq$freq)



##### wordnet

### simple function first 

checkWordnet <- function(checkword,found) {
  word <- checkword
  filter <- getTermFilter("ExactMatchFilter", word, TRUE)
  terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
  terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
  terms_NOUN <- getIndexTerms("NOUN", 5, filter)
  terms_VERB <- getIndexTerms("VERB", 5, filter)
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


checkWordnet("orden")
checkWordnet("socorro")
checkWordnet("serpiente")

checkWordnet("happy")
checkWordnet("make")
checkWordnet("may")




wordnet_gram_2_check <- read.csv("./../model/test_03/gram_2_twitter_nostem_01_1.txt", stringsAsFactors = FALSE)
head(wordnet_gram_2_check)
str(wordnet_gram_2_check)

##### for now just 1-gram

wordnet_gram_1_check <- read.csv("./../model/test_03/gram_1_twitter_nostem_01_1.txt", stringsAsFactors = FALSE)
str(wordnet_gram_1_check)

wordnet_gram_1_check_example <- wordnet_gram_1_check %>% arrange(desc(freq)) %>% filter(freq > 150)
str(wordnet_gram_1_check_example)

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
    found <- FALSE
  } 
  if(!is.null(terms_ADJECTIVE) == TRUE ||
     !is.null(terms_ADVERB) == TRUE ||
     !is.null(terms_NOUN) == TRUE ||
     !is.null(terms_VERB) == TRUE){
    found <- TRUE    
  } 
  return(found)
}

count <- 0
for (i in 1:nrow(wordnet_gram_1_check_example)){
  outcome <- checkWordnet(wordnet_gram_1_check_example$word[i])
  if (outcome == TRUE) {
    count <- count + 1
  }
  if(outcome ==FALSE){
    print("i")
    print(i)
    print("Not found in wordnet")
    print(wordnet_gram_1_check_example$word[i])
  }
  if(i == nrow(wordnet_gram_1_check_example)){
    print("count")
    print(count)
  }
}



#[1] "count"
#[1] 252
#> str(wordnet_gram_1_check_example)
#'data.frame':	298 obs. of  3 variables:
252/298

### synonym hunt

wordnet_gram_1_check_example$word

happy
follow
good
back
wait

amazing
watch
nice
make
start


#make

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

checkWordnet_synonyms("happy")
checkWordnet_synonyms("follow")
checkWordnet_synonyms("good")
checkWordnet_synonyms("back")
checkWordnet_synonyms("wait")
checkWordnet_synonyms("amazing")
checkWordnet_synonyms("watch")
checkWordnet_synonyms("nice")
checkWordnet_synonyms("start")



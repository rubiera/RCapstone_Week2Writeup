library(wordnet)
setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")

#https://www.sciencedirect.com/science/article/pii/S0957417414006472

library(tidyverse)
library(tidytext)

#http://wn-similarity.sourceforge.net/
#http://maraca.d.umn.edu/cgi-bin/similarity/similarity.cgi
#http://maraca.d.umn.edu/allwords/allwords.html


#   "ADJECTIVE" "ADVERB" "NOUN"  "VERB".
#  excluded words include determiners, prepositions, pronouns, conjunctions, and particles.

getFilterTypes()
#[1] "ContainsFilter"   "EndsWithFilter"   "ExactMatchFilter" "RegexFilter"      "SoundFilter"      "StartsWithFilter"
#[7] "WildcardFilter"

filter <- getTermFilter("ContainsFilter", "car", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
#[1] "a la carte"       "abies lasiocarpa" "acaracide"        "acardia"          "acariasis"  

filter <- getTermFilter("EndsWithFilter", "car", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
#[1] "armored car"  "armoured car" "baggage car"  "boxcar"       "buffet car"  

filter <- getTermFilter("ExactMatchFilter", "car", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
#[1] "car"

filter <- getTermFilter("RegexFilter", "^car[a-z]", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
#[1] "card" "care" "carp" "cart"

filter <- getTermFilter("SoundFilter", "car", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
#[1] "cairo"   "car"     "caraway" "care"    "carew"  

filter <- getTermFilter("StartsWithFilter", "car", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
#[1] "car"          "car-ferry"    "car-mechanic" "car battery"  "car bomb" 

filter <- getTermFilter("WildcardFilter", "*car*", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
#[1] "a la carte"       "abies lasiocarpa" "acaracide"        "acardia"          "acariasis"    

getSynonyms(terms[[1]])
#[1] "a la carte"
getSynonyms(terms[[2]])
#[1] "Abies lasiocarpa" "Alpine fir"       "subalpine fir"  
getSynonyms(terms[[3]])
#[1] "acaracide" "acaricide"
getSynonyms(terms[[4]])
#[1] "acardia"
getSynonyms(terms[[5]])
#[1] "acariasis"   "acaridiasis" "acariosis" 

#same result
getSynsets(terms[[1]])
sapply(terms, getSynsets)[[1]]

getSynsets(terms[[2]])
sapply(terms, getSynsets)[[2]]

getLemma(terms[[2]])
sapply(terms, getLemma)[[2]]

#did not find anything
if(!is.null(terms)) print("something here")
if(is.null(terms)) print("nothing here")
is.null(terms)
#[1] FALSE

#let's look up a few words we know are not english words
filter <- getTermFilter("ExactMatchFilter", "carro", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
is.null(terms)
#[1] TRUE
filter <- getTermFilter("ExactMatchFilter", "cigarro", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
is.null(terms)
#[1] TRUE

#I wish there was a way to look for the word minus POS
#   "ADJECTIVE" "ADVERB" "NOUN"  "VERB"
#only takes one POS, more than one POS does not work
terms <- getIndexTerms(c("ADJECTIVE","ADVERB","NOUN","VERB"), 5, filter)

#seems like a lot of work
#I am looking up a word FOUR times
filter <- getTermFilter("ExactMatchFilter", "grind", TRUE)
terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
sapply(terms_ADJECTIVE, getLemma)
is.null(terms_ADJECTIVE)
#[1] TRUE
terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
sapply(terms_ADVERB, getLemma)
is.null(terms_ADVERB)
#[1] TRUE
terms_NOUN <- getIndexTerms("NOUN", 5, filter)
sapply(terms_NOUN, getLemma)
is.null(terms_NOUN)
#[1] FALSE
terms_VERB <- getIndexTerms("VERB", 5, filter)
sapply(terms_VERB, getLemma)
is.null(terms_VERB)
#[1] FALSE

# https://www.k12reader.com/101-nouns-that-are-also-verbs/

filter <- getTermFilter("ExactMatchFilter", "order", TRUE)
terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
sapply(terms_ADJECTIVE, getLemma)
is.null(terms_ADJECTIVE)
#[1] TRUE
terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
sapply(terms_ADVERB, getLemma)
is.null(terms_ADVERB)
#[1] TRUE
terms_NOUN <- getIndexTerms("NOUN", 5, filter)
sapply(terms_NOUN, getLemma)
is.null(terms_NOUN)
#[1] FALSE
terms_VERB <- getIndexTerms("VERB", 5, filter)
sapply(terms_VERB, getLemma)
is.null(terms_VERB)
#[1] FALSE

#foreign word
word <- "orden"
filter <- getTermFilter("ExactMatchFilter", "orden", TRUE)
terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
sapply(terms_ADJECTIVE, getLemma)
is.null(terms_ADJECTIVE)
#[1] TRUE
terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
sapply(terms_ADVERB, getLemma)
is.null(terms_ADVERB)
#[1] TRUE
terms_NOUN <- getIndexTerms("NOUN", 5, filter)
sapply(terms_NOUN, getLemma)
is.null(terms_NOUN)
#[1] TRUE
terms_VERB <- getIndexTerms("VERB", 5, filter)
sapply(terms_VERB, getLemma)
is.null(terms_VERB)
#[1] TRUE
if(is.null(terms_ADJECTIVE) == TRUE &&
   is.null(terms_ADVERB) == TRUE &&
   is.null(terms_NOUN) == TRUE &&
   is.null(terms_VERB) == TRUE){
  print(paste0(checkword," is not in wordnet"))
}

checkWordnet <- function(checkword) {
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
  } 
  if(!is.null(terms_ADJECTIVE) == TRUE ||
     !is.null(terms_ADVERB) == TRUE ||
     !is.null(terms_NOUN) == TRUE ||
     !is.null(terms_VERB) == TRUE){
    print(paste0(checkword," is in wordnet"))
    
  } 
  
}

checkWordnet("orden")
checkWordnet("socorro")
checkWordnet("mayo")


checkWordnet("lapel")
checkWordnet("check")
checkWordnet("may")

#may
word <- "may"
filter <- getTermFilter("ExactMatchFilter", word, TRUE)
terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
terms_ADJECTIVE
sapply(terms_ADJECTIVE, getLemma)
is.null(terms_ADJECTIVE)
terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
terms_ADVERB
sapply(terms_ADVERB, getLemma)
is.null(terms_ADVERB)
terms_NOUN <- getIndexTerms("NOUN", 5, filter)
terms_NOUN
sapply(terms_NOUN, getLemma)
is.null(terms_NOUN)
terms_VERB <- getIndexTerms("VERB", 5, filter)
terms_VERB
sapply(terms_VERB, getLemma)
is.null(terms_VERB)

#May
word <- "May"
filter <- getTermFilter("ExactMatchFilter", word, TRUE)
terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
terms_ADJECTIVE
sapply(terms_ADJECTIVE, getLemma)
is.null(terms_ADJECTIVE)
terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
terms_ADVERB
sapply(terms_ADVERB, getLemma)
is.null(terms_ADVERB)
terms_NOUN <- getIndexTerms("NOUN", 5, filter)
terms_NOUN
sapply(terms_NOUN, getLemma)
is.null(terms_NOUN)
terms_VERB <- getIndexTerms("VERB", 5, filter)
terms_VERB
sapply(terms_VERB, getLemma)
is.null(terms_VERB)

#make
word <- "make"
filter <- getTermFilter("ExactMatchFilter", word, TRUE)
terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
terms_ADJECTIVE
sapply(terms_ADJECTIVE, getLemma)
is.null(terms_ADJECTIVE)
terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
terms_ADVERB
sapply(terms_ADVERB, getLemma)
is.null(terms_ADVERB)
terms_NOUN <- getIndexTerms("NOUN", 5, filter)
terms_NOUN
sapply(terms_NOUN, getLemma)
is.null(terms_NOUN)
terms_VERB <- getIndexTerms("VERB", 5, filter)
terms_VERB
sapply(terms_VERB, getLemma)
is.null(terms_VERB)

getSynonyms(terms_ADJECTIVE[[1]])
getSynonyms(terms_ADVERB[[1]])
getSynonyms(terms_NOUN[[1]])
length(getSynonyms(terms_NOUN[[1]]))
getSynonyms(terms_NOUN[[1]])[1]
getSynonyms(terms_VERB[[1]])
length(getSynonyms(terms_VERB[[1]]))
getSynonyms(terms_VERB[[1]])[1]

getSynsets(terms_ADJECTIVE[[1]])
getSynsets(terms_ADVERB[[1]])
getSynsets(terms_NOUN[[1]])
getSynsets(terms_VERB[[1]])

#####################

# this is my dictionary
dataset_gram_2_twitter <- read.csv("./../model/test_01/dataset_gram_2_twitter.txt", stringsAsFactors = FALSE)
dataset_gram_3_twitter <- read.csv("./../model/test_01/dataset_gram_3_twitter.txt", stringsAsFactors = FALSE)

head(dataset_gram_2_twitter)
head(dataset_gram_3_twitter)

gram_2_twitter_split <- dataset_gram_2_twitter %>%
  separate(word, c("word1", "word2"), sep = " ")

head(gram_2_twitter_split)

gram_3_twitter_split <- dataset_gram_3_twitter %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

head(gram_3_twitter_split)

#######################

# just 1 gram for now
dataset_gram_1_twitter <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_1.txt", stringsAsFactors = FALSE)
head(dataset_gram_1_twitter)

checkWordnet <- function(checkword) {
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
  } 
  if(!is.null(terms_ADJECTIVE) == TRUE ||
     !is.null(terms_ADVERB) == TRUE ||
     !is.null(terms_NOUN) == TRUE ||
     !is.null(terms_VERB) == TRUE){
    print(paste0(checkword," is in wordnet"))
    
  } 
  
}

for (i in 10:20){
  print(dataset_gram_1_twitter$word[i])
  checkWordnet(dataset_gram_1_twitter$word[i])
  
}

checkWordnet <- function(checkword) {
  word <- checkword
  filter <- getTermFilter("SoundFilter", word, TRUE)
  terms_ADJECTIVE <- getIndexTerms("ADJECTIVE", 5, filter)
  terms_ADVERB <- getIndexTerms("ADVERB", 5, filter)
  terms_NOUN <- getIndexTerms("NOUN", 5, filter)
  terms_VERB <- getIndexTerms("VERB", 5, filter)
  if(is.null(terms_ADJECTIVE) == TRUE &&
     is.null(terms_ADVERB) == TRUE &&
     is.null(terms_NOUN) == TRUE &&
     is.null(terms_VERB) == TRUE){
    print(paste0(checkword," is not in wordnet")) 
  } 
  if(!is.null(terms_ADJECTIVE) == TRUE ||
     !is.null(terms_ADVERB) == TRUE ||
     !is.null(terms_NOUN) == TRUE ||
     !is.null(terms_VERB) == TRUE){
    print(paste0(checkword," is in wordnet"))
    
  } 
  
}

for (i in 10:20){
  print(dataset_gram_1_twitter$word[i])
  checkWordnet(dataset_gram_1_twitter$word[i])
  
}


checkWordnet("orden")
checkWordnet("socorro")
checkWordnet("mayo")

####

#sounds like is wide open
#access wordnet before stemming


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

checkWordnet("happy")
found

checkWordnet("happy")

str(checkWordnet)

checkWordnet("lol",found)
#dont is fair, it's a pronoun
checkWordnet("don't")
#childes should work out of the wordnet FAQ
checkWordnet("childes")
#[1] "childes is not in wordnet"

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


for (i in 10:20){
  print(dataset_gram_1_twitter$word[i])
  outcome <- checkWordnet(dataset_gram_1_twitter$word[i])
  print(outcome)
  
}

# very slow but it works
count <- 0
for (i in 1:nrow(dataset_gram_1_twitter)){
  print(dataset_gram_1_twitter$word[i])
  outcome <- checkWordnet(dataset_gram_1_twitter$word[i])
  if (outcome == TRUE) {
    count <- count + 1
  }
  print("count")
  print(count)
}





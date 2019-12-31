library(wordnet)
setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")

#https://www.sciencedirect.com/science/article/pii/S0957417414006472


#   "ADJECTIVE" "ADVERB" "NOUN"  "VERB".

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

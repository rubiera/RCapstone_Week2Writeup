library(wordnet)
library(RWeka)
library(rJava)


Sys.getenv("JAVA_HOME")
#[1] "C:\\Program Files (x86)\\Java\\jre1.8.0_231"
Sys.setenv(JAVA_HOME="")

Sys.setenv(JAVA_HOME="C:\\Program Files (x86)\\Java\\jre1.8.0_231")

setDict("/Users/kasper2304/Desktop/WordNet-3.0/dict")

#C:\Users\rubie002\Documents\R\win-library\3.6

WNHOME
Sys.getenv("WNHOME")
Sys.setenv(WNHOME = "C:\\Program Files (x86)\\WordNet\\2.1")

#C:\Users\rubie002\Documents\R\win-library\3.6\wordnet

#C:\\sers\\rubie002\\Documents\\R\\win-library\\3.6\\wordnet\\
#seems to have worked
Sys.setenv(WNHOME="C:\\sers\\rubie002\\Documents\\R\\win-library\\3.6\\wordnet\\")

getFilterTypes()
#[1] "ContainsFilter"   "EndsWithFilter"   "ExactMatchFilter" "RegexFilter"      "SoundFilter"      "StartsWithFilter"
#[7] "WildcardFilter" 



Sys.setenv(WNHOME = "C:\\Program Files (x86)\\WordNet\\2.1\\dict")

#try these
Sys.setenv(WNHOME="C:\\sers\\rubie002\\Documents\\R\\win-library\\3.6\\wordnet\\")
install.packages("wordnet")
library(wordnet)
path <- file.path("usr", "share", "dict")
setDict(path)

synonyms("man", "NOUN")

###### I am making no pregress yet

setDict("C:\\sers\\rubie002\\Documents\\R\\win-library\\3.6\\wordnet")

###### install wordnet as a windows app
###### the dictionary is NOT in R, it's in the wordnet install
#  https://wordnet.princeton.edu/download/current-version#win

Sys.setenv(WNHOME="C:\\Program Files (x86)\\WordNet\\2.1\\dict")

filter <- getTermFilter("StartsWithFilter", "car", TRUE)
#[1] "Java-Object{com.nexagis.jawbone.filter.StartsWithFilter@b1bc7ed}"
setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)

filter <- getTermFilter("ExactMatchFilter", "company", TRUE)
terms <- getIndexTerms("NOUN", 1, filter)
getSynonyms(terms[[1]])
synonyms("company", "NOUN")
#[1] "caller"         "companionship"  "company"        "fellowship"     "party"          "ship's company" "society"       
#[8] "troupe"

filter <- getTermFilter("ExactMatchFilter", "hot", TRUE)
terms <- getIndexTerms("ADJECTIVE", 1, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "!")
sapply(related, getWord)
#[1] "cold"


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

library(markovchain)

gram_1_blogs <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_1.txt", stringsAsFactors = FALSE)
gram_2_blogs <- read.csv("./../skims/gram_2_blogs/gram_2_blogs_01_1.txt", stringsAsFactors = FALSE)
gram_3_blogs <- read.csv("./../skims/gram_3_blogs/gram_3_blogs_01_1.txt", stringsAsFactors = FALSE)
gram_4_blogs <- read.csv("./../skims/gram_4_blogs/gram_4_blogs_01_1.txt", stringsAsFactors = FALSE)

str(gram_1_blogs)
str(gram_2_blogs)
str(gram_3_blogs)
str(gram_4_blogs)

#'data.frame':	21127 obs. of  3 variables:
#'data.frame':	152157 obs. of  3 variables:
#'data.frame':	159496 obs. of  3 variables:
#'data.frame':	151501 obs. of  3 variables:

gram_1_blogs_pscs <- gram_1_blogs %>% filter(str_detect(word, c("parent","send","children","school")))

gram_1_blogs %>% filter(word %in% c("parent","send","children","school")) 
gram_2_blogs %>% filter(word %in% c("parent parent","parent send","parent children","parent school",
                                    "send parent","send send","send children","send school",
                                    "children parent","children send","children children","children school",
                                    "school parent","school send","school children","school school")) 

byRow <- TRUE
states = c("parent","send","children","school")
transitionMatrix = matrix(data = c(0.10, 0.70, 0.10, 0.10,
                                   0.10, 0.10, 0.70, 0.10,
                                   0.10, 0.10, 0.10, 0.70,
                                   0.70, 0.10, 0.10, 0.10), byrow = byRow, nrow = 4,
                                  dimnames = list(states, states))
gram_2_blogs_matrix <- as(transitionMatrix, "markovchain")

print(gram_2_blogs_matrix)
show(gram_2_blogs_matrix)
                          


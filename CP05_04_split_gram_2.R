library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(tidytext)


###################
###### gram_2

gram_1_twitter_01_1 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_1.txt")
gram_1_twitter_01_2 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_2.txt")
gram_1_twitter_01_3 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_3.txt")
gram_1_twitter_01_4 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_4.txt")
gram_1_twitter_01_5 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_5.txt")

gram_1_twitter_01_1 <- gram_1_twitter_01_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_01_2 <- gram_1_twitter_01_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_01_3 <- gram_1_twitter_01_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_01_4 <- gram_1_twitter_01_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_01_5 <- gram_1_twitter_01_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_1_twitter_01 <- rbind(gram_1_twitter_01_1, 
                           gram_1_twitter_01_2,
                           gram_1_twitter_01_3,
                           gram_1_twitter_01_4,
                           gram_1_twitter_01_5)

gram_1_twitter_02_1 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_02_1.txt")
gram_1_twitter_02_2 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_02_2.txt")
gram_1_twitter_02_3 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_02_3.txt")
gram_1_twitter_02_4 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_02_4.txt")
gram_1_twitter_02_5 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_02_5.txt")

gram_1_twitter_02_1 <- gram_1_twitter_02_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_02_2 <- gram_1_twitter_02_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_02_3 <- gram_1_twitter_02_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_02_4 <- gram_1_twitter_02_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_02_5 <- gram_1_twitter_02_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_1_twitter_02 <- rbind(gram_1_twitter_02_1, 
                           gram_1_twitter_02_2,
                           gram_1_twitter_02_3,
                           gram_1_twitter_02_4,
                           gram_1_twitter_02_5)

gram_1_twitter_03_1 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_03_1.txt")
gram_1_twitter_03_2 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_03_2.txt")
gram_1_twitter_03_3 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_03_3.txt")
gram_1_twitter_03_4 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_03_4.txt")
gram_1_twitter_03_5 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_03_5.txt")

gram_1_twitter_03_1 <- gram_1_twitter_03_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_03_2 <- gram_1_twitter_03_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_03_3 <- gram_1_twitter_03_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_03_4 <- gram_1_twitter_03_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_1_twitter_03_5 <- gram_1_twitter_03_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_1_twitter_03 <- rbind(gram_1_twitter_03_1, 
                           gram_1_twitter_03_2,
                           gram_1_twitter_03_3,
                           gram_1_twitter_03_4,
                           gram_1_twitter_03_5)

gram_1_twitter_test <- rbind(gram_1_twitter_01, 
                           gram_1_twitter_02,
                           gram_1_twitter_03)


#########

gram_2_twitter_01_1 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_1.txt")
gram_2_twitter_01_2 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_2.txt")
gram_2_twitter_01_3 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_3.txt")
gram_2_twitter_01_4 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_4.txt")
gram_2_twitter_01_5 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_5.txt")

gram_2_twitter_01_1 <- gram_2_twitter_01_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_01_2 <- gram_2_twitter_01_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_01_3 <- gram_2_twitter_01_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_01_4 <- gram_2_twitter_01_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_01_5 <- gram_2_twitter_01_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_2_twitter_01 <- rbind(gram_2_twitter_01_1, 
                           gram_2_twitter_01_2,
                           gram_2_twitter_01_3,
                           gram_2_twitter_01_4,
                           gram_2_twitter_01_5)

gram_2_twitter_02_1 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_02_1.txt")
gram_2_twitter_02_2 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_02_2.txt")
gram_2_twitter_02_3 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_02_3.txt")
gram_2_twitter_02_4 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_02_4.txt")
gram_2_twitter_02_5 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_02_5.txt")

gram_2_twitter_02_1 <- gram_2_twitter_02_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_02_2 <- gram_2_twitter_02_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_02_3 <- gram_2_twitter_02_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_02_4 <- gram_2_twitter_02_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_02_5 <- gram_2_twitter_02_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_2_twitter_02 <- rbind(gram_2_twitter_02_1, 
                           gram_2_twitter_02_2,
                           gram_2_twitter_02_3,
                           gram_2_twitter_02_4,
                           gram_2_twitter_02_5)

gram_2_twitter_03_1 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_03_1.txt")
gram_2_twitter_03_2 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_03_2.txt")
gram_2_twitter_03_3 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_03_3.txt")
gram_2_twitter_03_4 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_03_4.txt")
gram_2_twitter_03_5 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_03_5.txt")

gram_2_twitter_03_1 <- gram_2_twitter_03_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_03_2 <- gram_2_twitter_03_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_03_3 <- gram_2_twitter_03_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_03_4 <- gram_2_twitter_03_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_2_twitter_03_5 <- gram_2_twitter_03_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_2_twitter_03 <- rbind(gram_2_twitter_03_1, 
                           gram_2_twitter_03_2,
                           gram_2_twitter_03_3,
                           gram_2_twitter_03_4,
                           gram_2_twitter_03_5)

gram_2_twitter_test <- rbind(gram_2_twitter_01, 
                             gram_2_twitter_02,
                             gram_2_twitter_03)


#####################

gram_3_twitter_01_1 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_01_1.txt")
gram_3_twitter_01_2 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_01_2.txt")
gram_3_twitter_01_3 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_01_3.txt")
gram_3_twitter_01_4 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_01_4.txt")
gram_3_twitter_01_5 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_01_5.txt")

gram_3_twitter_01_1 <- gram_3_twitter_01_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_01_2 <- gram_3_twitter_01_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_01_3 <- gram_3_twitter_01_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_01_4 <- gram_3_twitter_01_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_01_5 <- gram_3_twitter_01_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_3_twitter_01 <- rbind(gram_3_twitter_01_1, 
                           gram_3_twitter_01_2,
                           gram_3_twitter_01_3,
                           gram_3_twitter_01_4,
                           gram_3_twitter_01_5)

gram_3_twitter_02_1 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_02_1.txt")
gram_3_twitter_02_2 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_02_2.txt")
gram_3_twitter_02_3 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_02_3.txt")
gram_3_twitter_02_4 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_02_4.txt")
gram_3_twitter_02_5 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_02_5.txt")

gram_3_twitter_02_1 <- gram_3_twitter_02_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_02_2 <- gram_3_twitter_02_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_02_3 <- gram_3_twitter_02_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_02_4 <- gram_3_twitter_02_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_02_5 <- gram_3_twitter_02_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_3_twitter_02 <- rbind(gram_3_twitter_02_1, 
                           gram_3_twitter_02_2,
                           gram_3_twitter_02_3,
                           gram_3_twitter_02_4,
                           gram_3_twitter_02_5)

gram_3_twitter_03_1 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_03_1.txt")
gram_3_twitter_03_2 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_03_2.txt")
gram_3_twitter_03_3 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_03_3.txt")
gram_3_twitter_03_4 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_03_4.txt")
gram_3_twitter_03_5 <- read.csv("./../skims/gram_3_twitter/gram_3_twitter_03_5.txt")

gram_3_twitter_03_1 <- gram_3_twitter_03_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_03_2 <- gram_3_twitter_03_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_03_3 <- gram_3_twitter_03_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_03_4 <- gram_3_twitter_03_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))
gram_3_twitter_03_5 <- gram_3_twitter_03_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                      term_freq = freq/sum(freq), 
                                                      logtf = log10(term_freq), logrank = log10(rank))

gram_3_twitter_03 <- rbind(gram_3_twitter_03_1, 
                           gram_3_twitter_03_2,
                           gram_3_twitter_03_3,
                           gram_3_twitter_03_4,
                           gram_3_twitter_03_5)

gram_3_twitter_test <- rbind(gram_3_twitter_01, 
                             gram_3_twitter_02,
                             gram_3_twitter_03)



#####################

#aggregate

gram_2_twitter_aggr <- aggregate(freq ~ word, 
                               data=gram_2_twitter_test,FUN=sum, 
                               na.rm = TRUE) 

gram_2_twitter_aggr <- filter(gram_2_twitter_aggr, freq > 100)
head(gram_2_twitter_aggr)
plot(sort(gram_2_twitter_aggr$freq,  decreasing = TRUE))

gram_2_split <- gram_2_twitter_aggr %>%
  separate(word, c("word1", "word2"), sep = " ")

head(gram_2_split)


#######################

gram_3_twitter_aggr <- aggregate(freq ~ word, 
                                 data=gram_3_twitter_test,FUN=sum, 
                                 na.rm = TRUE) 

head(sort(gram_3_twitter_aggr$freq,  decreasing = TRUE),100)

gram_3_twitter_aggr <- filter(gram_3_twitter_aggr, freq > 20)
head(gram_3_twitter_aggr)
plot(sort(gram_3_twitter_aggr$freq,  decreasing = TRUE))

gram_3_split <- gram_3_twitter_aggr %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

head(gram_3_split)

plot(sort(gram_3_split$freq,  decreasing = TRUE))

############

gram_2_split$word1
gram_2_split$word2
gram_3_split$word3




library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(tidytext)


gram_1_news <- read.csv("./../skims/gram_all_news/gram_1_news.txt")

str(gram_1_news)
head(gram_1_news)

gram_2_news <- read.csv("./../skims/gram_all_news/gram_2_news.txt")

str(gram_2_news)
head(gram_2_news)

gram_3_news <- read.csv("./../skims/gram_all_news/gram_3_news.txt")

str(gram_3_news)
head(gram_3_news)

#text mining in R basics
# Plot it
gram_1_news_filter <- filter(gram_1_news, freq >= 700)
gram_1_news_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

gram_2_news_filter <- filter(gram_2_news, freq >= 40)
gram_2_news_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

gram_3_news_filter <- filter(gram_3_news, freq >= 10)
gram_3_news_filter %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# post chapter 3 TF IDF

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

table(gram_1_twitter_01$doc)
#doc_01_1 doc_01_2 doc_01_3 doc_01_4 doc_01_5 
#17534    17771    17463    17633    17680 


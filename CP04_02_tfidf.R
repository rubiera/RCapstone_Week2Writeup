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

# chpter 3 TF IDF

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

gram_1_twitter_01 %>% 
  ggplot(aes(rank, term_freq, color = doc)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

gram_1_twitter_01_subset <- gram_1_twitter_01 %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(term_freq) ~ log10(rank), data = gram_1_twitter_01_subset)

gram_1_twitter_01 %>% 
  ggplot(aes(rank, term_freq, color = doc)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

bind_tf_idf(gram_1_twitter_01$word, gram_1_twitter_01$doc, gram_1_twitter_01$freq)
#here is my problem
#no applicable method for 'bind_tf_idf_' applied to an object of class "factor"

class(gram_1_twitter_01$word)
class(gram_1_twitter_01$doc)
class(gram_1_twitter_01$freq)

#> class(gram_1_twitter_01$word)
#[1] "factor"
#> class(gram_1_twitter_01$doc)
#[1] "character"
#> class(gram_1_twitter_01$freq)
#[1] "integer"

bind_tf_idf(as.character(gram_1_twitter_01$word), gram_1_twitter_01$doc, gram_1_twitter_01$freq)
#no applicable method for 'bind_tf_idf_' applied to an object of class "character"
bind_tf_idf(as.vector(gram_1_twitter_01$word), gram_1_twitter_01$doc, gram_1_twitter_01$freq)
#no applicable method for 'bind_tf_idf_' applied to an object of class "character"

#works!!!!
gram_1_twitter_01_tfidf <- bind_tf_idf(gram_1_twitter_01, word, doc, freq)
head(gram_1_twitter_01_tfidf)

gram_1_twitter_01_tfidf  %>% 
  arrange(desc(tf_idf)) %>% head(10) # tokens-documents-counts

gram_1_twitter_01_tfidf_sorted <- gram_1_twitter_01_tfidf  %>%
  arrange(desc(tf_idf)) 

head(gram_1_twitter_01_tfidf_sorted$word,50)

plot(gram_1_twitter_01_tfidf_sorted$tf_idf[1:100])

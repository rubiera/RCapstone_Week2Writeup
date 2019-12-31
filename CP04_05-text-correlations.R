library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(tidytext)


### twitter

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


############## blogs

gram_1_blogs_01_1 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_1.txt")
gram_1_blogs_01_2 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_2.txt")
gram_1_blogs_01_3 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_3.txt")
gram_1_blogs_01_4 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_4.txt")
gram_1_blogs_01_5 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_01_5.txt")

gram_1_blogs_01_1 <- gram_1_blogs_01_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_01_2 <- gram_1_blogs_01_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_01_3 <- gram_1_blogs_01_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_01_4 <- gram_1_blogs_01_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_01_5 <- gram_1_blogs_01_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))

gram_1_blogs_01 <- rbind(gram_1_blogs_01_1, 
                         gram_1_blogs_01_2,
                         gram_1_blogs_01_3,
                         gram_1_blogs_01_4,
                         gram_1_blogs_01_5)

gram_1_blogs_02_1 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_02_1.txt")
gram_1_blogs_02_2 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_02_2.txt")
gram_1_blogs_02_3 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_02_3.txt")
gram_1_blogs_02_4 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_02_4.txt")
gram_1_blogs_02_5 <- read.csv("./../skims/gram_1_blogs/gram_1_blogs_02_5.txt")

gram_1_blogs_02_1 <- gram_1_blogs_02_1 %>% mutate(doc = "doc_01_1", rank = row_number(), 
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_02_2 <- gram_1_blogs_02_2 %>% mutate(doc = "doc_01_2", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_02_3 <- gram_1_blogs_02_3 %>% mutate(doc = "doc_01_3", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_02_4 <- gram_1_blogs_02_4 %>% mutate(doc = "doc_01_4", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))
gram_1_blogs_02_5 <- gram_1_blogs_02_5 %>% mutate(doc = "doc_01_5", rank = row_number(),
                                                  term_freq = freq/sum(freq), 
                                                  logtf = log10(term_freq), logrank = log10(rank))

gram_1_blogs_02 <- rbind(gram_1_blogs_02_1, 
                         gram_1_blogs_02_2,
                         gram_1_blogs_02_3,
                         gram_1_blogs_02_4,
                         gram_1_blogs_02_5)



### news 

gram_1_news_01 <- read.csv("./../skims/gram_all_news/gram_1_news.txt")





### back to the source for correlations

skim_twitter_01 <- readLines("./../skims/skim_twitter_01.txt",skipNul = TRUE,warn=FALSE)
skim_twitter_02 <- readLines("./../skims/skim_twitter_02.txt",skipNul = TRUE,warn=FALSE)

skim_blogs_01 <- readLines("./../skims/skim_blogs_01.txt",skipNul = TRUE,warn=FALSE)
skim_blogs_02 <- readLines("./../skims/skim_blogs_02.txt",skipNul = TRUE,warn=FALSE)

skim_news_01 <- readLines("./../skims/skim_news_01.txt",skipNul = TRUE,warn=FALSE)
skim_news_02 <- readLines("./../skims/skim_news_02.txt",skipNul = TRUE,warn=FALSE)


# Convert to data frame with a line variable to track placement
skim_twitter_01_df <- tibble(text = skim_twitter_01[1:100000])
skim_twitter_02_df <- tibble(text = skim_twitter_02[1:100000])

skim_blogs_01_df <- tibble(text = skim_blogs_01[1:50000])
skim_blogs_02_df <- tibble(text = skim_blogs_02[1:50000])

skim_news_01_df <- tibble(text = skim_news_01[1:5000])
skim_news_02_df <- tibble(text = skim_news_02[1:5000])

##### tidy twitter

str(skim_twitter_01)
class(skim_twitter_01)

tidy_skim_twitter_01 <- skim_twitter_01_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_skim_twitter_02 <- skim_twitter_02_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

head(tidy_skim_twitter_01)

##### tidy blogs

str(skim_blogs_01)
class(skim_blogs_01)

tidy_skim_blogs_01 <- skim_blogs_01_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_skim_blogs_02 <- skim_blogs_02_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

head(tidy_skim_blogs_01)

##### tidy news

str(skim_news_01)
class(skim_news_01)

tidy_skim_news_01 <- skim_news_01_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_skim_news_02 <- skim_news_02_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

head(tidy_skim_news_01)

### adapted from text mining book

frequency <- bind_rows(mutate(tidy_skim_twitter_01, source_file = "skim_twitter_01"),
                       mutate(tidy_skim_twitter_02, source_file = "skim_twitter_02")) %>%
  # Avoid UTF-8 encoded text with underscores recording the underscores as words
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(source_file, word) %>%
  group_by(source_file) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>% 
  spread(source_file, proportion) %>% 
  gather(source_file, proportion, `skim_twitter_01`:`skim_twitter_02`)

head(frequency)

frequency <- na.omit(frequency)

### my way twittwer within

freq_01 <- mutate(tidy_skim_twitter_01, word = str_extract(word, "[a-z']+"))
freq_01 <- count(freq_01, word)
freq_01 <- mutate(freq_01, proportion_01 = n / sum(n))

freq_02 <- mutate(tidy_skim_twitter_02, word = str_extract(word, "[a-z']+")) 
freq_02 <- count(freq_02, word)
freq_02 <- mutate(freq_02, proportion_02 = n / sum(n))

head(freq_01)
head(freq_02)

nrow(freq_01)
nrow(freq_02)

#> nrow(freq_01)
#[1] 53541
#> nrow(freq_02)
#[1] 53653

# words found in both files from text mining in R

freq_join <- merge(freq_01, freq_02, by = "word")
head(freq_join)
freq_join <- na.omit(freq_join)

plot(freq_join$proportion_01,freq_join$proportion_02)

plot(freq_join$n.x,freq_join$n.y)

cor.test(freq_join$proportion_01,freq_join$proportion_02)

#let's do it for the word skims

head(gram_1_twitter_01)

#> nrow(freq_01)
#[1] 53541
#> nrow(freq_02)
#[1] 53653

# words found in both files twitter my way

freq_join_gram_1_twitter <- merge(gram_1_twitter_01, gram_1_twitter_02, by = "word")
head(freq_join_gram_1_twitter,100)
freq_join_gram_1_twitter <- filter(freq_join_gram_1_twitter,  freq.x != 0)

plot(freq_join_gram_1_twitter$freq.x,freq_join_gram_1_twitter$freq.y)

plot(freq_join_gram_1_twitter$X.x,freq_join_gram_1_twitter$X.y)

cor.test(freq_join_gram_1_twitter$freq.x,freq_join_gram_1_twitter$freq.y)

# words found in both files blogs my way

freq_join_gram_1_blogs <- merge(gram_1_blogs_01, gram_1_blogs_02, by = "word")
head(freq_join_gram_1_blogs,100)
freq_join_gram_1_blogs <- filter(freq_join_gram_1_blogs,  freq.x != 0)

plot(freq_join_gram_1_blogs$freq.x,freq_join_gram_1_blogs$freq.y)

plot(freq_join_gram_1_blogs$X.x,freq_join_gram_1_blogs$X.y)

cor.test(freq_join_gram_1_blogs$freq.x,freq_join_gram_1_blogs$freq.y)

###### cross correlations

#### twitter and blogs

freq_join_gram_1_twitter_blogs <- merge(gram_1_twitter_01, gram_1_blogs_01, by = "word")
head(freq_join_gram_1_twitter_blogs,10)
freq_join_gram_1_twitter_blogs <- filter(freq_join_gram_1_twitter_blogs,  freq.x != 0)

plot(freq_join_gram_1_twitter_blogs$freq.x,freq_join_gram_1_twitter_blogs$freq.y)

plot(freq_join_gram_1_twitter_blogs$X.x,freq_join_gram_1_twitter_blogs$X.y)

cor.test(freq_join_gram_1_twitter_blogs$freq.x,freq_join_gram_1_twitter_blogs$freq.y)

#### twitter and news

freq_join_gram_1_twitter_news <- merge(gram_1_twitter_01, gram_1_news_01, by = "word")
head(freq_join_gram_1_twitter_news,10)
freq_join_gram_1_twitter_news <- filter(freq_join_gram_1_twitter_news,  freq.x != 0)

plot(freq_join_gram_1_twitter_news$freq.x,freq_join_gram_1_twitter_news$freq.y)

plot(freq_join_gram_1_twitter_news$X.x,freq_join_gram_1_twitter_news$X.y)

cor.test(freq_join_gram_1_twitter_news$freq.x,freq_join_gram_1_twitter_news$freq.y)

#### blogs and news

freq_join_gram_1_blogs_news <- merge(gram_1_blogs_01, gram_1_news_01, by = "word")
head(freq_join_gram_1_blogs_news,10)
freq_join_gram_1_blogs_news <- filter(freq_join_gram_1_blogs_news,  freq.x != 0)

plot(freq_join_gram_1_blogs_news$freq.x,freq_join_gram_1_blogs_news$freq.y)

plot(freq_join_gram_1_blogs_news$X.x,freq_join_gram_1_blogs_news$X.y)

cor.test(freq_join_gram_1_blogs_news$freq.x,freq_join_gram_1_blogs_news$freq.y)













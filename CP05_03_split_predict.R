library(tidyr)
library(ggraph)
library(igraph)
library(markovchain)
library(tidyverse)
library(tidyr)


###################
###### gram_2

gram_2_twitter_example <- read.csv("./../barrel/gram_2/gram_2_twitter_happi_01_1.txt")

gram_2_split <- gram_2_twitter_example %>%
  separate(word, c("word1", "word2"), sep = " ")

gram_2_split_counts <- gram_2_split %>% 
  count(word1, word2, sort = TRUE)

gram_2_split_counts

###################
###### gram_3

gram_3_twitter_example <- read.csv("./../barrel/gram_3/gram_3_twitter_happi_01_1.txt")

gram_3_split <- gram_3_twitter_example %>%
  separate(word, c("word1", "word2","word3"), sep = " ")

gram_3_split_counts <- gram_3_split %>% 
  count(word1, word2, word3, sort = TRUE)

gram_3_split_counts

##### happi

set.seed(22)

gram_2_split_graph <- gram_2_split_counts %>%
  graph_from_data_frame()

gram_2_split_graph

ggraph(gram_2_split_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(gram_2_split_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#################

gram_2_twitter_common <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_1.txt")
head(gram_2_twitter_common)
gram_2_twitter_common <- filter(gram_2_twitter_common, freq > 15)

gram_2_split_common <- gram_2_twitter_common %>%
  separate(word, c("word1", "word2"), sep = " ")

gram_2_split_common <- select(gram_2_split_common, -X)

gram_2_split_common_graph <- gram_2_split_common  %>%
  graph_from_data_frame()

gram_2_split_common_graph

ggraph(gram_2_split_common_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(gram_2_split_common_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = freq), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

###################

# markovchain

### a working example for gram_1 

gram_1_twitter_01_1 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_1.txt")
gram_1_twitter_01_2 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_2.txt")
gram_1_twitter_01_3 <- read.csv("./../skims/gram_1_twitter/gram_1_twitter_01_3.txt")

gram_1_twitter_01_1 <- gram_1_twitter_01_1 %>% 
      mutate(doc = "doc_01_1", rank = row_number(), gram_1_freq = freq/sum(freq))
gram_1_twitter_01_2 <- gram_1_twitter_01_2 %>% 
  mutate(doc = "doc_02_1", rank = row_number(), gram_1_freq = freq/sum(freq))
gram_1_twitter_01_3 <- gram_1_twitter_01_3 %>% 
  mutate(doc = "doc_03_1", rank = row_number(), gram_1_freq = freq/sum(freq))

gram_1_twitter_01 <- rbind(gram_1_twitter_01_1, 
                           gram_1_twitter_01_2,
                           gram_1_twitter_01_3)

gram_1_twitter_01 <- na.omit(gram_1_twitter_01)

summary(gram_1_twitter_01)

gram_1_twitter_01_tfidf <- bind_tf_idf(gram_1_twitter_01, word, doc, freq)
head(gram_1_twitter_01_tfidf)

gram_1_twitter_01_tfidf <-na.omit(gram_1_twitter_01_tfidf)

summary(gram_1_twitter_01_tfidf$tf_idf)

plot(sort(gram_1_twitter_01_tfidf$tf_idf, decreasing = TRUE))

### gram_2

gram_2_twitter_01_1 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_1.txt")
gram_2_twitter_01_2 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_2.txt")
gram_2_twitter_01_3 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_3.txt")

gram_2_twitter_01_1 <- gram_2_twitter_01_1 %>% 
  mutate(doc = "doc_01_1", rank = row_number(), gram_2_freq = freq/sum(freq))
gram_2_twitter_01_2 <- gram_2_twitter_01_2 %>% 
  mutate(doc = "doc_02_1", rank = row_number(), gram_2_freq = freq/sum(freq))
gram_2_twitter_01_3 <- gram_2_twitter_01_3 %>% 
  mutate(doc = "doc_03_1", rank = row_number(), gram_2_freq = freq/sum(freq))

gram_2_twitter_01 <- rbind(gram_2_twitter_01_1, 
                           gram_2_twitter_01_2,
                           gram_2_twitter_01_3)

gram_2_twitter_01 <- na.omit(gram_2_twitter_01)

summary(gram_2_twitter_01)
head(gram_2_twitter_01)

gram_2_twitter_01_split <- gram_2_twitter_01 %>%
  separate(word, c("word1", "word2"), sep = " ")
head(gram_2_twitter_01_split)

### gram_2 for matrix

gram_2_twitter_01_1 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_1.txt")
gram_2_twitter_01_2 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_2.txt")
gram_2_twitter_01_3 <- read.csv("./../skims/gram_2_twitter/gram_2_twitter_01_3.txt")

gram_2_twitter_01_1 <- gram_2_twitter_01_1 %>% 
  mutate(gram_2_freq = freq/sum(freq))
gram_2_twitter_01_2 <- gram_2_twitter_01_2 %>% 
  mutate(gram_2_freq = freq/sum(freq))
gram_2_twitter_01_3 <- gram_2_twitter_01_3 %>% 
  mutate(gram_2_freq = freq/sum(freq))

gram_2_twitter_01 <- rbind(gram_2_twitter_01_1, 
                           gram_2_twitter_01_2,
                           gram_2_twitter_01_3)

gram_2_twitter_01 <- na.omit(gram_2_twitter_01)

summary(gram_2_twitter_01)
head(gram_2_twitter_01)

gram_2_twitter_01_split <- gram_2_twitter_01 %>%
  separate(word, c("word1", "word2"), sep = " ")

gram_2_twitter_01_split <- na.omit(gram_2_twitter_01_split) %>%
      select(-X) 

head(gram_2_twitter_01_split)
str(gram_2_twitter_01_split)

gram_2_twitter_01_split_small <- filter(gram_2_twitter_01_split, gram_2_freq > 0.00018)
str(gram_2_twitter_01_split_small)

gram_2_twitter_01_split_graph <- gram_2_twitter_01_split_small[1:2] %>%
               graph_from_data_frame()

ggraph(gram_2_twitter_01_split_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(gram_2_twitter_01_split_small, layout = "fr") +
  geom_edge_link(aes(edge_alpha = gram_2_freq), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# gram_2_twitter_01_split_small

head(gram_2_twitter_01_split_small)

### finally!  
# this is my markov matrix 

gram_2_twitter_01_small_matrix <- matrix(diag(gram_2_twitter_01_split_small$gram_2_freq),
       nrow = nrow(gram_2_twitter_01_split_small), ncol = nrow(gram_2_twitter_01_split_small),
       dimnames = list(gram_2_twitter_01_split_small$word1,gram_2_twitter_01_split_small$word2))

#           birthday         back         morn          day         luck        media       follow
#happi    0.0008425027 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0000000000
#follow   0.0000000000 0.0007714483 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0000000000
#good     0.0000000000 0.0000000000 0.0007714483 0.0000000000 0.0000000000 0.0000000000 0.0000000000
#mother   0.0000000000 0.0000000000 0.0000000000 0.0005075318 0.0000000000 0.0000000000 0.0000000000
#good     0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0004567786 0.0000000000 0.0000000000
#social   0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0003958748 0.0000000000
#pleas    0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0003958748

#word1    word2  gram_2_freq
#1  happi birthday 0.0008425027
#2 follow     back 0.0007714483
#3   good     morn 0.0007714483
#4 mother      day 0.0005075318
#5   good     luck 0.0004567786
#6 social    media 0.0003958748
#7  pleas   follow 0.0003958748




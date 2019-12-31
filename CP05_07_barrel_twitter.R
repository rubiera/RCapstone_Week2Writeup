library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(tidytext)


gram_2_barrel_twitter <- read.csv("./../barrel/dataset_gram_2_twitter.txt")
gram_3_barrel_twitter <- read.csv("./../barrel/dataset_gram_3_twitter.txt")


#aggregate

gram_2_bt_aggr <- aggregate(freq ~ word, 
                               data=gram_2_barrel_twitter,FUN=sum, 
                               na.rm = TRUE) 

gram_2_bt_aggr_sort <- gram_2_bt_aggr %>% arrange(desc(freq))

head(gram_2_bt_aggr_sort$freq)

gram_2_bt_aggr_sort_filter <- filter(gram_2_bt_aggr_sort, freq > 50)
head(gram_2_bt_aggr_sort_filter)
plot(gram_2_bt_aggr_sort_filter$freq)

gram_2_split <- gram_2_bt_aggr_sort %>%
  separate(word, c("word1", "word2"), sep = " ")

head(gram_2_split)
str(gram_2_split)


#######################

gram_3_bt_aggr <- aggregate(freq ~ word, 
                            data=gram_3_barrel_twitter,FUN=sum, 
                            na.rm = TRUE) 

gram_3_bt_aggr_sort <- gram_3_bt_aggr %>% arrange(desc(freq))

head(gram_3_bt_aggr_sort$freq)

gram_3_bt_aggr_sort_filter <- filter(gram_3_bt_aggr_sort, freq > 50)
head(gram_3_bt_aggr_sort_filter)
plot(gram_3_bt_aggr_sort_filter$freq)

gram_3_split <- gram_3_bt_aggr_sort %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

head(gram_3_split)
str(gram_3_split)

#######

head(gram_2_split)
head(gram_3_split)

####### 
# back to the source

mother_example <- readLines("./../skims/skim_twitter_01.txt",skipNul = TRUE,warn=FALSE)
mother_example <- as_tibble(mother_example)

str(mother_example)
colnames(mother_example)
head(mother_example)

mother_example <- mother_example  %>%
filter(str_detect(value, " happy mother'{0,}s{0,} day "))

str(mother_example)
colnames(mother_example)
head(mother_example)

write.csv(mother_example,"./../model/test_01/mother_example.csv")

random_example <- readLines("./../skims/skim_twitter_01.txt",skipNul = TRUE,warn=FALSE)
random_example <- as_tibble(random_example)

str(random_example)
colnames(random_example)
head(random_example)

random_example <- random_example[1:6,]

str(random_example)
colnames(random_example)
head(random_example)

write.csv(random_example,"./../model/test_01/random_example.csv")

foreign_example_de <- readLines("./../course-data/de_DE/de_DE.twitter.txt",skipNul = TRUE,warn=FALSE)
foreign_example_de <- as_tibble(foreign_example_de)
foreign_example_de <- foreign_example_de[1:6,]
foreign_example_de
write.csv(foreign_example_de,"./../model/test_01/foreign_example_de.csv")

foreign_example_ru <- readLines("./../course-data/ru_RU/ru_RU.twitter.txt",skipNul = TRUE,warn=FALSE)
foreign_example_ru <- as_tibble(foreign_example_ru)
foreign_example_ru <- foreign_example_ru[1:6,]
foreign_example_ru
write.csv(foreign_example_ru,"./../model/test_01/foreign_example_ru.csv")

foreign_example_pass_1 <- iconv(foreign_example, "latin1", "ASCII", sub="")
foreign_example_pass_2 <- iconv(foreign_example_pass_1, "english", sub="")




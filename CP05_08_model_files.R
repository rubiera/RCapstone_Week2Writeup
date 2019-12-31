library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(tidytext)


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

foreign_example_pass_ru <- iconv(foreign_example_ru, "latin1", "ASCII", sub="")
foreign_example_pass_de <- iconv(foreign_example_de, "latin1", "ASCII", sub="")





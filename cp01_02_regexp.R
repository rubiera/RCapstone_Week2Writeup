library(tidyverse)
library(stringr)

#for even faster work
file1_english_blogs_skim_20
file1_english_news_skim_20 
file1_english_twitter_skim_20 

#for (val in sequence)
#{
#  statement
#}

str_length(file1_english_blogs_skim_20[1])

## characters

length_blogs <- array(dim=20)  
class(length_blogs)

for (i in 1:length(file1_english_blogs_skim_20)) {
  print("item")
  print(i)
  print(str_length(file1_english_blogs_skim_20[i]))
  length_blogs[i] <- as.array(str_length(file1_english_blogs_skim_20[i]))
  print(length_blogs[i])
} 

# now the complete file
str(file1_english_blogs)
#chr [1:899288]

length_blogs_complete <- array(dim=899288) 
length_blogs_1000 <- array(dim=899288) 
length_blogs_big <- array(dim=899288)
for (i in 1:length(file1_english_blogs)) {
  print(i)
  length_blogs_complete[i] <- as.array(str_length(file1_english_blogs[i]))
    if(str_length(file1_english_blogs[i]) <= 1000){
      length_blogs_1000[i] <- as.array(str_length(file1_english_blogs[i]))
    } else {
      length_blogs_big[i] <- as.array(str_length(file1_english_blogs[i]))
    }
}

head(length_blogs_complete)
table(length_blogs_complete)
plot(sort(table(length_blogs_complete)))

head(length_blogs_1000)
table(length_blogs_1000)
plot(sort(table(length_blogs_1000)))

head(length_blogs_big)
table(length_blogs_big)
plot(sort(table(length_blogs_big)))


str(file1_english_news)
#chr [1:77259]

length_news_complete <- array(dim=77259) 
length_news_1000 <- array(dim=77259) 
length_news_big <- array(dim=77259)
for (i in 1:length(file1_english_news)) {
  print(i)
  length_news_complete[i] <- as.array(str_length(file1_english_news[i]))
  if(str_length(file1_english_news[i]) <= 1000){
    length_news_1000[i] <- as.array(str_length(file1_english_news[i]))
  } else {
    length_news_big[i] <- as.array(str_length(file1_english_news[i]))
  }
}

head(length_news_complete)
table(length_news_complete)
plot(sort(table(length_news_complete)))

head(length_news_1000)
table(length_news_1000)
plot(sort(table(length_news_1000)))

head(length_news_big)
table(length_news_big)
plot(sort(table(length_news_big)))

str(file1_english_twitter)
#chr [1:2360148]

length_twitter_complete <- array(dim=2360148) 
for (i in 1:length(file1_english_twitter)) {
  print(i)
  length_twitter_complete[i] <- as.array(str_length(file1_english_twitter[i]))
}

head(length_twitter_complete)
table(length_twitter_complete)
plot(sort(table(length_twitter_complete)))
  
#### words

#for even faster work
file1_english_blogs_skim_20
file1_english_news_skim_20 
file1_english_twitter_skim_20 

file1_english_blogs_skim_20[1]
str_count(file1_english_blogs_skim_20[1],boundary("word"))

file1_english_blogs_skim_20[2]

length_blogs_words <- array(dim=20) 
for (i in (1:length(file1_english_blogs_skim_20))) {
  print(i)
  #print(file1_english_blogs_skim_20[i])
  #print(str_count(file1_english_blogs_skim_20[i],boundary("word")))
  length_blogs_words[i] <- 
    as.array(str_count(file1_english_blogs_skim_20[i],boundary("word")))
  print(length_blogs_words[i])
}

#whole file

length_blogs_words <- array(dim=899288) 
length_blogs_words_250 <- array(dim=899288) 
length_blogs_words_big <- array(dim=899288)
for (i in 1:length(file1_english_blogs)) {
  print(i)
  length_blogs_words[i] <- 
    as.array(str_count(file1_english_blogs[i],boundary("word")))
  if(str_count(file1_english_blogs[i],boundary("word")) <= 250){
    length_blogs_words_250[i] <- 
      as.array(str_count(file1_english_blogs[i],boundary("word")))
  } else {
    length_blogs_words_big[i] <- 
      as.array(str_count(file1_english_blogs[i],boundary("word")))
  }
}

head(length_blogs_words)
table(length_blogs_words)
plot(sort(table(length_blogs_words)))

head(length_blogs_words_250)
table(length_blogs_words_250)
plot(sort(table(length_blogs_words_250)))

head(length_blogs_words_big)
table(length_blogs_words_big)
plot(sort(table(length_blogs_words_big)))

#worry about word counts later

  


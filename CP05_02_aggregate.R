library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(RWeka)
library(knitr)
library(quanteda)
library(tidytext)

###################################
###################################
###################################
#### aggregate all gram_1

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/gram_1_blogs"
file_list_gram_1_blogs <- list.files(skim_dir)
paste(skim_dir,file_list_gram_1_blogs[1],sep="/")

for (file in 1:length(file_list_gram_1_blogs)){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset_gram_1_blogs")){
    dataset_gram_1_blogs <- read.csv(paste(skim_dir,file_list_gram_1_blogs[file],sep="/"), header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset_gram_1_blogs")){
    temp_dataset <- read.csv(paste(skim_dir,file_list_gram_1_blogs[file],sep="/"), header=TRUE)
    dataset_gram_1_blogs<-rbind(dataset_gram_1_blogs, temp_dataset)
    rm(temp_dataset)
  }
  
}

head(dataset_gram_1_blogs)
str(dataset_gram_1_blogs)
class(dataset_gram_1_blogs)

### twitter

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/gram_1_twitter"
file_list_gram_1_twitter <- list.files(skim_dir)
paste(skim_dir,file_list_gram_1_twitter[1],sep="/")

for (file in 1:length(file_list_gram_1_twitter)){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset_gram_1_twitter")){
    dataset_gram_1_twitter <- read.csv(paste(skim_dir,file_list_gram_1_twitter[file],sep="/"), header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset_gram_1_twitter")){
    temp_dataset <- read.csv(paste(skim_dir,file_list_gram_1_twitter[file],sep="/"), header=TRUE)
    dataset_gram_1_twitter<-rbind(dataset_gram_1_twitter, temp_dataset)
    rm(temp_dataset)
  }
  
}

head(dataset_gram_1_twitter)
str(dataset_gram_1_twitter)
class(dataset_gram_1_twitter)

head(gram_1_news)

dataset_gram_1 <- rbind(dataset_gram_1_blogs,dataset_gram_1_twitter,gram_1_news)

head(dataset_gram_1)
str(dataset_gram_1)
class(dataset_gram_1)

dataset_gram_1_aggregated <- aggregate(freq ~ word, 
                                       data=dataset_gram_1,FUN=sum, 
                                       na.rm = TRUE) 

dataset_gram_1_aggregated$word <- as.character(dataset_gram_1_aggregated$word)
dataset_gram_1_aggregated <- filter(dataset_gram_1_aggregated, word != "")
head(dataset_gram_1_aggregated)
str(dataset_gram_1_aggregated)
class(dataset_gram_1_aggregated)

dataset_gram_1_aggregated <- dataset_gram_1_aggregated %>% arrange(desc(freq))

write.csv(dataset_gram_1_aggregated,"./dataset_gram_1_aggregated.txt")

###################################
###################################
###################################
#### aggregate all gram_2

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/gram_2_blogs"
file_list_gram_2_blogs <- list.files(skim_dir)
paste(skim_dir,file_list_gram_2_blogs[1],sep="/")

for (file in 1:length(file_list_gram_2_blogs)){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset_gram_2_blogs")){
    dataset_gram_2_blogs <- read.csv(paste(skim_dir,file_list_gram_2_blogs[file],sep="/"), header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset_gram_2_blogs")){
    temp_dataset <- read.csv(paste(skim_dir,file_list_gram_2_blogs[file],sep="/"), header=TRUE)
    dataset_gram_2_blogs<-rbind(dataset_gram_2_blogs, temp_dataset)
    rm(temp_dataset)
  }
  
}

head(dataset_gram_2_blogs)
str(dataset_gram_2_blogs)
class(dataset_gram_2_blogs)

write.csv(dataset_gram_2_blogs,"./dataset_gram_2_blogs.txt")

### twitter

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/gram_2_twitter"
file_list_gram_2_twitter <- list.files(skim_dir)
paste(skim_dir,file_list_gram_2_twitter[1],sep="/")

for (file in 1:length(file_list_gram_2_twitter)){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset_gram_2_twitter")){
    dataset_gram_2_twitter <- read.csv(paste(skim_dir,file_list_gram_2_twitter[file],sep="/"), header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset_gram_2_twitter")){
    temp_dataset <- read.csv(paste(skim_dir,file_list_gram_2_twitter[file],sep="/"), header=TRUE)
    dataset_gram_2_twitter<-rbind(dataset_gram_2_twitter, temp_dataset)
    rm(temp_dataset)
  }
  
}

head(dataset_gram_2_twitter)
str(dataset_gram_2_twitter)
class(dataset_gram_2_twitter)

write.csv(dataset_gram_2_twitter,"./dataset_gram_2_twitter.txt")

gram_2_news <- read.csv("./../skims/gram_all_news/gram_2_news.txt")
head(gram_2_news)

dataset_gram_2 <- rbind(dataset_gram_2_blogs,dataset_gram_2_twitter,gram_2_news)

head(dataset_gram_2)
str(dataset_gram_2)
class(dataset_gram_2)

dataset_gram_2_aggregated <- aggregate(freq ~ word, 
                                       data=dataset_gram_2,FUN=sum, 
                                       na.rm = TRUE) 

dataset_gram_2_aggregated$word <- as.character(dataset_gram_2_aggregated$word)
dataset_gram_2_aggregated <- filter(dataset_gram_2_aggregated, word != "")
head(dataset_gram_2_aggregated)
str(dataset_gram_2_aggregated)
class(dataset_gram_2_aggregated)

dataset_gram_2_aggregated <- dataset_gram_2_aggregated %>% arrange(desc(freq))

write.csv(dataset_gram_2_aggregated,"./dataset_gram_2_aggregated.txt")


###################################
###################################
###################################
#### aggregate all gram_3

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/gram_3_blogs"
file_list_gram_3_blogs <- list.files(skim_dir)
paste(skim_dir,file_list_gram_3_blogs[1],sep="/")

for (file in 1:length(file_list_gram_3_blogs)){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset_gram_3_blogs")){
    dataset_gram_3_blogs <- read.csv(paste(skim_dir,file_list_gram_3_blogs[file],sep="/"), header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset_gram_3_blogs")){
    temp_dataset <- read.csv(paste(skim_dir,file_list_gram_3_blogs[file],sep="/"), header=TRUE)
    dataset_gram_3_blogs<-rbind(dataset_gram_3_blogs, temp_dataset)
    rm(temp_dataset)
  }
  
}

head(dataset_gram_3_blogs)
str(dataset_gram_3_blogs)
class(dataset_gram_3_blogs)

write.csv(dataset_gram_3_blogs,"./dataset_gram_3_blogs.txt")

### twitter

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/gram_3_twitter"
file_list_gram_3_twitter <- list.files(skim_dir)
paste(skim_dir,file_list_gram_3_twitter[1],sep="/")

for (file in 1:length(file_list_gram_3_twitter)){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset_gram_3_twitter")){
    dataset_gram_3_twitter <- read.csv(paste(skim_dir,file_list_gram_3_twitter[file],sep="/"), header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset_gram_3_twitter")){
    temp_dataset <- read.csv(paste(skim_dir,file_list_gram_3_twitter[file],sep="/"), header=TRUE)
    dataset_gram_3_twitter<-rbind(dataset_gram_3_twitter, temp_dataset)
    rm(temp_dataset)
  }
  
}

head(dataset_gram_3_twitter)
str(dataset_gram_3_twitter)
class(dataset_gram_3_twitter)

write.csv(dataset_gram_3_twitter,"./dataset_gram_3_twitter.txt")

gram_3_news <- read.csv("./../skims/gram_all_news/gram_3_news.txt")
head(gram_3_news)

dataset_gram_3 <- rbind(dataset_gram_3_blogs,dataset_gram_3_twitter,gram_3_news)

head(dataset_gram_3)
str(dataset_gram_3)
class(dataset_gram_3)

dataset_gram_3_aggregated <- aggregate(freq ~ word, 
                                       data=dataset_gram_3,FUN=sum, 
                                       na.rm = TRUE) 

dataset_gram_3_aggregated$word <- as.character(dataset_gram_3_aggregated$word)
dataset_gram_3_aggregated <- filter(dataset_gram_3_aggregated, word != "")
head(dataset_gram_3_aggregated)
str(dataset_gram_3_aggregated)
class(dataset_gram_3_aggregated)

dataset_gram_3_aggregated <- dataset_gram_3_aggregated %>% arrange(desc(freq))

write.csv(dataset_gram_3_aggregated,"./dataset_gram_3_aggregated.txt")
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
###################################
###################################
#### aggregate barrel twitter gram_2

### twitter

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/barrel/gram_2"
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

###################################
###################################
###################################
#### aggregate barrel twitter  gram_3


### twitter

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/barrel/gram_3"
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


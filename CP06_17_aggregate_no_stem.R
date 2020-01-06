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
#### aggregate all gram_1 no_stem

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/no_stem/blogs"
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

skim_dir = "C:/Users/rubiera-laptop/Documents/Tony Work/EID Data Science/Capstone/skims/no_stem/twitter"
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



dataset_gram_1 <- rbind(dataset_gram_1_blogs,dataset_gram_1_twitter)

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
#558809
class(dataset_gram_1_aggregated)

dataset_gram_1_aggregated <- dataset_gram_1_aggregated %>% arrange(desc(freq))

write.csv(dataset_gram_1_aggregated,"./../skims/dataset_gram_1_aggregated_no_stem.txt")

table(dataset_gram_1_aggregated$freq)

dataset_gram_1_aggregated_select <- dataset_gram_1_aggregated %>% filter(freq > 2)
table(dataset_gram_1_aggregated_select$freq)
str(dataset_gram_1_aggregated_select)

write.csv(dataset_gram_1_aggregated_select,
          "./../skims/no_stem/dataset_gram_1_aggregated_no_stem_select.txt")

gram_1_aggr_no_stem_sel <- read.csv("./../skims/no_stem/dataset_gram_1_aggregated_no_stem_select.txt",
                                    stringsAsFactors = FALSE)
head(gram_1_aggr_no_stem_sel,20)





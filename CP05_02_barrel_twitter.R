library(textreadr)
library(tm)
library(caret)
library(tidyverse)
library(knitr)
library(stopwords)


###########################
### section 1
###########################


### turn this entire list of tasks into a loop over really small matrices.

#We load the data and segment it into small chunks that we analyze individually. This 'divide and conquer' method is a quick way to analyze the data given limited computing resources (a laptop).


# 1 to 15 is all

for (file_n in 1:15) {
  print("file_n")
  print(file_n)
  print("file_n")
  print(file_n)  
     if(file_n < 10){
     filenum <- paste("0",file_n,sep="")}
     else {filenum <- file_n}

for (fp in 1:5) {
  ibegin = 1+20000*(fp-1)
  print("2 gram")
  print(fp)
  iend = fp*20000
  
  en_twitter <- 
    read.csv(paste("./../skims/gram_2_twitter/gram_2_twitter_",filenum,"_",fp,".txt",sep=""),
             stringsAsFactors=FALSE)
  regexp_example  <- en_twitter %>% filter(str_detect(word, 
        "^happi ")) 
  write.csv(regexp_example,
        paste("./../barrel/gram_2/gram_2_twitter_happi_",filenum,"_",fp,".txt",sep=""))

}
  
for (fp in 1:5) {
  ibegin = 1+20000*(fp-1)
  print("3 gram")
  print(fp)
  iend = fp*20000
    
  en_twitter <- 
    read.csv(paste("./../skims/gram_3_twitter/gram_3_twitter_",filenum,"_",fp,".txt",sep=""),
             stringsAsFactors=FALSE)
  regexp_example  <- en_twitter %>% filter(str_detect(word, 
        "^happi ")) 
  write.csv(regexp_example,
        paste("./../barrel/gram_3/gram_3_twitter_happi_",filenum,"_",fp,".txt",sep=""))
    
  }
  
}


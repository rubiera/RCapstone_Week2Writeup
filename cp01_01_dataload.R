library(textreadr)


#for reading the whole file correctly
# skipNul = TRUE in readLines
cleanFiles<-function(file,newfile){
  writeLines(iconv(readLines(file,skipNul = TRUE)),newfile)
}

file1_english_blogs <- readLines("./../course-data/en_US/en_US.blogs.txt",skipNul = TRUE)
# warn=FALSE used to eliminate
#Warning message:
#  In readLines("./../course-data/en_US/en_US.news.txt", skipNul = TRUE) :
#  incomplete final line found on './../course-data/en_US/en_US.news.txt'
file1_english_news <- readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
file1_english_twitter <- readLines("./../course-data/en_US/en_US.twitter.txt",skipNul = TRUE)

str(file1_english_blogs)
#chr [1:899288]
str(file1_english_news)
#chr [1:77259]
str(file1_english_twitter)
#chr [1:2360148]

#skims
writeLines(file1_english_blogs[1:100000],"./../course-data/en_US_blogs_skim.txt")
writeLines(file1_english_news,"./../course-data/en_US_news_skim.txt")
writeLines(file1_english_twitter[1:200000],"./../course-data/en_US_twitter_skim.txt")

#for fast work
file1_english_blogs_skim <- file1_english_blogs[1:100000]
file1_english_news_skim <- file1_english_news
file1_english_twitter_skim <- file1_english_twitter[1:200000]

file1_english_blogs_skim[1:3]

#for even faster work
file1_english_blogs_skim_20 <- file1_english_blogs_skim[1:20]
file1_english_news_skim_20 <- file1_english_news_skim[1:20]
file1_english_twitter_skim_20 <- file1_english_twitter_skim[1:20]

head(file1_english_blogs_skim_20)
str(file1_english_blogs_skim_20)

write.csv(file1_english_blogs_skim_20, file="./../course-data/file1_english_blogs_skim_20.csv")
write.csv(file1_english_news_skim_20, file="./../course-data/file1_english_news_skim_20.csv")
write.csv(file1_english_twitter_skim_20, file="./../course-data/file1_english_twitter_skim_20.csv")



















library(textreadr)




en_blogs <- 
  readLines("./../course-data/en_US/en_US.blogs.txt",skipNul = TRUE,warn=FALSE)
en_news <- 
  readLines("./../course-data/en_US/en_US.news.txt",skipNul = TRUE,warn=FALSE)
en_twitter <- 
  readLines("./../course-data/en_US/en_US.twitter.txt",skipNul = TRUE,warn=FALSE)

str(en_blogs)
#chr [1:899288]
str(en_news)
#chr [1:77259]
str(en_twitter)
#chr [1:2360148]


#for fast work
skim_blogs_01 <- en_blogs[1:50000]
skim_blogs_02 <- en_blogs[50001:100000]
skim_blogs_03 <- en_blogs[100001:150000]
skim_blogs_04 <- en_blogs[150001:200000]
skim_blogs_05 <- en_blogs[200001:250000]
skim_blogs_06 <- en_blogs[250001:300000]
skim_blogs_07 <- en_blogs[300001:350000]
skim_blogs_08 <- en_blogs[350001:400000]
skim_blogs_09 <- en_blogs[400001:450000]
skim_blogs_10 <- en_blogs[450001:500000]

skim_blogs_11 <- en_blogs[500001:550000]
skim_blogs_12 <- en_blogs[550001:600000]
skim_blogs_13 <- en_blogs[600001:650000]
skim_blogs_14 <- en_blogs[650001:700000]
skim_blogs_15 <- en_blogs[700001:750000]
skim_blogs_16 <- en_blogs[750001:800000]
skim_blogs_17 <- en_blogs[800001:850000]
skim_blogs_18 <- en_blogs[850001:899288]

writeLines(skim_blogs_01,"./skim_blogs_01.txt")
writeLines(skim_blogs_02,"./skim_blogs_02.txt")
writeLines(skim_blogs_03,"./skim_blogs_03.txt")
writeLines(skim_blogs_04,"./skim_blogs_04.txt")
writeLines(skim_blogs_05,"./skim_blogs_05.txt")
writeLines(skim_blogs_06,"./skim_blogs_06.txt")
writeLines(skim_blogs_07,"./skim_blogs_07.txt")
writeLines(skim_blogs_08,"./skim_blogs_08.txt")
writeLines(skim_blogs_09,"./skim_blogs_09.txt")
writeLines(skim_blogs_10,"./skim_blogs_10.txt")

writeLines(skim_blogs_11,"./skim_blogs_11.txt")
writeLines(skim_blogs_12,"./skim_blogs_12.txt")
writeLines(skim_blogs_13,"./skim_blogs_13.txt")
writeLines(skim_blogs_14,"./skim_blogs_14.txt")
writeLines(skim_blogs_15,"./skim_blogs_15.txt")
writeLines(skim_blogs_16,"./skim_blogs_16.txt")
writeLines(skim_blogs_17,"./skim_blogs_17.txt")
writeLines(skim_blogs_18,"./skim_blogs_18.txt")

skim_news_01 <- en_news[1:40000]
skim_news_02 <- en_news[40001:77259]

writeLines(skim_news_01,"./skim_news_01.txt")
writeLines(skim_news_02,"./skim_news_02.txt")

skim_twitter_01 <- en_twitter[1:100000]
skim_twitter_02 <- en_twitter[100001:200000]
skim_twitter_03 <- en_twitter[200001:300000]
skim_twitter_04 <- en_twitter[300001:400000]
skim_twitter_05 <- en_twitter[400001:500000]
skim_twitter_06 <- en_twitter[500001:600000]
skim_twitter_07 <- en_twitter[600001:700000]
skim_twitter_08 <- en_twitter[700001:800000]
skim_twitter_09 <- en_twitter[800001:900000]
skim_twitter_10 <- en_twitter[900001:1000000]

skim_twitter_11 <- en_twitter[1000001:1100000]
skim_twitter_12 <- en_twitter[1100001:1200000]
skim_twitter_13 <- en_twitter[1200001:1300000]
skim_twitter_14 <- en_twitter[1300001:1400000]
skim_twitter_15 <- en_twitter[1400001:1500000]
skim_twitter_16 <- en_twitter[1500001:1600000]
skim_twitter_17 <- en_twitter[1600001:1700000]
skim_twitter_18 <- en_twitter[1700001:1800000]
skim_twitter_19 <- en_twitter[1800001:1900000]
skim_twitter_20 <- en_twitter[1900001:2000000]

skim_twitter_21 <- en_twitter[2000001:2100000]
skim_twitter_22 <- en_twitter[2100001:2200000]
skim_twitter_23 <- en_twitter[2200001:2300000]
skim_twitter_24 <- en_twitter[2300001:2360148]

writeLines(skim_twitter_01,"./skim_twitter_01.txt")
writeLines(skim_twitter_02,"./skim_twitter_02.txt")
writeLines(skim_twitter_03,"./skim_twitter_03.txt")
writeLines(skim_twitter_04,"./skim_twitter_04.txt")
writeLines(skim_twitter_05,"./skim_twitter_05.txt")
writeLines(skim_twitter_06,"./skim_twitter_06.txt")
writeLines(skim_twitter_07,"./skim_twitter_07.txt")
writeLines(skim_twitter_08,"./skim_twitter_08.txt")
writeLines(skim_twitter_09,"./skim_twitter_09.txt")
writeLines(skim_twitter_10,"./skim_twitter_10.txt")

writeLines(skim_twitter_11,"./skim_twitter_11.txt")
writeLines(skim_twitter_12,"./skim_twitter_12.txt")
writeLines(skim_twitter_13,"./skim_twitter_13.txt")
writeLines(skim_twitter_14,"./skim_twitter_14.txt")
writeLines(skim_twitter_15,"./skim_twitter_15.txt")
writeLines(skim_twitter_16,"./skim_twitter_16.txt")
writeLines(skim_twitter_17,"./skim_twitter_17.txt")
writeLines(skim_twitter_18,"./skim_twitter_18.txt")
writeLines(skim_twitter_19,"./skim_twitter_19.txt")
writeLines(skim_twitter_20,"./skim_twitter_20.txt")

writeLines(skim_twitter_21,"./skim_twitter_21.txt")
writeLines(skim_twitter_22,"./skim_twitter_22.txt")
writeLines(skim_twitter_23,"./skim_twitter_23.txt")
writeLines(skim_twitter_24,"./skim_twitter_24.txt")
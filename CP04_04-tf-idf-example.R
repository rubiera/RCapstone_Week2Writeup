# https://ethen8181.github.io/machine-learning/clustering_old/tf_idf/tf_idf.html
# environment 
library(tm)
library(proxy)
library(dplyr)

doc <- c( "The sky is blue.", "The sun is bright today.",
          "The sun in the sky is bright.", "We can see the shining sun, the bright sun." )

# create term frequency matrix using functions from tm library
doc_corpus <- Corpus( VectorSource(doc) )
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
tdm <- TermDocumentMatrix(doc_corpus, control = control_list)

# print
( tf <- as.matrix(tdm) )

# idf
( idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) )

# diagonal matrix
( idf <- diag(idf) )

tf_idf <- crossprod(tf, idf)
colnames(tf_idf) <- rownames(tf)
tf_idf

# Note that normalization is computed "row-wise"
tf_idf / sqrt( rowSums( tf_idf^2 ) )

doc

######


doc_twitter <- readLines("./../skims/skim_twitter_01.txt",skipNul = TRUE,warn=FALSE)
doc_twitter <- doc_twitter[1:10]

# create term frequency matrix using functions from tm library
doc_corpus_twitter <- Corpus( VectorSource(doc_twitter) )
inspect(doc_corpus_twitter)
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
tdm <- TermDocumentMatrix(doc_corpus_twitter, control = control_list)
inspect(tdm)

# print
( tf <- as.matrix(tdm) )

# idf
( idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) )

# diagonal matrix
( idf <- diag(idf) )

tf_idf <- crossprod(tf, idf)
colnames(tf_idf) <- rownames(tf)
tf_idf

# Note that normalization is computed "row-wise"
tf_idf / sqrt( rowSums( tf_idf^2 ) )

doc_twitter


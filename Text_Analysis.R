# William Dean
# Text and Emoji Analysis of Airbnb and Twitter Data

library(plyr)
library(tm)
library(stringr)
library(NLP)
options(stringsAsFactors = FALSE)

## Load Data --------------------

# Air bnb
bostonreview <- read.csv("boston/reviews.csv")
bostonlisting <- read.csv("boston/listings.csv")
load("Boston.Rda")
chicagolisting <- read.csv("chicago/listings.csv")
chicagoreview <- read.csv("chicago/reviews.csv")
load("Chicago.Rda")
load("Cambridge.Rda")
load("Tweets.Rda")

# Positive and Negative
pos <- read.table("sentiment/positive-words.txt")$V1
neg <- read.table("sentiment/negative-words.txt")$V1

## Text --------------------------------------------

ascii <- function(x) {
    # Convert text to ascii characters only
    iconv(x, from = "latin1", to = "ascii", sub = "byte") 
}

reviews <- bostonreview$comments
reviews <- ascii(reviews)
reviews <- removeWords(reviews, words = stopwords())
reviews <- removePunctuation(reviews)
reviews <- removeNumbers(reviews)
reviews <- stripWhitespace(reviews)
review <- Corpus(VectorSource(reviews))
review <- tm_map(review, tolower)
review.tdm <- TermDocumentMatrix(review)
review.tdm <- removeSparseTerms(review.tdm, .95)
re.d <- t(data.matrix(review.tdm))
re.df <- as.data.frame(re.d, 
                       stringsAsFactors = FALSE)
str(review.tdm)

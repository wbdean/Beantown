# William Dean
# Text and Emoji Analysis of Airbnb and Twitter Data

library(plyr)
library(tm)
library(tidytext)
library(stringr)
library(NLP)
library(sentimentr)
library(ggplot2)
library(tidyr)
library(dplyr)
options(stringsAsFactors = FALSE)
source("sentiment.R")

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

## Text --------------------------------------------

totSent <- function(text) {
    text = as.character(text)
    review = sentimentr::sentiment_by(text)
    reviews = reviews %>% 
        mutate(Sent = ifelse(sentiment > 0, "Positive", 
                             ifelse(sentiment < 0, "Negative", 
                                    "Neutral")))
    return(review)
}

sentSent <- function(text) {
    reviews = sentimentr::sentiment(text)
    reviews = reviews %>% 
        mutate(Sent = ifelse(sentiment > 0, "Positive", 
                             ifelse(sentiment < 0, "Negative", 
                                    "Neutral")))
    return(reviews)
}

SentSpread <- function(x) {
    x = x %>% 
        group_by(element_id, Sent) %>% 
        summarise(n = n()) %>% 
        spread(Sent, n, fill = 0)
    return(x)
}

bindSent <- function(df, sent) {
    df = cbind("element_id" = 1:nrow(df), df)
    df.sent = left_join(df, sent, by = "element_id")
    return(df.sent)
}


# Tweets.sent 
Tweets.sent <- sentSent(Tweets$text)
Tweets.sent <- bindSent(Tweets, Tweets.sent)
Tweets.sent.spread <- bindSent(Tweets, SentSpread(Tweets.sent))

# Airbnb
# Reviews
sample <- 1:1000
bostonreview.samp <- bostonreview[sample, ]
Boston.sent <- sentSent(bostonreview.samp$comments)
Boston.sent <- bindSent(bostonreview.samp, Boston.sent)
Boston.sent.spread <- bindSent(bostonreview.samp, 
                               SentSpread(Boston.sent))

# Listings
blisting.sent <- sentSent(bostonlisting$neighborhood_overview)
blisting.sent <- bindSent(bostonlisting, blisting.sent)
blisting.sent.spread <- bindSent(bostonlisting, 
                                 SentSpread(blisting.sent))

















Tweets <- cbind("id" = 1:nrow(Tweets), Tweets)
Tweets.sent <- left_join(reviews, Tweets %>% select(id, neighborhood, city), 
                         by = c("element_id" = "id"))
Tweets.sent <- Tweets.sent %>% 
    mutate(Sent = ifelse(sentiment > 0, "Positive", 
                         ifelse(sentiment < 0, "Negative", 
                                "Neutral")))
Sent.Neigh <- Tweets.sent %>% group_by(city, neighborhood, Sent) %>% summarize(n = n())

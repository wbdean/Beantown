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






### Analysis

Tweets.analysis <- Tweets.sent %>% 
    group_by(city, neighborhood, Sent) %>%
    summarise(n = n()) %>% 
    mutate(n2 = sum(n), prop = n / n2) %>% ungroup()









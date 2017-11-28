# William Dean
# Text and Emoji Analysis of Airbnb and Twitter Data

library(plyr)
library(tm)
library(tidytext)
library(stringr)
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

### Tidytext -------------------------------------------

## Boston ----------------------------------

# Neighborhood Overview of Boston

BNeigh <- bostonlisting %>% select(id, neighborhood_overview, neighbourhood_cleansed)
BNeigh.words <- BNeigh %>% 
    unnest_tokens(word, neighborhood_overview) %>% 
    count(neighbourhood_cleansed, word, sort = TRUE) %>% 
    anti_join(stop_words)

BNeigh.bi <- BNeigh %>% 
    unnest_tokens(bigram, neighborhood_overview, token = "ngrams", n = 2) %>% 
    count(neighbourhood_cleansed, bigram, sort = TRUE)
BNeigh.bi.sep <- BNeigh.bi %>%
    separate(bigram, c("word1", "word2"), sep = " ")

BNeigh.bi.filt <- BNeigh.bi.sep %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- BNeigh.bi.filt %>% 
    count(neighbourhood_cleansed, word1, word2, sort = TRUE)

BNeigh.sent <- BNeigh.words %>% 
    inner_join(get_sentiments())
BNeigh.sent <- BNeigh.sent %>% 
    mutate(totsent = n * score)

BNeigh.sent.2 <- BNeigh.sent %>% 
    group_by(neighbourhood_cleansed) %>%
    summarise(totsent = sum(totsent), tot = sum(n)) %>% 
    mutate(spw = totsent / tot) %>% 
    ungroup()

BNeigh.map <- left_join(boston.map, 
                        BNeigh.sent.2, 
                        by = c("Name" = "neighbourhood_cleansed"))

ggplot(BNeigh.map, aes(Long, Lat, fill = spw, group = Name)) + 
    geom_polygon()

## Host_about. People in each neighborhood

BHost <- bostonlisting %>% 
    select(id, neighbourhood_cleansed, host_about)

BHost.words <- BHost %>% 
    unnest_tokens(word, host_about) %>% 
    count(neighbourhood_cleansed, word, sort = TRUE) %>% 
    anti_join(stop_words)

BHost.bi <- BHost %>% 
    unnest_tokens(bigram, host_about, token = "ngrams", n = 2) %>% 
    count(neighbourhood_cleansed, bigram, sort = TRUE)

BHost.bi.sep <- BHost.bi %>%
    separate(bigram, c("word1", "word2"), sep = " ")

BHost.bi.filt <- BHost.bi.sep %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

BHost.bi.top <- BHost.bi.filt %>% 
    group_by(neighbourhood_cleansed) %>% 
    top_n(10) %>% 
    ungroup()

# new bigram counts:
bigram_countsHost <- BNeigh.bi.filt %>% 
    count(neighbourhood_cleansed, word1, word2, sort = TRUE)



## Property Type??

BProp <- bostonlisting %>% 
    select(id, neighbourhood_cleansed, property_type)

BProp <- BProp %>% 
    count(neighbourhood_cleansed, property_type, sort = TRUE) %>% 
    group_by(neighbourhood_cleansed) %>%
    top_n(2) %>% 
    ungroup() 

BProp <- BProp %>% 
    filter(property_type != "Apartment")

BProp.map <- left_join(boston.map, BProp, 
                       by = c("Name" = "neighbourhood_cleansed"))
ggplot(BProp.map, aes(Long, Lat, fill = property_type, 
                      group = Name)) +
    geom_polygon() + theme_minimal()


## Reviews

BReview <- left_join(bostonreview %>% 
                         select(listing_id, comments), 
                     bostonlisting %>% 
                         select(id, neighbourhood_cleansed), 
                     by = c("listing_id" = "id"))

BReview.words <- BReview %>% 
    unnest_tokens(word, comments) %>% 
    count(neighbourhood_cleansed, word, sort = TRUE) %>% 
    anti_join(stop_words)


BReview.bi <- BReview %>% 
    unnest_tokens(bigram, comments, token = "ngrams", n = 2) %>% 
    count(neighbourhood_cleansed, bigram, sort = TRUE)

BReview.bi.sep <- BReview.bi %>%
    separate(bigram, c("word1", "word2"), sep = " ")

BHost.bi.filt <- BReview.bi.sep %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

BReview.bi.top <- BHost.bi.filt %>% 
    group_by(neighbourhood_cleansed) %>% 
    top_n(10) %>% 
    ungroup()

# new bigram counts:
bigram_countsHost <- BNeigh.bi.filt %>% 
    count(neighbourhood_cleansed, word1, word2, sort = TRUE)








### sentimentr ------------------------------------------

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









# William Dean
# Text and Emoji Analysis of Airbnb and Twitter Data

library(plyr)
library(tm)
library(tidytext)
library(stringr)
library(sentimentr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(dplyr)
options(stringsAsFactors = FALSE)
source("sentiment.R")
source("Figures.R")
library(topicmodels)

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
Tweets <- bind_cols("id" = 1:nrow(Tweets), Tweets)

### Tidytext -------------------------------------------

## Boston --------------------------------

# Neighbourhood Overviews by host
BNeigh <- bostonlisting %>% 
    select(host_id, neighborhood_overview, neighbourhood_cleansed) %>% 
    distinct()
BNeigh.tfidf <- Get_tfidf(BNeigh)
BNeigh.tfidf <- Get_sent(BNeigh.tfidf)
BNeigh.2 <- bind_sent(BNeigh.tfidf)


# Host Overview. Info about each host
BHost <- bostonlisting %>% 
    select(host_id, host_about, neighbourhood_cleansed) %>% 
    distinct()
BHost.tfidf <- Get_tfidf(BHost)
BHost.tfidf <- Get_sent(BHost.tfidf)
BHost.2 <- bind_sent(BHost.tfidf)


# Reviews for each neighbourhood
BReview <- left_join(bostonreview %>% 
                         select(listing_id, comments), 
                     bostonlisting %>% 
                         select(id, neighbourhood_cleansed), 
                     by = c("listing_id" = "id"))
# Sample Reviews
BReview <- BReview[sample(1:nrow(BReview), 10000), ]
BReview.tfidf <- Get_tfidf(BReview)
BReview.tfidf <- Get_sent(BReview.tfidf)
BReview.2 <- bind_sent(BReview.tfidf)


# House Info Listing
BHouse <- bostonlisting %>% 
    select(host_id, name, summary, 
           space, neighbourhood_cleansed) %>%
    distinct() %>% 
    unite(info, name, summary, space, sep = ". ")
BHouse.tfidf <- Get_tfidf(BHouse)
BHouse.tfidf <- Get_sent(BHouse.tfidf)
BHouse.2 <- bind_sent(BHouse.tfidf)

# Tweet 
BTweets <- Tweets %>% 
    filter(city == "Boston") %>% 
    select(id, text, neighborhood)
BTweets.tfidf <- Get_tfidf(BTweets)
BTweets.tfidf <- Get_sent(BTweets.tfidf)
BTweets.2 <- bind_sent(BTweets.tfidf)

### Topic Models ------------------------------------
k <- 4
BNeigh.lda <- BReview.2 %>% 
    cast_dtm(neighbourhood, text, n)
BNeigh.lda <- LDA(BNeigh.lda, k = 25) %>% 
    tidy(matrix = "gamma")


BNeigh.lda %>% 
    group_by(document) %>% 
    mutate(maxgamma = max(gamma)) %>% 
    filter(maxgamma == gamma) %>% 
    left_join(boston.map, by = c("document" = "Name")) %>%
    ggplot(aes(x = Long, y = Lat, fill = factor(topic),
               group = document)) +
    geom_polygon(color = "black") + theme()

BNeigh.2 %>%
    group_by(neighbourhood) %>% 
    summarise(sentiment = mean(sentiment)) %>% 
    left_join(boston.map, by = c("neighbourhood" = "Name")) %>%
    ggplot(aes(x = Long, y = Lat, fill = sentiment, 
           group = neighbourhood))+ 
    geom_polygon(color = "black")
    

#### --------------------------------------------------

B.map <- to_mapdf(BReview.tfidf$sentence %>% 
                           group_by(neighbourhood) %>% 
                           summarise(m = mean(sentiment)), 
                       "boston")
quick_map(B.map)

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




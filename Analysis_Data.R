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
library(topicmodels)
options(stringsAsFactors = FALSE)
source("sentiment.R")
source("Figures.R")


## Load Data --------------------

# Air bnb
bostonreview <- read.csv("boston/reviews.csv")
bostonlisting <- read.csv("boston/listings.csv")
chicagolisting <- read.csv("chicago/listings.csv")
chicagoreview <- read.csv("chicago/reviews.csv")
load("Boston.Rda")
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

## Comparisons ------------------------------

# Just Home Name Listing
BHome <- bostonlisting %>% 
    select(host_id, name, neighbourhood_cleansed) %>% 
    distinct() 
BHome.2<- BHome %>% Get_tfidf() %>%
    Get_sent() %>% 
    bind_sent()
CHome <- chicagolisting %>% 
    select(host_id, name, neighbourhood) %>% 
    distinct()
CHome.2 <- CHome %>% Get_tfidf() %>%
    Get_sent() %>% 
    bind_sent()

Home <- bind_rows(BHome.2, 
                  CHome.2)
# Tweet 
Tweets.2 <- Tweets %>% 
    filter(city == "Boston" | city == "Chicago") %>% 
    select(id, text, neighborhood) %>% 
    Get_tfidf() %>% 
    Get_sent() %>% 
    bind_sent()

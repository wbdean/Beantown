### Analysis

# William Dean

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
source("Analysis_Data.R")


BNeigh.lda <- Home %>% 
    cast_dtm(neighbourhood, text, n)
BNeigh.lda <- LDA(BNeigh.lda, k = 25) %>% 
    tidy(matrix = "gamma")


BNeigh.lda %>% 
    group_by(document) %>% 
    top_n(1, gamma) %>% 
    left_join(boston.map, by = c("document" = "Name")) %>%
    ggplot(aes(x = Long, y = Lat,
               fill = factor(topic),
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

ggplot(BOSTON, aes(accommodates, overall_satisfaction)) +
    geom_point()


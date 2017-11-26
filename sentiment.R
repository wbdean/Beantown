# Sentiment Functions

library(stringr)
library(magrittr)
library(tidytext)
library(ggplot2)
library(dplyr)
options(stringsAsFactors = FALSE)


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



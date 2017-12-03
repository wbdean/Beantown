# Sentiment Functions

library(stringr)
library(magrittr)
library(tidytext)
library(ggplot2)
library(dplyr)
options(stringsAsFactors = FALSE)


## Text --------------------------------------------

### TFIDF

# Function to get Significant Words and Bigrams for a given text by Neighbourhood
# FORMAT: c("ID", "CONTENT", "NEIGHBOURHOOD")
Get_tfidf <- function(df) {
    # Data frame to right format
    names(df) = c("id", "content", "neighbourhood")
    # Get rid of weird Characters 
    df$content = df$content %>% 
        str_replace_all("[^[:alnum:]]", " ") %>% 
        str_replace_all('[[:digit:]]+', "")
    
    ## Word Unnesting
    df.words = df %>% 
        unnest_tokens(word, content) %>% 
        filter(str_detect(word, "[a-z']$")) %>%
        count(neighbourhood, word, sort = TRUE) %>% 
        anti_join(stop_words)
    df.words.tfidf = df.words %>% 
        bind_tf_idf(neighbourhood, word, n) %>% 
        group_by(neighbourhood) %>% 
        top_n(10) %>% 
        ungroup()
    
    ## BiGrams
    df.bi = df %>% 
        unnest_tokens(bigram, content,
                      token = "ngrams", n = 2) %>% 
        count(neighbourhood, bigram, sort = TRUE)
    df.bi.filt = df.bi %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        filter(str_detect(word1, "[a-z']$")) %>% 
        filter(str_detect(word2, "[a-z']$")) %>% 
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>% 
        unite(bigram, word1, word2, sep = " ")
    
    df.bi.tfidf = df.bi.filt %>% 
        bind_tf_idf(neighbourhood, bigram, n) %>% 
        group_by(neighbourhood) %>% 
        top_n(10) %>% 
        ungroup
    
    ## Sentences
    df.sent = df %>% 
        unnest_tokens(sentence, 
                      content,
                      token = "sentences") %>% 
        count(neighbourhood, sentence, sort = TRUE)
    df.sent.tfidf = df.sent %>% 
        bind_tf_idf(neighbourhood, sentence, n) %>% 
        group_by(neighbourhood) %>% 
        top_n(10) %>% 
        ungroup
        
    
    return(list(word = df.words.tfidf, 
                bigram = df.bi.tfidf, 
                sentence = df.sent.tfidf))
}

# Takes a Get_tfidf List and attaches sentiments to each word and bigram
Get_sent <- function(df.tfidf) {
    # Separate List
    df.words = df.tfidf$word
    df.bigram = df.tfidf$bigram
    df.sent = df.tfidf$sentence
    # Use sentimentr
    df.words = bind_cols(df.words, 
                         df.words$word %>% 
                              sentiment() %>% 
                              select(sentiment))
    df.bigram = bind_cols(df.bigram, 
                          df.bigram$bigram %>% 
                              sentiment() %>% 
                              select(sentiment))
    df.sent = bind_cols(df.sent, 
                         df.sent$sentence %>% 
                             sentiment() %>% 
                             select(sentiment))
    return(list(word = df.words, 
                bigram = df.bigram, 
                sentence = df.sent))
}

bind_sent <- function(df.tfidf) {
    names(df.tfidf$word)[2] = names(df.tfidf$bigram)[2] = "text"
    bind_rows(df.tfidf$word, df.tfidf$bigram)
}

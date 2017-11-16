# Sentiment Functions

library(stringr)

wordsInSet <- function(words, set) {
    # Returns how many words in one string, words, are in another, set
    return(words %>% 
               str_split(" ") %>% 
               sapply(function(x) (x %in% set) * 1) %>% 
               sum
           )
}

sentiment <- function(text) {
    pos = wordsInSet(text, pos)
    neg = wordsInSet(text, neg)
    return(pos + neg)
}

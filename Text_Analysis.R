# William Dean
# Text and Emoji Analysis of Airbnb and Twitter Data

## Load Data --------------------

# Air bnb
bostonreview <- read.csv("boston/reviews.csv")
bostonlisting <- read.csv("boston/listings.csv")
load("Boston.Rda")
chicagolisting <- read.csv("chicago/listings.csv")
chicagoreview <- read.csv("chicago/reviews.csv")
load("Chicago.Rda")
load("Cambridge.Rda")

## Text -------------------------------------

ascii <- function(x) {
    # Convert tex to ascii 
    iconv(x, from = "latin1", to = "ascii", sub = "byte") 
}

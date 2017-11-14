### William Dean
## MA 684 Boston University

library(ggplot2)
library(dplyr)
library(tidyr)

### Read Data -----------------------------
path <- "s3_files/"
## Boston
BOSTON <- NULL
for (i in 1:23) {
    boston <- read.csv(paste0(path, 
                              "boston/tomslee_airbnb_boston_",i,
                              ".csv"))
    boston <- boston %>% 
        select(room_type, neighborhood, 
               reviews, overall_satisfaction, 
               accommodates, bedrooms, price, 
               minstay, latitude, longitude)
    BOSTON <- rbind(BOSTON, boston)
}
rm(boston)
bostonreview <- read.csv("boston/reviews.csv")
bostonlisting <- read.csv("boston/listings.csv")

## Cambridge
CAMBRIDGE <- NULL
for (i in 1:6) {
    cambridge <- read.csv(paste0(path, 
                              "cambridge_ma/tomslee_airbnb_cambridge_ma_",i,
                              ".csv"))
    cambridge <- cambridge %>% 
        select(room_type, neighborhood, 
               reviews, overall_satisfaction, 
               accommodates, bedrooms, price, 
               minstay, latitude, longitude)
    CAMBRIDGE <- rbind(CAMBRIDGE, cambridge)
}
rm(cambridge)
## CHICAGO
CHICAGO <- NULL
for (i in 1:26) {
    chicago <- read.csv(paste0("s3_files/chicago/tomslee_airbnb_chicago_", i, ".csv"))
    chicago <- chicago %>% 
        select(room_type, neighborhood, 
               reviews, overall_satisfaction, 
               accommodates, bedrooms, price, 
               minstay, latitude, longitude)
    CHICAGO <- rbind(CHICAGO, chicago)
}
rm(chicago)
chicagolisting <- read.csv("chicago/listings.csv")
chicagoreview <- read.csv("chicago/reviews.csv")



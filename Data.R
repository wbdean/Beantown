### William Dean
## MA 684 Boston University

library(devtools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(remoji)
library(twitteR)
library(RMySQL)
library(ZillowR)


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
save(BOSTON, file = "Boston.Rda")
rm(boston)

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
save(CAMBRIDGE, file = "Cambridge.Rda")
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
save(CHICAGO, file = "Chicago.Rda")
rm(chicago)

# Twitter
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
rm(api_key, api_secret, access_token, access_token_secret, i, path)

# Get Tweet Data for each Neighborhood
Neighborhoods <- c(BOSTON$neighborhood %>% levels, 
                   CAMBRIDGE$neighborhood %>% levels,
                   CHICAGO$neighborhood %>% levels)
City <- c(rep("Boston", length(BOSTON$neighborhood %>% levels())), 
          rep("Cambridge", length(CAMBRIDGE$neighborhood %>% levels())), 
          rep("Chicago", length(CHICAGO$neighborhood %>% levels())))
Tweets <- NULL
for (i in 1:length(Neighborhoods)) {
    neigh <- Neighborhoods[i]
    tw <- searchTwitter(neigh, n = 100) 
    if (!(length(tw) == 0)) {
        tw <- tw %>% 
            twListToDF() %>% 
            select(text, created, favoriteCount,
                   retweetCount, longitude, latitude)
        tw$neighborhood <- rep(neigh, nrow(tw))
        tw$city <- rep(City[i], nrow(tw))
        Tweets <- rbind(Tweets, tw)
    }
}
rm(tw)
save(Tweets, file = "Tweets.Rda")






### Work in Progress --------------------------------------
## Instagram ------------------------------

require(httr)
full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)


# Emoji













### Sample Data ---------------------------------------

# Yelp
mydb <- dbConnect(MySQL(), 
                  user = "mssp", 
                  password = "mssp2017", 
                  dbname="yelp_db", 
                  host="45.63.90.29")
busin <- dbSendQuery(mydb, "select * from business limit 5000")
busin <- fetch(busin, n = -1)


# Zillow
ID <- "X1-ZWz1g47fqsgy6j_5886x"
set_zillow_web_service_id(ID)

zillow <- read.csv("median_price.csv")
zillow <- zillow %>% na.omit







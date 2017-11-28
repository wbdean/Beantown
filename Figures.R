# Mapping 

library(dplyr)
library(ggplot2)

load("boston.map.Rda")
load("chicago.map.Rda")

ggplot(boston.map, aes(Long, Lat, fill = Name, group = Name)) + geom_polygon()

Test.Boston <- Tweets.sent %>% group_by(neighborhood) %>% summarise(sent = mean(sentiment, na.rm = TRUE))

Test.Boston <- left_join(boston.map, Test.Boston, by = c("Name" = "neighborhood"))

ggplot(Test.Boston, aes(Long, Lat, fill = sent, group = Name)) + geom_polygon()

Test.Boston2 <- blisting.sent %>%
    group_by(neighbourhood_cleansed) %>% 
    summarise(sent = mean(sentiment, na.rm = TRUE))
names(Test.Boston2) <- c("Name", "sent")
Test.Boston2 <- left_join(boston.map, Test.Boston,
                          by = "Name")

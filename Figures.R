# Mapping 
# Functions for Mapping

library(dplyr)
library(ggplot2)
library(leaflet)

load("boston.map.Rda")
load("chicago.map.Rda")

to_mapdf <- function(df, city) {
    if (city == "boston") {
        map.df = left_join(boston.map, df, 
                  by = c("Name" = "neighbourhood"))
    }
    return(map.df)
}

quick_map <- function(map.df) {
    ggplot(map.df, aes(Long, Lat, fill = m, group = Name, 
                       color = Name)) + 
        geom_polygon()
}

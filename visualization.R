setwd("F:/Documents/Mine/Kaggle-West_Nile/")

library(dplyr)
library(readr)
library(ggmap)
data_dir <- "data"
train <- read_csv(file.path(data_dir, "train.csv"))
mapdata <- readRDS(file.path(data_dir, "mapdata_copyright_openstreetmap_contributors.rds"))
train$Date <- as.Date(train$Date)

# Which date has the most measurements?
counts_by_date <- train %>% group_by(Date) %>% 
    summarise(NumMeasurements = n()) %>% 
    arrange(desc(NumMeasurements)) %>% 
    head
counts_by_date

date_to_show <- counts_by_date$Date[1]

single_date_grouped_by_location <- train %>% 
    filter(Date == "2007-08-01") %>%
    group_by(Longitude, Latitude) %>%
    summarize(NumMosquitos = sum(NumMosquitos))

qplot(single_date_grouped_by_location$NumMosquitos) + 
    scale_x_log10() + 
    xlab("Number of Mosquitos") +
    ylab("Number of test sites w/ this many mosquitos") +
    ggtitle("Distribution of Mosquito Counts (Log Scale)")


ggmap(mapdata) + 
    geom_point(aes(x=Longitude, y=Latitude, color=NumMosquitos), 
               size=3, data=single_date_grouped_by_location) + 
    scale_color_continuous(trans="log") +
    ggtitle(sprintf("Spatial Arrangement of Mosquito Counts on %s", date_to_show))

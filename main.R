# Analysis of Minneapolis Snow Emergency Tow & Ticket Data
# Data Range: 2015-2019
# Author: Brandon Goblirsch
# Last updated: Feb 3 2020

# Ideas/Insights to explore:
# Find snowfall amounts for each emergency
# Intersect with Neighborhoods to make a nice clean map
#   See what you can find on population per neighborhood and census should have car ownership data
# Polk 2016 Tows XY data needs to be fixed

library(tidyverse)
library(ggmap)
library(lubridate)

register_google(key = "AIzaSyBNx8zvPru6GctBpEVpMIg4th6Ymtt0gEk")

tag_files <- list.files(path = "C:/Users/brand/OneDrive/Documents/GitHub/mpls-snow-emerg-tows-fines-data/raw_data/Tags")
tow_files <- list.files(path = "C:/Users/brand/OneDrive/Documents/GitHub/mpls-snow-emerg-tows-fines-data/raw_data/Tows")

tag1 <- readr::read_csv(tag_files[1],) %>% rename_all(tolower) # day
tag2 <- readr::read_csv(tag_files[2],) %>% rename_all(tolower) %>% rename(day = "day_id") 
tag3 <- readr::read_csv(tag_files[3],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag4 <- readr::read_csv(tag_files[4],) %>% rename_all(tolower)
tag5 <- readr::read_csv(tag_files[5],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag6 <- readr::read_csv(tag_files[6],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag7 <- readr::read_csv(tag_files[7],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag8 <- readr::read_csv(tag_files[8],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag9 <- readr::read_csv(tag_files[9],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag10 <- readr::read_csv(tag_files[10],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag11 <- readr::read_csv(tag_files[11],) %>% rename_all(tolower)   # day
tag12 <- readr::read_csv(tag_files[12],) %>% rename_all(tolower)   # day 
tag13 <- readr::read_csv(tag_files[13],) %>% rename_all(tolower)   # day
tag14 <- readr::read_csv(tag_files[14],) %>% rename_all(tolower) %>% rename(day = "day_id")
tag15 <- readr::read_csv(tag_files[15],) %>% rename_all(tolower)   # day
tag16 <- readr::read_csv(tag_files[16],) %>% rename_all(tolower) %>% rename(day = "day_id")

##################
# Clean Tag Data #
##################

# Trim each file down to (X, Y, date, day)
# Combine each file
tag_dataset <- data.frame()
setwd("C:/Users/brand/OneDrive/Documents/GitHub/mpls-snow-emerg-tows-fines-data/raw_data/Tags")
for (i in 1:length(tag_files)) {
  temp <- readr::read_csv(tag_files[i],) %>% rename_all(tolower)
  # change any "day_id"s to "day"
  if ("day_id" %in% colnames(temp)) {
    temp <- rename(temp, day = "day_id")
  }
  # change "service_da" to "date" and transform to characters
  if ("service_da" %in% colnames(temp)) {
    temp <- rename(temp, date = "service_da") %>%
    transform(date = as.character(date))
  } else {
    temp <- transform(temp, date = as.character(date))
  }
  
  temp <- select(temp, x, y, date, day)
  tag_dataset <- rbind(tag_dataset, temp)
}

# Convert to tibble and fix date column
tags <- transform(tag_dataset, date = as.Date(date)) %>% as_tibble(tag_dataset)

##################
# Clean Tow Data #
##################

# Trim each file down to (X, Y, date, day)
# Combine each file
tow_dataset <- data.frame()
setwd("C:/Users/brand/OneDrive/Documents/GitHub/mpls-snow-emerg-tows-fines-data/raw_data/Tows")
for (i in 1:length(tow_files)) {
  
  temp <- readr::read_csv(tow_files[i],) %>% rename_all(tolower)
  # change any "day_id"s to "day"
  if ("day_identi" %in% colnames(temp)) {
    temp <- rename(temp, day = "day_identi")
  }
  # change "service_da" to "date" and transform to characters
  if ("service_da" %in% colnames(temp)) {
    temp <- rename(temp, date = "service_da") %>%
      transform(date = as.character(date))
  } else {
    
    temp <- rename(temp, date ="call_taken")
  }
 
  temp <- select(temp, x, y, date, day)
  tow_dataset <- rbind(tow_dataset, temp)
}

# To filter out faulty XY data from Polk 2016 Tows
tow_dataset <- filter(tow_dataset, x > -10374957)

# Convert to tibble and fix date column
#tows <-  as_tibble(tow_dataset)
tows <- transform(tow_dataset, date = as.Date(date, format = "%m/%d/%y")) %>% as_tibble(tow_dataset)

################
# Data Analysis#
################


###########
# Mapping #
###########

# In order to get the legend working properly, need to combine tickets and tows into one tibble
# mutate new column "Type" with "ticket"/"tow"

bbox <- make_bbox(lon = tags$x, lat = tags$y, f = .5)
my_map <- get_map(location = bbox, maptype = "roadmap", source = "google", zoom = 12, scale = 2)

ggmap(my_map) +
  geom_point(data = tags,
             mapping = aes(x = x, y = y, fill = "Tickets"), color = "black", size = 0.8, alpha = 0.2
  ) +
  geom_point(data = tows,
             mapping = aes(x = x, y = y), color = "red", size = 0.8, alpha = 0.2,
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  ) +
  scale_fill_manual(name = "Type", values = c("Tickets" = "black")) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))
  #geom_line(data = emergency_routes,
  #          mapping = )

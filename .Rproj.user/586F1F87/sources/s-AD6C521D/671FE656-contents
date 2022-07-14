library(httr)
library(rvest)
library(tidyverse)

setwd('D:/sommer/jom_links/data')

# Download from ROAD
url <- 'https://www.roceeh.uni-tuebingen.de/api/roceeh/assemblagesForSimilarityCalculation.php'

road_api <- httr::GET(url)
tables <- html_nodes(content(road_api), "table")
data <-  html_table(tables[[1]],fill=TRUE)
data <- data %>% mutate(x=as.numeric(x)) %>% 
  mutate(y=as.numeric(y)) %>% 
  mutate(age_min=as.numeric(age_min)) %>% 
  mutate(age_max=as.numeric(age_max))
View(data)

# Preprocessing
# Remove NAs
data_excluded <- data %>% filter(is.na(x)|is.na(y)|is.na(age_min)|is.na(age_min)) 
data <- anti_join(data, data_excluded)

#Add UID
data <- data %>% rowid_to_column('uid')

saveRDS(data, 'ROAD_table.rds')
write.csv(data, 'road_table.csv')

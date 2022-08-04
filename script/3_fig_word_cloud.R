library(tidyverse)

data <- readRDS('data/ROAD_table.rds') %>% select(ochre:tg_18)
x <- colSums(data)


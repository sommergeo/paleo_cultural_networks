library(tidyverse)
library(sf)

data <- readRDS('work/lor7.rds') %>% select(-geometry)

cor(data$jaccard, data$dist, method = 'pearson')
cor(data$jaccard, data$dist, method = 'kendall')
cor(data$jaccard, data$dist, method = 'spearman')

cultures <- data %>% filter(culture.A==culture.B) %>% 
  group_by(culture.A) %>% 
  summarize(median_dist=median(dist),
            mean_dist=mean(dist),
            sd_dist=sd(dist),
            max_dist=max(dist),
            median_jaccard=median(jaccard),
            mean_jaccard=mean(jaccard),
            sd_jaccard=sd(jaccard),
            max_jaccard=max(jaccard),
            pearson=cor(jaccard, dist, method='pearson'),
            kendall=cor(jaccard, dist, method='kendall'),
            spearman=cor(jaccard, dist, method='spearman'))
write_csv(cultures, 'work/stats_cultures.csv')


periods <- data %>% filter(cultural_period.A==cultural_period.B) %>%
  group_by(cultural_period.A) %>% 
  summarize(median_dist=median(dist),
            mean_dist=mean(dist),
            sd_dist=sd(dist),
            max_dist=max(dist),
            median_jaccard=median(jaccard),
            mean_jaccard=mean(jaccard),
            sd_jaccard=sd(jaccard),
            max_jaccard=max(jaccard),
            pearson=cor(jaccard, dist, method='pearson'),
            kendall=cor(jaccard, dist, method='kendall'),
            spearman=cor(jaccard, dist, method='spearman'))
write_csv(periods, 'work/stats_periods.csv')


data_periods <- data %>% filter(cultural_period.A %in% c('MSA','Middle Paleolithic') & cultural_period.B %in% c('MSA','Middle Paleolithic')) %>% 
  mutate(
    connection=case_when(
      cultural_period.A == 'MSA' & cultural_period.B == 'MSA' ~ 'Middle Stone Age',
      cultural_period.A == 'Middle Paleolithic' & cultural_period.B == 'Middle Paleolithic' ~ 'Middle Paleolithic',
      cultural_period.A != cultural_period.B ~ 'Between MSA and MP'
  ))

data_cultures <- data %>% filter(culture.A == culture.B) %>% filter(culture.A %in% c())


ggplot(data=data_periods)+
  geom_point(aes(x=dist, y=jaccard))+
  facet_wrap(.~connection)

library(tidyverse)
library(sf)
data <- readRDS('work/lor7.rds')

cor(data$jaccard, data$dist, method = 'pearson')
cor(data$jaccard, data$dist, method = 'kendall')
cor(data$jaccard, data$dist, method = 'spearman')

cor_culture <- data %>% filter(culture.A==culture.B) #%>% 
  group_by(culture.A) %>% 
  summarize(median_dist=median(dist),
            mean_dist=mean_dist(dist),
            max_dist=max(dist),
            median_jaccard=median(jaccard),
            mean_jaccard=median(jaccard),
            max_jaccard=max(jaccard),
            pearson=cor(jaccard, dist, method='pearson'),
            kendall=cor(jaccard, dist, method='kendall',
            spearman=cor(jaccard, dist, method='spearman')


ggplot(data=data)+
  geom_point(aes(x=dist, y=jaccard))

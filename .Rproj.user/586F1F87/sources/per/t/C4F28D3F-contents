library(tidyverse)
library(sf)

data <- readRDS('work/lor7.rds') %>% select(-c(geometry, opacity))

# Statistics for ALL cultures ----

stats_all_cultures <- data %>% filter(culture.A==culture.B) %>% 
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
write_csv(stats_all_cultures, 'work/stats_all_cultures.csv')


stats_all_periods <- data %>% filter(cultural_period.A==cultural_period.B) %>%
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
write_csv(stats_all_cultures, 'work/stats_all_periods.csv')




# Statistics for selected cultures

## Subset data
data_periods <- data %>% filter(cultural_period.A %in% c('MSA','Middle Paleolithic') & cultural_period.B %in% c('MSA','Middle Paleolithic')) %>% 
  mutate(
    connection=case_when(
      cultural_period.A == 'MSA' & cultural_period.B == 'MSA' ~ 'Middle Stone Age',
      cultural_period.A == 'Middle Paleolithic' & cultural_period.B == 'Middle Paleolithic' ~ 'Middle Paleolithic',
      cultural_period.A != cultural_period.B ~ 'Between MSA and MP'
    ))
write_csv(data_periods, 'work/periods_edges.csv')


data_cultures <- data %>%
  mutate(
    connection=case_when(
      culture.A %in% c('Acheulean - Africa', 'Early Acheulean - Africa', 'Middle Acheulean - Africa', 'Late Acheulean - Africa') & culture.B %in% c('Acheulean - Africa', 'Early Acheulean - Africa', 'Middle Acheulean - Africa', 'Late Acheulean - Africa') ~ 'African Acheulean',
      culture.A %in% c('Acheulean - Levant', 'Early Acheulean - Levant', 'Middle Acheulean - Levant', 'Late Acheulean - Levant') & culture.B %in% c('Acheulean - Levant', 'Early Acheulean - Levant', 'Middle Acheulean - Levant', 'Late Acheulean - Levant') ~ 'Levantine Acheulean',
      culture.A == 'Acheulean - Europe' & culture.B == 'Acheulean - Europe' ~ 'European Acheulean',
      culture.A == 'Howiesonspoort' & culture.B == 'Howiesonspoort' ~ 'Howiesons Poort',
      culture.A == 'Aterian' & culture.B == 'Aterian' ~ 'Aterian',
      culture.A == 'Micoquian' & culture.B == 'Micoquian' ~ 'Micoquian',     
      culture.A == 'ELSA' & culture.B == 'ELSA' ~ 'Early Later Stone Age',
      culture.A == 'Gravettian' & culture.B == 'Gravettian' ~ 'Gravettian',
      culture.A == 'Initial Upper Paleolithic - Eurasia' & culture.B == 'Initial Upper Paleolithic - Eurasia' ~ 'Initial Upper Paleolithic'
    )) %>% 
  mutate(connection = factor(connection, levels = c('African Acheulean', 'Levantine Acheulean', 'European Acheulean', 'Howiesons Poort', 'Aterian', 'Micoquian', 'Early Later Stone Age', 'Gravettian', 'Initial Upper Paleolithic'))) %>% 
  filter(!is.na(connection))
write_csv(data_cultures, 'work/cultures_edges.csv')

# Statistics for subsets
stats_periods <- data_periods %>% group_by(connection) %>% 
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
write_csv(stats_periods, 'work/stats_selected_periods.csv')

stats_cultures <- data_cultures %>% group_by(connection) %>% 
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
write_csv(stats_cultures, 'work/stats_selected_cultures.csv')


# Plot Semivariogram

ggplot(data=data_periods)+
  geom_point(aes(x=dist, y=jaccard, color=connection))

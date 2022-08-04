library(tidyverse)
library(sf)
library(cowplot)
library(ggdark)
library(viridisLite)

data <- readRDS('work/lor7.rds') %>% select(-c(geometry, opacity))

# data subsets
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
  mutate(connection = factor(connection, levels = c('Early Later Stone Age', 'Gravettian', 'Initial Upper Paleolithic', 'Howiesons Poort', 'Aterian', 'Micoquian','African Acheulean', 'Levantine Acheulean', 'European Acheulean'))) %>% 
  filter(!is.na(connection))
write_csv(data_cultures, 'work/cultures_edges.csv')

data_selected <- rbind(data_periods, data_cultures) %>% mutate(connection = factor(connection, levels = c('Middle Stone Age', 'Between MSA and MP', 'Middle Paleolithic', 'Early Later Stone Age', 'Gravettian', 'Initial Upper Paleolithic', 'Howiesons Poort', 'Aterian', 'Micoquian','African Acheulean', 'Levantine Acheulean', 'European Acheulean'))) 
saveRDS(data_selected, 'work/selected_cultures_and_periods')
write_csv(data_selected, 'work/selected_cultures_and_periods_edges.csv')

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
write_csv(stats_all_cultures, 'results/stats_all_cultures.csv')


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
write_csv(stats_all_cultures, 'results/stats_all_periods.csv')





# Statistics for selected cultures ----
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
write_csv(stats_periods, 'results/stats_selected_periods.csv')

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
write_csv(stats_cultures, 'results/stats_selected_cultures.csv')


# Plot heatmap ----
plt1 <- ggplot(data=data_selected, aes(x=dist, y=jaccard))+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F) +
  facet_wrap(.~connection, scales='free_x', ncol=3)+
  labs(x=bquote('Distance [km]'), y = "Similarity [Jaccard Index]", caption = "") + 
  scale_x_continuous(labels=function(x)x/1000)+
  #scale_fill_gradient(low = "#660016", high = "#f35a7b", name='Density')+
  scale_fill_viridis_c(option = "inferno")+
  dark_theme_classic()+
  theme(text=element_text(size=10),
        strip.background = element_rect(fill = "#1a1a1a", colour = "#1a1a1a"),
        panel.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'),
        plot.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'),
        legend.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'))
plt1

ggsave("pres/fig_3.png", units="cm", width=18, height=18, dpi=300)
ggsave("pres/fig_3.tiff", units="cm", width=18, height=18, dpi=300)

# Plot idealized heat map ----
set.seed(1993)
dummydata <- data.frame(dist=runif(100, 0, 1000)) %>% mutate(jaccard=((dist/1000)*-1)+1+rnorm(100,0,.1))
plot(dummydata$dist, dummydata$jaccard)

ggplot(data=dummydata, aes(x=dist, y=jaccard))+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F) +
  labs(x=bquote('Distance [km]'), y = "Similarity [Jaccard Index]", caption = "", title='Idealized spatially autocorrelated distribution') + 
  scale_x_continuous(labels=function(x)x/1)+
  scale_y_continuous(breaks=seq(0,1,0.25))+
  #scale_fill_gradient(low = "#660016", high = "#f35a7b", name='Density')+
  scale_fill_viridis_c(option = "inferno")+
  dark_theme_classic()+
  theme(text=element_text(size=10),
        strip.background = element_rect(fill = "#1a1a1a", colour = "#1a1a1a"),
        panel.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'),
        plot.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'),
        legend.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'),
        plot.title = element_text(hjust = 0.5))

ggsave("pres/fig_3_idealized.png", units="cm", width=10, height=8, dpi=300)
ggsave("pres/fig_3_idealized.tiff", units="cm", width=10, height=8, dpi=300)

# Plot Jaccard distribution
library(ggridges)
data_selected %>%
  mutate(connection = factor(connection, levels = c('African Acheulean', 'Levantine Acheulean', 'European Acheulean', 'Howiesons Poort', 'Aterian', 'Micoquian', 'Early Later Stone Age', 'Gravettian', 'Initial Upper Paleolithic', 'Middle Stone Age', 'Between MSA and MP', 'Middle Paleolithic'))) %>%
  ggplot(aes(x=jaccard, y=connection, fill=connection))+
  geom_density_ridges2()+
  scale_fill_manual(values=c('#f35a7b', '#ffbd65', '#ffe565', '#ccff66','#66ff66', '#66ffff', '#70c3ff', '#8f78ff','#e668ff', '#fb7050', '#707073', '#73b2d8'), guide='none')+
  scale_color_manual(values=c('#f35a7b', '#ffbd65', '#ffe565', '#ccff66','#66ff66', '#66ffff', '#70c3ff', '#8f78ff','#e668ff', '#fb7050', '#707073', '#73b2d8'), guide='none')+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  dark_theme_classic()+
  labs(x="Similarity [Jaccard Index]", y="" , caption = "") + 
  theme(text=element_text(size=10),
        panel.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'),
        plot.background = element_rect(fill = '#1a1a1a', colour='#1a1a1a'))

ggsave("pres/fig_S1.png", units="cm", width=18, height=18, dpi=300)
ggsave("pres/fig_S1.tiff", units="cm", width=18, height=18, dpi=300)




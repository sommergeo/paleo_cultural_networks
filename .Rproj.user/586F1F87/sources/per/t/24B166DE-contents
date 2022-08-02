library(tidyverse)
library(gganimate)
library(ggfx)

d <- readRDS('data/ROAD_table.rds') %>% rename(query_age_min=age_min, query_age_max=age_max, locality.idlocality=locality, locality.x=x, locality.y=y) %>% 
  drop_na(c(query_age_min,query_age_max, locality.x, locality.y)) %>% 
  mutate(steps=as.integer(cut(query_age_min, # cut in 10ka-slices
                              breaks= seq(-1000,3000000,10000), label=FALSE)*10)) %>% 
  #mutate(steps_exp=log(steps))
  mutate(steps_exp=log(query_age_max), steps_dur=log(query_age_max)-log(query_age_min))

## World map from natural earth (rendering 10x faster)
world_shape <- rnaturalearth::ne_countries(scale = 110, type = 'countries', continent = c('europe', 'africa', 'asia', 'oceania'), returnclass = 'sp')

world <- ggplot() +
  borders(world_shape, colour = 'grey', fill = 'grey')+
  #coord_proj("+proj=robin +lon_0=50", xlim = c(-20, 180), ylim = c(-50, 90)) +
  coord_map(projection="mollweide", orientation=c(90,50,0), xlim=c(-20,180), ylim = c(-50, 90))+
  theme_void()
plot(world)


## Rendering with exponential speed
map <- world +
  geom_point(data=d, aes(x=locality.x, y=locality.y, group=locality.idlocality),
             size=2, color='#4472c4', alpha = 1, show.legend=F)+
  transition_time(time=-d$steps_exp, ) +
  shadow_mark(past=TRUE, size=2, colour="grey30", alpha=.1)+
  enter_fade() +
  exit_fade() +
  labs(title = '{format(round(exp(-frame_time), digits=-3), big.mark = ",", scientific=F)} years ago ') +
  theme(plot.title = element_text(size=18, colour="grey30", hjust=0.5))

animate(map, fps=5, duration=5, start_pause = 0, end_pause = 0)

#Export
animate(map, fps=24, duration=20, start_pause = 48, end_pause = 0, bg='white', width=20, height=15, units='cm', res=300)
anim_save(filename="results/fig_site_animation.gif", animation = last_animation())

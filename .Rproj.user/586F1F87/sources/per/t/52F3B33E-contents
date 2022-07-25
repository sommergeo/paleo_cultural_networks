library(tidyverse)
library(DescTools)
library(tictoc)

setwd('D:/SOMMER/jom_links/work')


table <- readRDS(table, file='../data/road_table.rds')
lor <- readRDS('../work/lor7.rds')

# Create maximal list of relationships
tic()
lor <- expand.grid(uidA=table$uid, uidB=table$uid) %>% 
  left_join(table %>% select(uid,locality,age_min,age_max),by=c('uidA'='uid'), suffix=c(".A",".B")) %>%
  left_join(table %>% select(uid,locality,age_min,age_max),by=c('uidB'='uid'), suffix=c(".A",".B"))
toc()
saveRDS(lor, file='../work/lor1.rds')

# Exclude equilocality
tic()
lor <- lor %>% filter(locality.A!=locality.B)
toc()
saveRDS(lor, file='../work/lor2.rds')

# Filter for contemporaneous assemblages
tic()
lor <- lor %>% filter(Overlap(lor %>% select(age_min.A, age_max.A), lor %>% select(age_min.B, age_max.B))!=0)
toc()
saveRDS(lor, file='../work/lor3.rds')

# Exclude duplicates (undirected links)
tic()
lor <- lor %>% filter(duplicated(apply(., 1, sort), MARGIN = 2)==F)
toc()
saveRDS(lor, file='../work/lor4.rds')


# Filter for similar assemblages
library(philentropy)
jaccard_similarity <- function(uidA,uidB,method='jaccard'){
  first <- subset(table, uid==uidA, select=ochre:tg_18)
  last <- subset(table, uid==uidB, select=ochre:tg_18)
  d <- rbind(first,last)
  j <- 1-distance(d,method='jaccard', mute.message=T) %>% as.numeric()
}

tic()
lor <- lor %>% rowwise %>% mutate(jaccard=jaccard_similarity(uidA, uidB))
toc()
saveRDS(lor, file='../work/lor5.rds')

tic()
lor <- lor %>% filter(jaccard>0)
toc()
saveRDS(lor, file='../work/lor6.rds')


# Geo
library(sf)
library(geosphere)

# calculate great circles and distances
tic()
lor <- cbind(lor,
             dist=distGeo(
               p1 = lor %>% left_join(table %>% select(uid,x,y),by=c('uidA'='uid')) %>%  select(x,y),
               p2 = lor %>% left_join(table %>% select(uid,x,y),by=c('uidB'='uid')) %>%  select(x,y)),
             gcIntermediate(
               p1 = lor %>% left_join(table %>% select(uid,x,y),by=c('uidA'='uid')) %>%  select(x,y),
               p2 = lor %>% left_join(table %>% select(uid,x,y),by=c('uidB'='uid')) %>%  select(x,y),
               n=50, sp=T, addStartEnd=T) %>% st_as_sf()
)
toc()



tic()
# Order columns and calculate opacity
lor <- lor %>% arrange(jaccard, desc(dist)) 
lor <- lor %>% mutate(opacity=1-(dist/max(dist))) 
toc()

lor <- lor %>% 
  left_join(table %>% select(uid, culture, cultural_period, regional_technocomplex),by=c('uidA'='uid'), suffix=c(".A",".B")) %>%
  left_join(table %>% select(uid, culture, cultural_period, regional_technocomplex),by=c('uidB'='uid'), suffix=c(".A",".B"))

saveRDS(lor, file='../work/lor7.rds')
lor <- readRDS(file='../work/lor7.rds')
st_write(lor, dsn="../results/relationships.gpkg", layer='relationships', layer_options = "OVERWRITE=YES", delete_dsn = TRUE )

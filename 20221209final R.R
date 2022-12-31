#R_final

#IMOS - National Reef Monitoring Network Sub-Facility - Global reef fish abundance and biomass

library(tidyverse)
reef.fish <- read.csv('RLSreeffishdataset.csv')

reef.fish.Aus <- reef.fish %>% filter(Country == 'Australia')

#Map----
library(maptools)
library(viridis) 
jpeg(file = 'Astralia map.jpg', res = 300, width = 35, height = 26, units = 'cm')
data("wrld_simpl")
points <- reef.fish.Aus.re[c('Ecoregion','meanlong','meanlat')]
plot(wrld_simpl, xlim = c(110, 160), ylim = c(-50, 0), col = 'lightgreen', bg = 'lightblue')
coords <- reef.fish.Aus.re[c('meanlong','meanlat')]
coords.sp <- coordinates(coords)
point.sp <- SpatialPoints(coords.sp)
region <- data.frame(ecoregion = reef.fish.Aus.re$Ecoregion)
pointre.sp <- SpatialPointsDataFrame(point.sp, region)
plot(pointre.sp, add = T, col = viridis(n = 18, option = 'plasma', direction = -1), pch = 19, cex = 1)
text(x = points$meanlong, y = points$meanlat, labels = points$Ecoregion)
dev.off()


#plot for depth 5m, 10m, 15m, 20m, 30m, 42m 
library(plotrix) #package to count error bar
reef.fish.Aus.se <- reef.fish.Aus  %>% group_by(Ecoregion) %>% summarise(std.er = std.error(Total)) #count error bar
reef.fish.Aus.lat <- reef.fish.Aus %>% group_by(Ecoregion) %>% summarise(meanlat = mean(SiteLat), meanlong = mean(SiteLong))
reef.fish.Aus.re <- reef.fish.Aus %>% group_by(Ecoregion) %>% summarise(mean = mean(Total)) %>% 
  left_join(., reef.fish.Aus.lat, by = 'Ecoregion') %>% arrange(., desc(meanlat)) #arrange from low latitude to high 
reef.fish.Aus.re$Ecoregion <- factor(reef.fish.Aus.re$Ecoregion, levels = unique(reef.fish.Aus.re$Ecoregion))

# Overall data-Latitudinal----
library(ggplot2)
library(RColorBrewer)
ggplot(reef.fish.Aus.re, aes(x = Ecoregion, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ecoregion))+
  geom_errorbar(aes(x = Ecoregion, 
                    ymin = mean-reef.fish.Aus.se$std.er , 
                    ymax = mean+reef.fish.Aus.se$std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Site (from low latitude to high latitude)', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)


#shallow(<=10) and deep(>=20) water----
reef.fish.Aus.dep <- reef.fish.Aus %>% filter(reef.fish.Aus$Depth <= 10 | reef.fish.Aus$Depth>= 20) %>% 
  mutate(water = ifelse(Depth <= 10,'shallow', 'deep'))
reef.fish.Aus.dep.se <-  reef.fish.Aus.dep %>% group_by(Ecoregion, water) %>% summarise(std.er = std.error(Total)) #count error bar
reef.fish.Aus.dep.lat <- reef.fish.Aus.dep %>% group_by(Ecoregion, water) %>% summarise(meanlat = mean(SiteLat), meanlong = mean(SiteLong))

reef.fish.Aus.dep.re <- reef.fish.Aus.dep %>% group_by(Ecoregion, water) %>% summarise(mean = mean(Total)) %>% 
  left_join(., reef.fish.Aus.dep.lat, by = c('Ecoregion','water')) %>% arrange(., desc(meanlat)) #arrange from low latitude to high 
reef.fish.Aus.dep.re$Ecoregion <- factor(reef.fish.Aus.dep.re$Ecoregion, levels = unique(reef.fish.Aus.dep.re$Ecoregion))
reef.fish.Aus.dep.re$water <- factor(reef.fish.Aus.dep.re$water, levels = unique(reef.fish.Aus.dep.re$water))

#plot
ggplot(reef.fish.Aus.dep.re, aes(x = Ecoregion, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ecoregion))+
  geom_errorbar(aes(x = Ecoregion, 
                    ymin = mean-reef.fish.Aus.dep.se$std.er , 
                    ymax = mean+reef.fish.Aus.dep.se$std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Site (from low latitude to high latitude)', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)+
  facet_grid(rows = vars(water))




#Separate by date(month)----
reef.fish.Aus.dat <- reef.fish.Aus %>% mutate(Month = sapply(strsplit(reef.fish.Aus$SurveyDate,'-'), '[', 2)) #split the character of month

reef.fish.Aus.dat.se <-  reef.fish.Aus.dat %>% group_by(Ecoregion, Month) %>% summarise(std.er = std.error(Total)) #count error bar
reef.fish.Aus.dat.lat <- reef.fish.Aus.dat %>% group_by(Ecoregion, Month) %>% summarise(meanlat = mean(SiteLat), meanlong = mean(SiteLong))

reef.fish.Aus.dat.re <- reef.fish.Aus.dat %>% group_by(Ecoregion, Month) %>% summarise(mean = mean(Total)) %>% 
  left_join(., reef.fish.Aus.dat.lat, by = c('Ecoregion', 'Month')) %>% arrange(., desc(meanlat)) #arrange from low latitude to high 
reef.fish.Aus.dat.re$Ecoregion <- factor(reef.fish.Aus.dat.re$Ecoregion, levels = unique(reef.fish.Aus.dat.re$Ecoregion))
reef.fish.Aus.dat.re <- reef.fish.Aus.dat.re %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% arrange(Month)



#plot
#x=Month
ggplot(reef.fish.Aus.dat.re, aes(x = Month, y = mean))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = Month, 
                    ymin = mean-reef.fish.Aus.dat.se$std.er , 
                    ymax = mean+reef.fish.Aus.dat.se$std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Month', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)+
  facet_grid(rows = vars(Ecoregion))

#x = Ecoregion
ggplot(reef.fish.Aus.dat.re, aes(x = Ecoregion, y = mean))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = Ecoregion, 
                    ymin = mean-reef.fish.Aus.dat.se$std.er , 
                    ymax = mean+reef.fish.Aus.dat.se$std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Month', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)+
  facet_grid(rows = vars(Month))

#combine all ecoregion--
reef.fish.Aus.dat <- reef.fish.Aus %>% mutate(Month = sapply(strsplit(reef.fish.Aus$SurveyDate,'-'), '[', 2)) #split the character of month
reef.fish.Aus.dat.se.onM <-  reef.fish.Aus.dat %>% group_by(Month) %>% summarise(std.er = std.error(Total)) #count error bar
reef.fish.Aus.dat.onM <- reef.fish.Aus.dat %>% group_by(Month) %>% summarise(mean = mean(Total))
reef.fish.Aus.dat.onM <- reef.fish.Aus.dat.onM %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% arrange(Month)

ggplot(reef.fish.Aus.dat.onM, aes(x = Month, y = mean))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = Month, 
                    ymin = mean-reef.fish.Aus.dat.se.onM$std.er , 
                    ymax = mean+reef.fish.Aus.dat.se.onM$std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Month', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)



#West and East----
#Latitude
reef.fish.Aus.re.W_E <- reef.fish.Aus.re %>% data.frame(Side = rep('East',18))
reef.fish.Aus.re.W_E$Side[c(1,3,7,8,9,11,13)] <- rep('West',7)
reef.fish.Aus.re.W_E <- reef.fish.Aus.re.W_E %>% group_by(Side) %>% arrange(., desc(meanlat), .by_group = T)
reef.fish.Aus.re.W_E <- full_join(reef.fish.Aus.re.W_E, reef.fish.Aus.se, by = 'Ecoregion')
reef.fish.Aus.re.W_E$Ecoregion <- factor(reef.fish.Aus.re.W_E$Ecoregion, levels = unique(reef.fish.Aus.re.W_E$Ecoregion))

reef.fish.Aus.re.E <- reef.fish.Aus.re.W_E %>% filter(Side == 'East') 

#plot for east and west 
ggplot(reef.fish.Aus.re.W_E, aes(x = Ecoregion, y = mean))+
  geom_bar(stat = "identity", aes(fill = Side))+
  geom_errorbar(aes(x = Ecoregion, 
                    ymin = mean-std.er , 
                    ymax = mean+std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Site (from low latitude to high latitude)', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)


reef.fish.Aus.re.E <- reef.fish.Aus.re.W_E %>% filter(Side == 'East') 
#plot only for east
ggplot(reef.fish.Aus.re.E, aes(x = Ecoregion, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ecoregion))+
  geom_errorbar(aes(x = Ecoregion, 
                    ymin = mean-std.er , 
                    ymax = mean+std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Site (from low latitude to high latitude)', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)


reef.fish.Aus.re.W <- reef.fish.Aus.re.W_E %>% filter(Side == 'West') 
#plot only for west
ggplot(reef.fish.Aus.re.W, aes(x = Ecoregion, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ecoregion))+
  geom_errorbar(aes(x = Ecoregion, 
                    ymin = mean-std.er , 
                    ymax = mean+std.er), 
                width=.1)+
  theme_light()+
  theme(axis.text.x = element_text(angle=70, hjust=1, vjust=1))+
  labs(x = 'Site (from low latitude to high latitude)', y = 'Average abundance')+
  scale_fill_viridis(discrete = T, option = 'plasma',direction = -1)


#Data analysis----
###Latitudinal variation----

#Shapiro

#sample size more than 5000
qqnorm(reef.fish.Aus$Total)
qqline(reef.fish.Aus$Total)
#=> Non-normal

#kruskal.test
#One-way non-parametric analysis
kruskal.test(Total ~ Ecoregion, data = reef.fish.Aus)
#Kruskal-Wallis chi-squared = 602.08, df = 17, p-value < 2.2e-16 => Significantly different

#post-hoc tests
pairwise.wilcox.test(reef.fish.Aus$Total, reef.fish.Aus$Ecoregion,
                     p.adjust.method = "BH") #=> They are not the same, see barplot


###Depth variation (Shallow and deep)----
##Shallow
reef.fish.Aus.dep.sha <- reef.fish.Aus.dep %>% subset(Depth <= 10)
#reef.fish.Aus.dep.re.sha <- reef.fish.Aus.dep.re %>% filter(water == 'shallow')
#Shapiro
#sample size more than 5000
qqnorm(reef.fish.Aus.dep.sha$Total)
qqline(reef.fish.Aus.dep.sha$Total)
#=> Non-normal

#kruskal.test
#One-way non-parametric analysis
kruskal.test(Total ~ Ecoregion, data = reef.fish.Aus.dep.sha)
#Kruskal-Wallis chi-squared = 554.98, df = 17, p-value < 2.2e-16 => Significantly different

#post-hoc tests
pairwise.wilcox.test(reef.fish.Aus.dep.sha$Total, reef.fish.Aus.dep.sha$Ecoregion,
                     p.adjust.method = "BH") #=> They are not the same, see barplot


##Deep
reef.fish.Aus.dep.deep <- reef.fish.Aus.dep %>% subset(Depth >= 20)

#Shapiro
shapiro.test(reef.fish.Aus.dep.deep$Total) 
#W = 0.14522, p-value < 2.2e-16 => Non-normal

qqnorm(reef.fish.Aus.dep.deep$Total)
qqline(reef.fish.Aus.dep.deep$Total)
#=> Non-normal

#kruskal.test
#One-way non-parametric analysis
kruskal.test(Total ~ Ecoregion, data = reef.fish.Aus.dep.deep)
#Kruskal-Wallis chi-squared = 8.6516, df = 5, p-value = 0.1238 => not Significantly different

##Subset those locations contain both shallow and deep samples
bothS_D <- c('Houtman','Lord Howe and Norfolk Islands','Manning-Hawkesbury','Leeuwin','Cape Howe','Bassian')
reef.fish.Aus.dep.bothS_D <- reef.fish.Aus.dep %>% filter(Ecoregion %in% bothS_D)
reef.fish.Aus.dep.bothS_D <- reef.fish.Aus.dep.bothS_D %>% mutate(Eco_water = paste(Ecoregion, water)) %>% arrange(Ecoregion)
reef.fish.Aus.dep.bothS_D$Eco_water <- factor(reef.fish.Aus.dep.bothS_D$Eco_water, 
                                              levels = unique(reef.fish.Aus.dep.bothS_D$Eco_water))
#Shapiro test
#sample size more than 5000
qqnorm(reef.fish.Aus.dep.bothS_D$Total)
qqline(reef.fish.Aus.dep.bothS_D$Total)
#=> Non-normal

#kruskal.test
#One-way non-parametric analysis
kruskal.test(Total ~ Eco_water, data = reef.fish.Aus.dep.bothS_D)
#Kruskal-Wallis chi-squared = 198.81, df = 11, p-value < 2.2e-16 => Significantly different

#post-hoc tests
pairwise.wilcox.test(reef.fish.Aus.dep.bothS_D$Total, reef.fish.Aus.dep.bothS_D$Eco_water,
                     p.adjust.method = "BH") #=> They are not the same, see barplot



###Month variation----

#Shapiro
#sample size more than 5000
qqnorm(reef.fish.Aus.dat$Total)
qqline(reef.fish.Aus.dat$Total)
#=> Non-normal

#kruskal.test
#One-way non-parametric analysis
kruskal.test(Total ~ Month, data = reef.fish.Aus.dat)
#Kruskal-Wallis chi-squared = 346.34, df = 11, p-value < 2.2e-16 => Significantly different


#post-hoc tests
reef.fish.Aus.dat.arranMon <- reef.fish.Aus.dat %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% arrange(Month)

pairwise.wilcox.test(reef.fish.Aus.dat.arranMon$Total, reef.fish.Aus.dat.arranMon$Month,
                     p.adjust.method = "BH") #=> They are not the same, see barplot









west <- c('Cocos-Keeling/Christimas Island',
          'Bonaparte Coast',
          'Exmouth to Broome',
          'Ningaloo',
          'Shark Bay',
          'Houtman',
          'Leeuwin')

east <- c('Arnhem Coast to Gulf of Carpenteria',
          'Torres Strait Northern Great Barrier Reef', 
          'Coral Sea',
          'Central and Southern Great Barrier Reef',
          'Tweed-Moreton',
          'Lord Howe and Norfolk Islands',
          'Manning-Hawkesbury',
          'Cape Howe',
          'Bassian',
          'Western Bassian',
          'South Australian Gulfs')

reef.fish.Aus.w_e <- reef.fish.Aus %>% mutate(Side = rep(1,length(reef.fish.Aus$Ecoregion)))
for (i in length(reef.fish.Aus$Ecoregion)){
  for (j in length(west)){
    reef.fish.Aus.w_e$Side[i] <- ifelse(str_detect(reef.fish.Aus$Ecoregion[i], west[j]), 'West', 'East')
  }
}


reef.fish.Aus.w_e$Side[1] <- ifelse(str_detect(reef.fish.Aus$Ecoregion[1], west[5]) == TRUE,'West','East')
?isTRUE()
reef.fish.Aus.w_e <- reef.fish.Aus.re %>% 
  mutate(Side = ifelse(Ecoregion == 'Cocos-Keeling/Christimas Island'|
                         'Bonaparte Coast'|
                         'Exmouth to Broome'|
                         'Ningaloo'|
                         'Shark Bay'|
                         'Houtman'|
                         'Leeuwin','West', 'East'))
summary(reef.fish.Aus$Ecoregion == 'Cocos-Keeling/Christimas Island'|'Bonaparte Coast'|
          'Exmouth to Broome'|
          'Ningaloo'|
          'Shark Bay'|
          'Houtman'|
          'Leeuwin')
grep('Cocos-Keeling/Christimas Island |Bonaparte Coast|Exmouth to Broome|Ningaloo|Shark Bay|Houtman|Leeuwin',reef.fish.Aus$Ecoregion[1])
grep('Arnhem Coast to Gulf of Carpenteria|
     Torres Strait Northern Great Barrier Reef| 
     Coral Sea|
     Central and Southern Great Barrier Reef|
     Tweed-Moreton|
     Lord Howe and Norfolk Islands|
     Manning-Hawkesbury|
     Cape Howe|
     Bassian|
     Western Bassian|
     South Australian Gulfs',reef.fish.Aus$Ecoregion[1])


?grep
summary(reef.fish.Aus$Ecoregion == east)
reef.fish.Aus.re

apply(reef.fish.Aus$Ecoregion, 1, str_detect(.,west)))
tapply(reef.fish.Aus$Ecoregion, west, str_detect)

summary(str_detect(east, reef.fish.Aus$Ecoregion))
?match
apply(reef.fish.Aus$Ecoregion,1,str_detect(.,west))
apply(reef.fish.Aus$Ecoregion,1,%in% west)
?sapply
vapply(reef.fish.Aus$Ecoregion, %in% west)

str_detect(reef.fish.Aus$Ecoregion[1], east)
str_detect(east, reef.fish.Aus$Ecoregion[1])
reef.fish.Aus$Ecoregion[1] %in% east
east %in% reef.fish.Aus$Ecoregion[1]
west %in% reef.fish.Aus$Ecoregion
1:10 %in% c(1,3,5,9)
unique(reef.fish.Aus$Ecoregion) == west

#seperate by depth

reef.fish.Aus.dep <- reef.fish.Aus %>% filter(Depth == c(5, 10, 15, 20, 30, 42))
reef.fish.Aus.dep.se <-  reef.fish.Aus.dep %>% group_by(Ecoregion, Depth) %>% summarise(std.er = std.error(Total)) #count error bar
reef.fish.Aus.dep.lat <- reef.fish.Aus %>% group_by(Ecoregion) %>% summarise(meanlat = mean(SiteLat), meanlong = mean(SiteLong))
reef.fish.Aus.re <- reef.fish.Aus %>% group_by(Ecoregion) %>% summarise(mean = mean(Total)) %>% 
  left_join(., reef.fish.Aus.lat, by = 'Ecoregion') %>% arrange(., desc(meanlat)) #arrange from low latitude to high 
reef.fish.Aus.re$Ecoregion <- factor(reef.fish.Aus.re$Ecoregion, levels = unique(reef.fish.Aus.re$Ecoregion))

  facet_grid(rows = vars(Depth))+

    reef.fish.Aus.se <- reef.fish.Aus %>% group_by(Ecoregion) %>% summarise(std.er = std.error(Total)) #count error bar
  
  reef.fish.Aus.se <- reef.fish.Aus %>% filter(Depth == c(5, 10, 15, 20, 30, 42)) %>% group_by(Ecoregion) %>% summarise(std.er = std.error(Total)) #count error bar
  


library(ggplot2)
library(sf)
library(rnaturalearth)
world <- ne_countries(scale = 'medium', returnclass = 'sf')
points <- reef.fish.Aus.re[c('Ecoregion','meanlong','meanlat')]

ggplot(data = world) +
  coord_sf(xlim = c(100, 170), ylim = c(-50, 0), expand = FALSE) +
  geom_sf(data = points, fill = NA) + 
  geom_text(data = points, aes(meanlong, meanlat, label = Ecoregion), size = 5, fontface = "bold")


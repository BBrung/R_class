#2022.10.18_R class_week6
#Mapping

#install.packages('maptools')
library(maptools)

#library(marmap) #data base from NOAA

#Sys.setlocale("LC_TIME", "English")
#Sys.setlocale(category = "LC_ALL", "Chinese (Traditional)_Taiwan.950") #把電腦系統改成讀懂中文字串(平常不能讀再用)

#用多邊形畫的世界地圖
data(wrld_simpl) 
plot(wrld_simpl)

#Zoom in Taiwan and add color with ploygen Map
plot(wrld_simpl,xlim=c(121,122),ylim=c(21,26),col='olivedrab3',bg='lightblue')

str(wrld_simpl)  ##內容看出很多polygen做的


#Read and save in gpx

#readOGR #Read special data, for each point you saved and to track it as line or points
#install.packages('rgdal')
library(rgdal)
par(mfrow=c(1,2))
run1 <- readOGR(dsn="Data/run.gpx",layer="tracks")
plot(run1, main='Line') # my running activity
run2 <- readOGR(dsn="Data/run.gpx",layer="track_points")
plot(run2, main='Points')
dev.off()

writeOGR(wrld_simpl, dsn="Data", layer = "world_test", driver = "ESRI Shapefile", overwrite_layer = TRUE) 
#write a OGR data by ourselves

world_shp <- readOGR(dsn = "Data",layer = "world_test")

#vector-based objects == add sth individaully


plot(wrld_simpl,xlim=c(115,128) ,ylim=c(19.5,27.5),col='#D2B48C',bg='lightblue') # TW map
coords <- matrix(c(121.537290,120.265541, 25.021335, 22.626524),ncol=2) # NTU and SYS univ. 
coords <- coordinates(coords) # assign values as spatial coordinates把數值變成經緯度
spoints <- SpatialPoints(coords) # create SpatialPoints
df <- data.frame(location=c("NTU","SYS")) # create a dataframe
spointsdf <- SpatialPointsDataFrame(spoints,df) # create a SpatialPointsDataFrame 上面兩個點位變成點
plot(spointsdf,add=T,col=c('black','black'),pch=19,cex=2.2) # plot it on our map **add = T
text(121,24, 'TAIWAN', cex=1)  #這裡的數值不是經緯座標，所以如果地圖範圍改變，這個字就會不在台灣上



plot(wrld_simpl,xlim=c(-130,-60),ylim=c(45,80),col='#D2B48C',bg='lightblue')
coords <- matrix(c(-110,-102,-102,-110,-110,60,60,49,49,60),ncol=2) #畫一圈所以需要五個點
l <- Line(coords)
ls <- Lines(list(l),ID="1")
sls <- SpatialLines(list(ls))
df <- data.frame(province="Saskatchewan")
sldf <- SpatialLinesDataFrame(sls,df)
plot(sldf,add=T,col='#3d2402', cex=2)
text(-114, 55, 'Saskatchewan', srt=90, cex=0.5)
text(-114, 63, 'CANADA', cex=1)

#Line換成polygon也可以
plot(wrld_simpl,xlim=c(-130,-60),ylim=c(45,80),col='#D2B48C',bg='lightblue')
coords <- matrix(c(-110,-102,-102,-110,-110,60,60,49,49,60),ncol=2)
p <- Polygon(coords)
ps <- Polygons(list(p),ID="1")
sps <- SpatialPolygons(list(ps))
df <- data.frame(province="Saskatchewan")
spdf <- SpatialPolygonsDataFrame(sps,df)
plot(spdf,add=T,col='#45220d') 
text(-114, 55, 'Saskatchewan', srt=90, cex=0.7)
text(-114, 63, 'CANADA', cex=1)
text(-103, 46, 'UNITED STATES', cex=1)
text(-40, 78, 'GREENLAND', cex=1)
text(-35, 55, 'Atlantic Ocean', cex=1, col='#071238')
#text自己決定要不要加，地圖顯示只顯示文章中需要的重要的點的名字，其他不重要的就不用加


#install.packages('raster') #Higher resolution of Map
library(raster)
#get data from GADM, country is showed as abbr. eg. Taiwan -> TWN
#網路速影響下載速度，大國家和有很多小島嶼的會畫很久
TWN1 <- getData('GADM', country="TWN", level=0) # data Taiwan
JPN <- getData('GADM', country="JPN", level=0) # data Japan
class(TWN1) # those datasets are SpatialPolygonsDataFrame
par(mfrow = c(1, 2))
plot(TWN1,axes=T,bg=colors()[431],col='grey')
plot(JPN,axes=T,bg=colors()[431],col='grey')

dev.off()

#zoom on a point a map
plot (TWN1, axes=T, xlim=c(121,122), ylim=c(24,25.5), bg=colors()[431],col='grey') 

#county scale map
TWN2 <- getData('GADM', country="TWN", level=1) #level = 0 -> level = 1
TWN2$NAME_1 #level = 0 country scale, level = 1 province scale
#EXP1
plot(TWN1,col="grey",xlim=c(119,122.5), ylim=c(21.5,25.5), bg=colors()[431], axes=T)
KAO <- TWN2[TWN2$NAME_1=="Kaohsiung",]
plot(KAO,col="grey 33",add=TRUE)
#EXP2
# base map
plot(TWN1,col="grey",xlim=c(121,122), ylim=c(24,25.5), bg=colors()[431], axes=T)
# adding  spatial polygones 
TAI <- TWN2[TWN2$NAME_1=="Taipei" | TWN2$NAME_1=="New Taipei" ,]
plot(TAI,col="black",add=TRUE)


# adding spatial points 
coords <- matrix(cbind(lon=c(121.2,121.55,121.8),lat=c(25,25.19,24.5)),ncol=2)
coords <- coordinates(coords)
spoints <- SpatialPoints(coords)
df <- data.frame(location=c("City 1","City 2","City 3"),pop=c(138644,390095,34562)) #pop = populations
spointsdf <- SpatialPointsDataFrame(spoints,df)
scalefactor <- sqrt(spointsdf$pop)/sqrt(max(spointsdf$pop))
plot(spointsdf,add=TRUE,col='white',pch=1,cex=scalefactor*3,lwd=2) 
# adding a location of NTU (not spatial point here)
points(121.537290,25.021335, type="p", pch=18, col='white', cex=1.5)
# adding text
text(121.53,24.921335,"NTU", col='white', font=2)
# adding scale
#install.packages('maps')
library(maps)
maps::map.scale(x=120, y=25.4)
# adding north arrow
#install.packages('GISTools') 
library(GISTools)
GISTools::north.arrow(xb=120.3,yb=24.7, len=0.06, lab='N')
##Why I cannot download GISTools?---- # search: ''gistools github install'' on website

#GEMP file?
url <- 'https://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=72874C55-884D-4CEA-B7D6-F60B0BE85AB0'
path1 <- tempfile(fileext = ".zip")
if (file.exists(path1))  'file alredy exists' else download.file(url, path1, mode="wb")
#install.packages('zip')
zip::unzip(zipfile = path1,exdir = 'Data')
#find shp format of file
##Why I cannot use ::? ---- I should install first, and then I don't need to library



##ggplot to draw a MAP
library(ggplot2)
#install.packages('sf') #has implemented the “simple feature”
library(sf)
#install.packages('rnaturalearth') #provides a map of countries of the entire world, only polygon
library(rnaturalearth)

theme_set(theme_bw()) 
#install.packages('rnaturalearthdata') #data for like population
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf()

ggplot(data = world) +
  geom_sf() +
  coord_sf(expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  coord_sf(expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

#unique 所有元素中只找非重複的東西 像factor功能

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen") +
  coord_sf(expand = FALSE) 

#viridis: color gradient
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) + #population estimate
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") # sqrt transform

#coord_sf()決定投影法，從網路上可以找寫法
ggplot(data = world) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")
# OR
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))


#畫台灣摟!!
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(118, 128), ylim = c(17, 27), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma") # linear scale
###????
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(118, 128), ylim = c(17, 27), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.5, "cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) 

sf::sf_use_s2(FALSE) # FOR ERROR turn off the s2 processing
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))


##ggsave
#save as PDF: you can move your element individually
#save as jpg: combine all together can not move individually

#
#install.packages('rgbif')
library(rgbif)
#GBIF可以下載一些生物資訊
gbif.res <- occ_search(scientificName = "Urocissa caerulea", limit = 1200)

map_ggplot(gbif.res) +
  coord_sf(xlim = c(120, 123), ylim = c(21, 26), expand = FALSE)



#colorRampPalette: make color gradient palette by ourselves
#'plasma' is a palette made by others

# query
#install.packages('marmap')
library(marmap)
TW.bathy <- getNOAA.bathy(lon1=118,lon2=124, lat1=21,lat2=26,resolution=1) # don't put too wide / resolution: 1 
# define palette
blues <- colorRampPalette(c("darkblue", "cyan"))
greys <- colorRampPalette(c(grey(0.4),grey(0.99)))
# make the plot
plot.bathy(TW.bathy,
           image=T,
           deepest.isobath=c(-6000,-120,-30,0),  
           shallowest.isobath=c(-1000,-60,0,0),
           step=c(2000,60,30,0),  #-6000~-1000切2000分，-120~-60切60分....
           lwd=c(0.3,1,1,2),
           lty=c(1,1,1,1),
           col=c("grey","black","black","black"), 
           drawlabels=c(T,T,T,F),
           bpal = list(c(0,max(TW.bathy),greys(100)),c(min(TW.bathy),0,blues(100))),
           land=T, xaxs="i"
)

tw.profile <-get.transect(TW.bathy,x1=119.5,y1=23.75, x2=122,y2=23.75, dis=TRUE)
#x1, y1 & x2, y2 are two point on the map.接著畫剖面圖
plotProfile(tw.profile) 

#### Not Run: extract a profile Manually
#### manual.profile<-get.transect (TW.bathy, loc=T,dist=T) 可以直接在圖上拉兩點
#### plotProfile(manual.profile) 印出剖面圖


#Interactive maps #有超級清楚的地圖!!!!
FRE <- paste(sep = "<br/>",
             "<b><a href='https://www.dipintothereef.com/'>FRELAb TAIWAN</a></b>",
             "Functional Reef Ecology Lab",
             "Institute of Oceanography, NTU") #create some information about Taiwan
#br/:mean go to next line


#install.packages('leaflet')
library(leaflet)
#taiwan 是之前在政府開放資料抓的舊資料
leaflet(taiwan) %>%
  addPolygons(weight=0.5) %>%  #weight: make the border of Taiwan more sharper
  addTiles(group="Kort") %>%
  addPopups(121.53725, 25.021252, FRE, options = popupOptions(closeButton = FALSE)) #location of blockers and 

#addTiles(group="Kort") 用 addProviderTiles(...)取代可以改變風格(...網路上面可以查)


?tempfile
?download.file

##Practice5.1----
url <- 'https://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=72874C55-884D-4CEA-B7D6-F60B0BE85AB0'
path1 <- tempfile(fileext = ".zip") #fileext: can be used as names for temporary files
if (file.exists(path1))  'file already exists' else download.file(url, path1, mode="wb") #wb for windows

#install.packages('zip')
zip::unzip(zipfile = path1, exdir = 'Data') #exdir: Directory to uncompress the archive to

#find shp format of file
library(rgdal)
taiwan <- readOGR('Data/COUNTY_MOI_1090820.shp', use_iconv=TRUE, encoding='UTF-8')

#Create and customize an interactive map

#What kind of MAP do you want to make?
#coral distribution & diving spot
library(leaflet)
library(rgbif)
library(tidyverse)
library(readxl)
#Read data from Florida
data <- read_xlsx('Data/diving_site.xlsx')

#Make my own icon: diver icon
diver.icon <- makeIcon(
  iconUrl = "https://cdn2.iconfinder.com/data/icons/sport-ii/100/24-512.png",
  iconWidth = 18, iconHeight = 20)

#Make different color for wreck diving or coral diving
icons <- iconList(darkblue = makeIcon('https://www.freeiconspng.com/thumbs/shipping-icon/-nautical-raw-ship-shipping-simple-yacht-icon--icon-search-engine-3.png',
                                      iconWidth = 10, iconHeight =6),
                  orange = makeIcon('https://cdn-icons-png.flaticon.com/512/202/202206.png', 
                                    iconWidth = 10, iconHeight =10))

data$color <- ifelse(data$Type == 'Wreck', "darkblue", "orange")

#Draw Map
leaflet() %>%
  addTiles() %>% setView(30.664237, 24.258327, zoom = 1) %>%
  addMarkers(lng=120.664237, lat = 24.258327, popup= "My sweet home", icon =  diver.icon) %>%
  addMarkers(~Lon, ~Lat, data = data, icon = icons[data$color])

?addTiles()
 ?markerOptions
?addMarkers
#coral icon: https://cdn-icons-png.flaticon.com/512/202/202206.png
#wreck icon: https://toppng.com/uploads/preview/home-home-sea-container-ship-icon-11563195830cq8dtw2n3l.png



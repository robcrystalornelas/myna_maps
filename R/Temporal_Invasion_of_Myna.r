##Myna Maps##
library(dismo)
library(rJava)
library(jsonlite)
library(sp)
library(fields)
library(ggplot2)
library(maps)
library(maptools)
library(rgdal)
library(tmap)
library(plyr)
library(dplyr)

setwd("~/Desktop/Myna Project/myna_maps/Data")

#download occurrences####
#myna<-gbif('Acridotheres', 'tristis', geo=T, removeZeros = T)
#save(myna, file='myna.rdata')
load('myna.rdata')
head(myna)
dim(myna)
names(myna)
myna.unique<- distinct(select(myna,lat,lon,country,species,year)) #unique myna points
myna2<-myna.unique[complete.cases(myna.unique),] #make sure there's no NAs
dim(myna2)

#tidy occurrence points
unique(myna2$country) 
class(myna2$country)
myna2<-filter(myna2, country=="United States") #just mynas in USA
myna3<-filter(myna2, lon>-88 & year>1980) #just mynas in FL (gets rid of HI), and from 1980 and higher
dim(myna3) #have approx 1,000 reports of mynas in FL
list(myna3$year) #show list of all years included in dataset
myna4<-fortify(myna3) #step to fortify data.frame
head(myna4)
unique(myna4$year) #all the years included in this d.f

#bin invasion years to help with visualization of front
myna4$year_bin<-cut(myna4$year,c(1985,1990,1995,2000,2005,2010,2015)) #bin reports in 5 year increments
head(myna4)
class(myna4$year_bin) #year column is a factor
myna.reports.bar<-ggplot(myna4, aes(x=year_bin, fill=year_bin)) + geom_bar() + 
  ggtitle("Frequency of Common Myna Reports in Florida") +
  xlab("Years") +
  ylab("reports") +
  scale_x_discrete(labels=c("1985-1990", "1990-1995","1995-2000","2000-2005","2005-2010","2010-2015")) +
  scale_fill_brewer(palette = "Blues") +
  guides(fill=FALSE)
myna.reports.bar

#prepare county maps####
county_map<-map_data("county")
florida_map<-map_data("county", region="florida")
class(florida_map)
names(florida_map)[6] <- "county" #rename subregion to county
head(florida_map)

#simple dot plots of florida occurrences####
p <- ggplot(florida_map, aes(x=long, y=lat,group=group)) + #create map
  geom_polygon(fill="white", colour="black")
p

#Dotplot of Myna Occurrences
ggplot() + geom_polygon(data=florida_map, aes(x=long, y=lat,group=group), fill="grey40", colour="grey90", alpha=.8) +
  labs(x="longitude", y="latitude", title ="Myna Colonization in Florida by Year") + #make labels for axes
  theme(axis.ticks.y= element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1)) + #this makes title bold, and adds some space
        geom_point(data=myna4, aes(x=lon, y=lat, colour=year_bin), alpha=.8, size=3) + #colour must be inside aes()
        scale_colour_manual(values=c("#fef0d9","#fdd49e","#fdbb84","#fc8d59","#e34a33","#b30000")) + #change color scale
        coord_equal(ratio=1) #this squares the plot

#prep coords and map
coords<-cbind(myna4$lon,myna4$lat)
head(coords)
points <- SpatialPoints(coords)
head(points)

#import shapefile
counties <- readShapePoly("cntbnd_jul11")
counties@data$id = rownames(counties@data)
counties.points = fortify(counties, region="id")
counties.df = join(counties.points, counties@data, by="id")
head(counties.df)
names(myna_reports_ag)[1] <- "NAME"
counties.df = left_join(counties.df, myna_reports_ag, by = 'NAME')
head(counties.df)

#works great for florida map!#
#florida has no myna reports just yet
counties.df$individ[is.na(counties.df$individ)] <- 0
ggplot(counties.df) + 
  aes(long,lat,group=group, fill=individ) + ##just changing grouping factor really messes with our plot
  geom_polygon() +
  coord_equal()

#Function assigning lat/long to counties####
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

head(coords)
#coords.new<-latlong2county(coords) #function assigns all lat/lon combos to their specific county in USA

#Add new county list to myna4####
myna_county<-mutate(myna4, county = latlong2county(coords))
myna_by_county <- group_by(myna_county, county)
myna_by_county

#Join county map and myna points####
counties.df #this is our data.frame of florida map
myna_by_county #this is set of occurrence data with county included
myna_by_county<-na.omit(myna_by_county)
unique(myna_by_county$county)

#Re-format county names w/ true county names####
myna_by_county$county<-gsub('florida,miami-dade','MIAMI-DADE',myna_by_county$county)
myna_by_county$county<-gsub('florida,broward','BROWARD',myna_by_county$county)
myna_by_county$county<-gsub('florida,monroe','MONROE',myna_by_county$county)
myna_by_county$county<-gsub('florida,collier','COLLIER',myna_by_county$county)
myna_by_county$county<-gsub('florida,palm beach','PALMBEACH',myna_by_county$county)
myna_by_county$county<-gsub('florida,lee','LEE',myna_by_county$county)
myna_by_county$county<-gsub('florida,santa rosa','SANTAROSA',myna_by_county$county)
myna_by_county$county<-gsub('florida,hendry','HENDRY',myna_by_county$county)
myna_by_county$county<-gsub('florida,hillsborough','HILLSBOROUGH',myna_by_county$county)
myna_by_county$county<-gsub('florida,charlotte','CHARLOTTE',myna_by_county$county)
myna_by_county$county<-gsub('florida,pasco','PASCO',myna_by_county$county)
myna_by_county$county<-gsub('florida,brevard','BREVARD',myna_by_county$county)
myna_by_county$county<-gsub('florida,indian river','INDIANRIVER',myna_by_county$county)
unique(myna_by_county$county)
myna_by_county #data.frame with correct county names

#Worked examples from tmap vignette####
dim(myna_by_county)
individ<-rep(1, 1144) #to get 1 bird for every report, makes summarizing easier
myna_by_county_indiv<-cbind(myna_by_county,individ) #add new column to show 1 individual bird for every report
head(myna_by_county_indiv)
myna_by_county_indiv$county<-as.factor(myna_by_county_indiv$county) #change character vector to factor
myna_by_county_indiv$year<-as.factor(myna_by_county_indiv$year) #also change year to factor
myna_by_county_indiv

#Function to sum by YEAR and by COUNTY
myna5<-ddply(myna_by_county_indiv, c("year","county"), summarise, reports=sum(individ)) ##Year and County
myna6<-ddply(myna_by_county_indiv, c("county"), summarise, reports=sum(individ)) #just combine by county, bunches years together
myna6 #reports per county from 1985-2015

########?#####?#
counties_f<- fortify(counties)
head(counties_f) #but lost information here
counties$id <- row.names(counties) # allocate an id variable to the sp data 
head(counties@data, n = 2) # final check before join (requires shared variable name!)
counties_f <- left_join(counties_f, counties@data) #now join our fortified data.frame with land@data
head(counties_f)

map <- ggplot(counties_f, aes(long, lat, group = NAME, fill = reports)) + geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "% Sports\nParticipation") + ggtitle("London Sports Participation")
map

#joining count data to county map
counties84 <- spTransform(counties, CRS("+init=epsg:4326"))
counties@data<-left_join(counties@data, myna6_new, by="NAME")
names(counties@data) #now reports is included! in our county data.frame
class(counties) #still an sp.data.frame!
counties@data #it's good, but so many NAs in the report column.
qtm(counties, "reports") #map of all reports from 1985-2015
qtm(shp = counties, fill = "reports", fill.palette = "-Blues") #change color palette

#map showing reports broken down by county
tm_shape(counties) +
  tm_fill("reports.y", thres.poly = 0) +
  tm_facets("NAME", free.coords=TRUE, drop.shapes=TRUE) +
  tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)

#Mapping using ggplot
head(counties@data)
counties.points <- fortify(counties, region = "id") #region needs to be a name that exists in counties
counties.df <- join(counties.points,counties@data, by = "id")
head(counties.df)
#whatever we assigned to region above, turns into 'id' column

#ggplot really distorts florida map#...maybe because long/lat look really weird?
counties.df$reports[is.na(counties.df$reports)] <- 0
head(counties.df)

ggplot(counties.df) + 
  aes(long,lat,group=NAME) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal()
  

map <- ggplot(counties.df, aes(long, lat, group = NAME, fill = reports)) + 
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)", fill = "% Myna\nReports") + 
  ggtitle("Common Myna Reports by County")
map


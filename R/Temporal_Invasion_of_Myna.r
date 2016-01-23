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
myna_unique<-myna.unique[complete.cases(myna.unique),] #dataset free of NAs
dim(myna_unique)
unique(myna_unique$country) #list of all countries
myna_usa<-filter(myna_unique, country=="United States") #just mynas in USA
myna_fl<-filter(myna_usa, lon>-88 & year>1980) #just mynas in FL
dim(myna_fl)
list(myna_fl$year) #all years in our dataset

#bin invasion years
myna_fl$year_bin<-cut(myna4$year,c(1985,1990,1995,2000,2005,2010,2015)) #bin reports in 5 year increments
head(myna_fl)
class(myna_fl$year_bin) #year column is a factor
myna.reports.bar<-ggplot(myna_fl, aes(x=year_bin, fill=year_bin)) + geom_bar() + 
  ggtitle("Frequency of Common Myna Reports in Florida \n1985-2015") +
  xlab("Years") +
  ylab("Reports") +
  scale_x_discrete(labels=c("1985-1990", "1990-1995","1995-2000","2000-2005","2005-2010","2010-2015")) +
  scale_fill_brewer(palette = "Reds") +
  guides(fill=FALSE)
myna.reports.bar

#import florida map####
#high quality map
#counties <- readShapePoly("cntbnd_jul11")
#counties@data$id = rownames(counties@data)
#counties.points = fortify(counties, region="id")
#counties.df = join(counties.points, counties@data, by="id")
#head(counties.df)
#names(myna_reports_ag)[1] <- "NAME"
#counties.df = left_join(counties.df, myna_reports_ag, by = 'NAME')
#head(counties.df)

#low quality county map
county_map<-map_data("county")
florida_map<-map_data("county", region="florida")
class(florida_map)
names(florida_map)[6] <- "NAME" #rename subregion to county
head(florida_map)
head(myna_fl)
number_of_reports<-dim(myna_fl)[1] #get the number of total myna reports
individ<-rep(1, number_of_reports) #to get 1 bird for every report, makes summarizing easier
myna_reports<-cbind(myna_fl,individ) #add new column to show 1 individual bird for every report
myna_reports$county<-as.factor(myna_reports$county) #change character vector to factor
myna_reports$year<-as.factor(myna_reports$year) #also change year to factor

#assign all myna reports to specific counties
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

#match county names with myna occurrences using function above
myna_coords_for_conversion<-as.matrix(myna_reports[,2:1]) #need to flip lat and lon for function to work here
myna_reports_with_county<-mutate(myna_reports, county = latlong2county(myna_coords_for_conversion))
head(myna_reports_with_county)

#replace county names so they align w/ county map
myna_reports_with_county$county<-gsub('florida,miami-dade','miami-dade',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,broward','broward',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,monroe','monroe',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,collier','collier',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,palm beach','palmbeach',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,lee','lee',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,santa rosa','santarosa',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,hendry','hendry',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,hillsborough','hillsborough',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,charlotte','charlotte',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,pasco','pasco',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,brevard','brevard',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,indian river','indianriver',myna_reports_with_county$county)
unique(myna_reports_with_county$county)
myna_reports_with_county #data.frame with correct county names

#aggregate all myna reports
myna_reports_aggregated<-aggregate(individ ~ county, FUN = sum, data = myna_reports_with_county) #aggregate myna counts by county
colnames(myna_reports_aggregated)[1]<-"NAME" #make sure column names are same for left_join
florida_map<-florida_map[,c(1,2,3,4,5,6)]#also change order of columns in fl_map too!

#join the florida map to the reports
head(florida_map)
head(myna_reports_aggregated)
joined_counties_report_and_map <- left_join(florida_map,myna_reports_aggregated, by= "NAME") ##HUGE REMINDER HERE left_join function must have inputs that are exactly the same. Not only column names, but county names as well
head(joined_counties_report_and_map)
sum(na.omit(joined_counties_report_and_map$individ))

#WORKS
ggplot(joined_counties_report_and_map) + 
  aes(long,lat,group=group, fill=individ) + ##just changing grouping factor really messes with our plot
  geom_polygon() +
  coord_equal()

#quick map of florida
p <- ggplot(florida_map, aes(x=long, y=lat,group=group)) + #create map
  geom_polygon(fill="white", colour="black")
p

#Dotplot of florida with Myna Occurrences
myna_dot_plot <- ggplot() + 
  geom_polygon(data=florida_map, aes(x=long, y=lat,group=group), fill="grey40", colour="grey90", alpha=.8) +
  labs(x="longitude", y="latitude", title ="Myna Colonization in Florida by Year") + #make labels for axes
  theme(axis.ticks.y= element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1)) + #this makes title bold, and adds some space
  geom_point(data=myna_fl, aes(x=lon, y=lat, colour=year_bin), alpha=.8, size=3) + #colour must be inside aes()
  scale_colour_manual(values=c("#fef0d9","#fdd49e","#fdbb84","#fc8d59","#e34a33","#b30000")) + #change color scale
  coord_equal(ratio=1) #this squares the plot
myna_dot_plot
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
library(tidyr)
library(RColorBrewer)
library(ggmap)

setwd("~/Desktop/Myna_Project/myna_maps/Data")

#download occurrences####
#myna<-gbif('Acridotheres', 'tristis', geo=T, removeZeros = T)
setwd("~/Desktop/Myna_Project/myna_SDM/Data")
myna_full_gbif<-read.csv("myna_full_gbif.csv")
setwd("~/Desktop/Myna_Project/myna_maps/Data")

head(myna_full_gbif)
dim(myna_full_gbif)
names(myna_full_gbif)
myna.unique<- distinct(select(myna_full_gbif,lat,lon,country,species,year)) #unique myna points
myna_unique<-myna.unique[complete.cases(myna.unique),] #dataset free of NAs
dim(myna_unique)
unique(myna_unique$country) #list of all countries
myna_usa<-filter(myna_unique, country=="United States") #just mynas in USA
myna_fl<-filter(myna_usa, lon>-88 & year>1980) #just mynas in FL
dim(myna_fl)
list(myna_fl$year) #all years in our dataset

#bin invasion years
myna_fl$year_bin<-cut(myna_fl$year,c(1985,1990,1995,2000,2005,2010,2016)) #bin reports in 5 year increments
unique(myna_fl$year_bin) #with this division... 1990-1995 bin doesn't include 1990...just includes 1991-1995
head(myna_fl)
class(myna_fl$year_bin) #year column is a factor
write.csv(myna_fl, file = "myna_fl.csv")

# Barplot for myna reports in 5 year increments
myna.reports.bar <- ggplot(myna_fl, aes(x=year_bin, fill=year_bin)) + geom_bar() + 
  ggtitle("Frequency of Common Myna Reports in Florida \n1986-2016") +
  xlab("Years") +
  ylab("Reports") +
  scale_x_discrete(labels=c("1985-1990", "1990-1995","1995-2000","2000-2005","2005-2010","2010-2015")) +
  scale_fill_brewer(palette = "Reds") +
  theme(plot.title = element_text(size = rel(2)),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank()) +
  guides(fill=FALSE)
myna.reports.bar

# Download florida map ####
#### TRYING WITH NEW SHAPEFILE! ###
# Import shapefile for florida
florida_shapefile <- readOGR(dsn = ".", layer = "cntbnd_jul11")
florida_shapefile_84<-spTransform(florida_shapefile, CRS("+init=epsg:4326"))
names(florida_shapefile_84)
florida_df <- fortify(florida_shapefile_84, region = "NAME")
head(florida_df)
unique(florida_df$id)

number_of_reports<-dim(myna_fl)[1] #get the number of total myna reports
individ<-rep(1, number_of_reports) #to get 1 bird for every report, makes summarizing easier
myna_reports<-cbind(myna_fl,individ) #add new column to show 1 individual bird for every report
myna_reports$year<-as.factor(myna_reports$year) #also change year to factor
head(myna_reports)

# Use function to assign all myna reports to specific counties
myna_coords_for_conversion<-as.matrix(myna_reports[,2:1]) #need to flip lat and lon for function to work here
myna_reports_with_county <- mutate(myna_reports, county = latlong2county(myna_coords_for_conversion))
head(myna_reports_with_county)
myna_reports_with_county <- myna_reports_with_county[complete.cases(myna_reports_with_county),]
head(myna_reports_with_county)
unique(myna_reports_with_county$county)

# #replace county names so they align w/ county map
myna_reports_with_county$county<-gsub('florida,miami-dade','MIAMI-DADE',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,broward','BROWARD',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,monroe','MONROE',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,collier','COLLIER',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,palm beach','PALMBEACH',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,lee','LEE',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,santa rosa','SANTAROSA',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,hendry','HENDRY',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,hillsborough','HILLSBOROUGH',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,charlotte','CHARLOTTE',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,pasco','PASCO',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,brevard','BREVARD',myna_reports_with_county$county)
myna_reports_with_county$county<-gsub('florida,indian river','INDIANRIVER',myna_reports_with_county$county)
unique(myna_reports_with_county$county)
head(myna_reports_with_county) #data.frame with correct county names
names(myna_reports_with_county)[names(myna_reports_with_county)=="county"] <- "id"
head(myna_reports_with_county)

#aggregate all myna reports
myna_reports_aggregated<-aggregate(individ ~ id, FUN = sum, data = myna_reports_with_county) #aggregate myna counts by county

#join the florida map to the reports
# head(florida_df_with_county)
# myna_reports_aggregated
# joined_counties_report_and_map <- left_join(florida_df_with_county, myna_reports_aggregated, by= "id") ##HUGE REMINDER HERE left_join function must have inputs that are exactly the same. Not only column names, but county names as well
# head(joined_counties_report_and_map)

# Choropleth of all florida myna reports
myna_report_choropleth <- ggplot(joined_counties_report_and_map) + 
  aes(long, lat, group = group, fill = individ) +
  geom_polygon() + 
  scale_fill_gradient(low="orange", high="red") + # this has individuals log transformed
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Frequency of Myna Reports in Florida 1986-2015") +
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_text(colour = "Black", size = 13, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20))
myna_report_choropleth

### dotplot of reports
myna_dot_plot_all_years <- ggplot() + 
  geom_polygon(data=florida_df, aes(x=long, y=lat,group=group), fill="grey40", colour="grey90", alpha=.8) +
  labs(x="longitude", y="latitude", title ="Myna Colonization in Florida by Year") + #make labels for axes
  theme(axis.ticks.y= element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(size = 20),
        legend.title=element_blank(),
        axis.title = element_text(size = 15)) +
  geom_point(data=myna_reports_with_county, aes(x=lon, y=lat, colour=year_bin), alpha=.8, size=3) + #colour must be inside aes()
  scale_colour_manual(values=c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d"),
                      labels=c("1985-1990", "1991-1995", "1996-2000", "2001-2005","2006-2010","2011-2016")) #change color scale
myna_dot_plot_all_years

################
#set up florida maps. One for each year
fl_1986 <- rep("1986",length(florida_map$long))
florida_map_1986 <- cbind(fl_1986, florida_map)
names(florida_map_1986)[1]<-"year"

fl_1987 <- rep("1987",length(florida_map$long))
florida_map_1987 <- cbind(fl_1987, florida_map)
names(florida_map_1987)[1]<-"year"

fl_1988 <- rep("1988",length(florida_map$long))
florida_map_1988 <- cbind(fl_1988, florida_map)
names(florida_map_1988)[1]<-"year"

fl_1989 <- rep("1989",length(florida_map$long))
florida_map_1989 <- cbind(fl_1989, florida_map)
names(florida_map_1989)[1]<-"year"

fl_1990 <- rep("1990",length(florida_map$long))
florida_map_1990 <- cbind(fl_1990, florida_map)
names(florida_map_1990)[1]<-"year"

fl_1991 <- rep("1991",length(florida_map$long))
florida_map_1991 <- cbind(fl_1991, florida_map)
names(florida_map_1991)[1]<-"year"

fl_1992 <- rep("1992",length(florida_map$long))
florida_map_1992 <- cbind(fl_1992, florida_map)
names(florida_map_1992)[1]<-"year"

fl_1993 <- rep("1993",length(florida_map$long))
florida_map_1993 <- cbind(fl_1993, florida_map)
names(florida_map_1993)[1]<-"year"

fl_1994 <- rep("1994",length(florida_map$long))
florida_map_1994 <- cbind(fl_1994, florida_map)
names(florida_map_1994)[1]<-"year"

fl_1995 <- rep("1995",length(florida_map$long))
florida_map_1995 <- cbind(fl_1995, florida_map)
names(florida_map_1995)[1]<-"year"

fl_1996 <- rep("1996",length(florida_map$long))
florida_map_1996 <- cbind(fl_1996, florida_map)
names(florida_map_1996)[1]<-"year"

fl_1997 <- rep("1997",length(florida_map$long))
florida_map_1997 <- cbind(fl_1997, florida_map)
names(florida_map_1997)[1]<-"year"

fl_1998 <- rep("1998",length(florida_map$long))
florida_map_1998 <- cbind(fl_1998, florida_map)
names(florida_map_1998)[1]<-"year"

fl_1999 <- rep("1999",length(florida_map$long))
florida_map_1999 <- cbind(fl_1999, florida_map)
names(florida_map_1999)[1]<-"year"

fl_2000 <- rep("2000",length(florida_map$long))
florida_map_2000 <- cbind(fl_2000, florida_map)
names(florida_map_2000)[1]<-"year"

fl_2001 <- rep("2001",length(florida_map$long))
florida_map_2001 <- cbind(fl_2001, florida_map)
names(florida_map_2001)[1]<-"year"

fl_2002 <- rep("2002",length(florida_map$long))
florida_map_2002 <- cbind(fl_2002, florida_map)
names(florida_map_2002)[1]<-"year"

fl_2003 <- rep("2003",length(florida_map$long))
florida_map_2003 <- cbind(fl_2003, florida_map)
names(florida_map_2003)[1]<-"year"

fl_2004 <- rep("2004",length(florida_map$long))
florida_map_2004 <- cbind(fl_2004, florida_map)
names(florida_map_2004)[1]<-"year"

fl_2005 <- rep("2005",length(florida_map$long))
florida_map_2005 <- cbind(fl_2005, florida_map)
names(florida_map_2005)[1]<-"year"

fl_2006 <- rep("2006",length(florida_map$long))
florida_map_2006 <- cbind(fl_2006, florida_map)
names(florida_map_2006)[1]<-"year"

fl_2007 <- rep("2007",length(florida_map$long))
florida_map_2007 <- cbind(fl_2007, florida_map)
names(florida_map_2007)[1]<-"year"

fl_2008 <- rep("2008",length(florida_map$long))
florida_map_2008 <- cbind(fl_2008, florida_map)
names(florida_map_2008)[1]<-"year"

fl_2009 <- rep("2009",length(florida_map$long))
florida_map_2009 <- cbind(fl_2009, florida_map)
names(florida_map_2009)[1]<-"year"

fl_2010 <- rep("2010",length(florida_map$long))
florida_map_2010 <- cbind(fl_2010, florida_map)
names(florida_map_2010)[1]<-"year"

fl_2011 <- rep("2011",length(florida_map$long))
florida_map_2011 <- cbind(fl_2011, florida_map)
names(florida_map_2011)[1]<-"year"

fl_2012 <- rep("2012",length(florida_map$long))
florida_map_2012 <- cbind(fl_2012, florida_map)
names(florida_map_2012)[1]<-"year"

fl_2013 <- rep("2013",length(florida_map$long))
florida_map_2013 <- cbind(fl_2013, florida_map)
names(florida_map_2013)[1]<-"year"

fl_2014 <- rep("2014",length(florida_map$long))
florida_map_2014 <- cbind(fl_2014, florida_map)
names(florida_map_2014)[1]<-"year"

fl_2015 <- rep("2015",length(florida_map$long))
florida_map_2015 <- cbind(fl_2015, florida_map)
names(florida_map_2015)[1]<-"year"
head(florida_map_2015)

all_florida_years_with_map<-rbind(florida_map_1986,florida_map_1987,florida_map_1988,florida_map_1989,florida_map_1990,florida_map_1991,florida_map_1992,florida_map_1993,florida_map_1994,florida_map_1995,florida_map_1996,florida_map_1997,florida_map_1998,florida_map_1999,florida_map_2000,florida_map_2001,florida_map_2002, florida_map_2003,florida_map_2004,florida_map_2005,florida_map_2006,florida_map_2007,florida_map_2008,florida_map_2009,florida_map_2010,florida_map_2011,florida_map_2012,florida_map_2013,florida_map_2014,florida_map_2015)
head(all_florida_years_with_map)

####
#Prep maps for binned years
####
head(florida_map)
fl_1985_1990 <- rep("(1985,1990]",length(florida_map$long))
florida_map_1985_1990 <- cbind(fl_1985_1990, florida_map)
names(florida_map_1985_1990)[1]<-"year_binned"

fl_1990_1995 <- rep("(1990,1995]",length(florida_map$long))
florida_map_1990_1995 <- cbind(fl_1990_1995, florida_map)
names(florida_map_1990_1995)[1]<-"year_binned"

fl_1995_2000 <- rep("(1995,2000]",length(florida_map$long))
florida_map_1995_2000 <- cbind(fl_1995_2000, florida_map)
names(florida_map_1995_2000)[1]<-"year_binned"

fl_2000_2005 <- rep("(2000,2005]",length(florida_map$long))
florida_map_2000_2005 <- cbind(fl_2000_2005, florida_map)
names(florida_map_2000_2005)[1]<-"year_binned"

fl_2005_2010 <- rep("(2005,2010]",length(florida_map$long))
florida_map_2005_2010 <- cbind(fl_2005_2010, florida_map)
names(florida_map_2005_2010)[1]<-"year_binned"

fl_2010_2015 <- rep("(2010,2015]",length(florida_map$long))
florida_map_2010_2015 <- cbind(fl_2010_2015, florida_map)
names(florida_map_2010_2015)[1]<-"year_binned"

all_florida_years_binned_with_map<-rbind(florida_map_1985_1990,florida_map_1990_1995,florida_map_1995_2000,florida_map_2000_2005,florida_map_2005_2010,florida_map_2010_2015)
head(all_florida_years_binned_with_map)

####
#THIS WORKS!!!
#####
myna_reports_with_NAME<-rename(myna_reports_with_county, NAME = county) 
lndf_new<-myna_reports_with_NAME
lndf_new<-lndf_new[,c(2,1,3,4,5,6,7,8)] #re-order myna reports data.frame
lndf_new<-na.omit(lndf_new)
head(lndf_new) #data.frame with lon/lat as well as reports
lndf_new_agg<-aggregate(lndf_new$individ, by = list(year = lndf_new$year, NAME = lndf_new$NAME), FUN=sum) #aggregate reports by year and county
head(all_florida_years_with_map)
head(lndf_new_agg)

#this way we can join our data.frames
lnd_f_new <- left_join(all_florida_years_with_map, lndf_new_agg) #left join so that we have data.frame with florida maps for each year, and aggregated reports
head(lnd_f_new)

#FACET! now, we can get 1 map per year
ggplot(data = lnd_f_new, # the input data
       aes(x = long, y = lat, fill = x, group = group)) + # define variables 
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ year) + # one plot per time slice
  scale_fill_gradient2(low = "gray88", mid = "darkorange2", high = "firebrick3",name = "Reports") +
  theme_nothing(legend = TRUE) + 
  labs(x = "Longitude", y = "Latitude",
       fill = "Reports") + ggtitle("Frequency of Myna Reports in Florida 1986-2015")

#   theme(axis.text = element_blank(), # change the theme options
#         axis.title = element_blank(), # remove axis titles
#         axis.ticks = element_blank(), # remove axis ticks
#         panel.background = element_rect(fill = "white"),
#         legend.title = element_text(colour = "Black", size = 13, face = "bold")) #sets background of plotting area

#####
#Binned by 5 year increments
######
lndf_for_binned_maps<-myna_reports_with_NAME
head(lndf_for_binned_maps)
lndf_for_binned_maps<-lndf_for_binned_maps[,c(2,1,3,4,5,6,7,8)] #re-order myna reports data.frame


#aggregate into 5 year bins
lndf_binned_agg<-aggregate(lndf_for_binned_maps$individ, by = list(year_binned = lndf_for_binned_maps$year_bin, NAME = lndf_for_binned_maps$NAME), FUN=sum) #aggregate reports by year and county
head(lndf_binned_agg)

#join data.frames
head(all_florida_years_binned_with_map)
lndf_binned_agg$year_binned<-as.factor(lndf_binned_agg$year_binned) #making sure 2 d.f I'm combining have exact same column names/class
head(lndf_binned_agg)
head(all_florida_years_binned_with_map)
class(lndf_binned_agg$year_binned)
class(all_florida_years_binned_with_map$year_binned)

lndf_new_binned <- left_join(all_florida_years_binned_with_map, lndf_binned_agg) #left join so that we have data.frame with florida maps for each year, and aggregated reports

######
#FACET 1 map for every 5 years in Florida
######
ggplot(data = lndf_new_binned, # the input data
       aes(x = long, y = lat, fill = x, group = group)) + # define variables 
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ year_binned) + # one plot per time slice
  #facet_grid( ~ year_binned, labeller=labeli2) + #this line re-names facets, but only
  scale_fill_gradient2(low = "gray88", mid = "darkorange2", high = "firebrick3",name = "Reports") +
  theme_nothing(legend = TRUE) + 
  labs(x = "Longitude", y = "Latitude", fill = "Reports") + 
  ggtitle("Frequency of Myna Reports in Florida 1986-2015")

#####

# Renaming facets

#####

#renaming function
labeli2 <- function(variable, value){
  value <- droplevels(value)
  names_li <- list('(1985,1990]'="1986-1990",'(1990,1995]'="1991-1995",'(1995,2000]'="1996-2000",'(2000,2005]'="2001-2005",'(2005,2010]'="2006-2010",'(2010-20105]'="2011-2015")
  return(names_li[value])
}

#####

#Function assigning lat/long to counties####

#####

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


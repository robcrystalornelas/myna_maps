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
nbrks<-6
ggplot(joined_counties_report_and_map) + 
  aes(long,lat,group=group, fill=individ) + ##just changing grouping factor really messes with our plot
  geom_polygon() + #fill="white", colour = "black", lwd = .01
  #scale_fill_continuous(low = "white", high = "black") +
  scale_fill_gradientn(colours = brewer.pal(nbrks,"YlOrRd"), breaks = c(brks)) + #set coloration of counties
  coord_equal() +
  labs(x = "Longitude", y = "Latitude",
       fill = "Reports") + ggtitle("Frequency of Myna Reports in Florida 1986-2015") +
  theme(panel.background = element_rect(fill = "white"),
        legend.title = element_text(colour = "Black", size = 13, face = "bold")) #sets background of plotting area

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

################
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

all_florida_years_with_map<-rbind(florida_map_1986,florida_map_1987,florida_map_1988,florida_map_1989,florida_map_1990,florida_map_1991,florida_map_1992,florida_map_1993,florida_map_1994,florida_map_1995,florida_map_1996,florida_map_1997,florida_map_1998,florida_map_1999,florida_map_2000,florida_map_2001,florida_map_2002, florida_map_2003,florida_map_2004,florida_map_2005,florida_map_2006,florida_map_2007,florida_map_2008,florida_map_2009,florida_map_2010,florida_map_2011,florida_map_2012,florida_map_2013,florida_map_2014,florida_map_2015)
head(all_florida_years_with_map)

##########
#Now to getting data together
#########
# ltidy2<- select(myna_reports_with_county, year, individ,county)
# ltidy2<-rename(ltidy2, NAME = county)
# head(ltidy2)
# dim(ltidy2)
# 
# ltidy2_aggregate<-aggregate(ltidy2$individ, by = list(year = ltidy2$year, county = ltidy2$NAME), FUN=sum)
# head(ltidy2_aggregate)
# ltidy2_aggregate <- ltidy2_aggregate[order(ltidy2_aggregate$year),]
# head(ltidy2_aggregate)
# 
# #now, merge our count data with florida geometry data
# myna_for_facet <- full_join(all_florida_years_with_map, ltidy2_aggregate)
# head(myna_for_facet)
# myna_for_facet$year<-as.character(myna_for_facet$year)
# myna_for_facet$year2<-as.character(myna_for_facet$year2)
# head(myna_for_facet)
# 
# #FACET! now, we can get 1 map per year
# head(myna_for_facet)
# unique(myna_for_facet$year)
# 
# ggplot(data = myna_for_facet, # the input data
#        aes(x = long, y = lat, fill = x, group = group)) + # define variables 
#   geom_polygon() +
#   geom_path(colour="black", lwd=0.01) +
#   coord_equal() + # fixed x and y scales
#   facet_wrap(~ year) + # one plot per time slice...but "year" shows no difference in population counts
#   #year2 shows change over time, but doesn't show rest of state!
#   #scale_fill_gradient2(low = "gray88", mid = "darkorange2", high = "firebrick3",name = "Reports") + # legend options 
#   theme(axis.text = element_blank(), # change the theme options
#         axis.title = element_blank(), # remove axis titles
#         axis.ticks = element_blank()) # remove axis ticks

####
#THIS WORKS!!!
#####

lndf_new<-myna_reports_with_NAME
lndf_new<-lndf_new[,c(2,1,3,4,5,6,7,8)] #re-order myna reports data.frame
lndf_new<-na.omit(lndf_new)
head(lndf_new) #data.frame with lon/lat as well as reports
lndf_new_agg<-aggregate(lndf_new$individ, by = list(year = lndf_new$year, NAME = lndf_new$NAME), FUN=sum) #aggregate reports by year and county
head(all_florida_years_with_map)

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


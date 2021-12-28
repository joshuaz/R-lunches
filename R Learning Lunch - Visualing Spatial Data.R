#########################################
#### Title: Visualizing Spatial Data ####
#### Author: Will and R Lunch Crew   ####
#### Date: March 8, 2018             ####
#########################################

#"But why would we ever do this in R when Tableau is so easy and beautiful?"
#A.) Reproducibility and Repeatability - templitizing Tableau is significantly more 
#    difficult, but copying R-script is easy! e.g. this entire script.
#B.) Don't discount GGPlot2 
#    https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/, 
#    also makes pretty maps. 
#C.) Tableau is a great BI tool for assessing and identifying high-level trends, 
#    but rarely do we use it for statistical analysis. R can do it all


##########################################
#Reference for this tutorial: https://github.com/Robinlovelace/Creating-maps-in-R

#ggmap: extends the plotting package ggplot2 for maps
#rgdal: R's interface to the popular C/C++ spatial data processing library gdal
#rgeos: R's interface to the powerful vector processing library geos
#maptools: provides various mapping functions
#dplyr and tidyr: fast and concise data manipulation packages
#tmap: a new packages for rapidly creating beautiful maps. tmap was created to overcome some of the limitations of base graphics and ggmap. With the tmap package, thematic maps can be generated with great flexibility.

####### Install Packages ########
install.packages("ggplot2")
install.packages("ggmap")
install.packages("rgdal")
install.packages("rgeos")
install.packages("maptools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tmap")
install.packages("leaflet")
install.packages("gmapsdistance")

####### Load Packages ###########

library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
library(leaflet)
library(gmapsdistance)

####### Intro ###########

#The shapefile format is a popular geospatial vector data format for geographic information 
#system (GIS) software. It is developed and regulated by Esri as a (mostly) open specification 
#for data interoperability among Esri and other GIS software products. 

#The shapefile format can spatially describe vector features: points, lines, and polygons, 
#representing, for example, water wells, rivers, and lakes. Each item usually has attributes 
#that describe it, such as name or temperature.

#Shapes (points/lines/polygons) together with data attributes can create infinitely many 
#representations about geographic data.

#The term "shapefile" is quite common, but is misleading since the format consists of a 
#collection of files with a common filename prefix, stored in the same directory. 
#The three mandatory files have filename extensions .shp, .shx, and .dbf. 
#The actual shapefile relates specifically to the .shp file, but alone is incomplete for 
#distribution as the other supporting files are required.

#Mandatory files include:
#.shp - shape format; the feature geometry itself
#.shx - shape index format; a positional index of the feature geometry to allow seeking 
#forwards and backwards quickly
#.dbf - attribute format; columnar attributes for each shape, in dBase IV format


####### Load Data ###########

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

lnd <- readOGR(dsn = "data/london_sport.shp")

# readOGR, from the rgdal package, automatically extracts the information regarding the data.
# dsn: "data source name"

#Spatial objects like the lnd object are made up of a number of different slots, 
#the key slots being @data (non geographic attribute data) and @polygons (or @lines for line data). 
#The data slot can be thought of as an attribute table and the geometry slot is the polygons that make up the physcial boundaries. 
#Specific slots are accessed using the @ symbol.

####### Explore Data ###########

head(lnd@data, n = 10)

mean(lnd$Partic_Per)

mean(lnd@data$Partic_Per)

#There are two important symbols at work in the above block of code: the @ symbol in the first line 
#of code is used to refer to the data slot of the lnd object. 
#The $ symbol refers to the Partic_Per column (a variable within the table) in the data slot, 
#which was identified from the result of running the first line of code.

#Mapping requires the use of defined data-types, so it may be required to assess and manipulate 
#R's interpretation of data classes. 

sapply(lnd@data, class)

#Pop_2001 is a factor, so coerce it to be a numeric
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001))


########## Plotting ###########

#Time for some basic plotting

plot(lnd)

#too much white space, let's change the margin of our canvas and plot again
par(mar=c(1,1,1,1)) 
plot(lnd) #much better

#Practice subsetting
#within square brackets, before comma refers to rows, after the comma refers to the columns
lnd@data[lnd$Partic_Per < 15, 1:3]

#More practice
sel <- lnd$Partic_Per > 25
plot(lnd[sel, ])

#To see subset in comparison to elsewhere
plot(lnd, col = "lightgrey")
plot(lnd[ sel, ], col = "turquoise", add = TRUE)


########## Overlaying ###########

# Next, we will select and plot zones that are close to the centre of London

rm(list=ls())
lnd <- readOGR(dsn = "data/london_sport.shp")

library(rgeos)
# find London's geographic centroid (add ", byid = T" for all)
cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
points(cent_lnd, cex = 3)
plot(cent_lnd)

# set 10 km buffer
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 
plot(lnd_buffer)

# subsetting selects any intersecting zones
plot(lnd, col = "grey")
lnd_central <- lnd[lnd_buffer,]

# test the selection for the previous method...
plot(lnd_central, col = "lightblue", add = T)
plot(lnd_buffer, add = T) # ...some areas just touch the buffer

# therefore instead, we are going to select and subset only points within the buffer
# create spatialpoints
plot(lnd, col = "grey")
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) 

sel <- lnd_cents[lnd_buffer,] # select points inside buffer
points(sel) # show where the points are located
lnd_central <- lnd[sel,] # select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)

# Add text to the plot!
text(coordinates(cent_lnd), "Central London")

########## Attribute joins (adding variables) ###########

#This is where/how we would add additional data for analysis. In this section, we will add london
#crime data to our current data. 

#If we had not conducted the above exercises, we would start by loading and plotting our spatial data:
library(rgdal) 
lnd <- readOGR("data/london_sport.shp")
plot(lnd)

#Load new data
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",  stringsAsFactors = FALSE)

#observe data
table(crime_data$CrimeType)

#Extract "Theft & Handling" crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]

#Calculate the sum of the crime count for each district, save result
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)

# Now that we have crime data at the borough level, the challenge is to join it to the lnd object. 
# We will base our join on the Borough variable from the crime_ag object and the name variable 
#from the lnd object. 

#It is not always straight-forward to join objects based on names as the names do not always match. 
#Let's see which names in the crime_ag object match the spatial data object, lnd.
#Compare the name column in lnd to Borough column in crime_ag to see which rows match. 
#We use %in% command to identify which values in lnd$name are also contained in the Borough names 
#of the aggregated crime data.

lnd$name %in% crime_ag$Borough

#See which values are not matched.

lnd$name[!lnd$name %in% crime_ag$Borough]

#To merge, use either merge or join. We use left_join because we want the length of the data frame 
#to remain unchanged, with variables from new data appended in new columns (see ?left_join). 
#The *join commands (including inner_join and anti_join) assume, by default, that matching 
#variables have the same name.

library(dplyr)
lnd@data <- left_join(lnd@data, crime_ag, by = c('name' = 'Borough'))

library(tmap)
qtm(lnd, "CrimeCount")
qtm(lnd, "Pop_2001")
qtm(lnd, "Partic_Per")

sapply(lnd@data, class)
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001))

qtm(lnd, "Pop_2001") #much better

########## Clipping and spatial joins ###########
# (Joins attributes from one feature to another based on the spatial relationship.)

library(rgdal)
# create new stations object using the "lnd-stns" shapefile.
stations <- readOGR(dsn = "data/lnd-stns.shp")
# this is the full geographical detail.
proj4string(stations)
# what's the coordinate reference system (CRS)
proj4string(lnd)
# the extent, 'bounding box' of stations
bbox(stations)
# return the bounding box of the lnd object
bbox(lnd) 

# Create reprojected stations object
stations <- spTransform(stations, CRSobj = CRS(proj4string(lnd)))

# plot London 
plot(lnd) 

# overlay the station points
points(stations) 

#To clip the stations so that only those falling within London boroughs are retained 
#we can use sp::over, or simply the square bracket notation for subsetting tabular data
stations <- stations[lnd, ] 
plot(stations)
plot(lnd)
points(stations,col = "red", pch = 19) #Click 'zoom' to see a larger graph

#After the spatial join, we can count how many stations are there in each region.
stationsperborough<-over(stations,lnd)
table<-table(stationsperborough$name)
table

#Save results as a data frame "num_stations"; name column variables
num_stations<-as.data.frame(table)
colnames(num_stations)<-c("Borough","Number of Stations")
head(num_stations)

#Use the same method we learned earlier from Attribute Joins
lnd@data <- left_join(lnd@data, num_stations, by = c('name' = 'Borough'))
qtm(lnd, "Number of Stations",fill.palette = "Blues",format="World")

#Let's say we want to highlight "Brent" and its stations
plot(stations)
plot(lnd)
plot(lnd[lnd$name == "Brent",], col = "blue", add=TRUE) #Hi Brent
points(stations,col = "red", pch = 19) #Click 'zoom' to see a larger graph

#To only display stations/point in Brent
brent <- lnd$name == "Brent"
brent <- lnd[brent,]
plot(stations[brent,])
plot(lnd)
points(stations[brent,],col = "red", pch = 19) #Click 'zoom' to see a larger graph

########## Mapping Universities ###########

Inst_Coords <- read.csv("data/Institution Coordinates.csv")
head(Inst_Coords)
library(ggmap)

qmplot(Longitude, Latitude, data = Inst_Coords,
       colour = I('red'), size = I(3), darken = .3)


########## Calculating Distanse ###########
install.packages("gmapsdistance")
library(gmapsdistance)

#Example: compute the driving distance between Washington DC, and New York City. 
#code returns the Time (seconds), the Distance (meters) and the Status of the query (OK if it was successful).

DC_NYC<- gmapsdistance(origin = "Washington+DC", 
              destination = "New+York+City+NY", 
              mode = "driving")
DC_NYC

DC_NYC_BOS <- gmapsdistance(origin = "Washington+DC", 
                       destination = c("New+York+City+NY","Boston"), 
                       mode = "driving")

results<-as.data.frame(DC_NYC_BOS$Time)
results<-gather(results,key = "Destination",value="Time")
results<-results[-1,]
results

########## Idea ###########

#This kind of analysis would be very useful for institutions targeting commuting students. 
#Could subset geographic areas with a specific 
#time travel radius.

Inst_Coords$Combined_Coords <- paste(Inst_Coords$Latitude,Inst_Coords$Longitude, sep = "+")
set.seed(123)
sample_cords <- Inst_Coords[sample(1:nrow(Inst_Coords), 300, replace = FALSE),]
head(sample_cords)

#for convenience, we are just calculate the distance between the first college in the sample
#and the rest of the colleges in the sample
results_time <- as.data.frame(gmapsdistance(origin = sample_cords$Combined_Coords[1], 
                                            destination = sample_cords$Combined_Coords[], 
                                            mode = "driving")$Time)

results<-gather(results_time,key = "Destination",value="Time")
results<-results[-1,]
sample_cords$time<-results[,2]

#from here we can select to map only the colleges that are within two hours from
#"Remington College-Fort Worth Campus"
sample_cords$time<-as.numeric(sample_cords$time)
nearby_colleges<-subset(sample_cords, time < 18000)

qmplot(Longitude, Latitude, data = nearby_colleges,zoom=8,
       colour = I('blue'), size = I(2.5), darken = .3)+
       geom_point(x=-97.21356, y=32.77598,color=I('red'), size = I(3))+
       annotate("text",x=-97.21356, y=32.77598,label="Remington",color=I('black'))
#API: AIzaSyCs_sejFuFK2HxmLvgIulZwRGIncnhyztU
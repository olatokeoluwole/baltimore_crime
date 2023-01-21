#required libraries
library (tidyverse)
library(hrbrthemes)
library(plotly)
library(patchwork)
library(viridis)
library(sf)
library(lubridate)
library(leaflet)
library(mapview)



#read in data 
#csv as stringsAsFactors
#shapefile as sf
crime=read.csv("C:/Users/Oluwole Olatoke/Desktop/CrimeData.csv")
shape= sf::st_read("C:/Users/Oluwole Olatoke/Desktop/New folder (5)/Maryland_Baltimore_City_Neighborhoods.shp")

#view of loaded data
crime=filter(crime, Latitude !=0)
crime=filter(crime, Longitude!=0)
head(crime)
str(crime)

plot(shape)
str(shape)

#wrangling
#crime=st_as_sf(crime, coords=c('Latitude','Longitude'),crs=4326) #converted crime data to sf

grouped_crime=crime %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime

grouped_crime=grouped_crime[-1,] # deleted blank row




shape=rename(shape,Neighborhood=NBRDESC) # I had to rename NBRDESC column on the shapefile to Neighborhood
                                         # so that I get a column to merge by
shape=st_transform(shape,4326) #bring to WGS 84



crime_nbhd=merge(grouped_crime,shape,"Neighborhood") # ready for chloropleth map


#ready to map data
crime # point maps of all crime in the data
crime_nbhd  # ready for chloropleth map of number of crime in each Neighborhood


#PLOTTINGS
#plot basic bar chat of number in each Neighborhood
ggplot(grouped_crime, aes(x=Neighborhood, y=n)) + 
  geom_bar(stat = "identity")


#plot crime points
ggplot(crime, aes(x=Longitude, y=Latitude)) + 
  geom_point()


ggplot() + 
  geom_sf(data = crime_nbhd, mapping = aes(fill = (n),geometry=geometry), show.legend = FALSE)

ggplot() + 
  geom_sf(data = shape, mapping = aes(fill = (ACRES),geometry=geometry), show.legend = FALSE)



st_intersection(shape,crime$GeoLocation)

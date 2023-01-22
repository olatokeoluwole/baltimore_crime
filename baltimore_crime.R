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
#library(d3heatmap)



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

#WRANGLING
## number of crime per Neighborhood
grouped_nc=crime %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_nc=grouped_crime[-1,] # deleted blank row

##find the frequency of each crime
grouped_crime=crime %>% 
  group_by(Description) %>% 
  summarise(n=n()) 

## find number of each type of crime per Neighborhood
###AGG. ASSAULT
aggass= filter(crime,Description=="AGG. ASSAULT")
grouped_aggass=aggass %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_aggass=rename(grouped_aggass, "AGG. ASSAULT"=n)
grouped_aggass=grouped_aggass[-1,] # deleted blank row


###ARSON
arson= filter(crime,Description=="ARSON")
grouped_arson=arson %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_arson=rename(grouped_arson, "ARSON"=n)
grouped_arson=grouped_arson[-1,] # deleted blank row


###AUTO THEFT
auteft= filter(crime,Description=="AUTO THEFT")
grouped_auteft=auteft %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_auteft=rename(grouped_auteft, "AUTO THEFT"=n)
grouped_auteft=grouped_auteft[-1,] # deleted blank row


###BURGLARY
burg= filter(crime,Description=="BURGLARY")
grouped_burg=burg %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_burg=rename(grouped_burg, "BURGLARY"=n)
grouped_burg=grouped_burg[-1,] # deleted blank row



###COMMON ASSAULT
comass= filter(crime,Description=="COMMON ASSAULT")
grouped_comass=comass %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_comass=rename(grouped_comass, "COMMON ASSAULT"=n)
grouped_comass=grouped_comass[-1,] # deleted blank row



###COMMON ASSAULT
comass= filter(crime,Description=="COMMON ASSAULT")
grouped_comass=comass %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_comass=rename(grouped_comass, "COMMON ASSAULT"=n)
grouped_comass=grouped_comass[-1,] # deleted blank row


###LARCENY
larcy= filter(crime,Description=="LARCENY")
grouped_larcy=larcy %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_larcy=rename(grouped_larcy, "LARCENY"=n)
grouped_larcy=grouped_larcy[-1,] # deleted blank row



###LARCENY FROM AUTO
larcy_fa= filter(crime,Description=="LARCENY FROM AUTO")
grouped_larcy_fa=larcy_fa %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_larcy_fa=rename(grouped_larcy_fa, "LARCENY FROM AUTO"=n)
grouped_larcy_fa=grouped_larcy_fa[-1,] # deleted blank row



###RAPE
rape= filter(crime,Description=="RAPE")
grouped_rape=rape %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_rape=rename(grouped_rape, "RAPE"=n)
grouped_rape=grouped_rape[-1,] # deleted blank row


###ROBBERY
robry= filter(crime,Description=="ROBBERY")
grouped_robry=robry %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_robry=rename(grouped_robry, "ROBBERY"=n)
grouped_robry=grouped_robry[-1,] # deleted blank row



###CARJACKING
carj= filter(crime,Description=="CARJACKING")
grouped_carj=carj %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_carj=rename(grouped_carj, "CARJACKING"=n)
grouped_carj=grouped_carj[-1,] # deleted blank row



###COMMERCIAL
comm= filter(crime,Description=="COMMERCIAL")
grouped_comm=comm %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_comm=rename(grouped_comm, "COMMERCIAL"=n)
grouped_comm=grouped_comm[-1,] # deleted blank row



SHOOTING
###SHOOTING
shotn= filter(crime,Description=="SHOOTING")
grouped_shotn=shotn %>% 
  group_by(Neighborhood) %>% 
  summarise(n=n()) # to have a neighborhood column and summarized number of crime
grouped_shotn=rename(grouped_shotn, "SHOOTING"=n)
grouped_shotn=grouped_shotn[-1,] # deleted blank row



## merger to get Table showing neighborhood and each type of crime

#step1 put all data frames into list
df_list <- list(grouped_aggass,grouped_arson,grouped_auteft,grouped_burg,
                grouped_comass,grouped_larcy,grouped_larcy_fa,grouped_rape,
                grouped_robry,grouped_shotn)      

#step2 merge all data frames together
crime_nbhd=reduce(df_list,full_join, by='Neighborhood')


w=merge(grouped_crime,shape,"Neighborhood")


shape=rename(shape,Neighborhood=NBRDESC) # I had to rename NBRDESC column on the shapefile to Neighborhood
                                         # so that I get a column to merge by


#shape=st_transform(shape,4326) #bring to WGS 84



crime_nbhd_shape=merge(crime_nbhd,shape,"Neighborhood") # ready for chloropleth map

st_as_sf(crime_nbhd_shape)#convert to sf object so that it can be plotted interactively


#READY TO PLOT DATA
crime # point maps of all crime in the data
crime_nbhd #tablular data for each crime in each neibourghood
crime_nbhd_shape #added shapefile to neibourghood per crime
grouped_crime # table showing crime and frequency

#PLOTTINGS
plot(crime_nbhd)# rough and dirty scatter plot



#plot basic bar chat of number in each Neighborhood
ggplot(crime_nbhd, aes(x=Neighborhood, y=ARSON)) + 
  geom_bar(stat = "identity")


#plot scatter point

df2=crime_nbhd %>% 
  plot_ly(x=~BURGLARY,y=~ARSON)
df2 # interactive scatter plot

#plot point location of crimes
crime_points=ggplot(crime, aes(x=Longitude, y=Latitude,geometry='geometry')) + 
  geom_point()
crime_points

#plot interactive choropleth map
fig <- ggplotly(
  ggplot(crime_nbhd_shape) +
    geom_sf(aes(fill = RAPE, geometry='geometry'))
) 

fig


###############
#points within shape


crime=st_as_sf(crime, coords = c("Longitude", "Latitude"), crs = 4326)
shape=st_transform(shape,4326)

str(crime)
st_crs(crime)
str(shape)
st_crs(shape)
w=st_intersection(crime,shape)

st_transform(shape,4326)
######




ggplot() + 
  geom_sf(data = shape, mapping = aes(fill = (ACRES),geometry=geometry), show.legend = FALSE)



st_intersection(shape,crime$GeoLocation)




library(tidyverse)
library(rgdal)
library(stringi)
library(leaflet)
library(ggmap)
library(mapview)
library(leafem)
library(raster)
library(fuzzyjoin)


############################################ Functions #####################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))  #I always like to create my own "opposite of %in%" for nice logicals.



# using the jaro-winkler string distance, must have an address vector called "Address" 
addJoin <- function(x,b3,threshold,method){
  joined <- 
    stringdist_left_join(x,
                         b3, 
                         by = "Address",
                         distance_col = "distance",
                         max_dist = threshold,
                         method = method
    )
}


#Need to convert all chars to lower case for easier matching. The column names and order MATTER. Edit the list in the function to add more terms to clean. 
add_clean<-function(char){
  lng<-c("us","-","avenue","street","highway","lane","drive","alley","boulevard","circle","court","county","north","west","east","south","road","second","first","third",".")
  shrt<-c("hwy"," ","ave","st","hwy","ln","dr","aly","blvd","cir","ct","co","n","w","e","s","rd","2nd","1st","3rd","")
  char %<>% stri_trans_tolower()    
  for (i in 1:length(lng)){
    char<-stri_replace_all_fixed(char,lng[i],shrt[i]) }
  char
}


bnds<-raster::shapefile("/Users/Pythagoras/Documents/GitHub/dps-stats/shp_bdry_mn_city_township_unorg/city_township_unorg.shp")
counties<-raster::shapefile("/Users/Pythagoras/Documents/GitHub/dps-stats/counties/County_Boundaries_in_Minnesota.shp")

leaflet(data = counties) %>% addTiles() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(fill = T, stroke = TRUE, color = "#03F")

leaflet(bnds) %>% addPolygons()

counties<- readOGR("/Users/Pythagoras/Documents/GitHub/dps-stats/counties/County_Boundaries_in_Minnesota.shp",
                  GDAL1_integer64_policy = TRUE)

#try with map view. Great this is working. 
m<-mapview(counties) 







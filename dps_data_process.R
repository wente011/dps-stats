library(rjson)
library(magrittr)
library(tidyverse)
library(stringi)
library(tidycensus)
library(tidycensus)
library(lubridate)



############################################ Functions #####################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))  #I always like to create my own "opposite of %in%" for nice logicals.

na.m<-function(df){
df[is.na(df)]<-0 
return(df)
}


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


######################################## Loading data now ###################################

endp<-"https://mn-state-patrol-scraper.herokuapp.com/api/incidents"
dps <-fromJSON(file=endp) %>% bind_rows() %>% as_tibble()
dps$date %<>% mdy_hm()

#Seperators: "COUNTY then take last character for each string

dps2<-dps %>% mutate(county=stri_split_fixed(location,",",simplify=T)[,3])

dps2$county[is.na(dps2$county)]<-stri_split_fixed(dps$location,",",simplify=T)[is.na(dps2$county),1]

dps2<-dps2 %>% mutate(city=stri_split_fixed(location,",",simplify=T)[,2]) %>% 
                mutate(county= 
                         stri_split_fixed(county,"County",simplify=T)[,1] %>% trimws()
                       ) %>% mutate(idx =stri_detect_fixed(county, "Twp")) %>%
                mutate(veh.year=stri_split_fixed(vehicle," ",simplify = T)[,1], 
                       make=stri_split_fixed(vehicle," ",simplify = T)[,2],
                       model=paste0(stri_split_fixed(vehicle," ",simplify = T)[,3]," ",stri_split_fixed(vehicle," ",simplify = T)[,4]))



repl<-stri_split_fixed(dps2$location,",",simplify=T)[stri_detect_fixed(dps2$county, "Twp"),4]
repl<-stri_split_fixed(repl,"County",simplify = T)[,1] %>% trimws()
dps2$county[stri_detect_fixed(dps2$county, "Twp")]<-repl
dps2$county[dps2$county==""]<-NA                        #65 completely missing values. 

dps2 %<>% mutate(city=stri_split_fixed(trimws(city),"Twp",simplify = T)[,1]) %>% mutate_at("alcoholInvolved",as.factor)

dps2$city[dps2$city==""]<-"Unknown Municipality"

################################## Pull in the Census Data ####################################
#### For a basic DPS County Map

ckey="47b95b7affd600590d66898d665397c137bf310e"  #TOKEN (CENSUS HERE)
census_api_key(ckey, install = TRUE)

v18 <- load_variables(2018, "acs5", cache = TRUE)

cvars<-c(median_income="B19013_001",pop="B00001_001")
ckey="47b95b7affd600590d66898d665397c137bf310e"  #TOKEN (CENSUS HERE)
census_api_key(ckey, install = TRUE)
cvars<-c(median_income="B19013_001",pop="B00001_001")
mn<-get_acs(geography = "county", variables=cvars)

mn %<>% mutate(County=stri_split_fixed(NAME,",",simplify = T)[,1]) %>% 
  mutate(County=trimws(stri_split_fixed(County,"County",simplify = T)[,1])) %>% 
  mutate(State=trimws(stri_split_fixed(NAME,",",simplify = T)[,2])) %>% filter(State=="Minnesota") %>% 
  spread(variable,estimate) %>% mutate(idx=seq(1,length(GEOID))) %>% group_by(County) %>% 
  summarize_at(c("moe","median_income","pop"),mean,na.rm=T)


############################### Back to the DPS data to clean missing counties ###########################
mn %<>% rename(county="County") 

dps2$county %<>% as.factor() %>%  fct_recode(McLeod="Mcleod",`Otter Tail`="Ottertail",`Lac qui Parle`="Lac Qui Parle",
                                             Morrison="Morrision")

#To deal with the duplicated city entries, you have to split and get the 4th column split string
dps2.1<-left_join(dps2,mn)
errs<-stri_split_fixed(dps2.1$location[is.na(dps2.1$median_income)],",",simplify=T)[,4] %>% stri_split_fixed(.,"County",simplify = T) %>% trimws()

dps2.1$county %<>% as.character()
dps2.1<-left_join(dps2,mn)   #rejoin this for good measure. 
dps2.1$county[is.na(dps2.1$median_income)]<-errs[,1]


dps2<-dps2.1 %>% mutate(Count=1,Month=month(date),Year=year(date),Week=week(date),Date=as.Date(date)) %>% mutate_if(is.character,trimws) %>%
        mutate_at("county",as.factor)
    

dps3.1<- dps2 %>% 
  group_by(alcoholInvolved,county) %>% summarize(Count=sum(Count)) %>% spread(alcoholInvolved,Count) %>% dplyr::select(county,Yes,No,Unknown) %>%
  mutate_all(na.m)

dpsm<- dps2 %>% group_by(alcoholInvolved,county,make,model)


dps3.2<-dps2 %>% mutate(Count=1,Month=month(date),Year=year(date),Week=week(date),Date=as.Date(date)) %>% 
  group_by(injury,county) %>% summarize(Count=sum(Count)) %>% spread(injury,Count) %>% mutate_all(na.m)

dps3<-left_join(dps3.1,dps3.2) %>% rename(AlcYes="Yes",AlcNo="No",AlcNA="Unknown")



################################## Census data ################


counties$pop<-mn$pop[match(mn$County,counties$COUNTY_NAM)]
counties$income<-mn$median_income[match(mn$County,counties$COUNTY_NAM)]
names(counties)[1]<-"county"

counties$injury

m2<-sp::merge(counties, dps3, by="county",duplicateGeoms = TRUE)


m3<-mapview(m2)

m3 %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                           opacity = 1.0, fillOpacity = 0.5,
                           fillColor = ~colorQuantile("YlOrRd", Count)(Count))

leaflet() %>% addTiles() %>% addPolygons(m2) + mapview(m2)



addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5,
            fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                bringToFront = TRUE))




  
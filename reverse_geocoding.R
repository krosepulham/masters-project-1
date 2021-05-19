library(googleway)
library(readtext)
library(tictoc)
library(dplyr)
library(stringr)
library(beepr)
library(STRbook)
library(ggplot2)
library(mgcv)
library(tidyr)
library(tmaptools)

#poking around with the API and explaining the data-----------------------------

#read in my super secret google API key that's also linked to my debit card -_-
set_key(readtext("../google_API_key.txt")$text)

#figure out how to get latitude and longitude data for one place, using 
#Corvallis as a guinea pig, also time this process. I'm using a wrapper from the
#"googleway" package. 
tic()
gbg <- google_places(search_string = "Corvallis, Oregon")
toc()#When I run this it takes about 0.57 seconds
cat("Latitude is", gbg$results$geometry$location$lat,
    "\nLongitude is",gbg$results$geometry$location$lng)

#let's see if using google_geocode() is faster
tic()
gbg1 <- google_geocode("Corvallis, Oregon")
toc() #this one's 0.39 seconds, which is better. 


#Ok, now that we know how to get latitude and longitude values from a text
#string, we need to isolate the part of our data set we're going to pull these 
#strings from
load("hate_crime.Rda")
head(hate_crime[c(3,4,5,6,7,8)])

#the column ORI has a unique string of characters for each public agency. 
#PUB_AGENCY_NAME has the name of the public agency. PUB_AGENCY_UNIT has 
#additional information about the organization, and sometimes includes things 
#like "Multnomah County", "Downtown Campus" or "District 3" in it that will be
#useful in geocoding. AGENCY_TYPE_NAME tells us the type of agency that's 
#reporting the crime, which can be city, county, other state agency, university 
#or college, state police, other, tribal, and federal. each of these types is 
#going to need to be put together into geocode queries a little differently. The
#state name and abbreviation is included as well, though this can also be 
#discerned from the first two digits of the ORI. 

#We'll begin by getting all this pertinent information into one data frame, and 
#eliminate the duplicate rows

incomplete_grand <- hate_crime%>%
  select(ORI,PUB_AGENCY_NAME,PUB_AGENCY_UNIT,AGENCY_TYPE_NAME,STATE_NAME)%>%
  distinct()%>%
  as_tibble()

#remove hate_crime from the workspace cause she thicc
remove(hate_crime)

#TO DO LIST
#figure out rate limitation 
#do it with lists of google place objects instead of lat/lng columns
#look up k-means, 

#geocoding for cities-----------------------------------------------------------
incomplete_city <- filter(incomplete_grand, AGENCY_TYPE_NAME=="City")
places <- list()

tic()
for (i in 1:length(incomplete_city$ORI)){
  search_str = str_glue(incomplete_city[i,2],", ",incomplete_city[i,5])
  place <- google_geocode(search_str)
  places[[i]] <- place
}
toc()
beep(4)#make some noise when it's done please

#save the data before something tragic happens to it. 
complete_city <- incomplete_city%>%
  mutate(place_object = places)
save(complete_city, file="complete_city.Rda")

#geocoding for counties---------------------------------------------------------
incomplete_county <- filter(incomplete_grand, AGENCY_TYPE_NAME=="County")
places <- list()

for (i in 1:length(incomplete_county$ORI)){
  search_str = str_glue(incomplete_county[i,2]," County, ",incomplete_county[i,5])
  place <- google_geocode(search_str)
  places[[i]] <- place
}
beep(4)

complete_county <- incomplete_county%>%
  mutate(place_object = places)
save(complete_county,file = "complete_county.Rda")

#geocoding for universities and colleges----------------------------------------
incomplete_univ <- filter(incomplete_grand, AGENCY_TYPE_NAME=="University or College")
places <- list()

for (i in 1:length(incomplete_univ$ORI)){
  search_str = str_glue(incomplete_univ[i,2]," ",
                        incomplete_univ[i,3],", ",
                        incomplete_univ[i,5])
  place <- google_geocode(search_str)
  places[[i]] <- place
}
beep(1)

complete_univ <- incomplete_univ %>%
  mutate(place_object = places)
save(complete_univ,file="complete_univ.Rda")

#geocoding for state police agencies--------------------------------------------
incomplete_statepo <- filter(incomplete_grand, AGENCY_TYPE_NAME=="State Police")
places <- list()

for (i in 1:length(incomplete_statepo$ORI)){
  search_str = str_glue(incomplete_statepo[i,2]," ",
                        incomplete_statepo[i,3],", ",
                        incomplete_statepo[i,5])
  place <- google_geocode(search_str)
  places[[i]] <- place
}
beep(1)

complete_statepo <- incomplete_statepo%>%
  mutate(place_object = places)
save(complete_statepo, file="complete_statepo.Rda")

#geocoding for Other------------------------------------------------------------
incomplete_other <- filter(incomplete_grand, AGENCY_TYPE_NAME=="Other")
places <- list()

for (i in 1:length(incomplete_other$ORI)){
  search_str = str_glue(incomplete_other[i,2]," ",
                        incomplete_other[i,3],", ",
                        incomplete_other[i,5])
  place <- google_geocode(search_str)
  places[[i]] <- place
}
beep(4)

complete_other <- incomplete_other%>%
  mutate(place_object = places)
save(complete_other, file="complete_other.Rda")

#geocoding for other state agency-----------------------------------------------
incomplete_otherstate <- filter(incomplete_grand, AGENCY_TYPE_NAME=="Other State Agency")
places <- list()

for(i in 1:length(incomplete_otherstate$ORI)){
  search_str = str_glue(incomplete_otherstate[i,2]," ",
                        incomplete_otherstate[i,3],", ",
                        incomplete_otherstate[i,5])
  place <- google_geocode(search_str)
  places[[i]] <- place
}

complete_otherstate <- incomplete_otherstate%>%
  mutate(place_object = places)
save(complete_otherstate,file="complete_otherstate.Rda")

#geocoding for Federal----------------------------------------------------------
incomplete_federal <- filter(incomplete_grand, AGENCY_TYPE_NAME=="Federal")
places <- list()

for(i in 1:length(incomplete_federal$ORI)){
  search_str = str_glue(incomplete_federal[i,2],", ",
                        incomplete_federal[i,3],", ",
                        str_sub(incomplete_federal$ORI[i],start=1,end=2))
  place <- google_geocode(search_str)
  places[[i]] <- place
}

complete_federal <- incomplete_federal %>%
  mutate(place_object = places)
save(complete_federal, file = "complete_federal.Rda")

#geocoding for Tribal-----------------------------------------------------------
incomplete_tribal <- filter(incomplete_grand, AGENCY_TYPE_NAME=="Tribal")
places <- list()

for(i in 1:length(incomplete_tribal$ORI)){
  search_str = str_glue(incomplete_tribal[i,2],", ",
                        incomplete_tribal[i,5]
                        )
  place <- google_geocode(search_str)
  places[[i]] <- place
}

complete_tribal <- incomplete_tribal%>%
  mutate(place_object = places)
save(complete_tribal, file="complete_tribal.Rda")

#combine all these tibbles into one---------------------------------------------
complete_grand <- rbind(complete_city,
                        complete_county,
                        complete_statepo,
                        complete_other,
                        complete_otherstate,
                        complete_federal,
                        complete_tribal,
                        complete_univ)
save(complete_grand, file = "complete_grand.Rda")

#Adding lat/lon values from the google geocoding objects------------------------
load("hate_crime.Rda")
load("complete_grand.Rda")

#check for geocoding that produced problems, record indices 
search_problems <- c()
coordinate_problems <- c()
close_coordinates <- c()
for(i in 1:length(complete_grand$ORI)){
  if(complete_grand$place_object[[i]]$status!="OK"){
    search_problems <- c(search_problems,i)
  }
  else{
    coordinates <- geocode_coordinates(complete_grand$place_object[[i]])
    if(length(coordinates$lng)>1){
      if(
        (max(coordinates$lng)-min(coordinates$lng))>0.5
        ||
        (max(coordinates$lat)-min(coordinates$lat))>0.5
      ){
        coordinate_problems <- c(coordinate_problems,i)
      }
      else{close_coordinates <- c(close_coordinates,i)}
      
    }
  }
}

latitudes <- rep(NaN,length(complete_grand$ORI))
longitudes<- rep(NaN,length(complete_grand$ORI))

#for loop gets latitudes and longitudes for non-problem results
for(i in 1:length(complete_grand$ORI)){
  if(!(i%in%close_coordinates||i%in%search_problems||i%in%coordinate_problems)){
    latitudes[i] <- geocode_coordinates(complete_grand$place_object[[i]])$lat
    longitudes[i]<-  geocode_coordinates(complete_grand$place_object[[i]])$lng
  }
}

#let's fix the problem locations with OpenStreetMap and tmaptools (THANKS PETER)
pLocIndx <- sort(c(close_coordinates,coordinate_problems,search_problems))
problem_locations <- complete_grand[pLocIndx,]
for(i in 1:length(pLocIndx)){
  search_string <- "NA"
  agency <- complete_grand[pLocIndx[i],]
  aType <- agency$AGENCY_TYPE_NAME
  
  if(aType=="City"){
    search_string <- str_glue(agency$PUB_AGENCY_NAME,", ",agency$STATE_NAME)
  }
  if(aType=="County"){
    if(grepl("County", agency$PUB_AGENCY_NAME)){
      search_string <- str_trunc(
        agency$PUB_AGENCY_NAME,
        width = str_locate(
          agency$PUB_AGENCY_NAME,
          "County"
        )[2],
        ellipsis=""
      )
    }
    else{
      search_string <- str_glue(agency$PUB_AGENCY_NAME," County, ",agency$STATE_NAME)
    }
  }
  if(aType=="University or College"){
    search_string <- str_glue(agency$PUB_AGENCY_NAME,agency$PUB_AGENCY_UNIT,", ",agency$STATE_NAME)
  }
  if(aType=="Federal"){
    if(agency$ORI!="DCUSN0000"){#need to ignore U.S. Navy Law Enforcement
      search_string <- str_glue(agency$PUB_AGENCY_UNIT,", ",agency$STATE_NAME)
    }
  }
  #we'll omit coordinates for other, other state agency, and state police for 
  #now as well. None of these show up in the PNW, so it won't effect the 
  #narrowed scope of this project
  latitudes[pLocIndx[i]]  <- geocode_OSM(search_string,keep.unfound=TRUE)$coords[2]
  longitudes[pLocIndx[i]] <- geocode_OSM(search_string,keep.unfound=TRUE)$coords[1]
}
beepr::beep(3)

#set DC manually for now
latitudes[54] <- 38.90719 
longitudes[54]<- -76.90939 


#add latitudes and longitudes to the "ledger" 
complete_grand <- mutate(complete_grand,lat=latitudes, lng=longitudes)
save(complete_grand, file = "complete_grand.Rda")


#add lat/lon values to points in hate_crime using this ledger-------------------
# gbgORIs <- c("AR0040200","AR0290100")
# gbg1 <- complete_grand[1:2,]
# gbg2 <- filter(hate_crime,ORI%in%gbgORIs)
hate_crime <- left_join(hate_crime[,1:33],complete_grand[,-6])
save(hate_crime, file = "hate_crime.Rda")

#no missing values in Washington, Oregon, Nevada, or Idaho
sum(is.na(hate_crime[which(hate_crime$STATE_ABBR%in%c("OR","WA","ID","NV")),]$lat))

#4 missing values in California
sum(is.na(filter(hate_crime,STATE_ABBR%in%c("CA"))$lat))

#those 4 are in Sacramento, which is around 38.56,-121.48
hate_crime%>%
  filter(STATE_ABBR%in%c("CA"))%>%
  filter(is.na(lat))


#this means (-125,-116)X(40,49) would be a good observation window for the PNW.
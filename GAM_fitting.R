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
library(purrr)

#Let's fit a bigger GAM
load("hate_crime.Rda")

#looking at GSM crimes in the lower 48 only
outer_states <- c("Alaska","Hawaii","Guam")
GSM_crimes <- hate_crime%>%
  filter(GSM_bias)%>%
  filter(!STATE_NAME%in%outer_states)%>%
  filter(-126<lng)%>%
  filter(-66>lng)%>%
  filter(49.1>lat)%>%
  filter(24<lat)%>%
  filter(!is.na(lng))

# #set up spaciotemporal grid
lat_max <- ceiling(max(GSM_crimes$lat))
lat_min <- floor(min(GSM_crimes$lat))
lng_max <- ceiling(max(GSM_crimes$lng))
lng_min <- floor(min(GSM_crimes$lng))
dx=0.5
dy=0.5
X <- seq(from=lng_min,to=lng_max,by=2*dx)
Y <- seq(from=lat_min,to=lat_max,by=2*dy)
Time <-2001:2019
stgrid <- expand.grid(X,Y,Time)
names(stgrid)<-c("lng","lat","year")

#function counts the points in a pixel REQUIRES dx AND dy TO BE DEFINED FIRST
countcrimes_GSM<- function(longitude,latitude,year){
  slice <- GSM_crimes%>%
    filter(DATA_YEAR==year)%>%
    filter(longitude-dx<=lng)%>%
    filter(lng<=longitude+dx)%>%
    filter(latitude -dy<=lat)%>%
    filter(lat<=latitude+ dx)
  return(length(slice$ORI))
}

#add counts to spaciotemporal grid
tic()
stgrid <- stgrid %>%
  mutate(count = pmap_dbl(.l=list(lng,lat,year),.f=countcrimes_GSM))
toc()
save(stgrid,file="stgrid.Rda")#optional: saving the object
load("stgrid.Rda")#load object if necessary

#ok let's fit a GAM-------------------------------------------------------------
f <- count ~ te(lng, lat, year,        # inputs over which to smooth
              bs = c("tp", "cr"),      # types of bases
              k = c(35, 15),            # knot count in each dimension
              d = c(2, 1))             # (s,t) basis dimension
tic()
cnts <- gam(f, family=nb(link="log"), data=stgrid)
toc()#267 seconds for (35,15) knots,("tp","cr") bases, (2,1) basis dimension 
     #and a STgrid with 29146 rows
beep(4)
save(cnts,file="GAM_for_GSM.Rda")


## Construct space-time grid for predictions
grid_locs <- expand.grid(lng = seq(lng_min,lng_max,length.out = 100),
                         lat = seq(lat_min,lat_max,length.out = 100),
                         year = Time)
#predict over grid
X <- predict(cnts, grid_locs, se.fit = TRUE)

## Put data to plot into data frame
grid_locs$pred <- X$fit
grid_locs$se <- X$se.fit

## Plot predictions and overlay observations
# oregonshape <- map_data("state")%>%
#   filter(region=="oregon")
# washingtonshape <- map_data("state")%>%
#   filter(region=="washington")%>%
#   filter(subregion=="main")
states <- map_data("state")
g1 <- ggplot() +
  geom_raster(data = grid_locs,
              aes(lng, lat, fill = pmin(pmax(pred, -1), 5))) +
  facet_wrap(~year, nrow = 4) +
  # geom_point(data = filter(MOcarolinawren_long, !is.na(cnt)),
  #            aes(lon, lat),
  #            colour = "black", size = 3) +
  # geom_point(data=filter(MOcarolinawren_long, !is.na(cnt)),
  #            aes(lon, lat, colour = log(cnt)),
  #            size = 2) +
  #geom_path(data = oregonshape,aes(x=long,y=lat))+
  #geom_path(data = washingtonshape,aes(x=long,y=lat))+
  geom_path(data=states,aes(x=long,y=lat,group=group))+
  fill_scale(limits = c(-1, 5),
             name = expression(log(Y[t]))) +
  col_scale(name = "log(cnt)", limits=c(-1, 5)) +
  theme_bw()
g1

## Plot prediction standard errors
g2 <- ggplot() +
  geom_raster(data = grid_locs,
              aes(lng, lat, fill = pmin(se, 2.5))) +
  facet_wrap(~year, nrow = 4) +
  fill_scale(palette = "BrBG",
             limits = c(0, 2.5),
             name = expression(s.e.)) +
  # geom_path(data = oregonshape,aes(x=long,y=lat))+
  # geom_path(data = washingtonshape,aes(x=long,y=lat))+
  geom_path(data=states,aes(x=long,y=lat,group=group))+  
  theme_bw()
g2



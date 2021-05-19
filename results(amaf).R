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
library(gridExtra)
library(usmap)
library(ggpubr)

load("hate_crime.Rda")
states <- map_data("state")

#(-125,-116)X(40,49) is the window of data I'm using for the PNW. This covers 
#Washington, Oregon, Western Idaho, and the very northern bits of California and
#Nevada. Also narrowing the scope to just be 2002-2019.
pnw_hc <- hate_crime%>%
  filter(lat>=40)%>%
  filter(lat<=49)%>%
  filter(lng>=-125)%>%
  filter(lng<=-116)%>%
  filter(DATA_YEAR>=2002)
remove(hate_crime)

#Some introductory data visualization-------------------------------------------
p1 <- ggplot(pnw_hc)+
  geom_bar(aes(x=STATE_NAME))+
  labs(x="State",y="Number of hate crimes",title="Total hate crimes by state",
       subtitle = "Washington, Oregon and parts of neighboring states")
p2 <- ggplot(pnw_hc)+
  geom_line(aes(x=DATA_YEAR,color=STATE_NAME),stat="count")+
  labs(x="Year",y="Number of hate Crimes",color="State",
       title = "Total Hate Crimes Over Time",
       subtitle="Washington, Oregon and parts of neighboring states")

#I want to see that line graph normalized for a (crude) population estimate for 
#oregon and Washington. I'm goin to use some data from 
#https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-state.html
#and
#https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html

census_data <- read.csv("~/masters-project/intercensal_estimates.csv")%>%
  filter(year>=2002)
census_data$hc_count <- rep(NA,length(census_data$year))
for(i in 1:length(census_data$year)){
  gbg <- pnw_hc%>%
    filter(DATA_YEAR==census_data$year[i])%>%
    filter(STATE_NAME==census_data$state[i])
  census_data$hc_count[i] <- length(gbg$ORI)
}
census_data <- census_data%>%
  mutate(hc_rate = hc_count/july1est)

p3 <- ggplot(census_data,aes(x=year,y=hc_rate,color=state))+
  geom_line()+
  labs(x="Year",y="Hate crime rate per capita",color="State",
       title="Hate Crime Rate Over Time",
       subtitle = "Washington and Oregon only")

p4 <- ggplot()+
  geom_path(data=map_data("state"),aes(x=long,y=lat,group=group))+
  geom_rect(data=NULL,aes(xmin=-125,xmax=-116,ymin=40,ymax=49),
            fill="cadetblue",alpha=0.5)+
  labs(title="Area of Interest")+
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))

ggarrange(p4,p1,p2,p3,nrow=2,ncol=2)

#Ok, let's bin the ST points and do some GAMs-----------------------------------

# #set up spaciotemporal grid
dx=0.2
dy=0.2
lat_max <- 49-dy
lat_min <- 40+dy
lng_max <- -116-dx
lng_min <- -125+dx
X <- seq(from=lng_min,to=lng_max,by=2*dx)
Y <- seq(from=lat_min,to=lat_max,by=2*dy)
Time <-2002:2019
stgrid <- expand.grid(X,Y,Time)
names(stgrid)<-c("lng","lat","year")

#function counts the points in a pixel REQUIRES dx AND dy TO BE DEFINED FIRST
countcrimes<- function(longitude,latitude,year){
  slice <- pnw_hc%>%
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
  mutate(count = pmap_dbl(.l=list(lng,lat,year),.f=countcrimes))
toc() #only takes 8 seconds with dx=dy=0.5 

#plot the binned counts:
ggplot(stgrid)+
  geom_raster(aes(x=lng,y=lat,fill=log(count)))+
  facet_wrap(~year,nrow=3)+
  geom_path(data=states,aes(x=long,y=lat,group=group))+
  xlim(-126,-116)+
  ylim(40,49)


#fit the gam
f <- count ~ te(lng, lat, year,        # inputs over which to smooth
                bs = c("tp", "cr"),      # types of bases
                k = c(50, 10),            # knot count in each dimension
                d = c(2, 1))             # (s,t) basis dimension
tic()
cnts <- gam(f, family=nb(link="log"), data=stgrid)
toc()#takes 655.8 seconds (11 minutes) now
save(cnts,file="PNW_GAM.Rda")
load("PNW_GAM.Rda")

##make model predictions for plotting
grid_locs <- expand.grid(lng = seq(lng_min,lng_max,length.out = 100),
                         lat = seq(lat_min,lat_max,length.out = 100),
                         year = Time)
tic()
X <- predict(cnts, grid_locs, se.fit = TRUE)
toc()
grid_locs$pred <- X$fit
grid_locs$se <- X$se.fit

#plot fitted values
pnw_hc1 <- mutate(pnw_hc,year=DATA_YEAR)
g1 <- ggplot() +
  geom_raster(data = grid_locs,
              aes(lng, lat, fill = pmin(pmax(pred, -1), 5))) +
  facet_wrap(~year, nrow = 4) +
  geom_path(data=states,aes(x=long,y=lat,group=group))+
  geom_point(data=pnw_hc1,aes(x=lng,y=lat),alpha=0.2)+
  fill_scale(limits = c(-1, 5),
             name = expression(log(Y[t]))) +
  col_scale(name = "log(cnt)", limits=c(-1, 5)) +
  xlim(-126,-116)+
  ylim(40,49)+
  theme_bw()
g1

## Plot prediction standard errors
g2 <- ggplot() +
  geom_raster(data = grid_locs,
              aes(lng, lat, fill = pmin(se, 2.5))) +
  facet_wrap(~year, nrow = 4) +
  geom_path(data=states,aes(x=long,y=lat,group=group))+
  fill_scale(palette = "BrBG",
             limits = c(0, 2.5),
             name = expression(s.e.)) +
  # geom_path(data = oregonshape,aes(x=long,y=lat))+
  # geom_path(data = washingtonshape,aes(x=long,y=lat))+
  geom_path(data=states,aes(x=long,y=lat,group=group))+ 
  xlim(-126,-116)+
  ylim(40,49)+
  theme_bw()
g2

#let's take a look at what's happening in Portland and Seattle
seattle_grid_locs <- expand.grid(lng = -122.3,
                                 lat = 47.6,
                                 year = seq(2002,2019,length.out = 200))
portland_grid_locs <- expand.grid(lng = -122.7,
                                  lat = 45.4,
                                  year = seq(2002,2019,length.out = 200))
Xs <-  predict(cnts, seattle_grid_locs, se.fit = TRUE)
Xp <-  predict(cnts, portland_grid_locs, se.fit = TRUE)
df <- tibble(
  location = c(rep("Seattle",200),rep("Portland",200)),
  year=c(seattle_grid_locs$year,portland_grid_locs$year),
  fit = c(Xs$fit,Xp$fit),
  se  = c(Xs$se.fit,Xp$se.fit)
)%>%
  mutate(ub = fit+se)%>%
  mutate(lb = fit-se)
ggplot(df)+
  geom_line(aes(x=year,y=fit,color=location))

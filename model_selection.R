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


states <- map_data("state")

#(-125,-116)X(40,49) is the window of data I'm using for the PNW. This covers 
#Washington, Oregon, Western Idaho, and the very northern bits of California and
#Nevada. Also narrowing the scope to just be 2002-2019.
load("hate_crime.Rda")
pnw_hc <- hate_crime%>%
  filter(lat>=40)%>%
  filter(lat<=49)%>%
  filter(lng>=-125)%>%
  filter(lng<=-116)%>%
  filter(DATA_YEAR>=2002)
remove(hate_crime)

#set up spaciotemporal grid
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
toc() #takes around a minute 
save(stgrid,file="stgrid.Rda")

#I want to fit models with spatial knot counts of 60,75,100,150,200 and with 
#temporal knot counts of 2,5,7. This section sets up a list that has the knot 
#counts, the AIC (NA to start) and the model objects (also NA)
models <- expand.grid(spatial_knots=c(60,75,100,150,200),
                      temporal_knots=c(2,5,7))%>%
  transpose()

#for loop iterates through these 15 knot count combos, fits a model using the 
#ST grid and saves the model object.
for(i in 1:length(models)){
    s_knots <- models[[i]]$spatial
    t_knots <- models[[i]]$temporal
    #set up model formula
    f <- count ~ te(lng, lat, year,          # inputs over which to smooth
                    bs = c("tp", "cr"),      # types of bases
                    k = c(s_knots, t_knots), # knot count in each dimension
                    d = c(2, 1))             # (s,t) basis dimension
    #fit the GAM
    tic()
    cnts <- gam(f, family=nb(link="log"), data=stgrid)
    model_name <- str_glue("GAM_","S",s_knots,"T",t_knots)
    #report progress and save object
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print(str_glue("Model fitted with ",
             s_knots,
             " spatial knots and ",
             t_knots,
             " temporal knots. Saving model object as ", model_name))
    print(str_glue(i, " of ", length(models)," models fitted."))
    toc()
    beep()
    save(cnts,file = str_glue("GAM_model_objects/",model_name,".Rda"))
}



#Ok, let's add these models and their AIC to the grid of models
# models <- expand.grid(spatial_knots=c(60,75,100,150,200),
#                       temporal_knots=c(2,5,7))%>%
#   mutate(AIC=rep(NA,length(spatial_knots)))%>%
#   mutate(model_object=list(rep(NA,length(spatial_knots)))%>%
#   as_tibble()

model_file_names <- c("GAM_model_objects/GAM_S60T2.Rda",
                      "GAM_model_objects/GAM_S75T2.Rda",
                      "GAM_model_objects/GAM_S150T2.Rda",
                      "GAM_model_objects/GAM_S100T2.Rda",
                      "GAM_model_objects/GAM_S200T2.Rda",
                      "GAM_model_objects/GAM_S60T5.Rda",
                      "GAM_model_objects/GAM_S75T5.Rda",
                      "GAM_model_objects/GAM_S100T5.Rda",
                      "GAM_model_objects/GAM_S150T5.Rda",
                      "GAM_model_objects/GAM_S200T5.Rda",
                      "GAM_model_objects/GAM_S60T7.Rda",
                      "GAM_model_objects/GAM_S75T7.Rda",
                      "GAM_model_objects/GAM_S100T7.Rda",
                      "GAM_model_objects/GAM_S150T7.Rda",
                      "GAM_model_objects/GAM_S200T7.Rda")
models <- tibble(
  spatial_knots = rep(NA,length(model_file_names)),
  temporal_knots = rep(NA,length(model_file_names)),
  AIC = rep(NA,length(model_file_names)),
  GCV = rep(NA,length(model_file_names))
)
model_objects <- list()
for(i in 1:length(model_file_names)){
  fname <- model_file_names[i]
  tdex <- str_locate(fname,"T")[1]
  sdex <- str_locate(fname,"S")[1]
  pdex <- str_locate(fname,".Rda")[1]
  models$spatial_knots[i] <- str_sub(fname,sdex+1,tdex-1)
  models$temporal_knots[i] <- str_sub(fname,tdex+1,pdex-1)
  load(fname)
  models$AIC[i] <- AIC(cnts)
  models$GCV[i] <- summary(cnts)$sp.criterion
  model_objects[[i]] <- cnts
  remove(cnts)
}

#ok, let's compare the AIC of the models in question
ggplot(models,aes(x=as.numeric(spatial_knots),y=as.numeric(temporal_knots)))+
  geom_tile(aes(fill=GCV))+
  geom_text(aes(label=GCV),angle=-45)+
  labs(x="Spatial Knots",y="Temporal Knots")
ggplot(models,aes(x=as.numeric(spatial_knots),y=as.numeric(temporal_knots)))+
  geom_tile(aes(fill=AIC))+
  geom_text(aes(label=AIC),angle=-45)+
  labs(x="Spatial Knots",y="Temporal Knots")

#it seems that the number of temporal knots is not effecting the AIC and GCV in 
#a substantial way, so I'll just use 5 temporal knots from here on and try and 
#find a more optimal number of spatial knots

#-------------------------------------------------------------------------------
#so, let's do it again

load("stgrid.Rda")
models <- expand.grid(spatial_knots=c(170,180,190),
                      temporal_knots=5)%>%
  transpose()

for(i in 1:length(models)){
  s_knots <- models[[i]]$spatial_knots
  t_knots <- models[[i]]$temporal_knots
  #set up model formula
  f <- count ~ te(lng, lat, year,          # inputs over which to smooth
                  bs = c("tp", "cr"),      # types of bases
                  k = c(s_knots, t_knots), # knot count in each dimension
                  d = c(2, 1))             # (s,t) basis dimension
  #fit the GAM
  tic()
  cnts <- gam(f, family=nb(link="log"), data=stgrid)
  model_name <- str_glue("GAM_","S",s_knots,"T",t_knots)
  #report progress and save object
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(str_glue("Model fitted with ",
                 s_knots,
                 " spatial knots and ",
                 t_knots,
                 " temporal knots. Saving model object as ", model_name))
  print(str_glue(i, " of ", length(models)," models fitted."))
  toc()
  beep()
  save(cnts,file = str_glue("GAM_model_objects/",model_name,".Rda"))
}

#great, now let's see how these compare
model_file_names <- c("GAM_model_objects/GAM_S60T5.Rda",
                      "GAM_model_objects/GAM_S75T5.Rda",
                      "GAM_model_objects/GAM_S100T5.Rda",
                      "GAM_model_objects/GAM_S150T5.Rda",
                      "GAM_model_objects/GAM_S200T5.Rda",
                      "GAM_model_objects/GAM_S170T5.Rda",
                      "GAM_model_objects/GAM_S180T5.Rda",
                      "GAM_model_objects/GAM_S190T5.Rda")
models <- tibble(
  spatial_knots = rep(NA,length(model_file_names)),
  temporal_knots = rep(NA,length(model_file_names)),
  AIC = rep(NA,length(model_file_names)),
  GCV = rep(NA,length(model_file_names))
)
model_objects <- list()
for(i in 1:length(model_file_names)){
  fname <- model_file_names[i]
  tdex <- str_locate(fname,"T")[1]
  sdex <- str_locate(fname,"S")[1]
  pdex <- str_locate(fname,".Rda")[1]
  models$spatial_knots[i] <- str_sub(fname,sdex+1,tdex-1)
  models$temporal_knots[i] <- str_sub(fname,tdex+1,pdex-1)
  load(fname)
  models$AIC[i] <- AIC(cnts)
  models$GCV[i] <- summary(cnts)$sp.criterion
  model_objects[[i]] <- cnts
  remove(cnts)
}

#ok, let's compare the GCV and AIC of the models again
ggplot(models,aes(x=as.numeric(spatial_knots),y=GCV))+
  geom_line()+
  labs(x="Spatial Knots",y="GCV")
ggplot(models,aes(x=as.numeric(spatial_knots),y=AIC))+
  geom_line()+
  labs(x="Spatial Knots",y="AIC")

#time to fit more gams! looks like we're getting close, so I'm going to go from
#200-250 in increments of 10and let my cpu do it's thing while I get some rest

load("stgrid.Rda")
models <- expand.grid(spatial_knots=c(210,220,230,240,250),
                      temporal_knots=5)%>%
  transpose()

for(i in 1:length(models)){
  s_knots <- models[[i]]$spatial_knots
  t_knots <- models[[i]]$temporal_knots
  #set up model formula
  f <- count ~ te(lng, lat, year,          # inputs over which to smooth
                  bs = c("tp", "cr"),      # types of bases
                  k = c(s_knots, t_knots), # knot count in each dimension
                  d = c(2, 1))             # (s,t) basis dimension
  #fit the GAM
  tic()
  cnts <- gam(f, family=nb(link="log"), data=stgrid)
  model_name <- str_glue("GAM_","S",s_knots,"T",t_knots)
  #report progress and save object
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(str_glue("Model fitted with ",
                 s_knots,
                 " spatial knots and ",
                 t_knots,
                 " temporal knots. Saving model object as ", model_name))
  print(str_glue(i, " of ", length(models)," models fitted."))
  toc()
  beep()
  save(cnts,file = str_glue("GAM_model_objects/",model_name,".Rda"))
}

#let's compare models again
model_file_names <- c("GAM_model_objects/GAM_S60T5.Rda",
                      "GAM_model_objects/GAM_S75T5.Rda",
                      "GAM_model_objects/GAM_S100T5.Rda",
                      "GAM_model_objects/GAM_S150T5.Rda",
                      "GAM_model_objects/GAM_S200T5.Rda",
                      "GAM_model_objects/GAM_S170T5.Rda",
                      "GAM_model_objects/GAM_S180T5.Rda",
                      "GAM_model_objects/GAM_S190T5.Rda",
                      "GAM_model_objects/GAM_S210T5.Rda",
                      "GAM_model_objects/GAM_S220T5.Rda",
                      "GAM_model_objects/GAM_S230T5.Rda",
                      "GAM_model_objects/GAM_S240T5.Rda",
                      "GAM_model_objects/GAM_S250T5.Rda")
models <- tibble(
  spatial_knots = rep(NA,length(model_file_names)),
  temporal_knots = rep(NA,length(model_file_names)),
  AIC = rep(NA,length(model_file_names)),
  GCV = rep(NA,length(model_file_names))
)
model_objects <- list()
for(i in 1:length(model_file_names)){
  fname <- model_file_names[i]
  tdex <- str_locate(fname,"T")[1]
  sdex <- str_locate(fname,"S")[1]
  pdex <- str_locate(fname,".Rda")[1]
  models$spatial_knots[i] <- str_sub(fname,sdex+1,tdex-1)
  models$temporal_knots[i] <- str_sub(fname,tdex+1,pdex-1)
  load(fname)
  models$AIC[i] <- AIC(cnts)
  models$GCV[i] <- summary(cnts)$sp.criterion
  model_objects[[i]] <- cnts
  remove(cnts)
}

#ok, let's compare the GCV and AIC of the models again
ggplot(models,aes(x=as.numeric(spatial_knots),y=GCV))+
  geom_line()+
  labs(x="Spatial Basis Dimension",y="GCV")
ggplot(models,aes(x=as.numeric(spatial_knots),y=AIC))+
  geom_line()+
  labs(x="Spatial Basis Dimension",y="AIC")

#-------------------------------------------------------------------------------
#let's make a plot for the (currently) best one, with k=250 for the TPRS:
dx=0.2
dy=0.2
lat_max <- 49-dy
lat_min <- 40+dy
lng_max <- -116-dx
lng_min <- -125+dx
X <- seq(from=lng_min,to=lng_max,by=2*dx)
Y <- seq(from=lat_min,to=lat_max,by=2*dy)
Time <-2002:2019
grid_locs <- expand.grid(lng = seq(lng_min,lng_max,length.out = 100),
                         lat = seq(lat_min,lat_max,length.out = 100),
                         year = Time)
tic()
X <- predict(cnts, grid_locs, se.fit = TRUE)
toc() #takes 5 minutes
grid_locs$pred <- X$fit
grid_locs$se <- X$se.fit
states <- map_data("state")
load("hate_crime.Rda")
pnw_hc <- hate_crime%>%
  filter(lat>=40)%>%
  filter(lat<=49)%>%
  filter(lng>=-125)%>%
  filter(lng<=-116)%>%
  filter(DATA_YEAR>=2002)
remove(hate_crime)
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

#after reading a bunch of Simon Wood's work, and using gam.check on the largest
#model, I've decided to kick up the basis dimension count to 300----------------
load("stgrid.Rda")
models <- expand.grid(spatial_knots=c(300),
                      temporal_knots=7)%>%
  transpose()

for(i in 1:length(models)){
  s_knots <- models[[i]]$spatial_knots
  t_knots <- models[[i]]$temporal_knots
  #set up model formula
  f <- count ~ te(lng, lat, year,          # inputs over which to smooth
                  bs = c("tp", "cr"),      # types of bases
                  k = c(s_knots, t_knots), # knot count in each dimension
                  d = c(2, 1))             # (s,t) basis dimension
  #fit the GAM
  tic()
  cnts <- gam(f, family=poisson(link="log"), data=stgrid)
  model_name <- str_glue("GAM_","S",s_knots,"T",t_knots)
  #report progress and save object
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(str_glue("Model fitted with ",
                 s_knots,
                 " spatial knots and ",
                 t_knots,
                 " temporal knots. Saving model object as ", model_name))
  print(str_glue(i, " of ", length(models)," models fitted."))
  toc()
  beep()
  save(cnts,file = str_glue("GAM_model_objects/",model_name,"POISSON.Rda"))
}

#let's compare models one last time---------------------------------------------
model_file_names <- c("GAM_model_objects/GAM_S60T5.Rda",
                      "GAM_model_objects/GAM_S75T5.Rda",
                      "GAM_model_objects/GAM_S100T5.Rda",
                      "GAM_model_objects/GAM_S150T5.Rda",
                      "GAM_model_objects/GAM_S200T5.Rda",
                      "GAM_model_objects/GAM_S170T5.Rda",
                      "GAM_model_objects/GAM_S180T5.Rda",
                      "GAM_model_objects/GAM_S190T5.Rda",
                      "GAM_model_objects/GAM_S210T5.Rda",
                      "GAM_model_objects/GAM_S220T5.Rda",
                      "GAM_model_objects/GAM_S230T5.Rda",
                      "GAM_model_objects/GAM_S240T5.Rda",
                      "GAM_model_objects/GAM_S250T5.Rda",
                      "GAM_model_objects/GAM_S260T5.Rda",
                      "GAM_model_objects/GAM_S270T5.Rda",
                      "GAM_model_objects/GAM_S280T5.Rda",
                      "GAM_model_objects/GAM_S290T5.Rda",
                      "GAM_model_objects/GAM_S300T5.Rda",
                      "GAM_model_objects/GAM_S60T7.Rda",
                      "GAM_model_objects/GAM_S75T7.Rda",
                      "GAM_model_objects/GAM_S100T7.Rda",
                      "GAM_model_objects/GAM_S150T7.Rda",
                      "GAM_model_objects/GAM_S200T7.Rda",
                      "GAM_model_objects/GAM_S170T7.Rda",
                      "GAM_model_objects/GAM_S180T7.Rda",
                      "GAM_model_objects/GAM_S190T7.Rda",
                      "GAM_model_objects/GAM_S210T7.Rda",
                      "GAM_model_objects/GAM_S220T7.Rda",
                      "GAM_model_objects/GAM_S230T7.Rda",
                      "GAM_model_objects/GAM_S240T7.Rda",
                      "GAM_model_objects/GAM_S250T7.Rda",
                      "GAM_model_objects/GAM_S260T7.Rda",
                      "GAM_model_objects/GAM_S270T7.Rda",
                      "GAM_model_objects/GAM_S280T7.Rda",
                      "GAM_model_objects/GAM_S290T7.Rda",
                      "GAM_model_objects/GAM_S300T7.Rda")
models <- tibble(
  spatialBD = rep(NA,length(model_file_names)),
  temporalBD = rep(NA,length(model_file_names)),
  AIC = rep(NA,length(model_file_names)),
  GCV = rep(NA,length(model_file_names)),
  k_index1 = rep(NA,length(model_file_names)),
  k_index2 = rep(NA,length(model_file_names)),
  k_index3 = rep(NA,length(model_file_names))
)

for(i in 1:length(model_file_names)){
  fname <- model_file_names[i]
  tdex <- str_locate(fname,"T")[1]
  sdex <- str_locate(fname,"S")[1]
  pdex <- str_locate(fname,".Rda")[1]
  models$spatialBD[i] <- str_sub(fname,sdex+1,tdex-1)
  models$temporalBD[i] <- str_sub(fname,tdex+1,pdex-1)
  load(fname)
  models$AIC[i] <- AIC(cnts)
  models$GCV[i] <- summary(cnts)$sp.criterion
  models$k_index1[i] <- k.check(cnts)[3]
  models$k_index2[i] <- k.check(cnts)[3]
  models$k_index3[i] <- k.check(cnts)[3]
  remove(cnts)
}
beep()
models_long <- models %>%
  pivot_longer(5:7,names_to="replicate",values_to="k_index")


#ok, let's compare the GCV, AIC, and k-index
p1 <- ggplot(models,aes(x=as.numeric(spatialBD),y=GCV,color=as.factor(temporalBD)))+
  geom_line(show.legend = FALSE)+
  labs(x="Spatial Basis Dimension",y="GCV")+
  labs(
    x="Spatial Basis Dimension",
    color="Temporal Basis Dimension"
  )+
  theme_bw()
p2 <- ggplot(models,aes(x=as.numeric(spatialBD),y=AIC,color=as.factor(temporalBD)))+
  geom_line(show.legend = FALSE)+
  labs(x="Spatial Basis Dimension",y="AIC")+
  labs(
    x="Spatial Basis Dimension",
    color="Temporal Basis Dimension"
  )+
  theme_bw()
df <- models%>%
  pivot_longer(5:7,names_to="replicate",values_to="k_index")%>%
  dplyr::group_by(spatialBD,temporalBD)%>%
  summarise(avg_k_index = mean(k_index))
p3 <- ggplot(data=NULL)+
  geom_point(data=models_long,aes(x=as.numeric(spatialBD),y=k_index,color=temporalBD))+
  geom_line(data=df,aes(x=as.numeric(spatialBD),y=avg_k_index,color=temporalBD))+
  labs(
    x="Spatial Basis Dimension",
    color="Temporal Basis Dimension",
    y="k-index"
  )+
  lims(y=c(0,1))+
  theme_bw()
modelsel1 <- arrangeGrob(arrangeGrob(p1,p2,nrow=1),p3,nrow=2)
ggsave(filename = "plots and figures/model_selection1.png",
       plot = modelsel1,
       width = 6.5,
       height = 4
)

#Plot k-index values for the poisson model
load("GAM_model_objects/GAM_S300T7POISSON.Rda")
nsim <- 100
kdex <- rep(NA,nsim)
for(i in 1:nsim){
  kdex[i] <- k.check(cnts)[3]
}
ggplot(data=NULL,aes(x=kdex))+
  geom_boxplot()+
  theme_bw()+
  labs(x="k-index",
       title="k-index of the poisson regression model",
       subtitle = "dim(spatial basis) = 300, dim(temporal basis) = 7")+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
ggsave(filename = "plots and figures/model_selection2.png",
       plot = last_plot(),
       width = 4,
       height = 2
)
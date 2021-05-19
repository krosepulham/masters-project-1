# current version of the STRbook package isn't available on CRAN version yet; 
# use devtools to download from github:
# library(devtools)
# install_github("andrewzm/STRbook")

library(STRbook)
library(dplyr)
library(ggplot2)
library(mgcv)
library(tidyr)
library(beepr)
library(tictoc)

data("MOcarolinawren_long",package="STRbook")

f <- cnt ~ te(lon, lat, t,        # inputs over which to smooth
              bs = c("tp", "cr"), # types of bases
              k = c(50, 10),      # knot count in each dimension
              d = c(2, 1))        # (s,t) basis dimension
tic()
cnts <- gam(f, family=nb(link="log"), data=MOcarolinawren_long)
toc() 
beep(4) #when i ran this it took 109 seconds for 1575 rows; I'm going to need to 
        #subset the data, or be ready to wait a long time. 

cnts$family$getTheta(trans = 1)   #relatively small r suggests overdispersion

MOlon <- MOcarolinawren_long$lon
MOlat <- MOcarolinawren_long$lat

## Construct space-time grid
grid_locs <- expand.grid(lon = seq(min(MOlon) - 0.2,
                                   max(MOlon) + 0.2,
                                   length.out = 80),
                         lat = seq(min(MOlat) - 0.2,
                                   max(MOlat) + 0.2,
                                   length.out = 80),
                         t = 1:max(MOcarolinawren_long$t))

#predict over grid
X <- predict(cnts, grid_locs, se.fit = TRUE)

## Put data to plot into data frame
grid_locs$pred <- X$fit
grid_locs$se <- X$se.fit
## Plot predictions and overlay observations
g1 <- ggplot() +
  geom_raster(data = grid_locs,
              aes(lon, lat, fill = pmin(pmax(pred, -1), 5))) +
  facet_wrap(~t, nrow = 3, ncol = 7) +
  geom_point(data = filter(MOcarolinawren_long, !is.na(cnt)),
             aes(lon, lat),
             colour = "black", size = 3) +
  geom_point(data=filter(MOcarolinawren_long, !is.na(cnt)),
             aes(lon, lat, colour = log(cnt)),
             size = 2) +
  fill_scale(limits = c(-1, 5),
             name = expression(log(Y[t]))) +
  col_scale(name = "log(cnt)", limits=c(-1, 5)) +
  theme_bw()
## Plot prediction standard errors
g2 <- ggplot() +
  geom_raster(data = grid_locs,
              aes(lon, lat, fill = pmin(se, 2.5))) +
  facet_wrap(~t, nrow = 3, ncol = 7) +
  fill_scale(palette = "BrBG",
             limits = c(0, 2.5),
             name = expression(s.e.)) +
  theme_bw()
 
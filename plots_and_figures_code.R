library(tidyverse)
library(gridExtra)
library(mgcv)
library(usmap)
#bias/variance tradeoff figure -------------------------------------------------
X <- runif(100,0,6)
Y <- 2 + sin(X) + rnorm(100,mean = 0,sd=0.25)
p1 <- ggplot(data=NULL,mapping=aes(x=X,y=Y))+
  geom_point()+
  geom_smooth(formula = y~x, method = "loess", span = .7, color = "blue", se=F)+
  labs(
    title = "Span = 0.7",
    x="",
    y=""
  )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p2 <- ggplot(data=NULL,mapping=aes(x=X,y=Y))+
  geom_point()+
  geom_smooth(formula = y~x, method = "loess", span = .10, color = "red", se=F)+
  labs(
    title = "Span = 0.1",
    x="",
    y=""
  )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p3 <- ggplot(data=NULL,mapping=aes(x=X,y=Y))+
  geom_point()+
  geom_smooth(method="loess", formula=y~x, span=2, color="orange",se=F)+
  labs(
    title = "Span = 2",
    x="",
    y=""
  )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
biasvar <- arrangeGrob(p2,p1,p3,nrow=1)
ggsave(filename = "plots and figures/biasvar.png",
       plot = biasvar,
       width = 6.5,
       height = 2
       )


#interpolation spline figure and cubic smoothing spline figure------------------
npoints <- 100
set.seed(16)
X <- runif(npoints,0,6)
Y <- 2 + sin(X) + rnorm(npoints,mean = 0,sd=0.25)
t <- seq(from=min(X),to=max(X),length.out=1000)
f <- splinefun(X,Y,method = "fmm")

p1 <- ggplot(data=NULL,mapping=aes(x=X,y=Y))+
  geom_point()+
  geom_line(mapping = aes(x=t,y=f(t)),color="red")+
  labs(
    x="",
    y=""
  )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p1
ggsave(filename = "plots and figures/interpolation spline.png",
       plot = p1,
       device = "png",
       height = 3,
       width = 3)

gam1 <- gam(formula = Y~s(X, bs="cr", sp = 1))
gam2 <- gam(formula = Y~s(X, bs="cr", sp = 1000))
gam3 <- gam(formula = Y~s(X, bs="cr", sp = 10000))
df <- data.frame(
  x=t,
  `1` = predict(gam1, data.frame(X=t)),
  `1000` = predict(gam2, data.frame(X=t)),
  `10000` = predict(gam3, data.frame(X=t))
)%>%
  pivot_longer(2:4,names_to = "lambda")%>%
  mutate(lambda = str_sub(lambda,start = 2))
p2 <- ggplot()+
  geom_line(data=df, mapping = aes(x=x,y=value,color = lambda))+
  geom_point(data=NULL, mapping=aes(x=X,y=Y))+
  labs(
    x="",
    y="",
    color = "Tuning parameter"
  )+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p2
ggsave(filename = "plots and figures/cubic smoothing spline.png",
       plot = p2,
       device = "png",
       height = 3,
       width = 5)

#Diagnostic Plot
load("GAM_model_objects/GAM_S300T7POISSON.Rda")
df <- tibble(
  fitvals = fitted.values(cnts),
  counts  = cnts$y,
  dev_resids = residuals(cnts,type = "deviance"),
  linpreds = cnts$linear.predictors
)
ggplot(df,aes(x=linpreds,y=dev_resids,color=log(counts)))+
  geom_point()+
  lims(x=c(-3,6))+
  labs(x="Linear Predictor",
       y="Deviance Residual",
       color="log(y)",
       title="Residuals Vs Linear Predictors")
ggsave("plots and figures/residuals1.png",
       plot=last_plot(),
       height=2,
       width=4)
qq.gam(cnts,pch=".")

#Plot a slice of the fitted model
dx=0.2
dy=0.2
lat_max <- 49
lat_min <- 40
lng_max <- -116
lng_min <- -125
Time <-2002:2019
grid_locs <- expand.grid(lng = seq(lng_min,lng_max,length.out = 200),
                         lat = seq(lat_min,lat_max,length.out = 200),
                         year = c(2014,2019))
X <- predict(cnts, grid_locs, se.fit = TRUE)
grid_locs$pred <- X$fit
grid_locs$se <- X$se.fit
#plot fitted values
load("hate_crime.Rda")
states <- ggplot2::map_data("state")
pnw_hc <- hate_crime%>%
  filter(lat>=40)%>%
  filter(lat<=49)%>%
  filter(lng>=-125)%>%
  filter(lng<=-116)%>%
  filter(DATA_YEAR>=2002)%>%
  mutate(year=DATA_YEAR)%>%
  filter(year%in%c(2014,2019))
remove(hate_crime)
states <- ggplot2::map_data("state")
g1 <- ggplot() +
  geom_raster(data = grid_locs,
              aes(lng, lat, fill = pmin(pmax(pred, -1), 5))) +
  geom_path(data=states,aes(x=long,y=lat,group=group))+
  geom_point(data=pnw_hc,aes(x=lng,y=lat),alpha=0.2)+
  facet_wrap(~year)+
  fill_scale(limits = c(-1, 6),name = "log(predicted values)")+
  col_scale(name = "log(cnt)", limits=c(-1, 5)) +
  xlim(-126,-116)+
  ylim(40,49)+
  theme_bw()+
  labs(
    title="Model predictions"
  )+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
g1
ggsave(
  filename = "plots and figures/slicemaps.png",
  plot=g1,
  height=3,
  width=6.5
)

##side by side maps
load("GAM_model_objects/GAM_S300T7POISSON.Rda")
tpredframe <- expand.grid(
  lat = c(45.523,47.608,44.058),
  year = seq(2002,2019,length.out = 1000))
tpredframe$lng <- rep(0,length(tpredframe$lat))
for(i in 1:length(tpredframe$lat)){
  tpredframe$lng[i] <- ifelse(
    tpredframe$lat[i]==45.523,
    -122.672,
    ifelse(
      tpredframe$lat[i]==47.608,
      -122.336,
      ifelse(
        tpredframe$lat[i]==44.058,
        -121.310,
        NULL
      )
    )
  )
}
predictions <- predict.gam(cnts,tpredframe,type="response")
tpredframe <- mutate(tpredframe,pred=predictions)
ggplot(tpredframe,aes(x=year,y=pred,color=as.factor(lat)))+
  geom_line()+
  labs(
    x="Year",
    y=""
  )
X <- cnts$model%>%
  mutate(yhat = fitted.values(cnts))%>%
  mutate(y=count)%>%
  select(-count)
p1 <- X%>%
  filter(lat>45)%>%
  filter(lat<46)%>%
  filter(lng< -122)%>%
  filter(lng> -123)%>%
  group_by(year)%>%
  summarise(predicted = mean(yhat),observed = mean(y))%>%
  pivot_longer(2:3,names_to = "obs_pred",values_to="value")%>%
  ggplot(aes(x=year,y=value,color=obs_pred))+
  geom_line(show.legend=FALSE)+
  labs(
    x="year",
    y="mean hate crimes per pixel",
    title="Observed and predicted hate crimes",
    subtitle = "Portland, Oregon metro area",
    color=""
  )
p2 <-  X%>%
  filter(lat>=47.0)%>%
  filter(lat<=47.8)%>%
  filter(lng<= -121.9)%>%
  filter(lng>= -122.6)%>%
  group_by(year)%>%
  summarise(predicted = mean(yhat),observed = mean(y))%>%
  pivot_longer(2:3,names_to = "obs_pred",values_to="value")%>%
  ggplot(aes(x=year,y=value,color=obs_pred))+
  geom_line()+
  labs(
    x="year",
    y="mean hate crimes per pixel",
    title="Observed and predicted hate crimes",
    subtitle = "Seattle, Washington metro area",
    color=""
  )
grob1 <- arrangeGrob(p1,p2,nrow=1,widths=c(2,3))

ggsave("sidebysidelines.png",
       plot=grob1,
       width=6,
       height=3)

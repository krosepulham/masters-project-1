library(dplyr)
library(ggplot2)
library(readr)
library(usmap)
library(readxl)
library(stringr)
library(purrr)
library(tidyr)

#import data
load("hate_crime.Rda")

#first EDA session--------------------------------------------------------------
#for some reason, the state code for nebraska was written as "NB" rather than 
#"NE" so we'll fix that really quick
hate_crime <- hate_crime%>%
  mutate(STATE_ABBR = ifelse(STATE_ABBR=="NB","NE",STATE_ABBR))

#create a line plot for hate crimes over time for each region
p1 <- ggplot(filter(hate_crime,REGION_NAME!="U.S. Territories"), 
             aes(x=DATA_YEAR,color=REGION_NAME))+
  stat_count(geom="line")+
  labs(title="Annual Hate Crimes",
       x="Year",
       y="Number of Hate Crimes",
       color="Region")
ggsave(filename="Figure_1.jpeg",
       plot=p1,
       device = "jpeg",
       path="~/masters-project/EDA_images",
       height = 5,
       width = 9)

df2 <- hate_crime%>%
  filter(REGION_NAME!="U.S. Territories")%>%
  filter(grepl("Islamic|Muslim|Arab",BIAS_DESC))

p3 <- ggplot(df2, 
             aes(x=DATA_YEAR))+
  stat_count(geom="line")+
  labs(title="Anti-Arab/Muslim Hate Crimes",
       x="Year",
       y="Number of Hate Crimes")
p3
ggsave("Figure_3.jpeg",
       plot=p3,
       path="~/masters-project/EDA_images",
       height=5,
       width=9)

df3 <- hate_crime%>%
  filter(REGION_NAME!="U.S. Territories")%>%
  filter(grepl("Gay|Lesbian|Transgender|Bisexual",BIAS_DESC))

p4 <- ggplot(df3, 
             aes(x=DATA_YEAR))+
  stat_count(geom="line")+
  labs(title="Anti-LGBT Hate Crimes",
       x="Year",
       y="Number of Hate Crimes")
p4
ggsave("Figure_4.jpeg",
       plot=p4,
       path="~/masters-project/EDA_images",
       height=5,
       width=9)

#create data frame with state names and population estimates as of July 1, 2019
population <- read_excel("nst-est2019-01.xlsx", col_names = FALSE, skip = 9)%>%
  select(1,13)%>%
  mutate(state = str_sub(...1, start = 2), pop19 = ...13)%>%
  select(state,pop19)

#plot a heatmap of the estimated ratio of hate crime/ population in 2019
df1 <- hate_crime%>%
  mutate(state=STATE_NAME)%>%
  group_by(state)%>%
  summarise(count=n())%>%
  left_join(population)%>%
  mutate(hate_crime_rate = count/pop19)

p2 <- plot_usmap(regions="states",
           data = df1 ,
           values="hate_crime_rate")+
  labs(title="Reported Hate Crimes per population by State (population estimated in 2019)",
       fill="Hate Crime Rate")

p2

ggsave(filename="Figure_2.jpeg",
       plot=p2,
       device = "jpeg",
       path="~/masters-project/EDA_images",
       height = 5,
       width = 8)

#second EDA session-------------------------------------------------------------
#what are all the unique bias descriptions?
biases <- hate_crime$BIAS_DESC%>%
  unique()%>%
  str_split(pattern=";")%>%
  unlist()%>%
  unique()
biases

racial_biases <- biases[c(1,2,3,4,9,12,13,15,18,26)]
religious_biases <- biases[c(5,6,7,10,14,19,28,29,30,31,32,33,34)]
GSM_biases <- biases[c(8,11,16,17,20,23,25)]
gender_biases <- biases[c(24,27)]
disability_biases <- biases[c(21,22)]

#let's make a function that can check whether or not a given string in 
#BIAS_DESC is in one of the above described groups. 
check_bias <- function(bias_desc, bias_type = c("racial","religious","GSM","gender","disability")){
  is_biased <- FALSE
  
  #pick out list of bias descriptions to check
  if(bias_type=="racial"){
    bias_type_tags <- racial_biases
  }else{
    if(bias_type=="religious"){
      bias_type_tags <- religious_biases
    }else{
      if(bias_type=="GSM"){
        bias_type_tags <- GSM_biases
      }else{
        if(bias_type=="gender"){
          bias_type_tags <- gender_biases
        }else{
          if(bias_type=="disability"){
            bias_type_tags <- disability_biases
          }else{
            warning("Invalid bias type; returning NaN")
            return(NaN)
          }
        }
      }
    }
  }
  
  #list of tags written down for this crime
  bias_tags <- str_split(bias_desc, pattern=";")%>%
    unlist()
  
  #cross reference the two lists (they're not list objects, they're vectors)
  for(i in 1:length(bias_tags)){
    for(j in 1:length(bias_type_tags)){
      if(bias_tags[i]==bias_type_tags[j]){
        is_biased <- TRUE
      }
    }
  }
  
  return(is_biased)
}

#single argument versions of this function for purrr (oh god this has become spaghetti code)
check_bias_racial <- function(x){check_bias(x,"racial")}
check_bias_GSM <- function(x){check_bias(x,"GSM")}
check_bias_religious <- function(x){check_bias(x,"religious")}
check_bias_gender <- function(x){check_bias(x,"gender")}
check_bias_disability <- function(x){check_bias(x,"disability")}

# test code to make sure this function works
# check_bias(hate_crime$BIAS_DESC[673],"racial")
# check_bias(hate_crime$BIAS_DESC[673],"GSM")
# check_bias(hate_crime$BIAS_DESC[671],"racial")
# check_bias(hate_crime$BIAS_DESC[671],"GSM")

#okay let's add some columns to hate_crime

hate_crime <- hate_crime %>%
  mutate(racial_bias = map_lgl(.x=BIAS_DESC,.f=check_bias_racial))%>%
  mutate(GSM_bias = map_lgl(.x=BIAS_DESC,.f=check_bias_GSM))%>%
  mutate(religious_bias = map_lgl(.x=BIAS_DESC,.f=check_bias_religious))%>%
  mutate(gender_bias = map_lgl(.x=BIAS_DESC,.f=check_bias_gender))%>%
  mutate(disability_bias = map_lgl(.x=BIAS_DESC,.f=check_bias_disability))
save(hate_crime,file="hate_crime.Rda")

#ok, let's compare these categories 
sum(hate_crime$GSM_bias) #ok, so there's only 34,000 GSM type hate crimes 
sum(hate_crime$racial_bias)
sum(hate_crime$religious_bias)

#I want an overlapping line graph showing the different categories of 
#hate crimes over time. 
t = unique(hate_crime$DATA_YEAR)
overtime_summary = tibble(year = t,
                          racial_crimes = numeric(length = length(t)),
                          GSM_crimes = numeric(length = length(t)),
                          religious_crimes = numeric(length = length(t)),
                          gender_crimes = numeric(length = length(t)),
                          disability_crimes = numeric(length = length(t))
)
for(i in t){
  df <- filter(hate_crime,DATA_YEAR==i)
  overtime_summary$racial_crimes[i-1990] <- sum(as.numeric(df$racial_bias))
  overtime_summary$GSM_crimes[i-1990] <- sum(as.numeric(df$GSM_bias))
  overtime_summary$religious_crimes[i-1990] <- sum(as.numeric(df$religious_bias))
  overtime_summary$gender_crimes[i-1990] <- sum(as.numeric(df$gender_bias))
  overtime_summary$disability_crimes[i-1990] <- sum(as.numeric(df$disability_bias))
}

#ok, let's FINALLY make a graph out of this thing
overtime_summary%>%
  pivot_longer(2:6,names_to = "Bias_group",values_to = "number_of_crimes")%>%
  ggplot()+
  geom_line(aes(x=year,y=number_of_crimes,color=Bias_group))
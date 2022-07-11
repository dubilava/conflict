library(data.table)
library(ggplot2)
library(fixest)
library(modelsummary)
library(backports)

# Preliminaries ----

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

# Data Management ----

## load the data
load("data_violence_acled.RData")


## aggregate by cell
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

## calculate the number of cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)
dataset_dt <- merge(dataset_dt,xysum_dt,by="country",all.x = T)

### descriptive stats
xy_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area))]
xy_dt[max_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area))]

## aggregate by cell-actor
xy_actor_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(actor,country,xy)]

### descriptive stats
xy_actor_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area)),by=.(actor)]
xy_actor_dt[max_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area)),by=.(actor)]



# Table 1: Main results----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/baseline.docx")


# Table 2: Mechanism - weather ----

load("precipitation.RData")
load("temperature.RData")

rain_dt <- rain_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,rain=as.numeric(rain))]

temp_dt <- temp_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,temp=as.numeric(temp),days=as.numeric(days))]

dataset1_dt <- merge(dataset_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset2_dt <- merge(dataset1_dt,temp_dt,by=c("year","mo","longitude","latitude"),all.x=T)

season_dt <- datacomb_dt[,.(longitude,latitude,mo,planted,season)]
season_dt <- unique(season_dt)

datarain_dt <- merge(rain_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)
datarain_dt[is.na(rain)]$rain <- 0

datatemp_dt <- merge(temp_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)
datatemp_dt[is.na(temp)]$temp <- 0
datatemp_dt[is.na(days)]$days <- 0

datarain_dt <- unique(datarain_dt)
datatemp_dt <- unique(datatemp_dt)

subset_dt <- merge(datarain_dt,datatemp_dt,by=c("year","mo","longitude","latitude","planted","season"))

# number of months in the growing season
subset_dt[,`:=`(gsm=ifelse(as.numeric(as.character(season))-as.numeric(as.character(planted))<0,12-(as.numeric(as.character(season))-as.numeric(as.character(planted))+12),12-(as.numeric(as.character(season))-as.numeric(as.character(planted)))))]

subset_dt <- subset_dt[order(longitude,latitude,year,mo)]

subset_dt[season==0]$gsm <- 0

# select data on harvest months (not using anymore)
harvest_dt <- subset_dt[season == 1]
harvest_dt$myr <- harvest_dt$year

harvest_dt <- harvest_dt[,.(year,myr,longitude,latitude,planted,season,gsm)]

# select data on planted months
planted_dt <- subset_dt[planted == 1]
planted_dt$myr <- planted_dt$year

planted_dt <- planted_dt[,.(year,myr,longitude,latitude,planted,season,gsm)]

# merge the weather data with the growing season data
submerge_dt <- merge(subset_dt,planted_dt,by=c("year","longitude","latitude","planted","season","gsm"),all.x=T)
submerge_dt <- submerge_dt[order(longitude,latitude,year,mo)]

# fill in the NAs
submerge_dt$myr <- as.numeric(as.character(submerge_dt$myr))
submerge_dt[,myr := nafill(myr,type="locf"),by=.(longitude,latitude)]
submerge_dt[,myr := nafill(myr,type="nocb"),by=.(longitude,latitude)]

# so some other stuff (no longer necessary but may as well keep it around)
submerge_dt$backward <- 12-as.numeric(as.character(submerge_dt$season))+1
submerge_dt$dif <- submerge_dt$gsm-submerge_dt$backward

subseason_dt <- submerge_dt[dif >= 0]

subseason_dt <- subseason_dt[,.(gsrain=sum(rain),gstemp=mean(temp),gsdays=sum(days)),by=.(longitude,latitude,myr)]

subseason_dt <- merge(submerge_dt,subseason_dt,by=c("longitude","latitude","myr"),all.x=T)

# basically this is it, this is the data on rainfall, temperature and extreme degree days
# during the growing season of the major crop in a given cell; the weather variables
# are kept constant for the duration of one calendar year beginning from the
# planting month; this means that for given cell, in a given year, the 
# pre-harvest and post-harvest periods are 'treated' with the same
# growing season weather; this makes the interpretation more straightforward,
# say, when we want to argue that there is some increase in violence
# just before the harvest due to bad weather, or something of that nature.
subseason_dt <- subseason_dt[year!=1996]
subseason_dt$myr <- NULL
subseason_dt$gsm <- NULL
subseason_dt$backward <- NULL
subseason_dt$dif <- NULL

dataset3_dt <- merge(dataset2_dt,subseason_dt,by=c("longitude","latitude","year","mo","planted","season","rain","temp","days"),all.x=T)
dataset3_dt[is.na(gsrain)]$gsrain <- 0
dataset3_dt$gsrain <- dataset3_dt$gsrain*1000
dataset3_dt[is.na(gstemp)]$gstemp <- 0
dataset3_dt[is.na(gsdays)]$gsdays <- 0


dataset3_dt[,`:=`(gsrain_mean=mean(gsrain),gsrain_sd=sd(gsrain),gsrain_lo=quantile(gsrain,.1),gsrain_mlo=quantile(gsrain,.3),gsrain_med=quantile(gsrain,.5),gsrain_mhi=quantile(gsrain,.7),gsrain_hi=quantile(gsrain,.9),gsdays_mean=mean(gsdays),gsdays_sd=sd(gsdays),gsdays_lo=quantile(gsdays,.1),gsdays_mlo=quantile(gsdays,.3),gsdays_med=quantile(gsdays,.5),gsdays_mhi=quantile(gsdays,.7),gsdays_hi=quantile(gsdays,.9),gstemp_mean=mean(gstemp),gstemp_sd=sd(gstemp),gstemp_lo=quantile(gstemp,.1),gstemp_mlo=quantile(gstemp,.3),gstemp_med=quantile(gstemp,.5),gstemp_mhi=quantile(gstemp,.7),gstemp_hi=quantile(gstemp,.9)),by=.(xy,actor)]

dataset3_dt[,`:=`(gsrain_z=(gsrain-gsrain_mean)/gsrain_sd,gsdays_z=(gsdays-gsdays_mean)/gsdays_sd,gstemp_z=(gstemp-gstemp_mean)/gstemp_sd)]
dataset3_dt[is.na(gsrain_z)]$gsrain_z <- 0
dataset3_dt[is.na(gsdays_z)]$gsdays_z <- 0
dataset3_dt[is.na(gstemp_z)]$gstemp_z <- 0


# regressions
coefrain1_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsrain_z+1)+log(population_mln) | xy+country^year, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coefrain2_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsrain_z)+log(population_mln) | xy+country^year, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coefrain3_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsrain_z-1)+log(population_mln) | xy+country^year, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)


coeftemp1_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsdays_z+1)+log(population_mln) | xy+country^year, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coeftemp2_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsdays_z)+log(population_mln) | xy+country^year, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coeftemp3_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsdays_z-1)+log(population_mln) | xy+country^year, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)


modelsummary(list(coefrain1_fe,coefrain2_fe,coefrain3_fe,coeftemp1_fe,coeftemp2_fe,coeftemp3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/weather.docx")


# Table 2: Mechanism - dose-response ----

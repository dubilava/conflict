library(data.table)
library(ggplot2)
library(fixest)
library(modelsummary)
library(backports)
library(fastDummies)
library(kableExtra)

# Preliminaries ----

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## functions to calculate 3-month and 9-month cumulatife impact

pstars <- function(ps){
  p_stars <- ifelse(ps<.01,"***",ifelse(ps<.05,"**",ifelse(ps<.1,"*","")))
  return(p_stars)
}

impact <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  format(round(h_coef,2),nsmall=2)
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

# Data Management ----

## load the data
load("data_violence_acled.RData")

## descriptive statistics ----

### combined data ----

## aggregate by cell
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

## the number of the cells with crop production
xy_dt[crop_area>0] 

## the number of cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)
dataset_dt <- merge(dataset_dt,xysum_dt,by="country",all.x = T)

## number of incidents and incidence inc the combined data
incidents_comb <- datacomb_dt[,.(incidents=sum(incidents),incidence=sum(incidents_dum),incidents_dum=mean(incidents_dum),cropland=mean(crop_area))]

incidents_cond_comb <- datacomb_dt[crop_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(crop_area))]


### actor-specific data ----

## aggregate by cell-actor
xy_actor_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(actor,country,xy)]

## number of incidents and incidence inc the actor-specific data
incidents_actor <- dataset_dt[,.(incidents=sum(incidents),incidence=sum(incidents_dum),incidents_dum=mean(incidents_dum),cropland=mean(crop_area)),by=.(actor)]

incidents_cond_actor <- dataset_dt[crop_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(crop_area)),by=.(actor)]


# Table 1: Descriptive Statistics ----

uncond <- rbind(incidents_actor[,.(incidents,incidence,incidents_dum)],incidents_comb[,.(incidents,incidence,incidents_dum)])

cond <- rbind(incidents_cond_actor[,.(incidents,incidents_dum)],incidents_cond_comb[,.(incidents,incidents_dum)])

incidents <- uncond$incidents
incidents_prop <- round(incidents/incidents[length(incidents)]*100,1)
incidence <- uncond$incidence
incidence_uncond <- round(uncond$incidents_dum*100,1)
incidence_cond <- round(cond$incidents_dum*100,1)

tab1 <- Reduce(rbind,list(incidents,incidents_prop,incidence,incidence_uncond,incidence_cond))

colnames(tab1) <- c("state","rebel","polit","ident","milit","comb")
rownames(tab1) <- c("incidents","proportion","incidence","unconditional","conditional")

kable_styling(kable(tab1))


# Table 2: Main results----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- MILITIAS COMBINED --###
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/baseline.docx")


## cumulative impact ----

## combined data
dum_dt <- dummy_cols(datacomb_dt[,.(xy,date,season)],select_columns = "season")
dum_dt$season <- NULL
dum_dt$season_0 <- NULL

datacomb_dt <- merge(datacomb_dt,dum_dt,by=c("xy","date"))

## re-specify some seasons to ensure that cumulative impact is directly obtained
datacomb_dt[,`:=`(season_2=season_2-season_1,season_3=season_3-season_1,season_4=season_4-season_12,season_5=season_5-season_12,season_6=season_6-season_12,season_7=season_7-season_12,season_8=season_8-season_12,season_9=season_9-season_12,season_10=season_10-season_12,season_11=season_11-season_12)]

## actor specific data
dum_dt <- dummy_cols(dataset_dt[,.(actor,xy,date,season)],select_columns = "season")
dum_dt$season <- NULL
dum_dt$season_0 <- NULL

dataset_dt <- merge(dataset_dt,dum_dt,by=c("actor","xy","date"))

## re-specify some seasons to ensure that cumulative impact is directly obtained
dataset_dt[,`:=`(season_2=season_2-season_1,season_3=season_3-season_1,season_4=season_4-season_12,season_5=season_5-season_12,season_6=season_6-season_12,season_7=season_7-season_12,season_8=season_8-season_12,season_9=season_9-season_12,season_10=season_10-season_12,season_11=season_11-season_12)]


c_state <- impact(dataset_dt[actor=="state"])
c_rebel <- impact(dataset_dt[actor=="rebel"])
c_polit <- impact(dataset_dt[actor=="polit"])
c_ident <- impact(dataset_dt[actor=="ident"])
c_milit <- impact(dataset_dt[actor=="milit"])
c_comb <- impact(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Table 3: Mechanism - weather ----

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

# these are the data on rainfall, temperature and extreme degree days
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


## rainfall
coefrain1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## heat days
coeftemp1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

modelsummary(list(coefrain1_fe,coefrain2_fe,coefrain3_fe,coeftemp1_fe,coeftemp2_fe,coeftemp3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/weather.docx")


# Appendix Tables B34-B39 ----

## robustness state ----
coefrain1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)

coefrain3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)

coeftemp1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)

modelsummary(list(coefrain1_fe,coefrain2_fe,coefrain3_fe,coeftemp1_fe,coeftemp2_fe,coeftemp3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/weather_state.docx")


## robustness rebel ----
coefrain1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)

coeftemp1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)

modelsummary(list(coefrain1_fe,coefrain2_fe,coefrain3_fe,coeftemp1_fe,coeftemp2_fe,coeftemp3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/weather_rebel.docx")


## robustness polit ----
coefrain1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coeftemp1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)

modelsummary(list(coefrain1_fe,coefrain2_fe,coefrain3_fe,coeftemp1_fe,coeftemp2_fe,coeftemp3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/weather_polit.docx")


## robustness ident ----
coefrain1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)

coeftemp1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z+1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z-1)+log(population_mln) | xy+country^year+mo, dataset3_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)

modelsummary(list(coefrain1_fe,coefrain2_fe,coefrain3_fe,coeftemp1_fe,coeftemp2_fe,coeftemp3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/weather_ident.docx")


## robustness comb ----
datacomb1_dt <- merge(datacomb_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
datacomb2_dt <- merge(datacomb1_dt,temp_dt,by=c("year","mo","longitude","latitude"),all.x=T)

datacomb3_dt <- merge(datacomb2_dt,subseason_dt,by=c("longitude","latitude","year","mo","planted","season","rain","temp","days"),all.x=T)
datacomb3_dt[is.na(gsrain)]$gsrain <- 0
datacomb3_dt$gsrain <- datacomb3_dt$gsrain*1000
datacomb3_dt[is.na(gstemp)]$gstemp <- 0
datacomb3_dt[is.na(gsdays)]$gsdays <- 0

datacomb3_dt[,`:=`(gsrain_mean=mean(gsrain),gsrain_sd=sd(gsrain),gsrain_lo=quantile(gsrain,.1),gsrain_mlo=quantile(gsrain,.3),gsrain_med=quantile(gsrain,.5),gsrain_mhi=quantile(gsrain,.7),gsrain_hi=quantile(gsrain,.9),gsdays_mean=mean(gsdays),gsdays_sd=sd(gsdays),gsdays_lo=quantile(gsdays,.1),gsdays_mlo=quantile(gsdays,.3),gsdays_med=quantile(gsdays,.5),gsdays_mhi=quantile(gsdays,.7),gsdays_hi=quantile(gsdays,.9),gstemp_mean=mean(gstemp),gstemp_sd=sd(gstemp),gstemp_lo=quantile(gstemp,.1),gstemp_mlo=quantile(gstemp,.3),gstemp_med=quantile(gstemp,.5),gstemp_mhi=quantile(gstemp,.7),gstemp_hi=quantile(gstemp,.9)),by=.(xy)]

datacomb3_dt[,`:=`(gsrain_z=(gsrain-gsrain_mean)/gsrain_sd,gsdays_z=(gsdays-gsdays_mean)/gsdays_sd,gstemp_z=(gstemp-gstemp_mean)/gstemp_sd)]
datacomb3_dt[is.na(gsrain_z)]$gsrain_z <- 0
datacomb3_dt[is.na(gsdays_z)]$gsdays_z <- 0
datacomb3_dt[is.na(gstemp_z)]$gstemp_z <- 0


coefrain1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z+1)+log(population_mln) | xy+country^year+mo, datacomb3_dt,vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z)+log(population_mln) | xy+country^year+mo, datacomb3_dt,vcov=conley(500,distance="spherical")~longitude+latitude)
coefrain3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsrain_z-1)+log(population_mln) | xy+country^year+mo, datacomb3_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

coeftemp1_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z+1)+log(population_mln) | xy+country^year+mo, datacomb3_dt,vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp2_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z)+log(population_mln) | xy+country^year+mo, datacomb3_dt,vcov=conley(500,distance="spherical")~longitude+latitude)
coeftemp3_fe <- feols(incidents_dum~price_ch:crop_area:i(season,keep=1:12)+price_ch:crop_area:i(season,keep=1:12):I(gsdays_z-1)+log(population_mln) | xy+country^year+mo, datacomb3_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

modelsummary(list(coefrain1_fe,coefrain2_fe,coefrain3_fe,coeftemp1_fe,coeftemp2_fe,coeftemp3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/weather_comb.docx")


# adjusted impact functions for rainfall
impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(gsrain_z+1)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

impact2 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(gsrain_z)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

impact3 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(gsrain_z-1)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}


p1_state <- impact1(dataset3_dt[actor=="state"])
p2_state <- impact2(dataset3_dt[actor=="state"])
p3_state <- impact3(dataset3_dt[actor=="state"])

p1_rebel <- impact1(dataset3_dt[actor=="rebel"])
p2_rebel <- impact2(dataset3_dt[actor=="rebel"])
p3_rebel <- impact3(dataset3_dt[actor=="rebel"])

p1_polit <- impact1(dataset3_dt[actor=="polit"])
p2_polit <- impact2(dataset3_dt[actor=="polit"])
p3_polit <- impact3(dataset3_dt[actor=="polit"])

p1_ident <- impact1(dataset3_dt[actor=="ident"])
p2_ident <- impact2(dataset3_dt[actor=="ident"])
p3_ident <- impact3(dataset3_dt[actor=="ident"])

p1_milit <- impact1(dataset3_dt[actor=="milit"])
p2_milit <- impact2(dataset3_dt[actor=="milit"])
p3_milit <- impact3(dataset3_dt[actor=="milit"])

p1_comb <- impact1(datacomb3_dt)
p2_comb <- impact2(datacomb3_dt)
p3_comb <- impact3(datacomb3_dt)


# adjusted impact functions for heat days
impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(gsdays_z+1)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

impact2 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(gsdays_z)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

impact3 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(gsdays_z-1)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}


h1_state <- impact1(dataset3_dt[actor=="state"])
h2_state <- impact2(dataset3_dt[actor=="state"])
h3_state <- impact3(dataset3_dt[actor=="state"])

h1_rebel <- impact1(dataset3_dt[actor=="rebel"])
h2_rebel <- impact2(dataset3_dt[actor=="rebel"])
h3_rebel <- impact3(dataset3_dt[actor=="rebel"])

h1_polit <- impact1(dataset3_dt[actor=="polit"])
h2_polit <- impact2(dataset3_dt[actor=="polit"])
h3_polit <- impact3(dataset3_dt[actor=="polit"])

h1_ident <- impact1(dataset3_dt[actor=="ident"])
h2_ident <- impact2(dataset3_dt[actor=="ident"])
h3_ident <- impact3(dataset3_dt[actor=="ident"])

h1_milit <- impact1(dataset3_dt[actor=="milit"])
h2_milit <- impact2(dataset3_dt[actor=="milit"])
h3_milit <- impact3(dataset3_dt[actor=="milit"])

h1_comb <- impact1(datacomb3_dt)
h2_comb <- impact2(datacomb3_dt)
h3_comb <- impact3(datacomb3_dt)


kable_styling(kable(data.table(p1_state$effect,p2_state$effect,p3_state$effect,h1_state$effect,h2_state$effect,h3_state$effect)))

kable_styling(kable(data.table(p1_rebel$effect,p2_rebel$effect,p3_rebel$effect,h1_rebel$effect,h2_rebel$effect,h3_rebel$effect)))

kable_styling(kable(data.table(p1_polit$effect,p2_polit$effect,p3_polit$effect,h1_polit$effect,h2_polit$effect,h3_polit$effect)))

kable_styling(kable(data.table(p1_ident$effect,p2_ident$effect,p3_ident$effect,h1_ident$effect,h2_ident$effect,h3_ident$effect)))

kable_styling(kable(data.table(p1_milit$effect,p2_milit$effect,p3_milit$effect,h1_milit$effect,h2_milit$effect,h3_milit$effect)))

kable_styling(kable(data.table(p1_comb$effect,p2_comb$effect,p3_comb$effect,h1_comb$effect,h2_comb$effect,h3_comb$effect)))






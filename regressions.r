library(data.table)
library(ggplot2)
library(cowplot)
library(stringr)
library(fixest)
library(backports)
library(stargazer)

rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

# load dataset
load("dataset.RData")

## to re-estimate for 'violence against civilians' only
# dataset_dt <- dataset_dt[event=="Violence"]

## combine the two events into one
dataset_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,date,yearmo,mo,month,actor,crop,tot_area,max_area,plant,season_srt,season,season_end,price,price_ch,price_d,price_maize,price_sorghum,price_wheat,price_rice,population)]

dataset_dt[,`:=` (population_mln = population/1000000)]

dataset_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0),plant_dum=ifelse(plant==1,1,0))]

## aggregate actors (interactions) into the events only
datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,date,yearmo,mo,month,crop,tot_area,max_area,plant,season_srt,season,season_end,price,price_ch,price_d,price_maize,price_sorghum,price_wheat,price_rice,population,population_mln)]

datacomb_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

## aggregate by harvest season
seasonal_dt <- datacomb_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]

seasonal_dt <- seasonal_dt[order(season)]

## aggregate all
annual_dt <- aggregate_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]

## aggregate by location-year
aggregate_dt <- datacomb_dt[,.(incidents=sum(incidents),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy,year)]

aggregate_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

## aggregate by location
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]


## baseline annual
base_fe <- feols(incidents_pop~price_ch:max_area | xy+year, aggregate_dt,se="cluster",weights=~population_mln)
summary(base_fe)

100*sd(datacomb_dt[max_area>0]$price_ch)*(summary(base_fe)$coefficients/annual_dt$incidents_pop)*annual_dt$max_area


### baseline monthly
##----- AGGREGATE -----##
base_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(base_fe)

##--- DISAGGREGATED ---###

base1_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, dataset_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(base1_fe)

base2_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, dataset_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(base2_fe)

base3_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, dataset_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(base3_fe)

base4_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, dataset_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(base4_fe)

base5_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, dataset_dt[actor=="other"],se="cluster",weights=~population_mln)
summary(base5_fe)


### seasonal monthly
##----- AGGREGATE -----###
harv_fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_fe)

##--- DISAGGREGATED ---###

harv1_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv1_fe)

harv2_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv2_fe)

harv3_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv3_fe)

harv4_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv4_fe)

harv5_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="other"],se="cluster",weights=~population_mln)
summary(harv5_fe)


100*sd(datacomb_dt[max_area>0]$price_ch)*(summary(harv_fe)$coefficients/seasonal_dt$incidents_pop)*seasonal_dt$max_area


sum(100*sd(datacomb_dt[max_area>0]$price_ch)*summary(harv_fe)$coefficients*(seasonal_dt$max_area/seasonal_dt$incidents_pop))



## print to latex
etable(base_fe,base1_fe,base2_fe,base3_fe,base4_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("All","State","Rebels","Political","Identity"))

etable(harv_fe,harv1_fe,harv2_fe,harv3_fe,harv4_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("All","State","Rebels","Political","Identity"))













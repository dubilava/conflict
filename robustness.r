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

load("dataset.RData")

## for now, only violence... may use riots later on
# dataset_dt <- dataset_dt[event=="Violence"]

## combine the two events into one
dataset_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,date,yearmo,mo,month,actor,crop,tot_area,max_area,plant,season_srt,season,season_end,price,price_ch,price_d,price_maize,price_sorghum,price_wheat,price_rice,population)]

dataset_dt[,`:=` (population_mln = population/1000000)]

dataset_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0),agri=ifelse(tot_area>.01,1,0),plant_dum=ifelse(plant==1,1,0))]

## combine all interactions into the events only
datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,date,yearmo,mo,month,crop,tot_area,max_area,plant,season_srt,season,season_end,plant_dum,price,price_ch,price_d,price_maize,price_sorghum,price_wheat,price_rice,population,population_mln)]

datacomb_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0),trend=as.numeric(as.factor(yearmo)))]


### different fixed effects
base_mfe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)

base_r1fe <- feols(incidents_pop~price_ch:max_area+i(trend,country) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)

base_r2fe <- feols(incidents_pop~price_ch:max_area | country+yearmo, datacomb_dt,se="cluster",weights=~population_mln)

base_r3fe <- feols(incidents_pop~price_ch:max_area | country+year+mo, datacomb_dt,se="cluster",weights=~population_mln)

base_r4fe <- feols(incidents_pop~price_ch:max_area | country+year, datacomb_dt,se="cluster",weights=~population_mln)


harv_mfe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)

harv_r1fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0)+i(trend,country) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)

harv_r2fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | country+yearmo, datacomb_dt,se="cluster",weights=~population_mln)

harv_r3fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | country+year+mo, datacomb_dt,se="cluster",weights=~population_mln)

harv_r4fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | country+year, datacomb_dt,se="cluster",weights=~population_mln)


## print the latex tables
etable(base_mfe,base_r1fe,base_r2fe,base_r3fe,base_r4fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Robustness Checks",subtitles = c("Main","Trend","Country","YearMo","Year"))

etable(harv_mfe,harv_r1fe,harv_r2fe,harv_r3fe,harv_r4fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Robustness Checks",subtitles = c("Main","Trend","Country","YearMo","Year"))


## data subsetting
base1_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt[latitude<=23.5],se="cluster",weights=~population_mln)
summary(base1_fe)

base2_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt[population_mln>=0.1],se="cluster",weights=~population_mln)
summary(base2_fe)

base3_fe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt[latitude<=23.5 & population_mln>=0.1],se="cluster",weights=~population_mln)
summary(base3_fe)


harv1_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, datacomb_dt[latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv1_fe)

harv2_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, datacomb_dt[population_mln>=0.1],se="cluster",weights=~population_mln)
summary(harv2_fe)

harv3_fe <- feols(incidents_pop~price_ch:max_area:(i(season,drop=0)) | xy+yearmo, datacomb_dt[latitude<=23.5 & population_mln>=0.1],se="cluster",weights=~population_mln)
summary(harv3_fe)


## print the latex tables
etable(base1_fe,base2_fe,base3_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("Sub","Pop","SubPop"))

etable(harv1_fe,harv2_fe,harv3_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("Sub","Pop","SubPop"))


library(data.table)
library(fixest)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the data
load("violence.RData")

## monthly aggregate
monthly_dt <- datacomb_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
## aggregates by harvest season (for croplands defined in two different ways)
seasonal_dt <- datacomb_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal_dt <- seasonal_dt[order(season)]

seasonaldum_dt <- datacomb_dt[area_dum==1,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonaldum_dt <- seasonaldum_dt[order(season)]

## aggregate by location-year
aggregate_dt <- datacomb_dt[,.(incidents=sum(incidents),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),area_dum=mean(area_dum),population_mln=mean(population_mln)),by=.(country,xy,year)]
aggregate_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

## aggregate by location
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

## aggregate
annual_dt <- aggregate_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]

## number of grid cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

dataset_dt <- merge(dataset_dt,xysum_dt,by="country",all.x = T)
datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)


#################################
###  seasonality of conflict  ###
#################################

###-- ALL ACTORS --###

harv_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_fe,cluster=~xy)

## percentage effect relative to the baseline conflict
100*.25*(summary(harv_fe)$coefficients/seasonal_dt$incidents_pop)


##--- SEPARATE ACTORS ---###

harv1_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv1_fe,cluster=~xy)

harv2_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv2_fe,cluster=~xy)

harv3_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv3_fe,cluster=~xy)

harv4_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv4_fe,cluster=~xy)

## bonferroni corrected p values
round(cbind(harv_fe$coeftable[,4],harv1_fe$coeftable[,4],harv2_fe$coeftable[,4],harv3_fe$coeftable[,4],harv4_fe$coeftable[,4])*5,3)

## print to latex
etable(harv_fe,harv1_fe,harv2_fe,harv3_fe,harv4_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("All","State","Rebels","Political","Identity"))

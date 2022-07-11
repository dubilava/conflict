library(data.table)
library(fixest)
library(modelsummary)
library(splines)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the data
load("data_violence_acled.RData")


# Appendix Table 1: Fixed Effects ----

coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+yearmo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^trend, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust1.docx")


# Appendix Table 2: Inference ----

coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(200,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(800,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"], vcov=~xy)

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"], vcov=~xy+country^year)

## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust2.docx")


# Appendix Table 3: Years ----

coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & year %in% 1997:2008],vcov=conley(500,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & year %in% 2001:2012],vcov=conley(500,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & year %in% 2005:2016],vcov=conley(500,distance="spherical")~longitude+latitude)

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & year %in% 2009:2020],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust3.docx")


# Appendix Table 4: Latitudes ----

coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & latitude <=22],vcov=conley(500,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & (latitude >22 | latitude <= 2)],vcov=conley(500,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & (latitude >2 | latitude <= -12)],vcov=conley(500,distance="spherical")~longitude+latitude)

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & latitude > -12],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust4.docx")


# Appendix Table 5: Other Crops ----

africa <- ne_countries(continent = "africa",returnclass = "sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

cocoa <- raster(paste("Local/Crops/cocoa_HarvAreaYield_Geotiff/cocoa_HarvestedAreaFraction.tif",sep=""))

area10 <- aggregate(cocoa,fact=12,fun=mean)
raster_mask <- mask(area10,africa)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
a_dt <- data.table(rm)
colnames(a_dt) <- c("longitude","latitude","cocoa_area")
cocoa_dt <- a_dt[order(longitude,latitude)]

coffee <- raster(paste("Local/Crops/coffee_HarvAreaYield_Geotiff/coffee_HarvestedAreaFraction.tif",sep=""))

area10 <- aggregate(coffee,fact=12,fun=mean)
raster_mask <- mask(area10,africa)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
a_dt <- data.table(rm)
colnames(a_dt) <- c("longitude","latitude","coffee_area")
coffee_dt <- a_dt[order(longitude,latitude)]


cash_dt <- Reduce(function(...) merge(...,all=T,by=c("longitude","latitude")),list(cocoa_dt,coffee_dt))

cash_dt[,`:=`(cash_area=apply(cash_dt[,-c(1:2)],1,max))]
cash_dt[,`:=`(Cocoa=ifelse(cocoa_area==cash_area & cocoa_area>0,cocoa_area,0),Coffee=ifelse(coffee_area==cash_area & coffee_area>0,coffee_area,0))]


dataset_dt <- merge(dataset_dt,cash_dt,by=c("longitude","latitude"),all.x=T)


dataset_dt[,`:=`(cash_more=ifelse(cash_area>max_area,1,0))]
dataset_dt[,`:=`(cocoa_more=ifelse(cocoa_area>max_area,1,0))]
dataset_dt[,`:=`(coffee_more=ifelse(coffee_area>max_area,1,0))]
dataset_dt[,`:=`(cash_some=ifelse(cash_area>0,1,0))]
dataset_dt[,`:=`(cocoa_some=ifelse(cocoa_area>0,1,0))]
dataset_dt[,`:=`(coffee_some=ifelse(coffee_area>0,1,0))]


load("Local/second.RData")

colnames(second_dt) <- c("longitude","latitude","area2","crop2")

dataset_dt <- merge(dataset_dt,second_dt[,.(longitude,latitude,crop2)],by=c("longitude","latitude"),all.x=T)
dataset_dt[is.na(crop2)]$crop2 <- "None"


dataset_dt[,`:=`(major_crop=ifelse(tot_area>0 & max_area/tot_area>.8,"Unique",ifelse(tot_area>0,"Mixed","None")))]


coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & cash_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & cash_some==0],vcov=conley(500,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & crop2=="None"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & major_crop%in%c("Unique","None")],vcov=conley(500,distance="spherical")~longitude+latitude)


## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust5.docx")


# Appendix Table 6: Hotspots ----

xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(xy,country)]
xycomb_dt <- xycomb_dt[order(-incidents,country)]

xycomb_dt[,`:=`(incidents_csum=cumsum(incidents))]
xycomb_dt[,`:=`(incidents_pcnt=incidents_csum/sum(incidents))]

xy_comb20incid <- xycomb_dt[1:9]$xy
xy_comb01cells <- xycomb_dt[1:25]$xy
xy_comb50incid <- xycomb_dt[1:65]$xy

countrycomb_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
countrycomb_dt <- countrycomb_dt[order(-incidents,country)]
country_comb06 <- countrycomb_dt[1:6]$country


coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & xy %!in% xy_comb20incid],vcov=conley(500,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & xy %!in% xy_comb01cells],vcov=conley(500,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & xy %!in% xy_comb50incid],vcov=conley(500,distance="spherical")~longitude+latitude)

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit" & country %!in% country_comb06],vcov=conley(500,distance="spherical")~longitude+latitude)


modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust6.docx")


# Appendix Table 7: Lags & Leads ----

dataset_dt[,`:=`(price_chl2=data.table::shift(price_ch,12,type="lag"),price_chl1=data.table::shift(price_ch,6,type="lag"),price_chf1=data.table::shift(price_ch,6,type="lead"),price_chf2=data.table::shift(price_ch,12,type="lead")),by=.(xy,actor)]

dataset_dt$shock <- dataset_dt$price_ch
coef0_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

dataset_dt$shock <- dataset_dt$price_chl2
coef1_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

dataset_dt$shock <- dataset_dt$price_chl1
coef2_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

dataset_dt$shock <- dataset_dt$price_chf1
coef3_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

dataset_dt$shock <- dataset_dt$price_chf2
coef4_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust7.docx")


# Appendix Table 8: Seasonality ----

load("precipitation.RData")

rain_dt <- rain_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,rain=as.numeric(rain))]

datacomb_dt <- merge(datacomb_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)

dataset_dt[,`:=`(season1=as.factor(as.numeric(as.character(season))))]

coef0_fe <- feols(incidents_dum~price_ch:max_area:(i(season1,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season1,keep=1:12))+log(population_mln) | xy+country^year+country^mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season1,keep=1:12))+log(population_mln)+rain | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season1,keep=1:12))+log(population_mln)+rain | xy+country^year+country^mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

dataset_dt[,`:=`(season1=as.factor(as.numeric(as.character(mo))))]

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season1,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust8.docx")


# Appendix Table 9: Intensity ----

dataset_dt[,`:=`(incidents1=ifelse(fatalities>=1,incidents,0),incidents10=ifelse(incidents>=10,10,incidents))]
dataset_dt[,`:=`(incidents2=ifelse(incidents1>=10,10,incidents1))]

coef0_fe <- feols(incidents~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef1_fe <- feols(incidents~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit" & incidents<=10],vcov=conley(500,distance="spherical")~longitude+latitude)

coef2_fe <- feols(incidents10~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef3_fe <- feols(incidents1~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+yearmo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

coef4_fe <- feols(incidents2~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^trend, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust9.docx")



# Appendix Table 10: Datasets ----

acled_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)


load("data_violence_ucdp.RData")

###-- ALL ACTORS COMBINED --###
ucdp0_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
ucdp1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
ucdp2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="nonstate"],vcov=conley(500,distance="spherical")~longitude+latitude)


## print out the table of results
modelsummary(list(acled_fe,ucdp0_fe,ucdp1_fe,ucdp2_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust10.docx")


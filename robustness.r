library(data.table)
library(fixest)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the data
load("violence.RData")

#-------------------------------#
#--  Table T1: Fixed Effects  --#
#-------------------------------#

harv_mfe <- feols(incidents_pop~price_ch:area_dum:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_mfe,cluster=~xy)

harv_r1fe <- feols(incidents_pop~price_ch:area_dum:i(season,drop=0) | xy+year, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_r1fe,cluster=~xy)

harv_r2fe <- feols(incidents_pop~price_ch:area_dum:i(season,drop=0)+i(trend,country) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_r2fe,cluster=~xy)

harv_r3fe <- feols(incidents_pop~price_ch:area_dum:i(season,drop=0)+i(trend,country) | xy+year, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_r3fe,cluster=~xy)

harv_r4fe <- feols(incidents_pop~price_ch:area_dum:i(season,drop=0) | xy+country^year, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_r4fe,cluster=~xy)

# bonferroni corrected p values
round(cbind(harv_mfe$coeftable[,4],harv_r1fe$coeftable[,4],harv_r2fe$coeftable[52:63,4],harv_r3fe$coeftable[52:63,4],harv_r4fe$coeftable[,4])*5,3)

## print the latex tables
etable(harv_mfe,harv_r1fe,harv_r2fe,harv_r3fe,harv_r4fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Robustness Checks",subtitles = c("Main","Year","Trend","TrendYear","CountryYear"))


#------------------------------#
#--  Table T2: Data Subsets  --#
#------------------------------#

## number of incidents, grid-cells with incidents, and average population by country
xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),max_area=mean(max_area),tot_area=mean(tot_area),area_dum=mean(area_dum),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country)]

xyconflict_dt <- xycomb_dt[incidents>0]

cycomb_dt <- xyconflict_dt[,.(xy_sum=.N,incidents_sum=sum(incidents),population_sum=sum(population_mln)),by=.(country)]

datacomb_dt <- merge(datacomb_dt,cycomb_dt,by="country",all.x=T)


harv1_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt[latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv1_fe)

harv2_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt[population_mln>=0.05],se="cluster",weights=~population_mln)
summary(harv2_fe)

harv3_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt[xy_sum>=10],se="cluster",weights=~population_mln)
summary(harv3_fe)

harv4_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt[incidents_sum>=750],se="cluster",weights=~population_mln)
summary(harv4_fe)

harv5_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt[population_sum>=1 & population_sum<100],se="cluster",weights=~population_mln)
summary(harv5_fe)

# bonferroni corrected p values
round(cbind(harv1_fe$coeftable[,4],harv2_fe$coeftable[,4],harv3_fe$coeftable[,4],harv4_fe$coeftable[,4],harv5_fe$coeftable[,4])*5,3)

## print the latex tables
etable(harv1_fe,harv2_fe,harv3_fe,harv4_fe,harv5_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("Sub","Pop","Conf_xy","Conf_n","Pop_country"))


#--------------------------------#
#--  Table T5: Lags and Leads  --#
#--------------------------------#

datacomb_dt[,`:=` (price_chl2=data.table::shift(price_ch,24,type="lag"),price_chl1=data.table::shift(price_ch,12,type="lag"),price_chf1=data.table::shift(price_ch,12,type="lead"),price_chf2=data.table::shift(price_ch,24,type="lead")),by=.(xy)]

datacomb_dt[,`:=` (price_shk=price_chl2)]
harvl2_fe <- feols(incidents_pop~(price_shk):area_dum:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harvl2_fe,cluster=~xy)

datacomb_dt[,`:=` (price_shk=price_chl1)]
harvl1_fe <- feols(incidents_pop~(price_shk):area_dum:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harvl1_fe,cluster=~xy)

datacomb_dt[,`:=` (price_shk=price_ch)]
harv_fe <- feols(incidents_pop~(price_shk):area_dum:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_fe,cluster=~xy)

datacomb_dt[,`:=` (price_shk=price_chf1)]
harvf1_fe <- feols(incidents_pop~(price_shk):area_dum:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harvf1_fe,cluster=~xy)

datacomb_dt[,`:=` (price_shk=price_chf2)]
harvf2_fe <- feols(incidents_pop~(price_shk):area_dum:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harvf2_fe,cluster=~xy)

# bonferroni corrected p values
round(cbind(harvl2_fe$coeftable[,4],harvl1_fe$coeftable[,4],harv_fe$coeftable[,4],harvf1_fe$coeftable[,4],harvf2_fe$coeftable[,4])*5,3)

## print the latex tables
etable(harvl2_fe,harvl1_fe,harv_fe,harvf1_fe,harvf2_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Robustness Checks",subtitles = c("Lag2","Lag1","Main","Lead1","Lead2"))


#------------------------------#
#--  Table T6: Sugar Prices  --#
#------------------------------#

harv_fe <- feols(incidents_pop~sugar_ch:area_dum:i(season,drop=0) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_fe,cluster=~xy)

harv1_fe <- feols(incidents_pop~sugar_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv1_fe,cluster=~xy)

harv2_fe <- feols(incidents_pop~sugar_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv2_fe,cluster=~xy)

harv3_fe <- feols(incidents_pop~sugar_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv3_fe,cluster=~xy)

harv4_fe <- feols(incidents_pop~sugar_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv4_fe,cluster=~xy)

# bonferroni corrected p values
round(cbind(harv_fe$coeftable[,4],harv1_fe$coeftable[,4],harv2_fe$coeftable[,4],harv3_fe$coeftable[,4],harv4_fe$coeftable[,4])*5,3)


## print the latex tables
etable(harv_fe,harv1_fe,harv2_fe,harv3_fe,harv4_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Robustness Checks",subtitles = c("Conflict","State","Rebels","Political","Identity"))


#-----------------------------------#
#--  Table T3: Smaller Threshold  --#
#-----------------------------------#

area_trs <- .005

## combine the two events into one
dataset_dt[,`:=` (area_dum=ifelse(max_area>=area_trs,1,0))]
datacomb_dt[,`:=` (area_dum=ifelse(max_area>=area_trs,1,0))]

harv_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_fe,cluster=~xy)

harv1_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv1_fe,cluster=~xy)

harv2_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv2_fe,cluster=~xy)

harv3_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv3_fe,cluster=~xy)

harv4_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv4_fe,cluster=~xy)

# bonferroni corrected p values
round(cbind(harv_fe$coeftable[,4],harv1_fe$coeftable[,4],harv2_fe$coeftable[,4],harv3_fe$coeftable[,4],harv4_fe$coeftable[,4])*5,3)

## print to latex
etable(harv_fe,harv1_fe,harv2_fe,harv3_fe,harv4_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("All","State","Rebels","Political","Identity"))


#----------------------------------#
#--  Table T4: Larger Threshold  --#
#----------------------------------#

area_trs <- .02

## combine the two events into one
dataset_dt[,`:=` (area_dum=ifelse(max_area>=area_trs,1,0))]
datacomb_dt[,`:=` (area_dum=ifelse(max_area>=area_trs,1,0))]

harv_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_fe,cluster=~xy)

harv1_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv1_fe,cluster=~xy)

harv2_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv2_fe,cluster=~xy)

harv3_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv3_fe,cluster=~xy)

harv4_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,drop=0)) | xy+yearmo, dataset_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv4_fe,cluster=~xy)

# bonferroni corrected p values
round(cbind(harv_fe$coeftable[,4],harv1_fe$coeftable[,4],harv2_fe$coeftable[,4],harv3_fe$coeftable[,4],harv4_fe$coeftable[,4])*5,3)

## print to latex
etable(harv_fe,harv1_fe,harv2_fe,harv3_fe,harv4_fe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("All","State","Rebels","Political","Identity"))


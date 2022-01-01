library(data.table)
library(fixest)
library(splines)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the data
load("data_violence_acled.RData")

#---------------------------------------#
#--  Appendix Table 1: Fixed Effects  --#
#---------------------------------------#

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+year, dataset_dt[actor=="polit"],vcov=~xy+country^year)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+yearmo, dataset_dt[actor=="polit"],vcov=~xy+country^year)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^trend, dataset_dt[actor=="polit"],vcov=~xy+country^year)


etable(coef1_fe,coef2_fe,coef3_fe,tex=T,digits=3,digits.stats = 3,title="Fixed Effects",headers=c("1","2","3"))


#------------------------------------#
#--  Appendix Table 2: Clustering  --#
#------------------------------------#

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"], vcov=~xy)

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"], vcov=~latitude)

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"], vcov=conley(500,distance="spherical")~longitude+latitude)


etable(coef1_fe,coef2_fe,coef3_fe,tex=T,digits=3,digits.stats = 3,title="Clustering",headers=c("1","2","3"))


#----------------------------------------#
#--  Appendix Table 3: Parallel Trends  --#
#----------------------------------------#

dataset_dt[,`:=`(price_chl2=data.table::shift(price_ch,12,type="lag"),price_chl1=data.table::shift(price_ch,6,type="lag"),price_chf1=data.table::shift(price_ch,6,type="lead"),price_chf2=data.table::shift(price_ch,12,type="lead")),by=.(xy,actor)]

dataset_dt$shock <- dataset_dt$price_chl2
coef1_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"],vcov=~xy+country^year)
summary(coef1_fe)

dataset_dt$shock <- dataset_dt$price_chl1
coef2_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"],vcov=~xy+country^year)
summary(coef1_fe)

dataset_dt$shock <- dataset_dt$price_chf1
coef3_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"],vcov=~xy+country^year)
summary(coef2_fe)

dataset_dt$shock <- dataset_dt$price_chf2
coef4_fe <- feols(incidents_dum~shock:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"],vcov=~xy+country^year)
summary(coef2_fe)


etable(coef1_fe,coef2_fe,coef3_fe,coef4_fe,tex=T,digits=3,digits.stats = 3,title="Lags and Leads",headers=c("1","2","3","4"))



#-----------------------------------------#
#--  Appendix Table 4: Monthly dummies  --#
#-----------------------------------------#

###-- ALL ACTORS COMBINED --###
comb_fe <- feols(incidents_dum~price_ch:max_area:(i(mo)):area_dum+log(population_mln) | xy+country^year, datacomb_dt,vcov=~xy+country^year)

###-- BY INDIVIDUAL ACTORS --###
split_fe <- feols(incidents_dum~price_ch:max_area:(i(mo)):area_dum+log(population_mln)  | xy+country^year, dataset_dt,split=~actor,vcov=~xy+country^year)

##---

## print to table
etable(comb_fe,split_fe,vcov=~xy+country^year,tex=T,digits=3,digits.stats = 3,title="Monthly dummies",headers=c("All","State","Rebel","Polit","Ident","Other"))


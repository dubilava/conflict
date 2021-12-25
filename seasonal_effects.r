library(data.table)
library(ggplot2)
library(cowplot)
library(fixest)
library(zoo)
library(fastDummies)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the data
load("violence.RData")


# incidents_actor_dt <- dataset_dt[,(incidents=sum(incidents)),by=.(actor)]
# 
# # select locations with all years with at least one conflict incident
# xyyear_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(xy,year)]
# xyyear_dt <- xyyear_dt[incidents>0]
# xyyear_dt[,`:=`(yr_count=.N),by=.(xy)]
# 
# xyyear_dt <- xyyear_dt[yr_count>=12]

## aggregates by actor type (for croplands defined in two different ways)
actor_dt <- dataset_dt[area_dum==1,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
# actor_dt <- actor_dt[order(actor)]

# ## aggregates by actor type (for croplands defined in two different ways)
# actor_conflict_dt <- dataset_dt[area_dum==1 & xy %in% unique(xyyear_dt$xy),.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(actor)]
# actor_conflict_dt <- actor_conflict_dt[order(actor)]


harv1_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,keep=1:12)) | xy+yearmo, dataset_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv1_fe,cluster=~xy)

harv2_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,keep=1:12)) | xy+yearmo, dataset_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv2_fe,cluster=~xy)

harv3_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,keep=1:12)) | xy+yearmo, dataset_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv3_fe,cluster=~xy)

harv4_fe <- feols(incidents_pop~price_ch:area_dum:(i(season,keep=1:12)) | xy+yearmo, dataset_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv4_fe,cluster=~xy)


100*.25*(summary(harv1_fe)$coefficients/actor_dt[actor=="state"]$incidents_pop)
100*.25*(summary(harv2_fe)$coefficients/actor_dt[actor=="rebel"]$incidents_pop)
100*.25*(summary(harv3_fe)$coefficients/actor_dt[actor=="polit"]$incidents_pop)
100*.25*(summary(harv4_fe)$coefficients/actor_dt[actor=="ident"]$incidents_pop)

## obtain cumulative effects for seasons 1--12

new_dt <- dataset_dt

# new_dt$iri_d <- gsub("-","m",new_dt$iri_day)

test_dt <- dummy_cols(new_dt[,.(longitude,latitude,date,season,actor)],select_columns = "season")

new_dt <- merge(new_dt,test_dt,by=c("longitude","latitude","date","season","actor"))
new_dt <- new_dt[order(longitude,latitude,date)]

new_dt[,`:=`(season_02=season_1-season_2,season_13=season_1-season_3,season_23=season_2-season_3,season_14=season_1-season_4,season_24=season_2-season_4,season_34=season_3-season_4,season_15=season_1-season_5,season_25=season_2-season_5,season_35=season_3-season_5,season_45=season_4-season_5,season_16=season_1-season_6,season_26=season_2-season_6,season_36=season_3-season_6,season_46=season_4-season_6,season_56=season_5-season_6,season_17=season_1-season_7,season_27=season_2-season_7,season_37=season_3-season_7,season_47=season_4-season_7,season_57=season_5-season_7,season_67=season_6-season_7,season_18=season_1-season_8,season_28=season_2-season_8,season_38=season_3-season_8,season_48=season_4-season_8,season_58=season_5-season_8,season_68=season_6-season_8,season_78=season_7-season_8,season_19=season_1-season_9,season_29=season_2-season_9,season_39=season_3-season_9,season_49=season_4-season_9,season_59=season_5-season_9,season_69=season_6-season_9,season_79=season_7-season_9,season_89=season_8-season_9,season_110=season_1-season_10,season_210=season_2-season_10,season_310=season_3-season_10,season_410=season_4-season_10,season_510=season_5-season_10,season_610=season_6-season_10,season_710=season_7-season_10,season_810=season_8-season_10,season_910=season_9-season_10,season_111=season_1-season_11,season_211=season_2-season_11,season_311=season_3-season_11,season_411=season_4-season_11,season_511=season_5-season_11,season_611=season_6-season_11,season_711=season_7-season_11,season_811=season_8-season_11,season_911=season_9-season_11,season_1011=season_10-season_11,season_112=season_1-season_12,season_212=season_2-season_12,season_312=season_3-season_12,season_412=season_4-season_12,season_512=season_5-season_12,season_612=season_6-season_12,season_712=season_7-season_12,season_812=season_8-season_12,season_912=season_9-season_12,season_1012=season_10-season_12,season_1112=season_11-season_12)]


#-------------------------------------------------#
#--  season specific regressions across actors  --#
#-------------------------------------------------#

## state forces

harv1_fe <- feols(incidents_pop~price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv1_fe,cluster=~xy)


harv12_fe <- feols(incidents_pop~price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv12_fe,cluster=~xy)

harv13_fe <- feols(incidents_pop~price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv13_fe,cluster=~xy)

harv14_fe <- feols(incidents_pop~price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv14_fe,cluster=~xy)

harv15_fe <- feols(incidents_pop~price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv15_fe,cluster=~xy)

harv16_fe <- feols(incidents_pop~price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv16_fe,cluster=~xy)

harv17_fe <- feols(incidents_pop~price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv17_fe,cluster=~xy)

harv18_fe <- feols(incidents_pop~price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv18_fe,cluster=~xy)

harv19_fe <- feols(incidents_pop~price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv19_fe,cluster=~xy)

harv110_fe <- feols(incidents_pop~price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv110_fe,cluster=~xy)

harv111_fe <- feols(incidents_pop~price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv111_fe,cluster=~xy)

harv112_fe <- feols(incidents_pop~price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12) | xy+yearmo, new_dt[actor=="state"],se="cluster",weights=~population_mln)
summary(harv112_fe,cluster=~xy)


## rebel groups

harv2_fe <- feols(incidents_pop~price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv2_fe,cluster=~xy)


harv22_fe <- feols(incidents_pop~price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv22_fe,cluster=~xy)

harv23_fe <- feols(incidents_pop~price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv23_fe,cluster=~xy)

harv24_fe <- feols(incidents_pop~price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv24_fe,cluster=~xy)

harv25_fe <- feols(incidents_pop~price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv25_fe,cluster=~xy)

harv26_fe <- feols(incidents_pop~price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv26_fe,cluster=~xy)

harv27_fe <- feols(incidents_pop~price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv27_fe,cluster=~xy)

harv28_fe <- feols(incidents_pop~price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv28_fe,cluster=~xy)

harv29_fe <- feols(incidents_pop~price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv29_fe,cluster=~xy)

harv210_fe <- feols(incidents_pop~price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv210_fe,cluster=~xy)

harv211_fe <- feols(incidents_pop~price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv211_fe,cluster=~xy)

harv212_fe <- feols(incidents_pop~price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12) | xy+yearmo, new_dt[actor=="rebel"],se="cluster",weights=~population_mln)
summary(harv212_fe,cluster=~xy)



## political militia

harv3_fe <- feols(incidents_pop~price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv3_fe,cluster=~xy)


harv32_fe <- feols(incidents_pop~price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv32_fe,cluster=~xy)

harv33_fe <- feols(incidents_pop~price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv33_fe,cluster=~xy)

harv34_fe <- feols(incidents_pop~price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv34_fe,cluster=~xy)

harv35_fe <- feols(incidents_pop~price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv35_fe,cluster=~xy)

harv36_fe <- feols(incidents_pop~price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv36_fe,cluster=~xy)

harv37_fe <- feols(incidents_pop~price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv37_fe,cluster=~xy)

harv38_fe <- feols(incidents_pop~price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv38_fe,cluster=~xy)

harv39_fe <- feols(incidents_pop~price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv39_fe,cluster=~xy)

harv310_fe <- feols(incidents_pop~price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv310_fe,cluster=~xy)

harv311_fe <- feols(incidents_pop~price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv311_fe,cluster=~xy)

harv312_fe <- feols(incidents_pop~price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12) | xy+yearmo, new_dt[actor=="polit"],se="cluster",weights=~population_mln)
summary(harv312_fe,cluster=~xy)


## identity militia

harv4_fe <- feols(incidents_pop~price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv4_fe,cluster=~xy)


harv42_fe <- feols(incidents_pop~price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv42_fe,cluster=~xy)

harv43_fe <- feols(incidents_pop~price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv43_fe,cluster=~xy)

harv44_fe <- feols(incidents_pop~price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv44_fe,cluster=~xy)

harv45_fe <- feols(incidents_pop~price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv45_fe,cluster=~xy)

harv46_fe <- feols(incidents_pop~price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv46_fe,cluster=~xy)

harv47_fe <- feols(incidents_pop~price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv47_fe,cluster=~xy)

harv48_fe <- feols(incidents_pop~price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv48_fe,cluster=~xy)

harv49_fe <- feols(incidents_pop~price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv49_fe,cluster=~xy)

harv410_fe <- feols(incidents_pop~price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv410_fe,cluster=~xy)

harv411_fe <- feols(incidents_pop~price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv411_fe,cluster=~xy)

harv412_fe <- feols(incidents_pop~price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12) | xy+yearmo, new_dt[actor=="ident"],se="cluster",weights=~population_mln)
summary(harv412_fe,cluster=~xy)


h01_dt <- as.data.table(harv1_fe$coeftable)[1,]
h12_dt <- as.data.table(harv12_fe$coeftable)[2,]
h13_dt <- as.data.table(harv13_fe$coeftable)[3,]
h14_dt <- as.data.table(harv14_fe$coeftable)[4,]
h15_dt <- as.data.table(harv15_fe$coeftable)[5,]
h16_dt <- as.data.table(harv16_fe$coeftable)[6,]
h17_dt <- as.data.table(harv17_fe$coeftable)[7,]
h18_dt <- as.data.table(harv18_fe$coeftable)[8,]
h19_dt <- as.data.table(harv19_fe$coeftable)[9,]
h110_dt <- as.data.table(harv110_fe$coeftable)[10,]
h111_dt <- as.data.table(harv111_fe$coeftable)[11,]
h112_dt <- as.data.table(harv112_fe$coeftable)[12,]

cumtab1_dt <- Reduce(rbind,list(h01_dt,h12_dt,h13_dt,h14_dt,h15_dt,h16_dt,h17_dt,h18_dt,h19_dt,h110_dt,h111_dt,h112_dt))
colnames(cumtab1_dt) <- c("est","se","trat","pval")
cumtab1_dt[,`:=`(season=as.factor(0:11),actor="state")]

coeftab1_dt <- as.data.table(harv1_fe$coeftable[1:12,])
colnames(coeftab1_dt) <- c("est","se","trat","pval")
coeftab1_dt[,`:=`(season=as.factor(0:11),actor="state")]



h01_dt <- as.data.table(harv2_fe$coeftable)[1,]
h12_dt <- as.data.table(harv22_fe$coeftable)[2,]
h13_dt <- as.data.table(harv23_fe$coeftable)[3,]
h14_dt <- as.data.table(harv24_fe$coeftable)[4,]
h15_dt <- as.data.table(harv25_fe$coeftable)[5,]
h16_dt <- as.data.table(harv26_fe$coeftable)[6,]
h17_dt <- as.data.table(harv27_fe$coeftable)[7,]
h18_dt <- as.data.table(harv28_fe$coeftable)[8,]
h19_dt <- as.data.table(harv29_fe$coeftable)[9,]
h110_dt <- as.data.table(harv210_fe$coeftable)[10,]
h111_dt <- as.data.table(harv211_fe$coeftable)[11,]
h112_dt <- as.data.table(harv212_fe$coeftable)[12,]

cumtab2_dt <- Reduce(rbind,list(h01_dt,h12_dt,h13_dt,h14_dt,h15_dt,h16_dt,h17_dt,h18_dt,h19_dt,h110_dt,h111_dt,h112_dt))
colnames(cumtab2_dt) <- c("est","se","trat","pval")
cumtab2_dt[,`:=`(season=as.factor(0:11),actor="rebel")]

coeftab2_dt <- as.data.table(harv2_fe$coeftable[1:12,])
colnames(coeftab2_dt) <- c("est","se","trat","pval")
coeftab2_dt[,`:=`(season=as.factor(0:11),actor="rebel")]


h01_dt <- as.data.table(harv3_fe$coeftable)[1,]
h12_dt <- as.data.table(harv32_fe$coeftable)[2,]
h13_dt <- as.data.table(harv33_fe$coeftable)[3,]
h14_dt <- as.data.table(harv34_fe$coeftable)[4,]
h15_dt <- as.data.table(harv35_fe$coeftable)[5,]
h16_dt <- as.data.table(harv36_fe$coeftable)[6,]
h17_dt <- as.data.table(harv37_fe$coeftable)[7,]
h18_dt <- as.data.table(harv38_fe$coeftable)[8,]
h19_dt <- as.data.table(harv39_fe$coeftable)[9,]
h110_dt <- as.data.table(harv310_fe$coeftable)[10,]
h111_dt <- as.data.table(harv311_fe$coeftable)[11,]
h112_dt <- as.data.table(harv312_fe$coeftable)[12,]

cumtab3_dt <- Reduce(rbind,list(h01_dt,h12_dt,h13_dt,h14_dt,h15_dt,h16_dt,h17_dt,h18_dt,h19_dt,h110_dt,h111_dt,h112_dt))
colnames(cumtab3_dt) <- c("est","se","trat","pval")
cumtab3_dt[,`:=`(season=as.factor(0:11),actor="polit")]

coeftab3_dt <- as.data.table(harv3_fe$coeftable[1:12,])
colnames(coeftab3_dt) <- c("est","se","trat","pval")
coeftab3_dt[,`:=`(season=as.factor(0:11),actor="polit")]


h01_dt <- as.data.table(harv4_fe$coeftable)[1,]
h12_dt <- as.data.table(harv42_fe$coeftable)[2,]
h13_dt <- as.data.table(harv43_fe$coeftable)[3,]
h14_dt <- as.data.table(harv44_fe$coeftable)[4,]
h15_dt <- as.data.table(harv45_fe$coeftable)[5,]
h16_dt <- as.data.table(harv46_fe$coeftable)[6,]
h17_dt <- as.data.table(harv47_fe$coeftable)[7,]
h18_dt <- as.data.table(harv48_fe$coeftable)[8,]
h19_dt <- as.data.table(harv49_fe$coeftable)[9,]
h110_dt <- as.data.table(harv410_fe$coeftable)[10,]
h111_dt <- as.data.table(harv411_fe$coeftable)[11,]
h112_dt <- as.data.table(harv412_fe$coeftable)[12,]

cumtab4_dt <- Reduce(rbind,list(h01_dt,h12_dt,h13_dt,h14_dt,h15_dt,h16_dt,h17_dt,h18_dt,h19_dt,h110_dt,h111_dt,h112_dt))
colnames(cumtab4_dt) <- c("est","se","trat","pval")
cumtab4_dt[,`:=`(season=as.factor(0:11),actor="ident")]

coeftab4_dt <- as.data.table(harv4_fe$coeftable[1:12,])
colnames(coeftab4_dt) <- c("est","se","trat","pval")
coeftab4_dt[,`:=`(season=as.factor(0:11),actor="ident")]



# coeftab_dt <- rbind(coeftab1_dt,coeftab2_dt,coeftab3_dt,coeftab4_dt)
# coeftab_dt$actor <- factor(coeftab_dt$actor,levels=c("state","rebel","polit","ident"),labels=c("State forces","Rebel groups","Political militas","Identity militias"))
# 
# cumtab_dt <- rbind(cumtab1_dt,cumtab2_dt,cumtab3_dt,cumtab4_dt)
# cumtab_dt$actor <- factor(cumtab_dt$actor,levels=c("state","rebel","polit","ident"),labels=c("State forces","Rebel groups","Political militas","Identity militias"))

scale_state <- 100*.1/actor_dt$incidents_pop
scale_rebel <- 100*.1/actor_dt$incidents_pop
scale_polit <- 100*.1/actor_dt$incidents_pop
scale_ident <- 100*.1/actor_dt$incidents_pop



coeftab2_dt$est <- coeftab2_dt$est*scale_rebel
coeftab2_dt$se <- coeftab2_dt$se*scale_rebel

coeftab3_dt$est <- coeftab3_dt$est*scale_polit
coeftab3_dt$se <- coeftab3_dt$se*scale_polit

coeftab_dt <- rbind(coeftab2_dt,coeftab3_dt)
coeftab_dt$actor <- factor(coeftab_dt$actor,levels=c("rebel","polit"),labels=c("Rebel groups","Political militias"))


cumtab2_dt$est <- cumtab2_dt$est*scale_rebel
cumtab2_dt$se <- cumtab2_dt$se*scale_rebel

cumtab3_dt$est <- cumtab3_dt$est*scale_polit
cumtab3_dt$se <- cumtab3_dt$se*scale_polit

cumtab_dt <- rbind(cumtab2_dt,cumtab3_dt)
cumtab_dt$actor <- factor(cumtab_dt$actor,levels=c("rebel","polit"),labels=c("Rebel groups","Political militias"))

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(actor,season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))

## circular plot for illustrating the seasonal effect
gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est,fill=actor))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=actor),alpha=.25)+
  geom_line(aes(color=actor,group=actor),size=.5)+
  geom_hline(yintercept = seq(-20,60,by=20),color="gray",size=.4,linetype=3) +
  scale_x_continuous(breaks = 1:12,labels=c(0:11))+
  scale_fill_manual(values=c("darkgray","steelblue"))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  coord_polar(start=-pi/6)+
  ylim(-20,60)+
  labs(x="",y="Percent change in violence")+
  theme(axis.text=element_text(size=8,margin=margin(t=2,r=2,b=2,l=2),color="black"),axis.title = element_text(size=9),plot.title = element_text(size=11),panel.grid=element_blank(),panel.background = element_blank(),axis.title.y = element_text(hjust=.95),legend.position = "none")

## line plot for illustrating the cumulative effect over the season
gg_cum <- ggplot(cumtab_dt,aes(x=season,y=est,fill=actor))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=actor),alpha=.25)+
  geom_line(aes(color=actor,group=actor),size=.5)+
  geom_hline(yintercept = seq(0,150,by=50),color="gray",size=.4,linetype=3) +
  scale_fill_manual(values=c("darkgray","steelblue"))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  coord_cartesian(ylim=c(-0,180))+
  labs(x=expression(paste("Months from harvest",sep="")),y="Cumulative precent change in violence")+
  theme_void()+
  theme(axis.text=element_text(size=8,margin=margin(t=2,r=2,b=2,l=2)),axis.title = element_text(size=9),axis.title.y = element_text(angle=90,margin=margin(t=0,r=4,b=0,l=0),hjust=.2),plot.title = element_text(size=11),legend.position=c(-.08,.94),legend.title=element_blank(),legend.text = element_text(size=7))


## putting the two together
gg_panel <- plot_grid(gg_coef,gg_cum,ncol=2,align="hv",axis="l")

## saving the plot
ggsave("seasonal_rebel_polit.png",gg_panel,width=6.5,height=3.5,device="png",dpi="retina")


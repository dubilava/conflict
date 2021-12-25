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
load("temperature.RData")

colnames(temperature_dt)[3:4] <- c("longitude","latitude")

temperature_dt$year <- as.factor(temperature_dt$year)
temperature_dt$mo <- as.factor(temperature_dt$mo)

## merge temperature data with datacomb

gs <- 7:11
ed <- 90#round((max(gs)-min(gs)+1)*30*2/3)

datamerge_dt <- merge(datacomb_dt,temperature_dt,by=c("year","mo","longitude","latitude"),all.x=T)

datamerge_dt[is.na(edd)]$edd <- 0

subset_dt <- datamerge_dt[,.(year,mo,longitude,latitude,planted,season,edd)]
subset_dt <- unique(subset_dt)

subset_dt$gsm <- ifelse(as.numeric(as.character(subset_dt$season))-as.numeric(as.character(subset_dt$planted))<0,as.numeric(as.character(subset_dt$season))-as.numeric(as.character(subset_dt$planted))+1+12,as.numeric(as.character(subset_dt$season))-as.numeric(as.character(subset_dt$planted))+1)

harvest_dt <- subset_dt[season == 1]
harvest_dt$myr <- harvest_dt$year

harvest_dt <- harvest_dt[,.(year,myr,longitude,latitude,planted,season,gsm)]

submerge_dt <- merge(subset_dt,harvest_dt,by=c("year","longitude","latitude","planted","season","gsm"),all.x=T)
submerge_dt <- submerge_dt[order(longitude,latitude,year,mo)]

submerge_dt[year=="1997" & mo=="01" & is.na(myr)]$myr <- "1996"
submerge_dt$myr <- as.numeric(as.character(submerge_dt$myr))

submerge_dt$myr <- as.factor(na.locf(submerge_dt$myr))

submerge_dt$backward <- 12-as.numeric(as.character(submerge_dt$season))+1
submerge_dt$dif <- submerge_dt$gsm-submerge_dt$backward

subseason_dt <- submerge_dt[dif >= 0]
subseason_dt <- subseason_dt[,.(cdd=sum(edd)),by=.(longitude,latitude,myr)]

subseason_dt$myr <- as.factor(as.numeric(as.character(subseason_dt$myr))+1)

cdd_dt <- merge(submerge_dt,subseason_dt,by=c("longitude","latitude","myr"),all.x=T)

cdd_dt[is.na(cdd)]$cdd <- 0
cdd_dt$edd <- NULL
cdd_dt$backward <- NULL
cdd_dt$dif <- NULL

datacomb1_dt <- merge(datamerge_dt,cdd_dt,by=c("longitude","latitude","year","mo","planted","season"),all.x=T)
##---

## merge temperature data with dataset
datamerge_dt <- merge(dataset_dt,temperature_dt,by=c("year","mo","longitude","latitude"),all.x=T)

datamerge_dt[is.na(edd)]$edd <- 0

subset_dt <- datamerge_dt[,.(year,mo,longitude,latitude,planted,season,edd)]
subset_dt <- unique(subset_dt)

subset_dt$gsm <- ifelse(as.numeric(as.character(subset_dt$season))-as.numeric(as.character(subset_dt$planted))<0,as.numeric(as.character(subset_dt$season))-as.numeric(as.character(subset_dt$planted))+1+12,as.numeric(as.character(subset_dt$season))-as.numeric(as.character(subset_dt$planted))+1)

harvest_dt <- subset_dt[season == 1]
harvest_dt$myr <- harvest_dt$year

harvest_dt <- harvest_dt[,.(year,myr,longitude,latitude,planted,season,gsm)]

submerge_dt <- merge(subset_dt,harvest_dt,by=c("year","longitude","latitude","planted","season","gsm"),all.x=T)
submerge_dt <- submerge_dt[order(longitude,latitude,year,mo)]

submerge_dt[year=="1997" & mo=="01" & is.na(myr)]$myr <- "1996"
submerge_dt$myr <- as.numeric(as.character(submerge_dt$myr))

submerge_dt$myr <- as.factor(na.locf(submerge_dt$myr))

submerge_dt$backward <- 12-as.numeric(as.character(submerge_dt$season))+1
submerge_dt$dif <- submerge_dt$gsm-submerge_dt$backward

subseason_dt <- submerge_dt[dif >= 0]
subseason_dt <- subseason_dt[,.(cdd=sum(edd)),by=.(longitude,latitude,myr)]

subseason_dt$myr <- as.factor(as.numeric(as.character(subseason_dt$myr))+1)

cdd_dt <- merge(submerge_dt,subseason_dt,by=c("longitude","latitude","myr"),all.x=T)

cdd_dt[is.na(cdd)]$cdd <- 0
cdd_dt$edd <- NULL
cdd_dt$backward <- NULL
cdd_dt$dif <- NULL

dataset1_dt <- merge(datamerge_dt,cdd_dt,by=c("longitude","latitude","year","mo","planted","season"),all.x=T)

##---


harv_mfe <- feols(incidents_pop~price_ch:area_dum:i(season,keep=1:12)+price_ch:area_dum:i(season,keep=1:12):I(cdd-100) | xy+yearmo, datacomb1_dt,se="cluster",weights=~population_mln)
summary(harv_mfe,cluster=~xy)


## obtain cumulative effects for seasons 1--12

ed <- quantile(datacomb1_dt[season!=0]$cdd,.75)

new_dt <- datacomb1_dt

# new_dt$iri_d <- gsub("-","m",new_dt$iri_day)

test_dt <- dummy_cols(new_dt[,.(longitude,latitude,date,season)],select_columns = "season")

new_dt <- merge(new_dt,test_dt,by=c("longitude","latitude","date","season"))
new_dt <- new_dt[order(longitude,latitude,date)]

new_dt[,`:=`(season_02=season_1-season_2,season_13=season_1-season_3,season_23=season_2-season_3,season_14=season_1-season_4,season_24=season_2-season_4,season_34=season_3-season_4,season_15=season_1-season_5,season_25=season_2-season_5,season_35=season_3-season_5,season_45=season_4-season_5,season_16=season_1-season_6,season_26=season_2-season_6,season_36=season_3-season_6,season_46=season_4-season_6,season_56=season_5-season_6,season_17=season_1-season_7,season_27=season_2-season_7,season_37=season_3-season_7,season_47=season_4-season_7,season_57=season_5-season_7,season_67=season_6-season_7,season_18=season_1-season_8,season_28=season_2-season_8,season_38=season_3-season_8,season_48=season_4-season_8,season_58=season_5-season_8,season_68=season_6-season_8,season_78=season_7-season_8,season_19=season_1-season_9,season_29=season_2-season_9,season_39=season_3-season_9,season_49=season_4-season_9,season_59=season_5-season_9,season_69=season_6-season_9,season_79=season_7-season_9,season_89=season_8-season_9,season_110=season_1-season_10,season_210=season_2-season_10,season_310=season_3-season_10,season_410=season_4-season_10,season_510=season_5-season_10,season_610=season_6-season_10,season_710=season_7-season_10,season_810=season_8-season_10,season_910=season_9-season_10,season_111=season_1-season_11,season_211=season_2-season_11,season_311=season_3-season_11,season_411=season_4-season_11,season_511=season_5-season_11,season_611=season_6-season_11,season_711=season_7-season_11,season_811=season_8-season_11,season_911=season_9-season_11,season_1011=season_10-season_11,season_112=season_1-season_12,season_212=season_2-season_12,season_312=season_3-season_12,season_412=season_4-season_12,season_512=season_5-season_12,season_612=season_6-season_12,season_712=season_7-season_12,season_812=season_8-season_12,season_912=season_9-season_12,season_1012=season_10-season_12,season_1112=season_11-season_12)]


#------------------------------#
#--  Table T6: Sugar Prices  --#
#------------------------------#


harv_fe <- feols(incidents_pop~price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv_fe,cluster=~xy)


harv12_fe <- feols(incidents_pop~price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv12_fe,cluster=~xy)

harv13_fe <- feols(incidents_pop~price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv13_fe,cluster=~xy)

harv14_fe <- feols(incidents_pop~price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv14_fe,cluster=~xy)

harv15_fe <- feols(incidents_pop~price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv15_fe,cluster=~xy)

harv16_fe <- feols(incidents_pop~price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv16_fe,cluster=~xy)

harv17_fe <- feols(incidents_pop~price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv17_fe,cluster=~xy)

harv18_fe <- feols(incidents_pop~price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv18_fe,cluster=~xy)

harv19_fe <- feols(incidents_pop~price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv19_fe,cluster=~xy)

harv110_fe <- feols(incidents_pop~price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12)+price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv110_fe,cluster=~xy)

harv111_fe <- feols(incidents_pop~price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12)+price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv111_fe,cluster=~xy)

harv112_fe <- feols(incidents_pop~price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12)+price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12):cdd | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv112_fe,cluster=~xy)


h01_dt <- as.data.table(harv_fe$coeftable)[1,]
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
cumtab1_dt[,`:=`(season=as.factor(0:11),climate="normal")]


coeftab1_dt <- as.data.table(harv_fe$coeftable[1:12,])
colnames(coeftab1_dt) <- c("est","se","trat","pval")
coeftab1_dt[,`:=`(season=as.factor(0:11),climate="normal")]



harv_fe <- feols(incidents_pop~price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv_fe,cluster=~xy)


harv12_fe <- feols(incidents_pop~price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_02+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv12_fe,cluster=~xy)

harv13_fe <- feols(incidents_pop~price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_13+season_23+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv13_fe,cluster=~xy)

harv14_fe <- feols(incidents_pop~price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_14+season_24+season_34+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv14_fe,cluster=~xy)

harv15_fe <- feols(incidents_pop~price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_15+season_25+season_35+season_45+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv15_fe,cluster=~xy)

harv16_fe <- feols(incidents_pop~price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_16+season_26+season_36+season_46+season_56+season_6+season_7+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv16_fe,cluster=~xy)

harv17_fe <- feols(incidents_pop~price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_17+season_27+season_37+season_47+season_57+season_67+season_7+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv17_fe,cluster=~xy)

harv18_fe <- feols(incidents_pop~price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_18+season_28+season_38+season_48+season_58+season_68+season_78+season_8+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv18_fe,cluster=~xy)

harv19_fe <- feols(incidents_pop~price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12)+price_ch:area_dum:(season_19+season_29+season_39+season_49+season_59+season_69+season_79+season_89+season_9+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv19_fe,cluster=~xy)

harv110_fe <- feols(incidents_pop~price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12)+price_ch:area_dum:(season_110+season_210+season_310+season_410+season_510+season_610+season_710+season_810+season_910+season_10+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv110_fe,cluster=~xy)

harv111_fe <- feols(incidents_pop~price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12)+price_ch:area_dum:(season_111+season_211+season_311+season_411+season_511+season_611+season_711+season_811+season_911+season_1011+season_11+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv111_fe,cluster=~xy)

harv112_fe <- feols(incidents_pop~price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12)+price_ch:area_dum:(season_112+season_212+season_312+season_412+season_512+season_612+season_712+season_812+season_912+season_1012+season_1112+season_12):I(cdd-ed) | xy+yearmo, new_dt,se="cluster",weights=~population_mln)
summary(harv112_fe,cluster=~xy)


h01_dt <- as.data.table(harv_fe$coeftable)[1,]
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
cumtab2_dt <- Reduce(rbind,list(h01_dt,h12_dt,h13_dt,h14_dt,h15_dt,h16_dt,h17_dt,h18_dt,h19_dt,h110_dt,h111_dt,h112_dt))
colnames(cumtab2_dt) <- c("est","se","trat","pval")
cumtab2_dt[,`:=`(season=as.factor(0:11),climate="extreme")]


coeftab2_dt <- as.data.table(harv_fe$coeftable[1:12,])
colnames(coeftab2_dt) <- c("est","se","trat","pval")
coeftab2_dt[,`:=`(season=as.factor(0:11),climate="extreme")]



coeftab_dt <- rbind(coeftab1_dt,coeftab2_dt)
coeftab_dt$climate <- factor(coeftab_dt$climate,levels=c("normal","extreme"))

cumtab_dt <- rbind(cumtab1_dt,cumtab2_dt)
cumtab_dt$climate <- factor(cumtab_dt$climate,levels=c("normal","extreme"))


gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est,color=climate))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_pointrange(aes(ymin=est-1.96*se,ymax=est+1.96*se,color=climate),size=.3,position=position_dodge(width=.5))+
  scale_color_manual(values=c("indianred","darkgray"))+
  coord_cartesian(ylim=c(-.2,1.6))+
  labs(title=" ",x=expression(paste("Months from harvest",sep="")),y="Change in conflict")+
  theme_classic()+
  theme(axis.text = element_text(size=8),axis.title = element_text(size=9),plot.title = element_text(size=11),legend.position = c(.18,.82),legend.title=element_blank())

gg_cum <- ggplot(cumtab_dt,aes(x=season,y=est,color=climate))+
  geom_hline(yintercept = 0,color="darkgray")+
  geom_pointrange(aes(ymin=est-1.96*se,ymax=est+1.96*se,color=climate),size=.3,position=position_dodge(width=.5))+
  scale_color_manual(values=c("indianred","darkgray"))+
  coord_cartesian(ylim=c(-.2,1.6))+
  labs(title=" ",x=expression(paste("Months from harvest",sep="")),y="Cumulative change in conflict")+
  theme_classic()+
  theme(axis.text = element_text(size=8),axis.title = element_text(size=9),plot.title = element_text(size=11),legend.position = "none")


## putting the two together
gg_panel <- plot_grid(gg_coef,gg_cum,ncol=2,align="hv",axis="l")

# gg_panel

ggsave("badharvest.png",gg_panel,width=6.5,height=3.5)


subset1_dt <- datacomb1_dt[season==1]

subset1_dt$crop <- factor(subset1_dt$crop,levels=rev(c("Maize","Sorghum","Wheat","Rice")))

gg_gs <- ggplot(subset1_dt,aes(x=gsm,fill=crop))+
  stat_count(color="white")+
  scale_fill_manual(values=rev(c("seagreen","indianred","goldenrod","gray60")))+
  labs(title=" ",x="Number of Months in a Growing Season",y="Count")+
  theme_classic()+
  theme(axis.text = element_text(size=8),axis.title = element_text(size=9),plot.title = element_text(size=11),legend.position = "none")

gg_ed <- ggplot(subset1_dt,aes(x=cdd,fill=crop))+
  geom_histogram(color="white",binwidth=10)+
  scale_fill_manual(values=rev(c("seagreen","indianred","goldenrod","gray60")))+
  coord_cartesian(xlim=c(0,240))+
  labs(title=" ",x="Extreme Heat Days in a Growing Season",y="Count")+
  theme_classic()+
  theme(axis.text = element_text(size=8),axis.title = element_text(size=9),plot.title = element_text(size=11),legend.position = c(.82,.82),legend.title=element_blank())


## putting the two together
gg_panel <- plot_grid(gg_gs,gg_ed,ncol=2,align="hv",axis="l")

# gg_panel

ggsave("growingseason.png",gg_panel,width=6.5,height=3.5)

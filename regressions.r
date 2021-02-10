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

dataset_dt[season==0]$season <- NA

dataset_dt[,`:=` (population_mln = population/1000000)]
dataset_dt[,`:=` (population_ihs = log(population_mln+sqrt(population_mln^2+1)))]

dataset_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0),fatalities_dum=ifelse(fatalities>0,1,0),agri=ifelse(tot_area>.01,1,0))]

datasub_dt <- dataset_dt[max_area>0,.(incidents=mean(incidents),area=mean(max_area)),by=.(season,event)]
datasub_dt <- datasub_dt[order(event,season)]


datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(longitude,latitude,mo,xy,date,yearmo,year,month,season_srt,season,season_end,crop,tot_area,max_area,agri,price,price_wt,price_ch,price_chwt,cocoa_ch,platinum_ch,population_mln,population_ihs)]

datacomb_dt[,`:=` (incidents_pop=incidents/population_mln)]

## VIOLENCE
base_vfe <- feols(incidents~price_ch:max_area | xy+yearmo, datacomb_dt,se="cluster")
summary(base_vfe)

harv_vfe <- feols(incidents~price_ch:max_area:season | xy+yearmo, datacomb_dt,se="cluster")
summary(harv_vfe)

base1_vfe <- feols(incidents~price_ch:max_area | xy+yearmo+season, datacomb_dt[latitude<=23.5],se="cluster")
summary(base1_vfe)

harv1_vfe <- feols(incidents~price_ch:max_area:season | xy+yearmo+season, datacomb_dt[latitude<=23.5],se="cluster")
summary(harv1_vfe)

base2_vfe <- feols(incidents~price_ch:max_area | xy+yearmo+season, datacomb_dt[population_mln >= 0.05],se="cluster")
summary(base2_vfe)

harv2_vfe <- feols(incidents~price_ch:max_area:season | xy+yearmo+season, datacomb_dt[population_mln >= 0.05],se="cluster")
summary(harv2_vfe)

## print the latex table
etable(base_vfe,harv_vfe,base1_vfe,harv1_vfe,base2_vfe,harv2_vfe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Baseline Results",subtitles = c("All","All","Sub","Sub","Pop","Pop"))


##--- PER MLN POP ---###

base_vfe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(base_vfe)

harv_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
summary(harv_vfe)

base1_vfe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt[latitude<=23.5],se="cluster",weights=~population_mln)
summary(base1_vfe)

harv1_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, datacomb_dt[latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv1_vfe)

base2_vfe <- feols(incidents_pop~price_ch:max_area | xy+yearmo, datacomb_dt[population_mln >= 0.05],se="cluster",weights=~population_mln)
summary(base2_vfe)

harv2_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, datacomb_dt[population_mln >= 0.05],se="cluster",weights=~population_mln)
summary(harv2_vfe)

## print the latex table
etable(base_vfe,harv_vfe,base1_vfe,harv1_vfe,base2_vfe,harv2_vfe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Baseline Results",subtitles = c("All","All","Sub","Sub","Pop","Pop"))

##---             ---###


##--- DISAGGREGATED ---###

harv1_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, dataset_dt[event==17],se="cluster",weights=~population_mln)
summary(harv1_vfe)

harv2_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, dataset_dt[event==27],se="cluster",weights=~population_mln)
summary(harv2_vfe)

harv3_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, dataset_dt[event==37],se="cluster",weights=~population_mln)
summary(harv3_vfe)

harv4_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, dataset_dt[event==47],se="cluster",weights=~population_mln)
summary(harv4_vfe)

harv5_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo, dataset_dt[event==78],se="cluster",weights=~population_mln)
summary(harv5_vfe)

## print the latex table
etable(harv1_vfe,harv2_vfe,harv3_vfe,harv4_vfe,harv5_vfe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("State","Rebels","Political","Identity","Other"))


harv1_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo+season, dataset_dt[event==17 & latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv1_vfe)

harv2_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo+season, dataset_dt[event==27 & latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv2_vfe)

harv3_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo+season, dataset_dt[event==37 & latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv3_vfe)

harv4_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo+season, dataset_dt[event==47 & latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv4_vfe)

harv5_vfe <- feols(incidents_pop~price_ch:max_area:season | xy+yearmo+season, dataset_dt[event==78 & latitude<=23.5],se="cluster",weights=~population_mln)
summary(harv5_vfe)

## print the latex table
etable(harv1_vfe,harv2_vfe,harv3_vfe,harv4_vfe,harv5_vfe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("State","Rebels","Political","Identity","Other"))

##---               ---###




sd(datacomb_dt[max_area>0]$price)*summary(harv_vfe)$coefficients*datasub_dt$area/datasub_dt$incidents
sd(dataset_dt[max_area>0]$price)*summary(harv_pfe)$coefficients*datasub_dt[event=="Protests"]$area/datasub_dt[event=="Protests"]$incidents


###=====


v_array <- array(dim=c(2,4,length(unique(dataset_dt$year))))
p_array <- array(dim=c(2,4,length(unique(dataset_dt$year))))

for(i in 1:length(unique(dataset_dt$year))){
  harv_vfe <- feols(incidents~price_ch:max_area:postharvest | xy+yearmo+postharvest, dataset_dt[event=="Violence" & year %!in% unique(dataset_dt$year)[i]],se="cluster")
  v_array[,,i] <- as.matrix(harv_vfe$coeftable)
  
  harv_pfe <- feols(incidents~price_ch:max_area:postharvest | xy+yearmo+postharvest, dataset_dt[event=="Protests" & year %!in% unique(dataset_dt$year)[i]],se="cluster")
  p_array[,,i] <- as.matrix(harv_pfe$coeftable)
}

coef_v <- data.table(year=unique(dataset_dt$year),t(v_array[1:2,1,]))
colnames(coef_v) <- c("year","preharvest","postharvest")
coef_v_lg <- melt(coef_v,id.vars="year")

se_v <- data.table(year=unique(dataset_dt$year),t(v_array[1:2,2,]))
colnames(se_v) <- c("year","preharvest","postharvest")
se_v_lg <- melt(se_v,id.vars="year")

param_v_lg <- merge(coef_v_lg,se_v_lg,by=c("year","variable"),suffixes = c("_coef", "_se"))
param_v_lg <- param_v_lg[order(variable,year)]

gg_v <- ggplot(param_v_lg,aes(x=year,color=variable,shape=variable)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.8,width=NA,alpha=.7) +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=2)+
  labs(x="Omitted Year",y="Coefficients")+
  scale_color_manual(values=c("gray50","indianred"))+
  scale_shape_manual(values=c(16,18))+
  theme_classic()+
  theme(legend.position="top", legend.title = element_blank(), axis.line.x=element_blank(), axis.text.x = element_text(angle=90),axis.ticks.x=element_blank())

ggsave("violenece_omit_year.png",gg_v,width=6.5,height=4.5)


coef_p <- data.table(year=unique(dataset_dt$year),t(p_array[1:2,1,]))
colnames(coef_p) <- c("year","preharvest","postharvest")
coef_p_lg <- melt(coef_p,id.vars="year")

se_p <- data.table(year=unique(dataset_dt$year),t(p_array[1:2,2,]))
colnames(se_p) <- c("year","preharvest","postharvest")
se_p_lg <- melt(se_p,id.vars="year")

param_p_lg <- merge(coef_p_lg,se_p_lg,by=c("year","variable"),suffixes = c("_coef", "_se"))
param_p_lg <- param_p_lg[order(variable,year)]

gg_p <- ggplot(param_p_lg,aes(x=year,color=variable,shape=variable)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.8,width=NA,alpha=.7) +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=2)+
  labs(x="Omitted Year",y="Coefficients")+
  scale_color_manual(values=c("gray50","steelblue"))+
  scale_shape_manual(values=c(16,18))+
  theme_classic()+
  theme(legend.position="top", legend.title = element_blank(), axis.line.x=element_blank(), axis.text.x = element_text(angle=90),axis.ticks.x=element_blank())

ggsave("protests_omit_year.png",gg_p,width=6.5,height=4.5)


denom <- 2
vd_array <- array(dim=c(2,4,length(unique(dataset_dt$year))/denom+1))
pd_array <- array(dim=c(2,4,length(unique(dataset_dt$year))/denom+1))

for(i in 1:(length(unique(dataset_dt$year))/denom+1)){
  harv_vfe <- feols(incidents~price_ch:max_area:postharvest | xy+yearmo+postharvest, dataset_dt[event=="Violence" & year %in% unique(dataset_dt$year)[i:(i+length(unique(dataset_dt$year))/denom-1)]],se="cluster")
  vd_array[,,i] <- as.matrix(harv_vfe$coeftable)
  
  harv_pfe <- feols(incidents~price_ch:max_area:postharvest | xy+yearmo+postharvest, dataset_dt[event=="Protests" & year %in% unique(dataset_dt$year)[i:(i+length(unique(dataset_dt$year))/denom-1)]],se="cluster")
  pd_array[,,i] <- as.matrix(harv_pfe$coeftable)
}

yrs <- paste(unique(dataset_dt$year)[1:(length(unique(dataset_dt$year))/denom+1)],"-",unique(dataset_dt$year)[(length(unique(dataset_dt$year))/denom):24],sep="")
  
coef_v <- data.table(years=yrs,t(vd_array[1:2,1,]))
colnames(coef_v) <- c("year","preharvest","postharvest")
coef_v_lg <- melt(coef_v,id.vars="year")

se_v <- data.table(years=yrs,t(vd_array[1:2,2,]))
colnames(se_v) <- c("year","preharvest","postharvest")
se_v_lg <- melt(se_v,id.vars="year")

param_v_lg <- merge(coef_v_lg,se_v_lg,by=c("year","variable"),suffixes = c("_coef", "_se"))
param_v_lg <- param_v_lg[order(variable,year)]

gg_v <- ggplot(param_v_lg,aes(x=year,color=variable,shape=variable)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.8,width=NA,alpha=.7) +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=2)+
  labs(x="Omitted Year",y="Coefficients")+
  scale_color_manual(values=c("gray50","indianred"))+
  scale_shape_manual(values=c(16,18))+
  theme_classic()+
  theme(legend.position="top", legend.title = element_blank(), axis.line.x=element_blank(), axis.text.x = element_text(angle=90),axis.ticks.x=element_blank())

ggsave("violenece_trim_years.png",gg_v,width=6.5,height=4.5)


coef_p <- data.table(year=yrs,t(pd_array[1:2,1,]))
colnames(coef_p) <- c("year","preharvest","postharvest")
coef_p_lg <- melt(coef_p,id.vars="year")

se_p <- data.table(year=yrs,t(pd_array[1:2,2,]))
colnames(se_p) <- c("year","preharvest","postharvest")
se_p_lg <- melt(se_p,id.vars="year")

param_p_lg <- merge(coef_p_lg,se_p_lg,by=c("year","variable"),suffixes = c("_coef", "_se"))
param_p_lg <- param_p_lg[order(variable,year)]

gg_p <- ggplot(param_p_lg,aes(x=year,color=variable,shape=variable)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.8,width=NA,alpha=.7) +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=2)+
  labs(x="Omitted Year",y="Coefficients")+
  scale_color_manual(values=c("gray50","steelblue"))+
  scale_shape_manual(values=c(16,18))+
  theme_classic()+
  theme(legend.position="top", legend.title = element_blank(), axis.line.x=element_blank(), axis.text.x = element_text(angle=90),axis.ticks.x=element_blank())

ggsave("protests_trim_years.png",gg_p,width=6.5,height=4.5)



# base_fe <- feols(incidents~i(agri,year,ref=2010) | xy+yearmo, dataset_dt[event=="Violence"])
# summary(base_fe,se="cluster")


base_vfe <- feols(incidents~(price+harvest):max_area+(price:max_area):harvest | xy+yearmo, dataset_dt[event=="Violence"])
summary(base_vfe,se="cluster")

harv_vfe <- feols(incidents~(price:max_area):I(1-harvest)+(price:max_area):harvest+max_area:harvest | xy+yearmo, dataset_dt[event=="Violence"])
summary(harv_vfe,se="cluster")


## PROTESTS

base_pfe <- feols(incidents~price:max_area | xy+yearmo, dataset_dt[event=="Protests" & population >= 10000])
# summary(base_pfe,se="cluster")

harv_pfe <- feols(incidents~(price:max_area):I(1-harvest)+(price:max_area):harvest | xy+yearmo, dataset_dt[event=="Protests" & population >= 10000])
# summary(harv_pfe,se="cluster")


etable(base_vfe,harv_vfe,base_pfe,harv_pfe,cluster=~xy,tex=TRUE,digits=3,digits.stats = 3,title="Main Results",subtitles = c("Violence","Violence","Protests","Protests"))

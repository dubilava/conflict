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
datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,date,yearmo,mo,month,crop,tot_area,max_area,plant,season_srt,season,season_end,price,price_ch,price_d,price_maize,price_sorghum,price_wheat,price_rice,price_cocoa,cocoa_ch,cocoa_d,population,population_mln)]

datacomb_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

## aggregate by harvest season
seasonal_dt <- datacomb_dt[,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]

seasonal_dt <- seasonal_dt[order(season)]

## aggregate by location-year
aggregate_dt <- datacomb_dt[,.(incidents=sum(incidents),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy,year)]

aggregate_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

y_vec <- unique(datacomb_dt$year)[order(unique(datacomb_dt$year))]
y_array <- array(dim=c(12,4,length(y_vec)))

for(i in 1:length(y_vec)){
  harv_fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | xy+yearmo, datacomb_dt[year %!in% unique(datacomb_dt$year)[i]],se="cluster",weights=~population_mln)
  y_array[,,i] <- as.matrix(harv_fe$coeftable)
}

coef_y <- data.table(year=unique(datacomb_dt$year),t(y_array[1:12,1,]))
colnames(coef_y) <- c("year",paste0("h",c(0:11)))
coef_y_lg <- melt(coef_y,id.vars="year")

se_y <- data.table(year=unique(datacomb_dt$year),t(y_array[1:12,2,]))
colnames(se_y) <- c("year",paste0("h",c(0:11)))
se_y_lg <- melt(se_y,id.vars="year")

param_y_lg <- merge(coef_y_lg,se_y_lg,by=c("year","variable"),suffixes = c("_coef", "_se"))
param_y_lg <- param_y_lg[order(variable,year)]

gg_y <- ggplot(param_y_lg[variable %in% c("h0","h1","h2")],aes(x=year)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.8,width=NA,alpha=.7,color="indianred") +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=2,color="indianred")+
  facet_wrap(~variable,ncol=4)+
  coord_flip()+
  labs(title="Parameter sensitivity to omitted data by year",x="Omitted Year",y="Coefficients")+
  theme_classic()+
  theme(legend.position="top",legend.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),strip.background=element_blank())

gg_y

ggsave("conflict_omit_year.png",gg_y,width=6.5,height=6.5)


l_vec <- unique(datacomb_dt$latitude)[order(unique(datacomb_dt$latitude))]
l_array <- array(dim=c(12,4,length(l_vec)))

for(i in 1:length(l_vec)){
  harv_fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | xy+yearmo, datacomb_dt[latitude %!in% l_vec[i]],se="cluster",weights=~population_mln)
  l_array[,,i] <- as.matrix(harv_fe$coeftable)
}

coef_l <- data.table(latitude=l_vec,t(l_array[1:12,1,]))
colnames(coef_l) <- c("latitude",paste0("h",c(0:11)))
coef_l_lg <- melt(coef_l,id.vars="latitude")

se_l <- data.table(latitude=l_vec,t(l_array[1:12,2,]))
colnames(se_l) <- c("latitude",paste0("h",c(0:11)))
se_l_lg <- melt(se_l,id.vars="latitude")

param_l_lg <- merge(coef_l_lg,se_l_lg,by=c("latitude","variable"),suffixes = c("_coef", "_se"))
param_l_lg <- param_l_lg[order(variable,latitude)]

gg_l <- ggplot(param_l_lg[variable %in% c("h0","h1","h2")],aes(x=latitude)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.8,width=NA,alpha=.7,color="indianred") +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=2,color="indianred")+
  scale_x_continuous(breaks=seq(-35,35,by=5))+
  facet_wrap(~variable,ncol=4)+
  coord_flip()+
  labs(title="Parameter sensitivity to omitted data by latitude",x="Omitted Latitude",y="Coefficients")+
  theme_classic()+
  theme(legend.position="top",legend.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),strip.background=element_blank())

gg_l

ggsave("conflict_omit_lat.png",gg_l,width=6.5,height=6.5)

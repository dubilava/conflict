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

theme_black <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    panel.grid=element_blank(),
    plot.background=element_rect(fill="transparent",color=NA),
    legend.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="gray55",family="sans"),
    axis.title=element_text(size=10,colour="gray55",family="sans"),
    axis.text=element_text(size=8,colour="gray55",family="sans",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line.x=element_line(colour="gray55"),
    axis.line.y=element_line(colour="gray55"),
    axis.ticks=element_line(colour="gray55"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="gray55",family="sans"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="gray55",family="sans",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}

theme_white <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    panel.grid=element_blank(),
    plot.background=element_rect(fill="transparent",color=NA),
    legend.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="gray35",family="sans"),
    axis.title=element_text(size=10,colour="gray35",family="sans"),
    axis.text=element_text(size=8,colour="gray35",family="sans",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line.x=element_line(colour="gray35"),
    axis.line.y=element_line(colour="gray35"),
    axis.ticks=element_line(colour="gray35"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="gray35",family="sans"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="gray35",family="sans",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}


## load the data
load("data_violence_acled.RData")
load("precipitation.RData")

rain_dt <- rain_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,rain=as.numeric(rain))]

datacomb1_dt <- merge(datacomb_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset1_dt <- merge(dataset_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)

season_dt <- datacomb_dt[,.(longitude,latitude,mo,planted,season)]
season_dt <- unique(season_dt)

datamerge_dt <- merge(rain_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)

datamerge_dt[is.na(rain)]$rain <- 0


subset_dt <- datamerge_dt[,.(year,mo,longitude,latitude,planted,season,rain)]
subset_dt <- unique(subset_dt)

# number of months in the growing season
subset_dt[,`:=`(gsm=ifelse(as.numeric(as.character(season))-as.numeric(as.character(planted))<0,12-(as.numeric(as.character(season))-as.numeric(as.character(planted))+12),12-(as.numeric(as.character(season))-as.numeric(as.character(planted)))))]

subset_dt <- subset_dt[order(longitude,latitude,year,mo)]

subset_dt[season==0]$gsm <- 0


harvest_dt <- subset_dt[season == 1]
harvest_dt$myr <- harvest_dt$year

harvest_dt <- harvest_dt[,.(year,myr,longitude,latitude,planted,season,gsm)]


planted_dt <- subset_dt[planted == 1]
planted_dt$myr <- planted_dt$year

planted_dt <- planted_dt[,.(year,myr,longitude,latitude,planted,season,gsm)]


submerge_dt <- merge(subset_dt,planted_dt,by=c("year","longitude","latitude","planted","season","gsm"),all.x=T)
submerge_dt <- submerge_dt[order(longitude,latitude,year,mo)]

submerge_dt$myr <- as.numeric(as.character(submerge_dt$myr))
submerge_dt[,myr := nafill(myr,type="locf"),by=.(longitude,latitude)]
submerge_dt[,myr := nafill(myr,type="nocb"),by=.(longitude,latitude)]

submerge_dt$backward <- 12-as.numeric(as.character(submerge_dt$season))+1
submerge_dt$dif <- submerge_dt$gsm-submerge_dt$backward

subseason_dt <- submerge_dt[dif >= 0]

subseason_dt <- subseason_dt[,.(gsrain=sum(rain)),by=.(longitude,latitude,myr)]

subseason_dt <- merge(submerge_dt,subseason_dt,by=c("longitude","latitude","myr"),all.x=T)

subseason_dt <- subseason_dt[year!=1996]
subseason_dt$myr <- NULL
subseason_dt$gsm <- NULL
subseason_dt$backward <- NULL
subseason_dt$dif <- NULL

datacomb2_dt <- merge(datacomb1_dt,subseason_dt,by=c("longitude","latitude","year","mo","planted","season","rain"),all.x=T)
datacomb2_dt[is.na(gsrain)]$gsrain <- 0
datacomb2_dt$gsrain <- datacomb2_dt$gsrain*1000

dataset2_dt <- merge(dataset1_dt,subseason_dt,by=c("longitude","latitude","year","mo","planted","season","rain"),all.x=T)
dataset2_dt[is.na(gsrain)]$gsrain <- 0
dataset2_dt$gsrain <- dataset2_dt$gsrain*1000


gsmean <- round(mean(datacomb2_dt[season==1]$gsrain))
gsstd <- round(sd(datacomb2_dt[season==1]$gsrain))
gsquant <- round(quantile(datacomb2_dt[season==1]$gsrain,c(.5,.75,.9)))


# polit
polit1_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsrain-303)+log(population) | xy+country^year, dataset2_dt[actor=="polit"])

polit2_fe <- feols(incidents_dum~price_ch:max_area:i(season,keep=1:12)+price_ch:max_area:i(season,keep=1:12):I(gsrain-303-214)+log(population) | xy+country^year, dataset2_dt[actor=="polit"])

seasonal3_dt <- dataset_dt[max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal3_dt <- seasonal3_dt[order(season)]

scale3_coef <- 100*sd(dataset_dt[max_area>0 & actor=="polit"]$price_ch)*seasonal3_dt$max_area/seasonal3_dt$incidents_dum


coef1_sum <- summary(polit1_fe,cluster=~xy+country^year)

coeftab1_dt <- as.data.table(coef1_sum$coeftable[2:13,])
colnames(coeftab1_dt) <- c("est","se","trat","pval")
coeftab1_dt[,`:=`(season=as.factor(0:11))]

coeftab1_dt$est <- coeftab1_dt$est*scale3_coef
coeftab1_dt$se <- coeftab1_dt$se*scale3_coef
coeftab1_dt$regime <- as.factor("Average rainfall")

coef2_sum <- summary(polit2_fe,cluster=~xy+country^year)

coeftab2_dt <- as.data.table(coef2_sum$coeftable[2:13,])
colnames(coeftab2_dt) <- c("est","se","trat","pval")
coeftab2_dt[,`:=`(season=as.factor(0:11))]

coeftab2_dt$est <- coeftab2_dt$est*scale3_coef
coeftab2_dt$se <- coeftab2_dt$se*scale3_coef
coeftab2_dt$regime <- as.factor("Excess rainfall")

coeftab_dt <- rbind(coeftab1_dt,coeftab2_dt)

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(regime,season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))


## circular plot for illustrating the seasonal effect
gg_coef_white <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=regime,alpha=regime))+
  geom_line(aes(color=regime),size=.6)+
  geom_hline(yintercept = seq(-16,16,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray30",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  scale_fill_manual(values=c("darkgray","steelblue"))+
  scale_alpha_manual(values=c(.4,.2))+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  facet_wrap(~regime)+
  coord_polar(start=-pi*2)+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_black <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=regime,alpha=regime))+
  geom_line(aes(color=regime),size=.6)+
  geom_hline(yintercept = seq(-16,16,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray30",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  scale_fill_manual(values=c("darkgray","steelblue"))+
  scale_alpha_manual(values=c(.4,.2))+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  facet_wrap(~regime)+
  coord_polar(start=-pi*2)+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("Figures/circular_violence_rain.png",gg_coef_white,width=6.5,height=3.5,dpi=200)
ggsave("Presentation/circular_violence_rain.png",gg_coef_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/circular_violence_rain.png",gg_coef_black,width=6.5,height=3.5,dpi="retina")


coefcomb_dt <- coeftab_dt[season!=12]
coefcomb_dt[,`:=`(est_cum=cumsum(est)),by=.(regime)]

## plot the the seasonal and non-seasonal cumulative impacts together 
gg_cum_white <- ggplot(coefcomb_dt,aes(x=season,y=est_cum,color=regime,linetype=regime))+
  geom_line(size=.6)+
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(0,24,4))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  scale_linetype_manual(values=c(1,5))+
  coord_cartesian(ylim=c(0,24))+
  labs(x="months after harvest (H)",y="cumulative % change in violence")+
  theme_classic()+
  theme_white()+
  theme(legend.position="top")

gg_cum_black <- ggplot(coefcomb_dt,aes(x=season,y=est_cum,color=regime,linetype=regime))+
  geom_line(size=.6)+
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(0,24,4))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  scale_linetype_manual(values=c(1,5))+
  coord_cartesian(ylim=c(0,24))+
  labs(x="months after harvest (H)",y="cumulative % change in violence")+
  theme_classic()+
  theme_black()+
  theme(legend.position="top")

ggsave("Presentation/cumulative_violence_rain.png",gg_cum_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/cumulative_violence_rain.png",gg_cum_black,width=6.5,height=3.5,dpi="retina")


gg_circular_cumulative_rain_white <- plot_grid(gg_coef_white,gg_cum_white,ncol=1,align="v",axis="b",rel_heights = c(4,3))

ggsave("Figures/circular_cumulative_violence_rain.png",gg_circular_cumulative_rain_white,width=6.5,height=6.5,dpi=200)


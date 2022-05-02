library(data.table)
library(ggplot2)
library(cowplot)
library(fixest)
library(backports)
library(msm)

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

## aggregate by location
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

## number of grid cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)
dataset_dt <- merge(dataset_dt,xysum_dt,by="country",all.x = T)

xy_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area))]
xy_dt[max_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area))]

## aggregate by location and actor
actor_xy_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(actor,country,xy)]

actor_xy_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area)),by=.(actor)]
actor_xy_dt[max_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area)),by=.(actor)]

#################################
###  seasonality of conflict  ###
#################################

datacomb_dt[,`:=`(incidents_lag=data.table::shift(incidents_dum,1)),by=.(xy)]

dataset_dt[,`:=`(incidents_lag=data.table::shift(incidents_dum,1)),by=.(xy,actor)]


datacomb_dt[,`:=`(postharvest=ifelse(season %in% 1:3,1,2),preharvest=ifelse(season %in% c(1,11:12),1,2))]
dataset_dt[,`:=`(postharvest=ifelse(season %in% 1:3,1,2),preharvest=ifelse(season %in% c(1,11:12),1,2))]


trs <- quantile(datacomb_dt[max_area>0]$max_area,c(.5,.8,.9))

datacomb_dt[,`:=`(area_dose=as.factor(ifelse(max_area>0 & max_area<=trs[1],1,ifelse(max_area>trs[1] & max_area<=trs[2],2,ifelse(max_area>trs[2] & max_area<=trs[3],3,ifelse(max_area>trs[3],4,0))))))]
dataset_dt[,`:=`(area_dose=as.factor(ifelse(max_area>0 & max_area<=trs[1],1,ifelse(max_area>trs[1] & max_area<=trs[2],2,ifelse(max_area>trs[2] & max_area<=trs[3],3,ifelse(max_area>trs[3],4,0))))))]


###-- ALL ACTORS COMBINED --###
comb_fe <- feols(incidents_dum~price_ch:(i(area_dose,season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt,vcov=~xy+country^year)
summary(comb_fe)

###-- BY INDIVIDUAL ACTORS --###
split_fe <- feols(incidents_dum~price_ch:(i(area_dose,season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt,split=~actor,vcov=~xy+country^year)
summary(split_fe)



seasonal3_dt <- dataset_dt[max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(area_dose,season)]
seasonal3_dt <- seasonal3_dt[order(area_dose,season)]

scale3_coef <- 100*sd(dataset_dt[max_area>0 & actor=="polit"]$price_ch)/seasonal3_dt$incidents_dum

coef3_fe <- feols(incidents_dum~price_ch:(i(area_dose,season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="polit"])
coef3_sum <- summary(coef3_fe,vcov=~xy+country^year)

coeftab3_dt <- as.data.table(coef3_sum$coeftable[-1,])
colnames(coeftab3_dt) <- c("est","se","trat","pval")
coeftab3_dt[,`:=`(dose=as.factor(rep(1:4,each=12)),season=as.factor(rep(0:11,4)))]

coeftab3_dt$est <- coeftab3_dt$est*scale3_coef
coeftab3_dt$se <- coeftab3_dt$se*scale3_coef
coeftab3_dt$actor <- as.factor("Political militias")

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab3_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab3_dt <- rbind(coeftab3_dt,extra_rows)
coeftab3_dt <- coeftab3_dt[order(actor,season)]

coeftab3_dt$season <- as.numeric(as.character(coeftab3_dt$season))
coeftab3_dt$dose <- factor(coeftab3_dt$dose,levels=c(1:4),labels=c(paste0("s < ",round(trs[1],3)),paste0(round(trs[1],3)," < s < ",round(trs[2],3)),paste0(round(trs[2],3)," < s < ",round(trs[3],3)),paste0(round(trs[3],3)," < s")))

## circular plot for illustrating the seasonal effect
gg_coef <- ggplot(coeftab3_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.6,color="steelblue")+
  geom_hline(yintercept = seq(-60,60,20),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-20,40,20))+
  facet_wrap(~dose)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("Figures/circular_violence_polit_dose.png",gg_coef,width=6.5,height=6.5,dpi=200)

## circular plot for illustrating the seasonal effect
gg_coef_white <- ggplot(coeftab3_dt[dose %in% unique(coeftab3_dt$dose)[1:2]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.6,color="steelblue")+
  geom_hline(yintercept = seq(-60,60,20),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-20,40,20))+
  facet_wrap(~dose)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_black <- ggplot(coeftab3_dt[dose %in% unique(coeftab3_dt$dose)[1:2]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.6,color="steelblue")+
  geom_hline(yintercept = seq(-60,60,20),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray55",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-20,40,20))+
  facet_wrap(~dose)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("Presentation/circular_violence_polit_dose_top.png",gg_coef_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/circular_violence_polit_dose_top.png",gg_coef_black,width=6.5,height=3.5,dpi="retina")

## circular plot for illustrating the seasonal effect
gg_coef_white <- ggplot(coeftab3_dt[dose %in% unique(coeftab3_dt$dose)[3:4]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.6,color="steelblue")+
  geom_hline(yintercept = seq(-60,60,20),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-20,40,20))+
  facet_wrap(~dose)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_black <- ggplot(coeftab3_dt[dose %in% unique(coeftab3_dt$dose)[3:4]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.6,color="steelblue")+
  geom_hline(yintercept = seq(-60,60,20),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray55",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-20,40,20))+
  facet_wrap(~dose)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("Presentation/circular_violence_polit_dose_bottom.png",gg_coef_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/circular_violence_polit_dose_bottom.png",gg_coef_black,width=6.5,height=3.5,dpi="retina")




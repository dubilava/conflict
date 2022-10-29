library(data.table)
library(fixest)
library(ggplot2)
library(cowplot)
library(Cairo)
library(stringr)
library(sf)
library(sp)
library(raster)
library(rworldmap)
library(ggstar)
library(haven)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")


## clean up the environment (just in case)
rm(list=ls())
gc()

## the 'not in' function
"%!in%" <- Negate("%in%")

## custom themes for figures
theme_white <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    panel.grid=element_blank(),
    plot.background=element_rect(fill="transparent",color=NA),
    legend.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="gray35"),
    axis.title=element_text(size=10,colour="gray35"),
    axis.text=element_text(size=8,colour="gray35",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line.x=element_line(colour="gray35"),
    axis.line.y=element_line(colour="gray35"),
    axis.ticks=element_line(colour="gray35"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="gray35"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="gray35",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}


## load the map of africa
africa <- ne_countries(scale="large",continent="africa",returnclass="sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## load the data
load("data_violence_acled.RData")
load("precipitation.RData")
load("temperature.RData")


# Descriptive Stats ----

## number of total incidents, grid-cells with incidents, and average population by country
xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),crop_area=mean(crop_area),tot_area=mean(tot_area),area_dum=mean(area_dum),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country,crop)]

xycomb_dt$crop <- factor(xycomb_dt$crop,levels=unique(xycomb_dt$crop)[c(4,3,5,2,1)])


## number of polit incidents, grid-cells with incidents, and average population by country
xyset_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),max_area=mean(max_area),tot_area=mean(tot_area),area_dum=mean(area_dum),population=mean(population),population_mln=mean(population_mln)),by=.(actor,xy,longitude,latitude,country,crop)]

xyset_dt$actor <- factor(xyset_dt$actor,levels=unique(xyset_dt$actor))


# Figures: Main text ----

## Figure 1: Violence (maps) ----

datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,actor)]
datasub_dt <- datasub_dt[incidents>=1]

datasub_dt$actor <- factor(datasub_dt$actor,levels=unique(datasub_dt$actor),labels=c("State forces","Rebel groups","Political militias","Identity militias","Militias"))

gg_conflictmaps_black <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasub_dt[actor!="Militias"],aes(x=longitude,y=latitude,size=incidents,color=actor))+
  scale_color_manual(values=rep("gray20",4),guide="none")+
  scale_size(range=c(.1,1.8))+
  coord_sf(ylim=c(-35,37))+
  facet_wrap(~actor)+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.60),legend.text = element_text(hjust=1))

gg_conflictmaps_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasub_dt[actor!="Militias"],aes(x=longitude,y=latitude,size=incidents,color=actor))+
  scale_color_manual(values=c("goldenrod","indianred","steelblue","forestgreen"),guide="none")+
  scale_size(range=c(.1,1.8))+
  coord_sf(ylim=c(-35,37))+
  facet_wrap(~actor)+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.60),legend.text = element_text(hjust=1))


# png
ggsave("Figures/map_violence.png",gg_conflictmaps_white,width=6.5,height=6.5,dpi=200)
ggsave("Figures/map_violence_bw.png",gg_conflictmaps_black,width=6.5,height=6.5,dpi="retina")

# eps
ggsave("Figures/map_violence.eps",gg_conflictmaps_white,width=6.5,height=6.5,dpi="retina",device=cairo_ps)
ggsave("Figures/map_violence_bw.eps",gg_conflictmaps_black,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


## Figure 2: Violence (series) ----

datasubset_dt <- dataset_dt[,.(xy,longitude,latitude,country,year,date,actor,incidents)]

datasubset_dt <- datasubset_dt[incidents!=0]

moset0_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(year,date,actor)]

setkey(datasubset_dt, "date")
datasubset_dt[, country := as.numeric(factor(country, levels = unique(country)))]
setkey(datasubset_dt, "date", "country")
moset3_dt <- datasubset_dt[J(unique(date)), mult="last"]
moset3_dt[, country := cummax(country)]

moset_dt <- merge(moset0_dt,moset3_dt[,.(year,date,country)],by=c("year","date"),all.x=T)
moset_dt <- moset_dt[order(actor,year,date)]

moset_dt$actor <- factor(moset_dt$actor,levels=unique(moset_dt$actor))

gg_tsincidents_black <- ggplot(moset_dt[actor!="milit"],aes(x=date,y=incidents,color=actor,linetype=actor))+
  geom_line(size=.5) +
  scale_color_manual(values=c("gray40","gray40","black","black"),labels=c("State forces","Rebel groups","Political militias","Identity militias"))+
  scale_linetype_manual(values=c(1,2,1,2),labels=c("State forces","Rebel groups","Political militias","Identity militias"))+
  labs(x="Year",y="Incidents")+
  theme_classic()+
  theme_white()+
  theme(legend.position = c(.12,.88))

gg_tsincidents_white <- ggplot(moset_dt[actor!="milit"],aes(x=date,y=incidents,color=actor,linetype=actor))+
  geom_line(size=.5) +
  scale_color_manual(values=c("goldenrod","indianred","steelblue","forestgreen"),labels=c("State forces","Rebel groups","Political militias","Identity militias"))+
  scale_linetype_manual(values=c(1,2,1,2),labels=c("State forces","Rebel groups","Political militias","Identity militias"))+
  labs(x="Year",y="Incidents")+
  theme_classic()+
  theme_white()+
  theme(legend.position = c(.12,.88))


# png
ggsave("Figures/series_violence.png",gg_tsincidents_white,width=6.5,height=3.5,dpi=200)
ggsave("Figures/series_violence_bw.png",gg_tsincidents_black,width=6.5,height=3.5,dpi="retina")

# eps
ggsave("Figures/series_violence.eps",gg_tsincidents_white,width=6.5,height=3.5,dpi="retina",device=cairo_ps)
ggsave("Figures/series_violence_bw.eps",gg_tsincidents_black,width=6.5,height=3.5,dpi="retina",device=cairo_ps)


crops_dt <- datacomb_dt[date==max(date),.(xy,longitude,latitude,crop=crop2,maize_area,sorghum_area,wheat_area,rice_area)]

crops_dt[,`:=`(maize_dum=ifelse(crop=="Maize",1,0),sorghum_dum=ifelse(crop=="Sorghum",1,0),wheat_dum=ifelse(crop=="Wheat",1,0),rice_dum=ifelse(crop=="Rice",1,0))]

## grayscale maps

gg_maize_black <- ggplot(data = africa) +
  geom_sf(color="gray75",fill=NA,size=.25)+
  geom_point(data=crops_dt[maize_area>=0.001],aes(x=longitude,y=latitude,size=maize_area,shape=as.factor(maize_dum),color=as.factor(maize_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","black"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Maize")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold")) 

gg_sorghum_black <- ggplot(data = africa) +
  geom_sf(color="gray75",fill=NA,size=.25)+
  geom_point(data=crops_dt[sorghum_area>=0.001],aes(x=longitude,y=latitude,size=sorghum_area,shape=as.factor(sorghum_dum),color=as.factor(sorghum_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","black"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Sorghum")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

gg_wheat_black <- ggplot(data = africa) +
  geom_sf(color="gray75",fill=NA,size=.25)+
  geom_point(data=crops_dt[wheat_area>=0.001],aes(x=longitude,y=latitude,size=wheat_area,shape=as.factor(wheat_dum),color=as.factor(wheat_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","black"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Wheat")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

gg_rice_black <- ggplot(data = africa) +
  geom_sf(color="gray75",fill=NA,size=.25)+
  geom_point(data=crops_dt[rice_area>=0.001],aes(x=longitude,y=latitude,size=rice_area,shape=as.factor(rice_dum),color=as.factor(rice_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","black"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Rice")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))


gg_cereals_black <- plot_grid(gg_maize_black,gg_sorghum_black,gg_wheat_black,gg_rice_black,ncol=2,align="hv")

# png
ggsave("Figures/map_cereals_bw.png",gg_cereals_black,width=6.5,height=6.5,dpi="retina")

# eps
ggsave("Figures/map_cereals_bw.eps",gg_cereals_black,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


## color maps

gg_maize_white <- ggplot(data = africa) +
  geom_sf(color="gray60",fill=NA,size=.25)+
  geom_point(data=crops_dt[maize_area>=0.001],aes(x=longitude,y=latitude,size=maize_area,shape=as.factor(maize_dum),color=as.factor(maize_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","forestgreen"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Maize")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

gg_sorghum_white <- ggplot(data = africa) +
  geom_sf(color="gray60",fill=NA,size=.25)+
  geom_point(data=crops_dt[sorghum_area>=0.001],aes(x=longitude,y=latitude,size=sorghum_area,shape=as.factor(sorghum_dum),color=as.factor(sorghum_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","indianred"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Sorghum")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

gg_wheat_white <- ggplot(data = africa) +
  geom_sf(color="gray60",fill=NA,size=.25)+
  geom_point(data=crops_dt[wheat_area>=0.001],aes(x=longitude,y=latitude,size=wheat_area,shape=as.factor(wheat_dum),color=as.factor(wheat_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","goldenrod"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Wheat")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

gg_rice_white <- ggplot(data = africa) +
  geom_sf(color="gray60",fill=NA,size=.25)+
  geom_point(data=crops_dt[rice_area>=0.001],aes(x=longitude,y=latitude,size=rice_area,shape=as.factor(rice_dum),color=as.factor(rice_dum)))+
  scale_size(range=c(.1,1.4),guide="none")+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Rice")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))


gg_cereals_white <- plot_grid(gg_maize_white,gg_sorghum_white,gg_wheat_white,gg_rice_white,ncol=2,align="hv")

# png
ggsave("Figures/map_cereals.png",gg_cereals_white,width=6.5,height=6.5,dpi=200)

# eps
ggsave("Figures/map_cereals.eps",gg_cereals_white,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


## Figure 4: Prices ----

datasub_dt <- dataset_dt[(longitude==unique(longitude)[1] & latitude==unique(latitude)[1]),.(Maize=price_maize,Sorghum=price_sorghum,Wheat=price_wheat,Rice=price_rice),by=.(date)]
datasub_dt <- unique(datasub_dt)
prices_dt <- melt(datasub_dt,id.vars="date",variable.name="cereal",value.name="price")
prices_dt$cereal <- factor(prices_dt$cereal,levels=unique(prices_dt$cereal))

gg_prices_black <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.5)+
  scale_color_manual(name="cereal",values=c("gray40","gray40","black","black"))+
  scale_linetype_manual(name="cereal",values=c(1,2,1,2))+
  labs(x="Year",y="Price")+
  theme_classic()+
  theme_white()+
  theme(legend.position=c(0.05,.95),legend.justification=c(0,1),legend.text = element_text(size=10),legend.key.size=unit(.75,'lines'))

gg_prices_white <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.5)+
  scale_color_manual(name="cereal",values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_linetype_manual(name="cereal",values=c(1,2,1,2))+
  labs(x="Year",y="Price")+
  theme_classic()+
  theme_white()+
  theme(legend.position=c(0.05,.95),legend.justification=c(0,1),legend.text = element_text(size=10),legend.key.size=unit(.75,'lines'))

# png
ggsave("Figures/series_prices.png",gg_prices_white,width=6.5,height=3.5,dpi=200)
ggsave("Figures/series_prices_bw.png",gg_prices_black,width=6.5,height=3.5,dpi="retina")

# eps
ggsave("Figures/series_prices.eps",gg_prices_white,width=6.5,height=3.5,dpi="retina",device=cairo_ps)
ggsave("Figures/series_prices_bw.eps",gg_prices_black,width=6.5,height=3.5,dpi="retina",device=cairo_ps)


## Figure 5: Main effect ----

seasonal_dt <- dataset_dt[crop_area>0 & actor=="milit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
# seasonal_dt <- seasonal_dt[order(season)]

scale_coef <- 100*sd(dataset_dt[crop_area>0 & actor=="milit"]$price_ch)*seasonal_dt$crop_area/seasonal_dt$incidents_dum

coef_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="milit"])
coef_sum <- summary(coef_fe,vcov=conley(500,distance="spherical")~longitude+latitude)

coeftab_dt <- as.data.table(coef_sum$coeftable[-1,])
colnames(coeftab_dt) <- c("est","se","trat","pval")
coeftab_dt[,`:=`(season=as.factor(0:11))]

coeftab_dt$est <- coeftab_dt$est*scale_coef
coeftab_dt$se <- coeftab_dt$se*scale_coef
coeftab_dt$actor <- as.factor("Militias")

extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(actor,season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))


## circular plot for political militias
gg_coef_white <- ggplot(coeftab_dt[actor=="Militias"],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(color="steelblue",size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray60",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray40",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,10,4))+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_text(size=14),axis.text = element_text(size=12))


gg_coef_black<- ggplot(coeftab_dt[actor=="Militias"],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="gray40",alpha=.25)+
  geom_line(color="gray20",size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray60",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray40",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,10,4))+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_text(size=14),axis.text = element_text(size=12))

# png
ggsave("Figures/circular_violence_polit.png",gg_coef_white,width=6.5,height=6.5,dpi=200)
ggsave("Figures/circular_violence_polit_bw.png",gg_coef_black,width=6.5,height=6.5,dpi="retina")

# eps
ggsave("Figures/circular_violence_polit.eps",gg_coef_white,width=6.5,height=6.5,dpi="retina",device=cairo_ps)
ggsave("Figures/circular_violence_polit_bw.eps",gg_coef_black,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


## Figure 6: Dose-response ----

dataset_dt[crop_area>0,`:=`(tr1=quantile(crop_area,.5),tr2=quantile(crop_area,.8),tr3=quantile(crop_area,.9)),by=.(crop)]
dataset_dt[is.na(dataset_dt)] <- 0

dataset_dt[,`:=`(area_dose=as.factor(ifelse(crop_area>0 & crop_area<=tr1,1,ifelse(crop_area>tr1 & crop_area<=tr2,2,ifelse(crop_area>tr2 & crop_area<=tr3,3,ifelse(crop_area>tr3,4,0))))))]

dose_fe <- feols(incidents_dum~price_ch:(i(area_dose,season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

seasonal_dt <- dataset_dt[crop_area>0 & actor=="milit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(area_dose)]
seasonal_dt <- seasonal_dt[order(area_dose)]

scale_coef <- 100*sd(dataset_dt[crop_area>0 & actor=="milit"]$price_ch)/rep(seasonal_dt$incidents_dum,each=12)

coef_sum <- summary(dose_fe,vcov=conley(500,distance="spherical")~longitude+latitude)

coeftab_dt <- as.data.table(coef_sum$coeftable[-1,])
colnames(coeftab_dt) <- c("est","se","trat","pval")
coeftab_dt[,`:=`(dose=as.factor(rep(1:4,each=12)),season=as.factor(rep(0:11,4)))]

coeftab_dt$est <- coeftab_dt$est*scale_coef
coeftab_dt$se <- coeftab_dt$se*scale_coef
coeftab_dt$actor <- as.factor("Militias")

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(actor,season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))
coeftab_dt$dose <- factor(coeftab_dt$dose,levels=c(1:4),labels=c("very low","low","medium","high"))

## circular plot for illustrating the seasonal effect
gg_coef_white <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.6,color="steelblue")+
  geom_hline(yintercept = seq(-60,40,20),color="gray60",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-20,40,20))+
  facet_wrap(~dose)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_black <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="gray40",alpha=.25)+
  geom_line(size=.6,color="gray40")+
  geom_hline(yintercept = seq(-60,40,20),color="gray60",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-20,40,20))+
  facet_wrap(~dose)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

# png
ggsave("Figures/circular_violence_dose.png",gg_coef_white,width=6.5,height=6.5,dpi=200)
ggsave("Figures/circular_violence_dose_bw.png",gg_coef_black,width=6.5,height=6.5,dpi="retina")

# eps
ggsave("Figures/circular_violence_dose.eps",gg_coef_white,width=6.5,height=6.5,dpi=200,device=cairo_ps)
ggsave("Figures/circular_violence_dose_bw.eps",gg_coef_black,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


# Figures: Appendix C----

## Figure C1: Conflict prevalence----

## some descriptive statistics mentioned in the text
xyconflict_dt <- xycomb_dt[incidents>0]
xycropland_dt <- xycomb_dt[crop_area>0]
xyagricult_dt <- xycomb_dt[area_dum==1]

cycomb_dt <- xyconflict_dt[,.(xy_sum=.N,incidents_sum=sum(incidents),population_sum=sum(population_mln)),by=.(country)]

comb_dt <- xycomb_dt[,.(xy_tot=.N,incidents_tot=sum(incidents),population_tot=sum(population_mln)),by=.(country)]

cycomb_dt <- merge(cycomb_dt,comb_dt,by="country")

cycomb_dt[,`:=`(xy_prop=round(xy_sum/xy_tot,2),pop_prop=round(population_sum/population_tot,2))]

country_dt <- dataset_dt[actor!="milit",.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),crop_area=mean(crop_area),tot_area=mean(tot_area),population=mean(population),population_mln=mean(population_mln)),by=.(country)]

country_dt <- country_dt[order(country)]

country_dt <- merge(country_dt,cycomb_dt,by="country")
country_dt[,`:=`(countrylab=paste0(country," (",sprintf("%.2f",xy_prop),")"))] 

## generate the barplot
country_dt <- country_dt[order(-incidents_sum)]

country_dt$country <- factor(country_dt$country,levels=unique(country_dt$country))

gg_conflict_white <- ggplot(country_dt,aes(x=reorder(countrylab,incidents),y=incidents))+
  geom_bar(stat="identity",fill="indianred",color="white",size=.25)+
  coord_flip(ylim=c(0,max(country_dt$incidents)*1.25))+
  labs(x="Countries/Territories (proportion of cells with at least one incident)",y="Incidents")+
  theme_classic()+
  theme_white()

## generate the map
datasum_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude)]
datasum_dt <- datasum_dt[incidents>=1]

gg_map_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasum_dt,aes(x=longitude,y=latitude,size=incidents),color="indianred")+
  scale_size(range=c(.2,2.6),name="Incidents")+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.88,.88),legend.text = element_text(hjust=1),legend.title = element_text(size=10))

## generate the radial legend
dataseason_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(month)]

dataseason_dt$month <- factor(dataseason_dt$month,levels=month.abb)

gg_legend_white <- ggplot(dataseason_dt, aes(x=month,y=incidents,fill=month)) +
  geom_bar(stat="identity",fill="indianred") +
  ylim(0,5500)+
  coord_polar() +
  theme_void() +
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title=element_blank(),axis.text.x=element_text(size=7),axis.text.y=element_text(size=6))

aligned_plots_white <- align_plots(gg_conflict_white,gg_map_white,gg_legend_white,align="hv",axis="tblr")

gg_rankmap_white <- ggdraw(aligned_plots_white[[1]]) + draw_plot(aligned_plots_white[[2]],x=.03,y=-.03) + draw_plot(aligned_plots_white[[3]],x=.12,y=.07,width=.5,height=.5)

ggsave("Figures/bars_map_legend_violence.png",gg_rankmap_white,width=6.5,height=6.5,dpi=200)
ggsave("Figures/bars_map_legend_violence.eps",gg_rankmap_white,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


## Figure C2: Population----

datasub_dt <- datacomb_dt[year==2020,.(population=mean(population_mln)),by=.(longitude,latitude)]
datasub_dt <- datasub_dt[population>=.05]

gg_map_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=population),color="gray20")+
  geom_hline(yintercept = c(23.43633,0,-23.43633),color="gray35",linetype=2,size=.5)+
  annotate("text",x=-35,y=25.43633, label="Tropic of Cancer",color="gray35",size=4,hjust = 0)+
  annotate("text",x=-35,y=2, label="Equator",color="gray35",size=4,hjust = 0)+
  annotate("text",x=-35,y=-21.43633, label="Tropic of Capricorn",color="gray35",size=4,hjust = 0)+
  scale_size(range=c(.2,2.8),breaks = c(.5,1,10,20),name="Population (mln)")+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.30,.35),legend.text = element_text(hjust=1),legend.title = element_text(size=10))

ggsave("Figures/map_population.png",gg_map_white,width=6.5,height=6.5,dpi=200)
ggsave("Figures/map_population.eps",gg_map_white,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


# histogram of cropland fractions by grid-cells
gg_cropland_black <- ggplot(xycomb_dt[crop_area>0],aes(x=crop_area))+
  geom_bar(aes(fill=crop),position=position_dodge(),color="gray60",alpha=.9,size=.25) +
  scale_x_binned(n.breaks=7,limits=c(0,0.06),labels=c("0.00",seq(0.01,.05,.01),"0.10+"))+
  scale_fill_manual(name="Cereal",values=c("forestgreen","indianred","steelblue","goldenrod"))+
  labs(x="Cropland area fraction",y="Cells")+
  theme_classic()+
  theme_black()+
  theme(legend.position=c(.82,.82))

gg_cropland_white <- ggplot(xycomb_dt[crop_area>0],aes(x=crop_area))+
  geom_bar(aes(fill=crop),position=position_dodge(),color="gray65",alpha=.9,size=.25) +
  scale_x_binned(n.breaks=7,limits=c(0,0.06),labels=c("0.00",seq(0.01,.05,.01),"0.10+"))+
  scale_fill_manual(name="Cereal",values=c("forestgreen","indianred","steelblue","goldenrod"))+
  labs(x="Cropland area fraction",y="Cells")+
  theme_classic()+
  theme_white()+
  theme(legend.position=c(.82,.82))

ggsave("Figures/cropland_distribution.png",gg_cropland_white,width=6.5,height=3.5,dpi=200)
ggsave("Figures/cropland_distribution.eps",gg_cropland_white,width=6.5,height=3.5,dpi="retina",device=cairo_ps)
ggsave("Presentation/cropland_distribution.png",gg_cropland_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/cropland_distribution.png",gg_cropland_black,width=6.5,height=3.5,dpi="retina")


## Figure C3: Harvest season----

datasrt_dt <- datacomb_dt[year==2020]
datasrt_dt <- datasrt_dt[season_srt %in% 1]
datasrt_dt <- datasrt_dt[,.(xy,country,harvest_srt=month,crop=crop2,area=crop_area,season_srt)]
datasrt_dt$season_srt <- NULL
datasrt_dt <- unique(datasrt_dt)

datamid_dt <- datacomb_dt[year==2020]
datamid_dt <- datamid_dt[season %in% 1]
datamid_dt <- datamid_dt[,.(xy,country,harvest_mid=month,crop=crop2,area=crop_area,season)]
datamid_dt$season <- NULL
datamid_dt <- unique(datamid_dt)

dataend_dt <- datacomb_dt[year==2020]
dataend_dt <- dataend_dt[season_end %in% 1]
dataend_dt <- dataend_dt[,.(xy,country,harvest_end=month,crop=crop2,area=crop_area,season_end)]
dataend_dt$season_end <- NULL
dataend_dt <- unique(dataend_dt)

datahs_dt <- merge(datasrt_dt,dataend_dt,by=c("xy","country","crop","area"))
datahs_dt <- merge(datamid_dt,datahs_dt,by=c("xy","country","crop","area"))

datahs_dt$harvest_srt <- factor(datahs_dt$harvest_srt,levels=month.abb)
datahs_dt$harvest_mid <- factor(datahs_dt$harvest_mid,levels=month.abb)
datahs_dt$harvest_end <- factor(datahs_dt$harvest_end,levels=month.abb)

country_xy_dt <- datahs_dt[,.(xy,country,crop,area,harvest_srt,harvest_mid,harvest_end)]
country_xy_dt <- unique(country_xy_dt)

country_dt <- country_xy_dt[,.(gridcells=.N,area=mean(area)),by=.(country,crop,harvest_srt,harvest_mid,harvest_end)]

country <- as.character(country_dt$country)
month <- month.abb

country_month <- CJ(country,month)
country_month_dt <- unique(country_month)

country_month_dt <- merge(country_dt,country_month_dt,by=c("country"),allow.cartesian=T)

country_month_dt <- country_month_dt[order(country,crop,-gridcells,month)]

country_month_dt$month <- factor(country_month_dt$month,levels=month.abb)
country_month_dt$crop <- factor(country_month_dt$crop,levels=c("Maize","Sorghum","Wheat","Rice"))

# adjust instances when the starting and ending months are 'backwards'
country_month_dt[,`:=`(m_dif=as.numeric(as.Date(paste("2020",harvest_end,"01",sep="-"),format="%Y-%b-%d")-as.Date(paste("2020",harvest_srt,"01",sep="-"),format="%Y-%b-%d")))]

temp_dt <- country_month_dt[m_dif<0]

temp1_dt <- temp_dt
temp1_dt$harvest_end <- "Dec"
temp2_dt <- temp_dt
temp2_dt$harvest_srt <- "Jan"

dbl_dt <- rbind(temp1_dt,temp2_dt)

country_month_dt <- rbind(country_month_dt[m_dif>=0],dbl_dt)
country_month_dt$m_dif <- NULL


gg_hs_white <- ggplot(country_month_dt,aes(x=month,y=crop))+
  geom_linerange(aes(xmin=harvest_srt,xmax=harvest_end,size=gridcells,color=crop,alpha=area))+
  geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop),shape=21)+
  facet_wrap(~country,nrow=10)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme_white()+
  theme(strip.text=element_text(size=8,colour="gray55",face="bold",margin=margin(.1,0,.1,0,"cm")))

ggsave("Figures/panel_harvestseasons.png",gg_hs_white,width=6.5,height=8.0,dpi=200)
ggsave("Figures/panel_harvestseasons.eps",gg_hs_white,width=6.5,height=8.0,dpi="retina",device=cairo_ps)


## Figure C4: Growing season----

dataplant_dt <- datacomb_dt[year==2020]
dataplant_dt <- dataplant_dt[planted %in% 1]
dataplant_dt <- dataplant_dt[,.(xy,country,plant=month,crop=crop2,area=crop_area,planted)]
dataplant_dt$planted <- NULL
dataplant_dt <- unique(dataplant_dt)

dataharv_dt <- datacomb_dt[year==2020]
dataharv_dt <- dataharv_dt[season %in% 1]
dataharv_dt <- dataharv_dt[,.(xy,country,harvest=month,crop=crop2,area=crop_area,season)]
dataharv_dt$season <- NULL
dataharv_dt <- unique(dataharv_dt)

datags_dt <- merge(dataplant_dt,dataharv_dt,by=c("xy","country","crop","area"))

datags_dt$plant <- factor(datags_dt$plant,levels=month.abb)
datags_dt$harvest <- factor(datags_dt$harvest,levels=month.abb)

country_xy_dt <- datags_dt[,.(xy,country,crop,area,plant,harvest)]
country_xy_dt <- unique(country_xy_dt)

country_dt <- country_xy_dt[,.(gridcells=.N,area=mean(area)),by=.(country,crop,plant,harvest)]

country <- as.character(country_dt$country)
month <- month.abb

country_month <- CJ(country,month)
country_month_dt <- unique(country_month)

country_month_dt <- merge(country_dt,country_month_dt,by=c("country"),allow.cartesian=T)

country_month_dt <- country_month_dt[order(country,crop,-gridcells,month)]

country_month_dt$month <- factor(country_month_dt$month,levels=month.abb)
country_month_dt$crop <- factor(country_month_dt$crop,levels=c("Maize","Sorghum","Wheat","Rice"))

# adjust instances when the starting and ending months are 'backwards'
country_month_dt[,`:=`(m_dif=as.numeric(as.Date(paste("2020",harvest,"01",sep="-"),format="%Y-%b-%d")-as.Date(paste("2020",plant,"01",sep="-"),format="%Y-%b-%d")))]

temp_dt <- country_month_dt[m_dif<0]

temp1_dt <- temp_dt
temp1_dt$harvest <- "Dec"
temp2_dt <- temp_dt
temp2_dt$plant <- "Jan"

dbl_dt <- rbind(temp1_dt,temp2_dt)

country_month_dt <- rbind(country_month_dt[m_dif>=0],dbl_dt)
country_month_dt$m_dif <- NULL

country_month_dt <- country_month_dt[order(country,crop)]

gg_gs_white <- ggplot(country_month_dt,aes(x=month,y=crop))+
  geom_linerange(aes(xmin=plant,xmax=harvest,size=gridcells,color=crop,alpha=area))+
  facet_wrap(~ country,nrow=10)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme_white()+
  theme(strip.text=element_text(size=8,colour="gray55",face="bold",margin=margin(.1,0,.1,0,"cm")))

ggsave("Figures/panel_growingseasons.png",gg_gs_white,width=6.5,height=8.0,dpi=200)
ggsave("Figures/panel_growingseasons.eps",gg_gs_white,width=6.5,height=8.0,dpi="retina",device=cairo_ps)


## Figure C5: Harvest month----

datamid_dt <- datacomb_dt[year==2020]
datamid_dt <- datamid_dt[season %in% 1]
datamid_dt <- datamid_dt[,.(xy,longitude,latitude,country,harvest_mid=month,crop=crop2,area=crop_area,season)]
datamid_dt$season <- NULL
datamid_dt <- unique(datamid_dt)

datamid_dt$crop <- factor(datamid_dt$crop,levels=c("Maize","Sorghum","Wheat","Rice"))

datamid_dt$month <- factor(datamid_dt$harvest_mid,levels = month.abb)

fourseasons <- colorRampPalette(colors=c("skyblue","seagreen","forestgreen","tan","indianred","goldenrod","darkgray","skyblue"),interpolate="spline")

fourseasons_col <- fourseasons(13)[c(13,2:12)]

gg_map_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datamid_dt,aes(x=longitude,y=latitude,size=area,color=month))+
  scale_size(range=c(.2,2.8),guide="none")+
  scale_color_manual(values=fourseasons_col,breaks=month.abb,guide="none")+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme_white()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank())

datasum_dt <- datamid_dt[,.(cells=.N),by=.(month)]
datasum_dt <- rbind(datasum_dt,data.table(month="Feb",cells=0))

gg_legend_white <- ggplot(datasum_dt, aes(x=month,y=cells,fill=month)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=fourseasons_col,guide="none") +
  ylim(0,450)+
  coord_polar() +
  theme_void() +
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text.x = element_text(size=10),axis.text.y = element_blank())


aligned_plots_white <- align_plots(gg_map_white,gg_legend_white,align="hv", axis="tblr")
gg_rankmap_white <- ggdraw(aligned_plots_white[[1]]) + draw_plot(aligned_plots_white[[2]],x=.15,y=.20,width=.3,height=.3)

ggsave("Figures/map_harvest.png",gg_rankmap_white,width=6.5,height=6.5,dpi=200)
ggsave("Figures/map_harvest.eps",gg_rankmap_white,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


## Figure C6: Major crops ----

## auxiliary data
load("auxiliary.RData")

auxiliary_dt[,`:=`(cash_area=apply(auxiliary_dt[,.(cocoa_area,coffee_area)],1,max))]
auxiliary_dt[,`:=`(Cocoa=ifelse(cocoa_area==cash_area & cocoa_area>0,cocoa_area,0),Coffee=ifelse(coffee_area==cash_area & coffee_area>0,coffee_area,0))]

datacomb_dt <- merge(datacomb_dt,auxiliary_dt,by=c("longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,auxiliary_dt,by=c("longitude","latitude"),all.x=T)

datacomb_dt[,`:=`(cassava_more=ifelse(cassava_area>crop_area,1,0))]
datacomb_dt[,`:=`(cash_more=ifelse(cash_area>crop_area,1,0))]
datacomb_dt[,`:=`(cocoa_more=ifelse(cocoa_area>crop_area,1,0))]
datacomb_dt[,`:=`(coffee_more=ifelse(coffee_area>crop_area,1,0))]

datacomb_dt[,`:=`(cassava_some=ifelse(cassava_area>0,1,0))]
datacomb_dt[,`:=`(cash_some=ifelse(cash_area>0,1,0))]
datacomb_dt[,`:=`(cocoa_some=ifelse(cocoa_area>0,1,0))]
datacomb_dt[,`:=`(coffee_some=ifelse(coffee_area>0,1,0))]

dataset_dt[,`:=`(cassava_more=ifelse(cassava_area>crop_area,1,0))]
dataset_dt[,`:=`(cash_more=ifelse(cash_area>crop_area,1,0))]
dataset_dt[,`:=`(cocoa_more=ifelse(cocoa_area>crop_area,1,0))]
dataset_dt[,`:=`(coffee_more=ifelse(coffee_area>crop_area,1,0))]

dataset_dt[,`:=`(cassava_some=ifelse(cassava_area>0,1,0))]
dataset_dt[,`:=`(cash_some=ifelse(cash_area>0,1,0))]
dataset_dt[,`:=`(cocoa_some=ifelse(cocoa_area>0,1,0))]
dataset_dt[,`:=`(coffee_some=ifelse(coffee_area>0,1,0))]


## second harvest season
load("second.RData")

colnames(second_dt) <- c("longitude","latitude","area_second","crop_second")

datacomb_dt <- merge(datacomb_dt,second_dt[,.(longitude,latitude,crop_second)],by=c("longitude","latitude"),all.x=T)
datacomb_dt[is.na(crop_second)]$crop_second <- "None"

datacomb_dt[,`:=`(major_crop=ifelse(tot_area>0 & crop_area/tot_area>.8,"Unique",ifelse(tot_area>0,"Mixed","None")))]

dataset_dt <- merge(dataset_dt,second_dt[,.(longitude,latitude,crop_second)],by=c("longitude","latitude"),all.x=T)
dataset_dt[is.na(crop_second)]$crop_second <- "None"

dataset_dt[,`:=`(major_crop=ifelse(tot_area>0 & crop_area/tot_area>.8,"Unique",ifelse(tot_area>0,"Mixed","None")))]


## plot maps
subset_dt <- datacomb_dt[yearmo==max(yearmo),.(longitude,latitude,crop_area,crop_second,major_crop,cash_area,cash_more,cassava_area,cassava_more,pastoral_area,mines_area,mines_dum=as.factor(mines_dum))]

subset_dt <- unique(subset_dt)


### dominant crop ----
gg_major_white <- ggplot(data=africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=subset_dt[crop_area>0],aes(x=longitude,y=latitude,size=crop_area),color=ifelse(subset_dt[crop_area>0]$major_crop=="Unique","darkgray","indianred"),shape=ifelse(subset_dt[crop_area>0]$major_crop=="Unique",16,1))+
  scale_size(range=c(.1,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="(a) Multiple Crops")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

### single-season crop ----
gg_second_white <- ggplot(data=africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=subset_dt[crop_area>0],aes(x=longitude,y=latitude,size=crop_area),color=ifelse(subset_dt[crop_area>0]$crop_second!="None","forestgreen","darkgray"),shape=ifelse(subset_dt[crop_area>0]$crop_second!="None",1,16))+
  scale_size(range=c(.1,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="(b) Double Season")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

### cash crops ----
gg_cash_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=subset_dt[crop_area>0],aes(x=longitude,y=latitude,size=cash_area,shape=as.factor(cash_more),color=as.factor(cash_more)))+
  scale_size(range=c(.1,.9),guide="none")+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("darkgray","indianred"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="(c) Coffee & Cocoa")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

### cassava ----
gg_cassava_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=subset_dt[crop_area>0],aes(x=longitude,y=latitude,size=cassava_area,shape=as.factor(cassava_more),color=as.factor(cassava_more)))+
  scale_size(range=c(.1,.9),guide="none")+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("darkgray","goldenrod"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="(d) Cassava")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))


### nomad pastoral ----
gg_pastoral_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=subset_dt[pastoral_area>.3 & crop_area>0],aes(x=longitude,y=latitude,size=crop_area),color="forestgreen",shape=1)+
  geom_point(data=subset_dt[(pastoral_area>0 & pastoral_area<=.3) & crop_area>0],aes(x=longitude,y=latitude,size=crop_area),color="darkgray",shape=16)+
  scale_size(range=c(.1,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="(e) Pastoral")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

### mining sites ----
gg_mines_white <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=subset_dt[mines_area>0],aes(x=longitude,y=latitude,size=mines_area,shape=as.factor(mines_dum),color=as.factor(mines_dum)))+
  scale_size(range=c(.1,.6),guide="none")+
  scale_shape_manual(values=c(1,1))+
  scale_color_manual(values=c("darkgray","indianred"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="(f) Mining")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))


gg_all_six <- plot_grid(gg_major_white,gg_second_white,gg_cash_white,gg_cassava_white,gg_pastoral_white,gg_mines_white,ncol=2,align="hv")

### png
ggsave("Figures/maps_all-six.png",gg_all_six,width=6.5,height=7.5,dpi=200)

### eps
ggsave("Figures/maps_all-six.eps",gg_all_six,width=6.5,height=7.5,dpi="retina",device=cairo_ps)


## Figure C7: Hotspots ----

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

xy_combcountry <- xycomb_dt[country%in%country_comb06]$xy

country_comb06[which(country_comb06=="Dem. Rep. Congo")] <- "Democratic Republic of the Congo"

hotspots_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(xy,longitude,latitude)]

hotspots_dt <- hotspots_dt[incidents>0]

gg_hotspots <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=ifelse(africa$geounit %in% country_comb06, 'indianred',NA),alpha=.25,size=.25)+
  geom_point(data=hotspots_dt[xy %!in% xy_comb01cells],aes(x=longitude,y=latitude,size=incidents),color="darkgray")+
  geom_point(data=hotspots_dt[xy %in% xy_comb01cells[-c(1:9)]],aes(x=longitude,y=latitude),size=1.9,color="indianred")+
  geom_star(data=hotspots_dt[xy %in% xy_comb20incid],aes(x=longitude,y=latitude),size=2.2,color="indianred",fill="indianred")+
  scale_size(range=c(.2,1.8),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="Hotspots")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=0.5,size=10,colour="gray35",face="bold"))

# png
ggsave("Figures/map_hotspots.png",gg_hotspots,width=6.5,height=6.5,dpi=200)

# eps
ggsave("Figures/map_hotspots.eps",gg_hotspots,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


library(data.table)
library(ggplot2)
library(cowplot)
library(Cairo)
library(stringr)
library(sf)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the map of africa
africa <- ne_countries(continent="africa",returnclass="sf")

## load the data
load("violence.RData")

## number of incidents, grid-cells with incidents, and average population by country
xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),max_area=mean(max_area),tot_area=mean(tot_area),area_dum=mean(area_dum),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country)]

## some descriptive statistics mentioned in the text
xyconflict_dt <- xycomb_dt[incidents>0]
xycropland_dt <- xycomb_dt[max_area>0]
xyagricult_dt <- xycomb_dt[area_dum==1]


#################
###  FIGURES  ###
#################

#---------------------------------#
#--  Figure 1: Country Ranking  --#
#---------------------------------#

cycomb_dt <- xyconflict_dt[,.(xy=.N,incidents_sum=sum(incidents),population_sum=sum(population_mln)),by=.(country)]

country_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),max_area=mean(max_area),tot_area=mean(tot_area),population=mean(population),population_mln=mean(population_mln)),by=.(country)]

country_dt <- country_dt[order(country)]

country_dt <- merge(country_dt,cycomb_dt,by="country")
country_dt$countrylab <- paste0(country_dt$country," (",country_dt$xy,")")

country_dt <- country_dt[order(-incidents_sum)]

country_dt$country <- factor(country_dt$country,levels=unique(country_dt$country))

gg_conflict <- ggplot(country_dt,aes(x=reorder(countrylab,incidents),y=incidents))+
  geom_bar(stat="identity",fill="indianred",alpha=.75)+
  coord_flip()+
  labs(x="Countries/Territories",y="Incidents",caption="Data Source: Armed Conflict Location & Event Data\n (ACLED) Project; available at https://acleddata.com")+
  theme_classic()+
  theme(legend.position=c(.65,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=9),plot.title=element_text(size=16),axis.line.y=element_blank(),axis.ticks.y=element_blank())

ggsave("Paper/conflict.png",gg_conflict,width=6.5,height=7.5,dpi="retina")


#-------------------------------#
#--  Figure 2: Conflict Maps  --#
#-------------------------------#

datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,actor)]
datasub_dt <- datasub_dt[incidents>=1]

gg_v1map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="state"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="gray60")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.25,2.25))+
  labs(title="State Forces")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

gg_v2map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="rebel"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="seagreen")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.25,2.25))+
  labs(title="Rebel Groups")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

gg_v3map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="polit"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.25,2.25))+
  labs(title="Political Militia")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

gg_v4map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="ident"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="goldenrod")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.25,2.25))+
  labs(title="Identity Militia")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

pg <- plot_grid(gg_v1map,gg_v2map,NULL,NULL,gg_v3map,gg_v4map,ncol=2,rel_heights=c(1,.05,1))

caption <- ggdraw() + 
  draw_label("Data Source: Armed Conflict Location & Event Data\n (ACLED)  Project; available at https://acleddata.com",x=1,hjust=1,size=8,fontface="italic",colour="gray50")

gg_vmap <- plot_grid(pg,caption,ncol=1,rel_heights=c(1,.1))

ggsave("Paper/maps_violence.png",gg_vmap,width=6.5,height=6.5,dpi="retina")


#----------------------------------------#
#--  Figure 3: Cereals Map and Prices  --#
#----------------------------------------#

datasub_dt <- datacomb_dt[,.(crop,area=max_area),by=.(longitude,latitude)]
datasub_dt <- unique(datasub_dt)
datasub_dt <- datasub_dt[crop!="None" & area >=0.01]
crop_list <- c("Maize","Sorghum","Wheat","Rice")
datasub_dt$crop <- factor(datasub_dt$crop,levels=crop_list)

gg_cereals <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,color=crop,size=area,alpha=area))+
  scale_size(range=c(.25,2.25),guide=F)+
  scale_alpha(range=c(.5,.75),guide=F)+
  scale_color_manual(name="Cereal",values=c("seagreen","indianred","goldenrod","gray60"))+
  labs(title="Geographic Distribution of Cereal Production")+
  theme_void()+
  theme(legend.position=c(0.05,.45),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

datasub_dt <- dataset_dt[(longitude==unique(longitude)[1] & latitude==unique(latitude)[1]),.(Maize=price_maize,Sorghum=price_sorghum,Wheat=price_wheat,Rice=price_rice),by=.(date)]
datasub_dt <- unique(datasub_dt)
prices_dt <- melt(datasub_dt,id.vars="date",variable.name="cereal",value.name="price")
prices_dt$cereal <- factor(prices_dt$cereal,levels=crop_list)

gg_prices <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.8,alpha=.75)+
  scale_color_manual(name="cereal",values=c("Seagreen","indianred","goldenrod","gray60"))+
  labs(title="International Cereal Prices",x="Year",y="Price")+
  theme_classic()+
  theme(legend.position=c(0.05,.95),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

pg <- plot_grid(gg_cereals,gg_prices,ncol=2)

caption <- ggdraw() + 
  draw_label("Data Sources: Center for Sustainability and the Global Environment \u2013 Nelson Institute,\n available at https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php; and\n IMF Primary Commodity Prices, available at https://www.imf.org/en/Research/commodity-prices",x=1,hjust=1,size=8,fontface="italic",colour="gray50")

gg_cereals_prices <- plot_grid(pg,caption,ncol=1,rel_heights=c(1,.1))

ggsave("Paper/cereals_prices.png",gg_cereals_prices,width=6.5,height=3.5,dpi="retina")


#-----------------------------#
#--  Figure F1: Population  --#
#-----------------------------#
datasub_dt <- datacomb_dt[,.(population=mean(population_mln)),by=.(longitude,latitude)]
datasub_dt <- datasub_dt[population>=.05]

gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=population,alpha=population),color="gray50")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(caption="Data Source: NASA SEDAC \u2013 Gridded Population of the World,\n Version 4 (GPWv4) available at https://sedac.ciesin.columbia.edu")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11))

ggsave("Paper/map_population.png",gg_map,width=6.5,height=6.5,dpi="retina")


#---------------------------------#
#--  Figure F2: Harvest Months  --#
#---------------------------------#

datasub_dt <- datacomb_dt[year=="2010" & season==1 & max_area > 0.01]
datasub_dt$month <- factor(datasub_dt$month,levels=month.abb[1:12])

month_sum <- datasub_dt[,.N,by=.(month)]

datasub_dt <- merge(datasub_dt,month_sum,by="month",all.x=T)

datasub_dt[,`:=` (month_num=paste0(month," (",N,")"))]

datasub_dt <- datasub_dt[order(month)]
datasub_dt$month_num <- factor(datasub_dt$month_num,levels=unique(datasub_dt$month_num))

gg_harvest <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,color=month_num),alpha=.75)+
  scale_color_discrete(name="area")+
  labs(caption="Data Source: Center for Sustainability and the Global Environment \u2013 Nelson Institute\n available at https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php")+
  theme_void()+
  theme(legend.position=c(.15,.52),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=0),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11))

ggsave("Paper/map_harvest.png",gg_harvest,width=6.5,height=6.5,dpi="retina")

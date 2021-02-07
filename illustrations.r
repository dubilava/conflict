library(data.table)
library(ggplot2)
library(cowplot)
library(stringr)
library(sf)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(fixest)
library(backports)

rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

# load the map of africa
africa <- ne_countries(continent = "africa",returnclass = "sf")

# load the data
load("dataset.RData")

datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(longitude,latitude,mo,xy,date,yearmo,year,month,season,crop,tot_area,max_area,price,price_wt,price_ch,price_chwt,cocoa_ch,platinum_ch,population)]


## harvest months
datasub_dt <- datacomb_dt[yearmo=="2010-01" & max_area>=0.01]

gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,color=season))+
  scale_color_discrete(name="area")+
  # scale_alpha(name="area",range=c(.5,.75))+
  # scale_size(name="area",range=c(.5,4.5))+
  labs(title="Harvest Months",subtitle="(grid-cells with harvested area \u2265 1%)",caption="Data Source: Center for Sustainability and the Global Environment \n Nelson Institute at University of Wisconsin-Madison \n https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php")+
  theme_void()+
  theme(legend.position=c(.15,.52),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_map

ggsave("map_harvest.png",gg_map,width=6.5,height=6.5)

##---

## population
datasub_dt <- datacomb_dt[,.(population=mean(population)/1000000),by=.(longitude,latitude)]
datasub_dt <- datasub_dt[population>=.1]

gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=population,alpha=population),color="gray50")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Population (million)",subtitle="(grid-cells with population \u2265 100K in 2010)",caption="Data Source: NASA Socioeconomic Data and Applications Center (SEDAC)\n Gridded Population of the World, Version 4 (GPWv4)\n https://sedac.ciesin.columbia.edu")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_map

ggsave("map_population.png",gg_map,width=6.5,height=6.5)

##---

## violence
datasub_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude)]
datasub_dt <- datasub_dt[incidents>=10]

gg_vmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Against Civilians",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_vmap

ggsave("map_violence.png",gg_vmap,width=6.5,height=6.5)


datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,event)]
datasub_dt <- datasub_dt[incidents>=5]

gg_v1map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event==17],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Against Civilians by State Forces",subtitle="(grid-cells with incidents \u2265 5 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v1map

ggsave("map_state.png",gg_v1map,width=6.5,height=6.5)


gg_v2map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event==27],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Against Civilians by Rebel Groups",subtitle="(grid-cells with incidents \u2265 5 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v2map

ggsave("map_rebel.png",gg_v2map,width=6.5,height=6.5)



gg_v3map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event==37],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Against Civilians by Political Militia",subtitle="(grid-cells with incidents \u2265 5 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v3map

ggsave("map_political.png",gg_v3map,width=6.5,height=6.5)



gg_v4map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event==47],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Against Civilians by Identity Militia",subtitle="(grid-cells with incidents \u2265 5 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v4map

ggsave("map_identity.png",gg_v4map,width=6.5,height=6.5)



## cereal crops
datasub_dt <- datacomb_dt[,.(crop,area=max_area),by=.(longitude,latitude)]
datasub_dt <- unique(datasub_dt)
datasub_dt <- datasub_dt[crop!="None" & area >=0.01]

crop_list <- c("Maize","Sorghum","Wheat","Rice")

datasub_dt$crop <- factor(datasub_dt$crop,levels=crop_list)

gg_cereals <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,color=crop,size=area,alpha=area))+
  scale_size(range=c(.5,4.5),guide=F)+
  scale_alpha(range=c(.5,.75),guide=F)+
  scale_color_manual(name="Cereal",values=c("Seagreen","indianred","goldenrod","gray60"))+
  labs(title="Geographical Distribution of Major Cereal Crops",subtitle="(grid-cells with harvested area \u2265 1%)",caption="Data Source: Center for Sustainability and the Global Environment \n Nelson Institute at University of Wisconsin-Madison \n https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_cereals

ggsave("map_crops_01.png",gg_cereals,width=6.5,height=6.5)

##---


## prices
datasub_dt <- dataset_dt[(longitude==unique(longitude)[1] & latitude==unique(latitude)[1]),.(Maize=price_maize,Sorghum=price_sorghum,Wheat=price_wheat,Rice=price_rice),by=.(date)]
datasub_dt <- unique(datasub_dt)

prices_dt <- melt(datasub_dt,id.vars="date",variable.name="cereal",value.name="price")

prices_dt$cereal <- factor(prices_dt$cereal,levels=crop_list)

gg_prices <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.8,alpha=.75)+
  scale_color_manual(name="cereal",values=c("Seagreen","indianred","goldenrod","gray60"))+
  labs(title="Major Cereal Crop Prices",subtitle="(natural logarithm of the mean-centered price series)",caption="Data Source: International Monetary Fund Primary Commodity Prices \n https://www.imf.org/en/Research/commodity-prices",x="Year",y="Price")+
  theme_classic()+
  theme(legend.position=c(.05,.95),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_prices

ggsave("prices.png",gg_prices,width=6.5,height=6.5)


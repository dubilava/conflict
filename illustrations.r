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

dataset_dt[,`:=` (population_mln = population/1000000)]
dataset_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,date,yearmo,mo,month,crop,tot_area,max_area,plant,season_srt,season,season_end,price,price_ch,price_d,price_maize,price_sorghum,price_wheat,price_rice,price_cocoa,cocoa_ch,cocoa_d,population,population_mln)]
datacomb_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

xysum_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),price_sd=sd(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population=mean(population),population_mln=mean(population_mln)),by=.(event,xy,longitude,latitude,country)]

xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),price_sd=sd(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country)]
xycomb_dt <- xycomb_dt[incidents>0]
cycomb_dt <- xycomb_dt[,.(xy=.N,incidents_sum=sum(incidents)),by=.(country)]

# xysum_dt <- merge(xysum_dt,xycomb_dt[,.(xy)],by="xy")

country_dt <- xysum_dt[,.(incidents=sum(incidents)),by=.(event,country)]
country_dt <- country_dt[order(country,event)]

country_dt <- merge(country_dt,cycomb_dt,by="country")
country_dt$countrylab <- paste0(country_dt$country," (",country_dt$xy,")")

country_dt <- country_dt[order(-incidents_sum,event)]

country_dt$country <- factor(country_dt$country,levels=unique(country_dt$country))

gg_conflict <- ggplot(country_dt,aes(x=reorder(countrylab,incidents),y=incidents,fill=event,group=event))+
  geom_bar(stat="identity",alpha=.75)+
  coord_flip()+
  scale_fill_manual(values=c("Riots"="steelblue","Violence"="indianred"))+
  labs(title="Country Ranking by Conflict Incidence",subtitle="(total number over 1997-2020 period)",x="Countries",y="Incidents",caption="Data Source: Armed Conflict Location & Event Data Project (ACLED)\n https://acleddata.com")+
  theme_classic()+
  theme(legend.position=c(.65,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16),axis.line.y=element_blank(),axis.ticks.y=element_blank())

gg_conflict

ggsave("conflict.png",gg_conflict,width=6.5,height=7.5)

xymean_dt <- datacomb_dt[,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country)]

aggregate_dt <- datacomb_dt[,.(incidents=sum(incidents),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country,year)]

aggregate_dt[,`:=` (incidents_pop=incidents/population_mln,incidents_dum=ifelse(incidents>0,1,0))]

xyaggregate_dt <- aggregate_dt[,.(incident_year=sum(incidents_dum),crop_area=mean(tot_area),population_mean=mean(population)),by=.(xy)]

countryaggregate_dt <- aggregate_dt[,.(incidents=sum(incidents)),by=.(country)]

aggregate_dt[,`:=` (agri=ifelse(max_area>.01,1,0))]

yearaggregate_dt <- aggregate_dt[,.(incidents_pop=mean(incidents_pop)),by=.(year,agri)]

ggplot(xymean_dt,aes(x=incidents,y=incidents_dum))+
  geom_point()

# gg_scatter <- ggplot(superaggregate_dt,aes(x=crop_area,y=incident_year))+
#   geom_point(color="steelblue",alpha=.5)+
#   labs(title="Population and Crop Production",subtitle="(all grid-cells)",x="Population (log-scale)",y="Cropland Share",caption="Data Sources: Center for Sustainability and the Global Environment \u2013 Nelson Institute;\n NASA SEDAC \u2013 Gridded Population of the World, Version 4 (GPWv4)")+
#   theme_classic()+
#   theme(legend.position=c(.15,.52),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=0),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))
# 
# ggsave("scatter_pop_crop.png",gg_scatter,width=6.5,height=4.5)


datacomb_dt <- merge(datacomb_dt,xyaggregate_dt,by=c("xy"),all.x=T)


### TABLES

descriptive1_dt <- datacomb_dt[,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive2_dt <- datacomb_dt[incident_year>0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive3_dt <- datacomb_dt[tot_area>0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive4_dt <- datacomb_dt[tot_area>0 & incident_year>0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive5_dt <- datacomb_dt[population_mln>0.1,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive6_dt <- datacomb_dt[population_mln>0.1 & incident_year>0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]

descriptive7_dt <- datacomb_dt[price_ch>0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive8_dt <- datacomb_dt[price_ch<=0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]

descriptive11_dt <- datacomb_dt[population_mln>0.1 & price_ch>0 & tot_area>0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive01_dt <- datacomb_dt[population_mln>0.1 & price_ch<=0 & tot_area>0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive10_dt <- datacomb_dt[population_mln>0.1 & price_ch>0 & tot_area==0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]
descriptive00_dt <- datacomb_dt[population_mln>0.1 & price_ch<=0 & tot_area==0,.(incidents=mean(incidents),sd=sd(incidents),incidents_pop=mean(incidents_pop),pop_sd=sd(incidents_pop),incidents_dum=mean(incidents_dum),dum_sd=sd(incidents_dum))]


### FIGURES

#-- Conflict by Type
datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,event)]
datasub_dt <- datasub_dt[incidents>=10]

gg_vmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event=="Violence"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Against Civilians",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data Project (ACLED)\n https://acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_vmap

ggsave("map_violence.png",gg_vmap,width=6.5,height=6.5)

gg_rmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event=="Riots"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="steelblue")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Riots",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data Project (ACLED)\n https://acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_rmap

ggsave("map_riots.png",gg_rmap,width=6.5,height=6.5)


#-- Conflict by Actor
datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,actor)]
datasub_dt <- datasub_dt[incidents>=10]

gg_v1map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[actor=="state"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="gray60")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Involving State Forces",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data Project (ACLED)\n https://acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v1map

ggsave("map_state.png",gg_v1map,width=6.5,height=6.5)


gg_v2map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[actor=="rebel"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="seagreen")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Involving Rebel Groups",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data Project (ACLED)\n https://acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v2map

ggsave("map_rebel.png",gg_v2map,width=6.5,height=6.5)


gg_v3map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[actor=="polit"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Involving Political Militia",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data Project (ACLED)\n https://acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v3map

ggsave("map_political.png",gg_v3map,width=6.5,height=6.5)


gg_v4map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[actor=="ident"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="goldenrod")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Involving Identity Militia",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data Project (ACLED)\n https://acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_v4map

ggsave("map_identity.png",gg_v4map,width=6.5,height=6.5)



#-- Cropland
# library(viridis)
# library(RColorBrewer)

# harvest months
datasub_dt <- datacomb_dt[year=="2010" & season==1 & max_area > 0.01]
datasub_dt$month <- factor(datasub_dt$month,levels=month.abb[1:12])

gg_harvest <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,color=month),alpha=.75)+
  scale_color_discrete(name="area")+
  # scale_color_viridis(name="area",discrete=T,alpha=.75,option="E")+
  labs(title="Harvest Months",subtitle="(grid-cells with cropland share \u2265 0.01)",caption="Data Source: Center for Sustainability and the Global Environment \u2013 Nelson Institute\n https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php")+
  theme_void()+
  theme(legend.position=c(.15,.52),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=0),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_harvest

ggsave("map_harvest.png",gg_harvest,width=6.5,height=6.5)


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
  scale_color_manual(name="Cereal",values=c("seagreen","indianred","goldenrod","gray60"))+
  labs(title="Geographical Distribution of Major Cereal Crops",subtitle="(grid-cells with harvested area \u2265 1%)",caption="Data Source: Center for Sustainability and the Global Environment \u2013 Nelson Institute\n https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_cereals

ggsave("map_crops.png",gg_cereals,width=6.5,height=6.5)


#-- Prices
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


### population
datasub_dt <- datacomb_dt[,.(population=mean(population_mln)),by=.(longitude,latitude)]
datasub_dt <- datasub_dt[population>=.1]

gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=population,alpha=population),color="gray50")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Population (million)",subtitle="(grid-cells with average population \u2265 100K over 1997-2020 period)",caption="Data Source: NASA SEDAC \u2013 Gridded Population of the World, Version 4 (GPWv4)\n https://sedac.ciesin.columbia.edu")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_map

ggsave("map_population.png",gg_map,width=6.5,height=6.5)



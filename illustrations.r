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

# xy2cty = function(points,continent=NULL,country=NULL){
#   countries_sf <- ne_countries(scale="medium",returnclass="sf",continent=continent,country=country)
#   points_sf <- st_as_sf(sp::SpatialPoints(points))
#   st_crs(points_sf) <- st_crs(countries_sf)
#   indices <- st_join(points_sf,countries_sf,join=st_intersects,left=T)
#   countries <- indices$name
#   coords <- as.data.table(st_coordinates(indices))
#   colnames(coords) <- colnames(points)
#   return(list(countries,coords))
# }

# load the map of africa
africa <- ne_countries(continent = "africa",returnclass = "sf")

# load the data
load("dataset.RData")


datasub_dt <- dataset_dt[,.(population=mean(population)/1000000),by=.(longitude,latitude)]

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



# dataset_dt$country <- xy2cty(dataset_dt[,.(longitude,latitude)])[[1]]

# length(unique(dataset_dt$country))
length(unique(dataset_dt$xy))

# data_country_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(country,event)]
# data_country_dt <- data_country_dt[order(event,-incidents)]

datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,event)]

datasub_dt <- datasub_dt[incidents>=10]

gg_pmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event=="Protests"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="steelblue")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Protests and Riots",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_pmap

ggsave("map_protests.png",gg_pmap,width=6.5,height=6.5)


gg_vmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=datasub_dt[event=="Violence"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,4.5))+
  labs(title="Violence Against Civilians",subtitle="(grid-cells with incidents \u2265 10 over 1997-2020 period)",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_vmap

ggsave("map_violence.png",gg_vmap,width=6.5,height=6.5)



datasub_dt <- dataset_dt[,.(crop,area=max_area),by=.(longitude,latitude)]
datasub_dt <- unique(datasub_dt)
datasub_dt <- datasub_dt[crop!="None" & area >=0.01]

crop_list <- c("Maize","Sorghum","Wheat","Rice")

datasub_dt$crop <- factor(datasub_dt$crop,levels=crop_list)

# plot the map of crops
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



datasub_dt <- dataset_dt[(longitude==unique(longitude)[1] & latitude==unique(latitude)[1]),.(Maize=price_maize,Sorghum=price_sorghum,Wheat=price_wheat,Rice=price_rice),by=.(date)]
datasub_dt <- unique(datasub_dt)

prices_dt <- melt(datasub_dt,id.vars="date",variable.name="cereal",value.name="price")

# prices_dt <- prices_dt[,.(date,price,price_norm=log(price/mean(price))),by=.(cereal)]

prices_dt$cereal <- factor(prices_dt$cereal,levels=crop_list)

gg_prices <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.8,alpha=.75)+
  scale_color_manual(name="cereal",values=c("Seagreen","indianred","goldenrod","gray60"))+
  labs(title="Major Cereal Crop Prices",subtitle="(natural logarithm of the mean-centered price series)",caption="Data Source: International Monetary Fund Primary Commodity Prices \n https://www.imf.org/en/Research/commodity-prices",x="Year",y="Price")+
  theme_classic()+
  theme(legend.position=c(.05,.95),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_prices

ggsave("prices.png",gg_prices,width=6.5,height=6.5)


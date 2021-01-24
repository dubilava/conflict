library(data.table)
library(ggplot2)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


rm(list=ls())
gc()

# ## prices
# prices_dt <- fread("Local/prices.csv")
# prices_dt$Date <- as.Date(prices_dt$Date,format="%d/%m/%Y")
# 
# save(prices_dt,file="Local/prices.RData")

"%!in%" <- Negate("%in%")

# load the map of africa
africa <- ne_countries(continent = "africa",returnclass = "sf")

load("Local/prices.RData")
load("Local/crops.RData")

crop_dt$latitude <- as.numeric(crop_dt$y)
crop_dt$longitude <- as.numeric(crop_dt$x)

crop_dt[Max_area<0.00]$Crop <- NA

crop_list <- c("Maize","Sorghum","Wheat","Rice")

crop_dt <- crop_dt[Crop %in% crop_list]
crop_dt$Crop <- factor(crop_dt$Crop,levels=crop_list)

# plot the map of crops
gg_cereals <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=crop_dt,aes(x=longitude,y=latitude,color=Crop,size=Max_area,alpha=Max_area))+
  scale_size(range=c(.5,2.5),guide=F)+
  scale_alpha(range=c(.5,.75),guide=F)+
  scale_color_manual(name="Cereal",values=c("Seagreen","indianred","goldenrod","gray60"))+
  labs(title="Geographical Distribution of Major Cereal Crops",subtitle="(harvested area > 0%)",caption="Data Source: Center for Sustainability and the Global Environment \n Nelson Institute at University of Wisconsin-Madison \n https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_cereals

ggsave("map_crops_00.png",gg_cereals,width=6.5,height=6.5)


prices_lg <- melt(prices_dt,id.vars="Date",variable.name="Cereal",value.name="Price")

prices_lg <- prices_lg[,.(Date,Price,Price_norm=log(Price/mean(Price))),by=.(Cereal)]

prices_lg$Cereal <- factor(prices_lg$Cereal,levels=crop_list)

gg_prices <- ggplot(prices_lg,aes(x=Date,y=Price_norm,color=Cereal,linetype=Cereal,group=Cereal))+
  geom_line(size=.8,alpha=.75)+
  scale_color_manual(name="Cereal",values=c("Seagreen","indianred","goldenrod","gray60"))+
  labs(title="Price Series of Major Cereal Crops",subtitle="(natural logarithm of the mean-centered series)",caption="Data Source: International Monetary Fund Primary Commodity Prices \n https://www.imf.org/en/Research/commodity-prices",x="Year",y="Price")+
  theme_classic()+
  theme(legend.position=c(.05,.95),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_prices

ggsave("prices.png",gg_prices,width=6.5,height=6.5)





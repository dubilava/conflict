library(data.table)
library(ggplot2)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

# load the map of africa
africa <- ne_countries(continent = "africa",returnclass = "sf")


load("crops.RData")

crop_dt$latitude <- as.numeric(crop_dt$y)
crop_dt$longitude <- as.numeric(crop_dt$x)

crop_dt[Max_area<0.05]$Crop <- NA

crop_dt <- crop_dt[Crop %in% c("Maize","Millet","Sorghum","Wheat","Rice")]
crop_dt$Crop <- factor(crop_dt$Crop,levels=c("Maize","Millet","Sorghum","Wheat","Rice"))

# plot the map of crops
gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=crop_dt,aes(x=longitude,y=latitude,color=Crop,size=Max_area,alpha=Max_area))+
  scale_size(range=c(.5,2.5),guide=F)+
  scale_alpha(range=c(.5,.75),guide=F)+
  scale_color_manual(values=c("Seagreen","steelblue","indianred","goldenrod","gray60"))+
  labs(title="Geographical Distribution of Major Crops",subtitle="(harvested area > 5%)",caption="Data Source: Center for Sustainability and the Global Environment \n Nelson Institute at University of Wisconsin-Madison \n https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_text(size=14),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))

gg_map

ggsave("map_crops_05.png",gg_map,width=6.5,height=6.5)



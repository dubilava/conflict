library(acled.api)
library(data.table)
library(ggplot2)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# acled_df <- acled.api(email.address = "david.ubilava@sydney.edu.au",
#                       access.key = "f9c1b-!TmXELNx-IcYvI",
#                       region=c(1:5),
#                       add.variables=c("geo_precision","time_precision","longitude","latitude"))
# 
# acled_dt <- as.data.table(acled_df)
# 
# save(acled_dt,file="acled.RData")

rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

load("acled.RData")

acled_dt$latitude <- as.numeric(acled_dt$latitude)
acled_dt$longitude <- as.numeric(acled_dt$longitude)

acled_dt <- acled_dt[geo_precision %in% c(1,2) & year != 2021]

# load the map of africa
africa <- ne_countries(continent = "africa",returnclass = "sf")

# # plot all incidents
# ggplot(data = africa) +
#   geom_sf()+
#   geom_point(data=acled_dt,aes(x=longitude,y=latitude),color="indianred",alpha=.25)+
#   theme_void()+
#   theme(legend.position = c(.25,.25))

# aggregate geo-precision to 1 degree
acled_dt$lon1deg <- round(acled_dt$longitude-.5)+.5
acled_dt$lat1deg <- round(acled_dt$latitude-.5)+.5

acled_1deg_dt <- acled_dt[,.(Incidents=.N),by=.(lon1deg,lat1deg)]

# plot the map of incidents
gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled_1deg_dt,aes(x=lon1deg,y=lat1deg,size=Incidents,color=Incidents,alpha=Incidents),color="gray30")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,2.5))+
  labs(title="Conflict and Violence",subtitle="all incidents",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_text(size=14),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=12),plot.title=element_text(size=16))

gg_map

ggsave("map_incidents.png",gg_map,width=6.5,height=6.5)


acled_fatal_1deg_dt <- acled_dt[fatalities>0,.(Incidents=.N),by=.(lon1deg,lat1deg)]

# plot the map of incidents
gg_fmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled_fatal_1deg_dt,aes(x=lon1deg,y=lat1deg,size=Incidents,color=Incidents,alpha=Incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,2.5))+
  labs(title="Conflict and Violence",subtitle="all fatal incidents",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_text(size=14),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=12),plot.title=element_text(size=16))

gg_fmap

ggsave("map_fatal_incidents.png",gg_fmap,width=6.5,height=6.5)

# unique(acled_dt$event_type)
# unique(acled_dt[event_type %in% c("Protests","Riots")]$sub_event_type)
# unique(acled_dt[event_type %in% c("Violence against civilians")]$sub_event_type)


acled_violence_1deg_dt <- acled_dt[event_type %in% c("Violence against civilians"),.(Incidents=.N),by=.(lon1deg,lat1deg)]

# plot the map of incidents
gg_vmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled_violence_1deg_dt,aes(x=lon1deg,y=lat1deg,size=Incidents,color=Incidents,alpha=Incidents),color="indianred")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,2.5))+
  labs(title="Conflict and Violence",subtitle="violence against civilians",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_text(size=14),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=12),plot.title=element_text(size=16))

gg_vmap

ggsave("map_violence.png",gg_vmap,width=6.5,height=6.5)


acled_protests_1deg_dt <- acled_dt[event_type %in% c("Protests","Riots"),.(Incidents=.N),by=.(lon1deg,lat1deg)]

# plot the map of incidents
gg_pmap <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled_protests_1deg_dt,aes(x=lon1deg,y=lat1deg,size=Incidents,color=Incidents,alpha=Incidents),color="gray30")+
  scale_alpha(range=c(.5,.75))+
  scale_size(range=c(.5,2.5))+
  labs(title="Conflict and Violence",subtitle="protests and riots",caption="Data Source: Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_text(size=14),legend.text=element_text(size=14,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=12),plot.title=element_text(size=16))

gg_pmap

ggsave("map_protests.png",gg_pmap,width=6.5,height=6.5)



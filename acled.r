library(acled.api)
library(data.table)
library(ggplot2)
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

# plot all incidents
ggplot(data = africa) +
  geom_sf()+
  geom_point(data=acled_dt,aes(x=longitude,y=latitude),color="indianred",alpha=.25)+
  theme_void()+
  theme(legend.position = c(.25,.25))

# aggregate geo-precision to 1 degree
acled_dt$lon1deg <- round(acled_dt$longitude)
acled_dt$lat1deg <- round(acled_dt$latitude)

acled_1deg_dt <- acled_dt[,.(Incidents=.N),by=.(lon1deg,lat1deg)]

# plot the incidents
gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled_1deg_dt,aes(x=lon1deg,y=lat1deg,size=Incidents),color="indianred",alpha=.75)+
  theme_void()+
  theme(legend.position = c(.25,.25))

ggsave("map_incidents.png",gg_map,width=6.5,height=6.5)

acled_1degyear_dt <- acled_dt[,.(Incidents=.N),by=.(lon1deg,lat1deg,year)]

gg_panel <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled_1degyear_dt,aes(x=lon1deg,y=lat1deg,size=Incidents),color="indianred",alpha=.75)+
  facet_wrap(~year,ncol=5)+
  theme_void()+
  theme(legend.position = c(.95,.05))

ggsave("panel_incidents.png",gg_panel,width=6.5,height=6.5)



ggplot(acled_1degyear_dt,aes(x=as.factor(year),y=Incidents))+
  geom_boxplot(outlier.shape = NA, color="indianred")+
  scale_y_continuous(limits = quantile(acled_1degyear_dt$Incidents,c(0,0.9)))+
  labs(x="Year")+
  theme_classic()

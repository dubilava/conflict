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
# devtools::install_github("ropensci/rnaturalearthhires")

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


## load the map of africa
africa <- ne_countries(scale="large",continent="africa",returnclass="sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## load the data
load("data_violence_acled.RData")


## number of incidents, grid-cells with incidents, and average population by country
xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),max_area=mean(max_area),tot_area=mean(tot_area),area_dum=mean(area_dum),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country,crop)]

xycomb_dt$crop <- factor(xycomb_dt$crop,levels=unique(xycomb_dt$crop)[c(4,3,5,2,1)])


# histogram of cropland fractions by grid-cells
gg_cropland_black <- ggplot(xycomb_dt[max_area>0],aes(x=max_area))+
  geom_bar(aes(fill=crop),position=position_dodge(),color="gray55",alpha=.9,size=.25) +
  scale_x_binned(n.breaks=10,limits=c(0,0.11),labels=c("0.00",seq(0.01,.09,.01),"0.10","0.10+"))+
  scale_fill_manual(name="Cereal",values=c("forestgreen","indianred","steelblue","goldenrod"))+
  labs(x="cropland area fraction",y="grid cells")+
  theme_classic()+
  theme_black()

gg_cropland_white <- ggplot(xycomb_dt[max_area>0],aes(x=max_area))+
  geom_bar(aes(fill=crop),position=position_dodge(),color="gray65",alpha=.9,size=.25) +
  scale_x_binned(n.breaks=10,limits=c(0,0.11),labels=c("0.00",seq(0.01,.09,.01),"0.10","0.10+"))+
  scale_fill_manual(name="Cereal",values=c("forestgreen","indianred","steelblue","goldenrod"))+
  labs(x="cropland area fraction",y="grid cells")+
  theme_classic()+
  theme_white()

ggsave("Figures/cropland_distribution.png",gg_cropland_white,width=6.5,height=3.5,dpi="retina")
ggsave("Presentation/cropland_distribution.png",gg_cropland_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/cropland_distribution.png",gg_cropland_black,width=6.5,height=3.5,dpi="retina")


# histogram of incidents by grid-cells
gg_incidents_black <- ggplot(xycomb_dt[incidents>0],aes(x=incidents))+
  geom_bar(fill="indianred",color="gray55",alpha=.9,size=.25) +
  scale_x_binned(n.breaks=20,limits=c(0,105),labels=c(seq(0,100,5),"100+"))+
  labs(x="number of incidents",y="grid cells")+
  theme_classic()+
  theme_black()

# histogram of incidents by grid-cells
gg_incidents_white <- ggplot(xycomb_dt[incidents>0],aes(x=incidents))+
  geom_bar(fill="indianred",color="gray65",alpha=.9,size=.25) +
  scale_x_binned(n.breaks=20,limits=c(0,105),labels=c(seq(0,100,5),"100+"))+
  labs(x="number of incidents",y="grid cells")+
  theme_classic()+
  theme_white()

ggsave("Figures/incident_distribution.png",gg_incidents_white,width=6.5,height=3.5,dpi="retina")
ggsave("Presentation/incident_distribution.png",gg_incidents_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/incident_distribution.png",gg_incidents_black,width=6.5,height=3.5,dpi="retina")


# prevalence_dt <- dataset_dt[actor!="other",.(incidents=sum(incidents)),by=.(actor,cropland=area_dum)]
# 
# prevalence_dt$cropland <- as.factor(prevalence_dt$cropland)
# prevalence_dt$actor <- factor(prevalence_dt$actor,labels=c("Identity Militia","Political Militia","Rebel Groups","State Forces"),levels=c("ident","polit","rebel","state"))
# 
# # bar-chart of incidents by actor and cropland
# gg_prop <- ggplot(prevalence_dt,aes(y=incidents,x=actor,fill=cropland:actor,label=incidents)) + 
#   geom_bar(position="fill",stat="identity",color="gray65",alpha=.9)+
#   geom_text(aes(color=cropland,label=format(incidents,big.mark=",",scientific=F)),size=5,position=position_fill(vjust=0.5))+
#   coord_flip()+
#   scale_fill_manual(values=c("cornsilk","cornsilk","cornsilk","cornsilk","forestgreen","steelblue","indianred","gray50"))+
#   scale_color_manual(values=c("black","white"))+
#   labs(x="",y="proportion of attacks")+
#   theme_classic()+
#   my_theme()+
#   theme(axis.line.x=element_blank(),axis.line.y=element_blank())
# 
# ggsave("Figures/incidents_by_actor_cropland.png",gg_prop,width=6.5,height=5.5,dpi="retina")
# ggsave("Presentation/incidents_by_actor_cropland.png",gg_prop,width=6.5,height=5.5,dpi="retina")


#---------------------#
#--  Conflict Maps  --#
#---------------------#

datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,actor)]
datasub_dt <- datasub_dt[incidents>=1]

datasub_dt$actor <- factor(datasub_dt$actor,levels=unique(datasub_dt$actor),labels=c("State forces","Rebel groups","Political militas","Identity militas","other"))

gg_conflictmaps_white <- ggplot(data = africa) +
  geom_sf(color="gray65",fill=NA,size=.25)+
  geom_point(data=datasub_dt[actor!="other"],aes(x=longitude,y=latitude,size=incidents,alpha=incidents,fill=actor),shape=21,color="gray35")+
  scale_fill_manual(values=c("gray50","indianred","steelblue","forestgreen"),guide="none")+
  scale_size(range=c(.2,2))+
  scale_alpha(range=c(.6,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  facet_wrap(~actor)+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.60),legend.text = element_text(hjust=1))

ggsave("Figures/maps_violence.png",gg_conflictmaps_white,width=6.5,height=6.5,dpi="retina")


gg_conflictmaps_top_black <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasub_dt[actor %in% unique(datasub_dt$actor)[1:2]],aes(x=longitude,y=latitude,size=incidents,alpha=incidents,fill=actor),shape=21,color="gray55")+
  scale_fill_manual(values=c("gray50","indianred"),guide="none")+
  scale_size(range=c(.2,2))+
  scale_alpha(range=c(.6,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  facet_wrap(~actor)+
  theme_void()+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.30),legend.text = element_text(hjust=1))

gg_conflictmaps_top_white <- ggplot(data = africa) +
  geom_sf(color="gray65",fill=NA,size=.25)+
  geom_point(data=datasub_dt[actor %in% unique(datasub_dt$actor)[1:2]],aes(x=longitude,y=latitude,size=incidents,alpha=incidents,fill=actor),shape=21,color="gray35")+
  scale_fill_manual(values=c("gray50","indianred"),guide="none")+
  scale_size(range=c(.2,2))+
  scale_alpha(range=c(.6,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  facet_wrap(~actor)+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.30),legend.text = element_text(hjust=1))

ggsave("Presentation/maps_violence_top.png",gg_conflictmaps_top_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/maps_violence_top.png",gg_conflictmaps_top_black,width=6.5,height=3.5,dpi="retina")


gg_conflictmaps_bottom_black <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasub_dt[actor %in% unique(datasub_dt$actor)[3:4]],aes(x=longitude,y=latitude,size=incidents,alpha=incidents,fill=actor),shape=21,color="gray55")+
  scale_fill_manual(values=c("steelblue","forestgreen"),guide="none")+
  scale_size(range=c(.2,2))+
  scale_alpha(range=c(.6,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  facet_wrap(~actor)+
  theme_void()+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.30),legend.text = element_text(hjust=1))

gg_conflictmaps_bottom_white <- ggplot(data = africa) +
  geom_sf(color="gray65",fill=NA,size=.25)+
  geom_point(data=datasub_dt[actor %in% unique(datasub_dt$actor)[3:4]],aes(x=longitude,y=latitude,size=incidents,alpha=incidents,fill=actor),shape=21,color="gray35")+
  scale_fill_manual(values=c("steelblue","forestgreen"),guide="none")+
  scale_size(range=c(.2,2))+
  scale_alpha(range=c(.6,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  facet_wrap(~actor)+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.30),legend.text = element_text(hjust=1))

ggsave("Presentation/maps_violence_bottom.png",gg_conflictmaps_bottom_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/maps_violence_bottom.png",gg_conflictmaps_bottom_black,width=6.5,height=3.5,dpi="retina")


#------------------------------#
#--  Harvest Map and Prices  --#
#------------------------------#

datasub_dt <- datacomb_dt[,.(crop,area=max_area),by=.(longitude,latitude)]
datasub_dt <- unique(datasub_dt)
datasub_dt <- datasub_dt[crop!="None" & area > 0]
crop_list <- c("Maize","Sorghum","Wheat","Rice")
datasub_dt$crop <- factor(datasub_dt$crop,levels=crop_list)

gg_cereals_black <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=area,alpha=area,fill=crop),shape=21,color="gray55")+
  scale_size(range=c(.2,2),guide="none")+
  scale_alpha(range=c(.6,.9),guide="none")+
  scale_fill_manual(name="Cereal",values=c("forestgreen","indianred","goldenrod","steelblue"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Geographic Distribution of Cereal Production")+
  theme_void()+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.30))

gg_cereals_white <- ggplot(data = africa) +
  geom_sf(color="gray65",fill=NA,size=.25)+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=area,alpha=area,fill=crop),shape=21,color="gray35")+
  scale_size(range=c(.2,2),guide="none")+
  scale_alpha(range=c(.6,.9),guide="none")+
  scale_fill_manual(name="Cereal",values=c("forestgreen","indianred","goldenrod","steelblue"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Geographic Distribution of Cereal Production")+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.30))


datasub_dt <- dataset_dt[(longitude==unique(longitude)[1] & latitude==unique(latitude)[1]),.(Maize=price_maize,Sorghum=price_sorghum,Wheat=price_wheat,Rice=price_rice),by=.(date)]
datasub_dt <- unique(datasub_dt)
prices_dt <- melt(datasub_dt,id.vars="date",variable.name="cereal",value.name="price")
prices_dt$cereal <- factor(prices_dt$cereal,levels=crop_list)

gg_prices_black <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.5,alpha=.9)+
  scale_color_manual(name="cereal",values=c("forestgreen","indianred","goldenrod","steelblue"))+
  labs(title="Temporal Variation of Cereal Prices",x="Year",y="Price")+
  theme_classic()+
  theme_black()+
  theme(legend.position=c(0.05,.95),legend.justification=c(0,1))

gg_prices_white <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.5,alpha=.9)+
  scale_color_manual(name="cereal",values=c("forestgreen","indianred","goldenrod","steelblue"))+
  labs(title="Temporal Variation of Cereal Prices",x="Year",y="Price")+
  theme_classic()+
  theme_white()+
  theme(legend.position=c(0.05,.95),legend.justification=c(0,1))


gg_cereals_prices_black <- plot_grid(gg_cereals_black,gg_prices_black,ncol=2,align="h",axis="b")
gg_cereals_prices_white <- plot_grid(gg_cereals_white,gg_prices_white,ncol=2,align="h",axis="b")

ggsave("Figures/cereals_prices.png",gg_cereals_prices_white,width=6.5,height=3.5,dpi="retina")
ggsave("Presentation/cereals_prices.png",gg_cereals_prices_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/cereals_prices.png",gg_cereals_prices_black,width=6.5,height=3.5,dpi="retina")


#-----------------------------------#
#--  Country Ranking of Incidents --#
#-----------------------------------#

## some descriptive statistics mentioned in the text
xyconflict_dt <- xycomb_dt[incidents>0]
xycropland_dt <- xycomb_dt[max_area>0]
xyagricult_dt <- xycomb_dt[area_dum==1]

cycomb_dt <- xyconflict_dt[,.(xy=.N,incidents_sum=sum(incidents),population_sum=sum(population_mln)),by=.(country)]

country_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),max_area=mean(max_area),tot_area=mean(tot_area),population=mean(population),population_mln=mean(population_mln)),by=.(country)]

country_dt <- country_dt[order(country)]

country_dt <- merge(country_dt,cycomb_dt,by="country")
country_dt$countrylab <- paste0(country_dt$country," (",country_dt$xy,")")

country_dt <- country_dt[order(-incidents_sum)]

country_dt$country <- factor(country_dt$country,levels=unique(country_dt$country))

gg_conflict_white <- ggplot(country_dt,aes(x=reorder(countrylab,incidents),y=incidents))+
  geom_bar(stat="identity",fill="indianred",color="gray65",alpha=.9,size=.25)+
  coord_flip(ylim=c(0,max(country_dt$incidents)*1.25))+
  labs(x="Countries/Territories",y="Incidents")+
  theme_classic()+
  theme_white()

gg_conflict_black <- ggplot(country_dt,aes(x=reorder(countrylab,incidents),y=incidents))+
  geom_bar(stat="identity",fill="indianred",color="gray55",alpha=.9,size=.25)+
  coord_flip(ylim=c(0,max(country_dt$incidents)*1.25))+
  labs(x="Countries/Territories",y="Incidents")+
  theme_classic()+
  theme_black()

ggsave("Figures/incidents_by_country.png",gg_conflict_white,width=6.5,height=7.5,dpi="retina")


datasum_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude)]
datasum_dt <- datasum_dt[incidents>=1]

gg_map_white <- ggplot(data = africa) +
  geom_sf(color="gray65",fill=NA,size=.25)+
  geom_point(data=datasum_dt,aes(x=longitude,y=latitude,size=incidents,alpha=incidents),shape=21,fill="indianred",color="gray35")+
  scale_alpha(range=c(.6,.9))+
  scale_size(range=c(.2,3))+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.30,.30),legend.text = element_text(hjust=1))

gg_map_black <- ggplot(data = africa) +
  geom_sf(color="gray55",fill=NA,size=.25)+
  geom_point(data=datasum_dt,aes(x=longitude,y=latitude,size=incidents,alpha=incidents),shape=21,fill="indianred",color="gray55")+
  scale_alpha(range=c(.6,.9))+
  scale_size(range=c(.2,3))+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.30,.30),legend.text = element_text(hjust=1))


aligned_plots_black <- align_plots(gg_conflict_black,gg_map_black,align="hv", axis="tblr")
gg_rankmap_black <- ggdraw(aligned_plots_black[[1]]) + draw_plot(aligned_plots_black[[2]],x=.03,y=-.03)

aligned_plots_white <- align_plots(gg_conflict_white,gg_map_white,align="hv", axis="tblr")
gg_rankmap_white <- ggdraw(aligned_plots_white[[1]]) + draw_plot(aligned_plots_white[[2]],x=.03,y=-.03)

ggsave("Figures/incidents_by_country_map.png",gg_rankmap_white,width=6.5,height=6.5,dpi="retina")
ggsave("Presentation/incidents_by_country_map.png",gg_rankmap_white,width=6.5,height=6.5,dpi="retina")
ggsave("Online/incidents_by_country_map.png",gg_rankmap_black,width=6.5,height=6.5,dpi="retina")


#------------------#
#--  Population  --#
#------------------#
datasub_dt <- datacomb_dt[year==2020,.(population=mean(population_mln)),by=.(longitude,latitude)]
datasub_dt <- datasub_dt[population>=.05]

gg_map_white <- ggplot(data = africa) +
  geom_sf(color="gray",fill=NA,size=.25)+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=population,alpha=population),color="steelblue")+
  geom_hline(yintercept = c(22,2,-12),color="gray35",linetype=2,size=.5)+
  annotate("text",x=-25,y=20, label="22°N",color="gray35",size=4)+
  annotate("text",x=-25,y=0, label="2°N",color="gray35",size=4)+
  annotate("text",x=-25,y=-14, label="12°S",color="gray35",size=4)+
  scale_size(range=c(.2,2),breaks = c(.1,.5,1,5,10))+
  scale_alpha(range=c(.3,.9),guide="none")+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.10,.30),legend.text = element_text(hjust=1))

ggsave("Figures/map_population.png",gg_map_white,width=6.5,height=6.5,dpi="retina")


#----------------------#
#--  Harvest Season  --#
#----------------------#

datasrt_dt <- datacomb_dt[year==2020]
datasrt_dt <- datasrt_dt[season_srt %in% 1]
datasrt_dt <- datasrt_dt[,.(xy,country,harvest_srt=month,crop,area=max_area,season_srt)]
datasrt_dt$season_srt <- NULL
datasrt_dt <- unique(datasrt_dt)

datamid_dt <- datacomb_dt[year==2020]
datamid_dt <- datamid_dt[season %in% 1]
datamid_dt <- datamid_dt[,.(xy,country,harvest_mid=month,crop,area=max_area,season)]
datamid_dt$season <- NULL
datamid_dt <- unique(datamid_dt)

dataend_dt <- datacomb_dt[year==2020]
dataend_dt <- dataend_dt[season_end %in% 1]
dataend_dt <- dataend_dt[,.(xy,country,harvest_end=month,crop,area=max_area,season_end)]
dataend_dt$season_end <- NULL
dataend_dt <- unique(dataend_dt)

datahs_dt <- merge(datasrt_dt,dataend_dt,by=c("xy","country","crop","area"))
datahs_dt <- merge(datamid_dt,datahs_dt,by=c("xy","country","crop","area"))

datahs_dt$harvest_srt <- factor(datahs_dt$harvest_srt,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
datahs_dt$harvest_mid <- factor(datahs_dt$harvest_mid,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
datahs_dt$harvest_end <- factor(datahs_dt$harvest_end,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

country_xy_dt <- datahs_dt[,.(xy,country,crop,area,harvest_srt,harvest_mid,harvest_end)]
country_xy_dt <- unique(country_xy_dt)

country_dt <- country_xy_dt[,.(gridcells=.N,area=mean(area)),by=.(country,crop,harvest_srt,harvest_mid,harvest_end)]

country <- as.character(country_dt$country)
month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

country_month <- CJ(country,month)
country_month_dt <- unique(country_month)

country_month_dt <- merge(country_dt,country_month_dt,by=c("country"),allow.cartesian=T)

country_month_dt <- country_month_dt[order(country,crop,-gridcells,month)]

country_month_dt$month <- factor(country_month_dt$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
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
  geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop,alpha=area),shape=21)+
  facet_wrap(~country,nrow=10)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme_white()

ggsave("Figures/harvestseasons.png",gg_hs_white,width=6.5,height=7.5,dpi="retina")

country_sub <- unique(country_month_dt$country)[seq(1,45,4)]

gg_hs_white <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
  geom_linerange(aes(xmin=harvest_srt,xmax=harvest_end,size=gridcells,color=crop,alpha=area))+
  geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop,alpha=area),shape=21)+
  facet_wrap(~country,ncol=4)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme_white()

gg_hs_black <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
  geom_linerange(aes(xmin=harvest_srt,xmax=harvest_end,size=gridcells,color=crop,alpha=area))+
  geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop,alpha=area),shape=21)+
  facet_wrap(~country,ncol=4)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme_black()

ggsave("Presentation/harvestseasons_sub.png",gg_hs_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/harvestseasons_sub.png",gg_hs_black,width=6.5,height=3.5,dpi="retina")

#----------------------#
#--  Growing Season  --#
#----------------------#

dataharv_dt <- datacomb_dt[year==2020]
dataharv_dt <- dataharv_dt[season %in% 1]
dataharv_dt <- dataharv_dt[,.(xy,country,harvest=month,crop,area=max_area,season)]
dataharv_dt$season <- NULL
dataharv_dt <- unique(dataharv_dt)

dataplant_dt <- datacomb_dt[year==2020]
dataplant_dt <- dataplant_dt[planted %in% 1]
dataplant_dt <- dataplant_dt[,.(xy,country,plant=month,crop,area=max_area,planted)]
dataplant_dt$planted <- NULL
dataplant_dt <- unique(dataplant_dt)

datags_dt <- merge(dataplant_dt,dataharv_dt,by=c("xy","country","crop","area"))

datags_dt$plant <- factor(datags_dt$plant,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
datags_dt$harvest <- factor(datags_dt$harvest,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

country_xy_dt <- datags_dt[,.(xy,country,crop,area,plant,harvest)]
country_xy_dt <- unique(country_xy_dt)

country_dt <- country_xy_dt[,.(gridcells=.N,area=mean(area)),by=.(country,crop,plant,harvest)]

country <- as.character(country_dt$country)
month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

country_month <- CJ(country,month)
country_month_dt <- unique(country_month)

country_month_dt <- merge(country_dt,country_month_dt,by=c("country"),allow.cartesian=T)

country_month_dt <- country_month_dt[order(country,crop,-gridcells,month)]

country_month_dt$month <- factor(country_month_dt$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
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
  theme_white()

ggsave("Figures/growingseasons.png",gg_gs_white,width=6.5,height=7.5,dpi="retina")


gg_gs_white <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
  geom_linerange(aes(xmin=plant,xmax=harvest,size=gridcells,color=crop,alpha=area))+
  facet_wrap(~ country,ncol=4)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme_white()

gg_gs_black <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
  geom_linerange(aes(xmin=plant,xmax=harvest,size=gridcells,color=crop,alpha=area))+
  facet_wrap(~ country,ncol=4)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme_black()

ggsave("Presentation/growingseasons_sub.png",gg_gs_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/growingseasons_sub.png",gg_gs_black,width=6.5,height=3.5,dpi="retina")


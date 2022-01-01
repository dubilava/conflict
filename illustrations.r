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

## load the map of africa
africa <- ne_countries(scale="large",continent="africa",returnclass="sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## load the data
load("data_violence_acled.RData")


## number of incidents, grid-cells with incidents, and average population by country
xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),max_area=mean(max_area),tot_area=mean(tot_area),area_dum=mean(area_dum),population=mean(population),population_mln=mean(population_mln)),by=.(xy,longitude,latitude,country)]


# histogram of cropland fractions by grid-cells
gg_cropland <- ggplot(xycomb_dt[max_area>0],aes(x=max_area))+
  geom_bar(fill="steelblue",alpha=.8) +
  scale_x_binned(n.breaks=20,limits=c(0,0.105),labels=c(seq(0,.1,.005),"0.100+"))+
  labs(x="Fraction of cropland",y="Number of grid cells")+
  theme_classic()+
  theme(legend.position="none",plot.caption=element_text(color="gray50",face="italic",size=9),plot.title=element_text(size=14),axis.title=element_text(size=10),axis.text = element_text(size=8))

ggsave("Figures/cropland_distribution.png",gg_cropland,width=6.5,height=3.5,dpi="retina")

# histogram of incidents by grid-cells
gg_incidents <- ggplot(xycomb_dt[incidents>0],aes(x=incidents))+
  geom_bar(fill="indianred",alpha=.8) +
  scale_x_binned(n.breaks=20,limits=c(0,105),labels=c(seq(0,100,5),"100+"))+
  labs(x="Number of incidents",y="Number of grid cells")+
  theme_classic()+
  theme(legend.position="none",plot.caption=element_text(color="gray50",face="italic",size=9),plot.title=element_text(size=14),axis.title=element_text(size=10),axis.text = element_text(size=8))

ggsave("Figures/incident_distribution.png",gg_incidents,width=6.5,height=3.5,dpi="retina")




prevalence_dt <- dataset_dt[actor!="other",.(incidents=sum(incidents)),by=.(actor,cropland=area_dum)]

prevalence_dt$cropland <- as.factor(prevalence_dt$cropland)
prevalence_dt$actor <- factor(prevalence_dt$actor,labels=c("Identity Militia","Political Militia","Rebel Groups","State Forces"),levels=c("ident","polit","rebel","state"))

# bar-chart of incidents by actor and cropland
gg_prop <- ggplot(prevalence_dt,aes(fill=cropland,y=incidents,x=actor,label=incidents)) + 
  geom_bar(position="fill",stat="identity")+
  geom_text(aes(color=cropland,label=format(incidents,big.mark=",",scientific=F)),size=5,position=position_fill(vjust=0.5))+
  coord_flip()+
  scale_fill_manual(values=c("cornsilk","forestgreen"))+
  scale_color_manual(values=c("black","white"))+
  labs(x="",y="Proportion of Attacks")+
  theme_classic()+
  theme(legend.position="none",plot.caption=element_text(color="gray50",face="italic",size=9),plot.title=element_text(size=16),axis.line.y=element_blank(),axis.ticks.y=element_blank(),axis.line.x=element_blank(),axis.title = element_text(size=14),axis.text = element_text(size=12),axis.text.y = element_text(hjust=0))

ggsave("Figures/incidents_by_actor_cropland.png",gg_prop,width=6.5,height=5.5,dpi="retina")



#---------------------#
#--  Conflict Maps  --#
#---------------------#

datasub_dt <- dataset_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude,actor)]
datasub_dt <- datasub_dt[incidents>=1]

gg_v1map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="state"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="darkgray")+
  scale_size(range=c(.1,1.75),guide="none")+
  scale_alpha(range=c(.25,.85),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="State Forces")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

gg_v2map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="rebel"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="indianred")+
  scale_size(range=c(.1,1.75),guide="none")+
  scale_alpha(range=c(.25,.85),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="Rebel Groups")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

gg_v3map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="polit"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="steelblue")+
  scale_size(range=c(.1,1.75),guide="none")+
  scale_alpha(range=c(.25,.85),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="Political Militia")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

gg_v4map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt[actor=="ident"],aes(x=longitude,y=latitude,size=incidents,color=incidents,alpha=incidents),color="forestgreen")+
  scale_size(range=c(.1,1.75),guide="none")+
  scale_alpha(range=c(.25,.85),guide="none")+
  coord_sf(ylim=c(-35,37))+
  labs(title="Identity Militia")+
  theme_void()+
  theme(legend.position=c(.15,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

pg <- plot_grid(gg_v1map,gg_v2map,NULL,NULL,gg_v3map,gg_v4map,ncol=2,rel_heights=c(1,.05,1))

# caption <- ggdraw() + 
#   draw_label("Data Source: Armed Conflict Location & Event Data\n (ACLED)  Project; available at https://acleddata.com",x=1,hjust=1,size=8,fontface="italic",colour="gray50")
# 
# gg_vmap <- plot_grid(pg,caption,ncol=1,rel_heights=c(1,.1))

ggsave("Figures/maps_violence.png",pg,width=6.5,height=6.5,dpi="retina")


#------------------------------#
#--  Harvest Map and Prices  --#
#------------------------------#

datasub_dt <- datacomb_dt[,.(crop,area=max_area),by=.(longitude,latitude)]
datasub_dt <- unique(datasub_dt)
datasub_dt <- datasub_dt[crop!="None" & area > 0]
crop_list <- c("Maize","Sorghum","Wheat","Rice")
datasub_dt$crop <- factor(datasub_dt$crop,levels=crop_list)

gg_cereals <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,color=crop,size=area,alpha=area))+
  scale_size(range=c(.1,1.75),guide="none")+
  scale_alpha(range=c(.25,.85),guide="none")+
  scale_color_manual(name="Cereal",values=c("forestgreen","indianred","goldenrod","steelblue"))+
  coord_sf(ylim=c(-35,37))+
  labs(title="Geographic Distribution of Cereal Production")+
  theme_void()+
  theme(legend.position=c(0.05,.45),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

datasub_dt <- dataset_dt[(longitude==unique(longitude)[1] & latitude==unique(latitude)[1]),.(Maize=price_maize,Sorghum=price_sorghum,Wheat=price_wheat,Rice=price_rice),by=.(date)]
datasub_dt <- unique(datasub_dt)
prices_dt <- melt(datasub_dt,id.vars="date",variable.name="cereal",value.name="price")
prices_dt$cereal <- factor(prices_dt$cereal,levels=crop_list)

gg_prices <- ggplot(prices_dt,aes(x=date,y=price,color=cereal,linetype=cereal,group=cereal))+
  geom_line(size=.6,alpha=.75)+
  scale_color_manual(name="cereal",values=c("forestgreen","indianred","goldenrod","steelblue"))+
  labs(title="Temporal Variation of Cereal Prices",x="Year",y="Price")+
  theme_classic()+
  theme(legend.position=c(0.05,.95),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))

pg <- plot_grid(gg_cereals,gg_prices,ncol=2,align="h",axis="b")

# caption <- ggdraw() + 
#   draw_label("Data Sources: Center for Sustainability and the Global Environment \u2013 Nelson Institute,\n available at https://nelson.wisc.edu/sage/data-and-models/crop-calendar-dataset/index.php; and\n IMF Primary Commodity Prices, available at https://www.imf.org/en/Research/commodity-prices",x=1,hjust=1,size=8,fontface="italic",colour="gray50")
# 
# gg_cereals_prices <- plot_grid(pg,caption,ncol=1,rel_heights=c(1,.1))

ggsave("Figures/cereals_prices.png",pg,width=6.5,height=3.5,dpi="retina")


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

gg_conflict <- ggplot(country_dt,aes(x=reorder(countrylab,incidents),y=incidents))+
  geom_bar(stat="identity",fill="indianred",alpha=.75)+
  coord_flip(ylim=c(0,max(country_dt$incidents)*1.25))+
  labs(x="Countries/Territories",y="Incidents")+
  theme_classic()+
  theme(legend.position=c(.65,.4),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=9),plot.title=element_text(size=16),axis.line.y=element_blank(),axis.ticks.y=element_blank())

ggsave("Figures/incidents_by_country.png",gg_conflict,width=6.5,height=7.5,dpi="retina")


datasum_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(longitude,latitude)]
datasum_dt <- datasum_dt[incidents>=1]

gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasum_dt,aes(x=longitude,y=latitude,size=incidents,alpha=incidents),color="indianred")+
  scale_alpha(range=c(.25,.75))+
  scale_size(range=c(.5,3.5))+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme(legend.position=c(.25,.3),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11),legend.key.size=unit(.75,'lines'))


aligned_plots <- align_plots(gg_conflict,gg_map,align="hv", axis="tblr")
gg_rankmap <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]],x=.03,y=-.03)

ggsave("Figures/incidents_by_country_map.png",gg_rankmap,width=6.5,height=6.5,dpi="retina")


#------------------#
#--  Population  --#
#------------------#
datasub_dt <- datacomb_dt[year==2020,.(population=mean(population_mln)),by=.(longitude,latitude)]
datasub_dt <- datasub_dt[population>=.05]

gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill="white",size=.25)+
  geom_point(data=datasub_dt,aes(x=longitude,y=latitude,size=population,alpha=population),color="steelblue")+
  geom_hline(yintercept = c(22,2,-12),color="gray50",linetype=2,size=.5)+
  annotate("text",x=-25,y=20, label="22°N",color="gray30",size=4)+
  annotate("text",x=-25,y=0, label="2°N",color="gray30",size=4)+
  annotate("text",x=-25,y=-14, label="12°S",color="gray30",size=4)+
  scale_size(range=c(.5,3.75),breaks = c(.1,.5,1,5,10))+
  scale_alpha(range=c(.5,.85),guide="none")+
  coord_sf(ylim=c(-35,37))+
  theme_void()+
  theme(legend.position=c(.25,.3),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=10,hjust=1),plot.caption=element_text(color="gray50",face="italic",size=8),plot.title=element_text(size=11))

ggsave("Figures/map_population.png",gg_map,width=6.5,height=6.5,dpi="retina")


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


gg_hs <- ggplot(country_month_dt,aes(x=month,y=crop))+
  geom_linerange(aes(xmin=harvest_srt,xmax=harvest_end,size=gridcells,color=crop,alpha=area))+
  geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop,alpha=area),shape=21)+
  facet_wrap(~country,nrow=10)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme(legend.position="none",plot.caption=element_text(color="gray50",face="italic",size=9),plot.title=element_text(size=16),axis.title=element_text(size=11),axis.text = element_text(size=8),axis.text.y=element_text(hjust=0),axis.text.x=element_text(size=7),strip.background = element_blank(),strip.text = element_text(size=8,face="bold"))

ggsave("Figures/harvestseasons.png",gg_hs,width=6.5,height=7.5,dpi="retina")


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

gg_gs <- ggplot(country_month_dt,aes(x=month,y=crop))+
  geom_linerange(aes(xmin=plant,xmax=harvest,size=gridcells,color=crop,alpha=area))+
  facet_wrap(~ country,nrow=10)+
  scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
  scale_alpha(range=c(.05,.35))+
  scale_size(range=c(.5,3.5))+
  scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
  labs(x="Months",y="Crops")+
  theme_classic()+
  theme(legend.position="none",plot.caption=element_text(color="gray50",face="italic",size=9),plot.title=element_text(size=16),axis.title=element_text(size=11),axis.text = element_text(size=8),axis.text.y=element_text(hjust=0),axis.text.x=element_text(size=7),strip.background = element_blank(),strip.text = element_text(size=8,face="bold"))

ggsave("Figures/growingseasons.png",gg_gs,width=6.5,height=7.5,dpi="retina")

library(data.table)
library(fixest)
library(backports)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the data
load("data_violence_acled.RData")

## aggregate by location
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

## aggregates by harvest season (for croplands defined in two different ways)
seasonal1_dt <- datacomb_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal1_dt <- seasonal1_dt[order(season)]

seasonal2_dt <- datacomb_dt[area_dum==1,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal2_dt <- seasonal2_dt[order(season)]


## number of grid cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

dataset_dt <- merge(dataset_dt,xysum_dt,by="country",all.x = T)
datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)


#################################
###  seasonality of conflict  ###
#################################

# datacomb_dt[,`:=`(price_lag=data.table::shift(price_ch,12),price_lead=data.table::shift(price_ch,12,type="lead")),by=.(xy)]
# 
# dataset_dt[,`:=`(price_lag=data.table::shift(price_ch,12),price_lead=data.table::shift(price_ch,12,type="lead")),by=.(xy,actor)]

###-- ALL ACTORS COMBINED --###
harv_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt,se="cluster")
summary(harv_fe,cluster=~xy+country^year)
summary(harv_fe,conley(500,distance="spherical")~longitude+latitude)
summary(harv_fe,cluster=~xy)
summary(harv_fe,cluster=~latitude)
summary(harv_fe,cluster=~country)

## percentage effect relative to the baseline conflict
100*sd(datacomb_dt[max_area>0]$price_ch)*seasonal1_dt$max_area*summary(harv_fe)$coefficients[-1]/seasonal1_dt$incidents_dum


###-- INDIVIDUAL ACTORS --###
harv1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="state"],se="cluster")
summary(harv1_fe,cluster=~xy+country^year)
summary(harv1_fe,conley(500,distance="spherical")~longitude+latitude)
summary(harv1_fe,cluster=~xy)
summary(harv1_fe,cluster=~latitude)
summary(harv1_fe,cluster=~country)

harv2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="rebel"],se="cluster")
summary(harv2_fe,cluster=~xy+country^year)
summary(harv2_fe,conley(500,distance="spherical")~longitude+latitude)
summary(harv2_fe,cluster=~xy)
summary(harv2_fe,cluster=~latitude)
summary(harv2_fe,cluster=~country)

harv3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="polit"],se="cluster")
summary(harv3_fe,cluster=~xy+country^year)
summary(harv3_fe,conley(500,distance="spherical")~longitude+latitude)
summary(harv3_fe,cluster=~xy)
summary(harv3_fe,cluster=~latitude)
summary(harv3_fe,cluster=~country)

harv4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, dataset_dt[actor=="ident"],se="cluster")
summary(harv4_fe,cluster=~xy+country^year)
summary(harv4_fe,conley(500,distance="spherical")~longitude+latitude)
summary(harv4_fe,cluster=~xy)
summary(harv4_fe,cluster=~latitude)
summary(harv4_fe,cluster=~country)

##---

## print to table
etable(harv_fe,harv1_fe,harv2_fe,harv3_fe,harv4_fe,cluster=~xy,tex=F,digits=3,digits.stats = 3,title="Main Results",headers=c("All","State","Rebel","Polit","Ident"))




coef_sum <- summary(harv_fe,cluster=~xy+country^year)
# coef_sum <- summary(harv_fe,conley(500,distance="spherical")~longitude+latitude)

coeftab_dt <- as.data.table(coef_sum$coeftable[-1,])
colnames(coeftab_dt) <- c("est","se","trat","pval")
coeftab_dt[,`:=`(season=as.factor(0:11))]

scale_coef <- 100*sd(datacomb_dt[max_area>0]$price_ch)*seasonal1_dt$max_area/seasonal1_dt$incidents_dum

coeftab_dt$est <- coeftab_dt$est*scale_coef
coeftab_dt$se <- coeftab_dt$se*scale_coef

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))

## circular plot for illustrating the seasonal effect
gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.6,color="steelblue")+
  geom_hline(yintercept = seq(-6,6,2),color="gray50",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray30",size=.4,linetype=2) +
  scale_x_continuous(breaks = 1:12,labels=c(0:11))+
  scale_y_continuous(breaks = seq(-2,4,2),labels=seq(-2,4,2))+
  coord_polar(start=-pi/6)+
  labs(x="Months after harvest",y="Percent change in violence")+
  theme(axis.text=element_text(size=10,margin=margin(t=1,r=1,b=1,l=1),color="black"),axis.title = element_text(size=10),plot.title = element_text(size=12),panel.grid=element_blank(),panel.background = element_blank(),axis.title.y = element_text(hjust=.81),legend.position = "none")

## saving the plot
ggsave("Figures/circular_acled.png",gg_coef,width=6.5,height=6.5,device="png",dpi="retina")

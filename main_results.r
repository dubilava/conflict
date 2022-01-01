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

## number of grid cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)
dataset_dt <- merge(dataset_dt,xysum_dt,by="country",all.x = T)

xy_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area))]
xy_dt[max_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area))]

## aggregate by location and actor
actor_xy_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(actor,country,xy)]

actor_xy_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area)),by=.(actor)]
actor_xy_dt[max_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(max_area)),by=.(actor)]

#################################
###  seasonality of conflict  ###
#################################

datacomb_dt[,`:=`(incidents_lag=data.table::shift(incidents_dum,1)),by=.(xy)]

dataset_dt[,`:=`(incidents_lag=data.table::shift(incidents_dum,1)),by=.(xy,actor)]

###-- ALL ACTORS COMBINED --###
comb_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt)

###-- BY INDIVIDUAL ACTORS --###
split_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt,split=~actor)

##---

## print to table
etable(comb_fe,split_fe,vcov=~xy+country^year,tex=T,digits=3,digits.stats = 3,title="Main Results",headers=c("All","State","Rebel","Polit","Ident","Other"))

## print to table
etable(comb_fe,split_fe,vcov=conley(500,distance="spherical")~longitude+latitude,tex=T,digits=3,digits.stats = 3,title="Main Results",headers=c("All","State","Rebel","Polit","Ident","Other"))




### state
seasonal1_dt <- dataset_dt[max_area>0 & actor=="state",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal1_dt <- seasonal1_dt[order(season)]

scale1_coef <- 100*sd(dataset_dt[max_area>0 & actor=="state"]$price_ch)*seasonal1_dt$max_area/seasonal1_dt$incidents_dum

coef1_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="state"])
coef1_sum <- summary(coef1_fe,vcov=~xy+country^year)

coeftab1_dt <- as.data.table(coef1_sum$coeftable[-1,])
colnames(coeftab1_dt) <- c("est","se","trat","pval")
coeftab1_dt[,`:=`(season=as.factor(0:11))]

coeftab1_dt$est <- coeftab1_dt$est*scale1_coef
coeftab1_dt$se <- coeftab1_dt$se*scale1_coef
coeftab1_dt$actor <- as.factor("State forces")

### rebel
seasonal2_dt <- dataset_dt[max_area>0 & actor=="rebel",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal2_dt <- seasonal2_dt[order(season)]

scale2_coef <- 100*sd(dataset_dt[max_area>0 & actor=="rebel"]$price_ch)*seasonal2_dt$max_area/seasonal2_dt$incidents_dum

coef2_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="rebel"])
coef2_sum <- summary(coef2_fe,vcov=~xy+country^year)

coeftab2_dt <- as.data.table(coef2_sum$coeftable[-1,])
colnames(coeftab2_dt) <- c("est","se","trat","pval")
coeftab2_dt[,`:=`(season=as.factor(0:11))]

coeftab2_dt$est <- coeftab2_dt$est*scale2_coef
coeftab2_dt$se <- coeftab2_dt$se*scale2_coef
coeftab2_dt$actor <- as.factor("Rebel groups")

### polit
seasonal3_dt <- dataset_dt[max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal3_dt <- seasonal3_dt[order(season)]

scale3_coef <- 100*sd(dataset_dt[max_area>0 & actor=="polit"]$price_ch)*seasonal3_dt$max_area/seasonal3_dt$incidents_dum

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="polit"])
coef3_sum <- summary(coef3_fe,vcov=~xy+country^year)

coeftab3_dt <- as.data.table(coef3_sum$coeftable[-1,])
colnames(coeftab3_dt) <- c("est","se","trat","pval")
coeftab3_dt[,`:=`(season=as.factor(0:11))]

coeftab3_dt$est <- coeftab3_dt$est*scale3_coef
coeftab3_dt$se <- coeftab3_dt$se*scale3_coef
coeftab3_dt$actor <- as.factor("Political militias")

### ident
seasonal4_dt <- dataset_dt[max_area>0 & actor=="ident",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal4_dt <- seasonal4_dt[order(season)]

scale4_coef <- 100*sd(dataset_dt[max_area>0 & actor=="ident"]$price_ch)*seasonal4_dt$max_area/seasonal4_dt$incidents_dum

coef4_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="ident"])
coef4_sum <- summary(coef4_fe,vcov=~xy+country^year)

coeftab4_dt <- as.data.table(coef4_sum$coeftable[-1,])
colnames(coeftab4_dt) <- c("est","se","trat","pval")
coeftab4_dt[,`:=`(season=as.factor(0:11))]

coeftab4_dt$est <- coeftab4_dt$est*scale4_coef
coeftab4_dt$se <- coeftab4_dt$se*scale4_coef
coeftab4_dt$actor <- as.factor("Identity Militias")


coeftab_dt <- rbind(coeftab1_dt,coeftab2_dt,coeftab3_dt,coeftab4_dt)

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(actor,season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))

## circular plot for illustrating the seasonal effect
gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=actor),alpha=.25)+
  geom_line(aes(color=actor),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray50",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray30",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c(0:11))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("darkgray","indianred","steelblue","forestgreen"))+
  scale_fill_manual(values=c("darkgray","indianred","steelblue","forestgreen"))+
  facet_wrap(~actor)+
  coord_polar(start=-pi*2)+
  labs(x="Months after harvest",y="Percent change in violence")+
  theme(axis.text=element_text(size=8,margin=margin(t=1,r=1,b=1,l=1),color="black"),axis.title = element_text(size=10),plot.title = element_text(size=12),panel.grid=element_blank(),panel.background = element_blank(),legend.position = "none",strip.background = element_blank())


ggsave("Figures/circular_violence.png",gg_coef,width=6.5,height=6.5,dpi="retina")

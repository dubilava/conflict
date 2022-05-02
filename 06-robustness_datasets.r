library(data.table)
library(ggplot2)
library(fixest)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

####################
###  ACLED DATA  ###
####################

## load the data
load("data_violence_acled.RData")

## aggregate by location
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

count_dt <- datacomb_dt[incidents>0,.(incidents=sum(incidents),incidents_dum=.N)]
baseline_dt <- datacomb_dt[max_area>0,.(incidents_frac=mean(incidents_dum))]

## number of grid cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)

# aggregates by harvest season
seasonal_acled_dt <- datacomb_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal_acled_dt <- seasonal_acled_dt[order(season)]

scale_acled <- 100*sd(datacomb_dt[max_area>0]$price_ch)*seasonal_acled_dt$max_area/seasonal_acled_dt$incidents_dum

# the regression
acled_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt)

acled_sum <- summary(acled_fe,cluster=~xy+country^year)

coef_acled_dt <- as.data.table(acled_sum$coeftable[-1,])
colnames(coef_acled_dt) <- c("est","se","trat","pval")
coef_acled_dt[,`:=`(season=as.factor(0:11))]

coef_acled_dt$est <- coef_acled_dt$est*scale_acled
coef_acled_dt$se <- coef_acled_dt$se*scale_acled
coef_acled_dt$source <- as.factor("ACLED")

rm(list=setdiff(ls(),"coef_acled_dt"))


###################
###  UCDP DATA  ###
###################

## load the data
load("data_violence_ucdp.RData")

## aggregate by location
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

count_dt <- datacomb_dt[incidents>0,.(incidents=sum(incidents),incidents_dum=.N)]
baseline_dt <- datacomb_dt[max_area>0,.(incidents_frac=mean(incidents_dum))]

## number of grid cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)

# aggregates by harvest season
seasonal_ucdp_dt <- datacomb_dt[max_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal_ucdp_dt <- seasonal_ucdp_dt[order(season)]

scale_ucdp <- 100*sd(datacomb_dt[max_area>0]$price_ch)*seasonal_ucdp_dt$max_area/seasonal_ucdp_dt$incidents_dum

# the regression
ucdp_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year, datacomb_dt)

ucdp_sum <- summary(ucdp_fe,cluster=~xy+country^year)

coef_ucdp_dt <- as.data.table(ucdp_sum$coeftable[-1,])
colnames(coef_ucdp_dt) <- c("est","se","trat","pval")
coef_ucdp_dt[,`:=`(season=as.factor(0:11))]

coef_ucdp_dt$est <- coef_ucdp_dt$est*scale_ucdp
coef_ucdp_dt$se <- coef_ucdp_dt$se*scale_ucdp
coef_ucdp_dt$source <- as.factor("UCDP")

rm(list=setdiff(ls(),c("coef_acled_dt","coef_ucdp_dt")))

coeftab_dt <- rbind(coef_acled_dt,coef_ucdp_dt)

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(source,season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))



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

## circular plot for illustrating the seasonal effect
gg_coef_white <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=source),alpha=.25)+
  geom_line(aes(color=source),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("indianred","darkgray"))+
  scale_fill_manual(values=c("indianred","darkgray"))+
  facet_wrap(~source)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_black <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=source),alpha=.25)+
  geom_line(aes(color=source),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray55",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("indianred","darkgray"))+
  scale_fill_manual(values=c("indianred","darkgray"))+
  facet_wrap(~source)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

## saving the plot
ggsave("Figures/circular_acled_ucdp.png",gg_coef_white,width=6.5,height=3.5,device="png",dpi=200)
ggsave("Presentation/circular_acled_ucdp.png",gg_coef_white,width=6.5,height=3.5,device="png",dpi="retina")
ggsave("Online/circular_acled_ucdp.png",gg_coef_black,width=6.5,height=3.5,device="png",dpi="retina")


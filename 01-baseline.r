library(data.table)
library(ggplot2)
library(fixest)
library(backports)

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

## print to table the main results with cell and country-year clustered s.e.
etable(comb_fe,split_fe,vcov=~xy+country^year,tex=T,digits=3,digits.stats = 3,title="Main Results",headers=c("All","State","Rebel","Polit","Ident","Other"))

## print to table the main results with Conley (500km) s.e.
etable(comb_fe,split_fe,vcov=conley(500,distance="spherical")~longitude+latitude,tex=T,digits=3,digits.stats = 3,title="Main Results",headers=c("All","State","Rebel","Polit","Ident","Other"))


###-- re-estimate the actor-specific parameters, obtain the impacts evaluated at
###-- the average cropland for a one-standard-deviation price shock relative to
###-- the unconditional (benchmark) conflict incidence

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

nseasonal3_dt <- dataset_dt[max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]

scale3_coef <- 100*sd(dataset_dt[max_area>0 & actor=="polit"]$price_ch)*seasonal3_dt$max_area/seasonal3_dt$incidents_dum

nscale3_coef <- 100*sd(dataset_dt[max_area>0 & actor=="polit"]$price_ch)*nseasonal3_dt$max_area/nseasonal3_dt$incidents_dum

coef3_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="polit"])
coef3_sum <- summary(coef3_fe,vcov=~xy+country^year)

coeftab3_dt <- as.data.table(coef3_sum$coeftable[-1,])
colnames(coeftab3_dt) <- c("est","se","trat","pval")
coeftab3_dt[,`:=`(season=as.factor(0:11))]

coeftab3_dt$est <- coeftab3_dt$est*scale3_coef
coeftab3_dt$se <- coeftab3_dt$se*scale3_coef
coeftab3_dt$actor <- as.factor("Political militias")

## for presentation, obtain parameters without seasonal effects
nons3_fe <- feols(incidents_dum~price_ch:max_area+log(population_mln)  | xy+country^year, dataset_dt[actor=="polit"])
nons3_sum <- summary(nons3_fe,vcov=~xy+country^year)

nonstab3_dt <- as.data.table(t(nons3_sum$coeftable[-1,]))
colnames(nonstab3_dt) <- c("est","se","trat","pval")

nonstab3_dt$est <- nonstab3_dt$est*mean(scale3_coef)
nonstab3_dt$se <- nonstab3_dt$se*mean(scale3_coef)

coefcomb_dt <- data.table(seasonal=coeftab3_dt$est,nonseasonal=nonstab3_dt$est)
coefcomb_dt[,`:=`(cums_s=cumsum(seasonal),cum_n=cumsum(nonseasonal))]
coefcomb_dt$season <- c(0:11)
coefcomb_dt$seasonal <- NULL
coefcomb_dt$nonseasonal <- NULL

coefcomb_lg <- melt(coefcomb_dt,id.vars = "season")
coefcomb_lg$variable <- factor(coefcomb_lg$variable,levels = unique(coefcomb_lg$variable),labels=c("seasonal model","nonseasonal model"))

## plot the the seasonal and non-seasonal cumulative impacts together 
gg_cum_white <- ggplot(coefcomb_lg,aes(x=season,y=value,color=variable,linetype=variable))+
  geom_line(size=.6)+
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(0,12,2))+
  scale_color_manual(values=c("steelblue","gray50"))+
  scale_linetype_manual(values=c(1,5))+
  labs(x="months after harvest (H)",y="cumulative % change in violence")+
  theme_classic()+
  theme_white()+
  theme(legend.position = c(.85,.15),legend.key.width = unit(1,"cm"))

gg_cum_black <- ggplot(coefcomb_lg,aes(x=season,y=value,color=variable,linetype=variable))+
  geom_line(size=.6)+
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(0,12,2))+
  scale_color_manual(values=c("steelblue","gray50"))+
  scale_linetype_manual(values=c(1,5))+
  labs(x="months after harvest (H)",y="cumulative % change in violence")+
  theme_classic()+
  theme_black()+
  theme(legend.position = c(.85,.15),legend.key.width = unit(1,"cm"))

ggsave("Figures/cumulative_violence.png",gg_cum_white,width=6.5,height=3.5,dpi=200,device="png")
ggsave("Presentation/cumulative_violence.png",gg_cum_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/cumulative_violence.png",gg_cum_black,width=6.5,height=3.5,dpi="retina")


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
gg_coef_white <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=actor),alpha=.25)+
  geom_line(aes(color=actor),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("darkgray","indianred","steelblue","forestgreen"))+
  scale_fill_manual(values=c("darkgray","indianred","steelblue","forestgreen"))+
  facet_wrap(~actor)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("Figures/circular_violence.png",gg_coef_white,width=6.5,height=6.5,dpi=200)


## the main plot (this goes to my website)
gg_main <- ggplot(coeftab_dt[actor=="Political militias"],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(aes(color=actor),color="steelblue",size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("../../dubilava.github.io/Papers/conflict.png",gg_main,width=3.5,height=3.5,dpi="retina")

## circular plot for illustrating the seasonal effect (top)
gg_coef_white <- ggplot(coeftab_dt[actor %in% c("State forces","Rebel groups")],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=actor),alpha=.25)+
  geom_line(aes(color=actor),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("darkgray","indianred"))+
  scale_fill_manual(values=c("darkgray","indianred"))+
  facet_wrap(~actor)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_black <- ggplot(coeftab_dt[actor %in% c("State forces","Rebel groups")],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=actor),alpha=.25)+
  geom_line(aes(color=actor),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray55",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("darkgray","indianred"))+
  scale_fill_manual(values=c("darkgray","indianred"))+
  facet_wrap(~actor)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("Presentation/circular_violence_top.png",gg_coef_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/circular_violence_top.png",gg_coef_black,width=6.5,height=3.5,dpi="retina")

## circular plot for illustrating the seasonal effect (bottom)
gg_coef_white <- ggplot(coeftab_dt[actor %in% c("Political militias","Identity Militias")],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=actor),alpha=.25)+
  geom_line(aes(color=actor),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("steelblue","forestgreen"))+
  scale_fill_manual(values=c("steelblue","forestgreen"))+
  facet_wrap(~actor)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_black <- ggplot(coeftab_dt[actor %in% c("Political militias","Identity Militias")],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se,fill=actor),alpha=.25)+
  geom_line(aes(color=actor),size=.6)+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray55",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  scale_color_manual(values=c("steelblue","forestgreen"))+
  scale_fill_manual(values=c("steelblue","forestgreen"))+
  facet_wrap(~actor)+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

ggsave("Presentation/circular_violence_bottom.png",gg_coef_white,width=6.5,height=3.5,dpi="retina")
ggsave("Online/circular_violence_bottom.png",gg_coef_black,width=6.5,height=3.5,dpi="retina")



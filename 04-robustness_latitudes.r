library(data.table)
library(fixest)
library(ggplot2)
library(cowplot)

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

y_vec <- unique(datacomb_dt$latitude)[order(-unique(datacomb_dt$latitude))]

trs <- c(37,22,2,-12,-35)

y_array <- array(dim=c(12,4,4))

for(i in 1:4){
  harv_fe <- feols(incidents_dum~price_ch:max_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year, dataset_dt[actor=="polit" & latitude %!in% y_vec[y_vec<trs[i] & y_vec>trs[i+1]]],se="cluster")
  y_array[,,i] <- as.matrix(summary(harv_fe,vcov=~xy+country^year)$coeftable)[-1,]
}

coef_y <- data.table(latitudes=c("37°N-22°N","22°N-2°N","2°N-12°S","12°S-35°S"),t(y_array[1:12,1,]))
colnames(coef_y) <- c("latitudes",0:11)
coef_y_lg <- melt(coef_y,id.vars="latitudes")

se_y <- data.table(latitudes=c("37°N-22°N","22°N-2°N","2°N-12°S","12°S-35°S"),t(y_array[1:12,2,]))
colnames(se_y) <- c("latitudes",0:11)
se_y_lg <- melt(se_y,id.vars="latitudes")

coeftab_dt <- merge(coef_y_lg,se_y_lg,by=c("latitudes","variable"),suffixes = c("_coef", "_se"))
coeftab_dt <- coeftab_dt[order(variable,latitudes)]

seasonal1_dt <- dataset_dt[latitude %!in% y_vec[y_vec<trs[1] & y_vec>trs[1+1]] & max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal1_dt <- seasonal1_dt[order(season)]

scale1_coef <- 100*sd(dataset_dt[latitude %!in% y_vec[y_vec<trs[1] & y_vec>trs[1+1]] & max_area>0 & actor=="polit"]$price_ch)*seasonal1_dt$max_area/seasonal1_dt$incidents_dum

seasonal2_dt <- dataset_dt[latitude %!in% y_vec[y_vec<trs[2] & y_vec>trs[2+1]] & max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal2_dt <- seasonal2_dt[order(season)]

scale2_coef <- 100*sd(dataset_dt[latitude %!in% y_vec[y_vec<trs[2] & y_vec>trs[2+1]] & max_area>0 & actor=="polit"]$price_ch)*seasonal2_dt$max_area/seasonal2_dt$incidents_dum

seasonal3_dt <- dataset_dt[latitude %!in% y_vec[y_vec<trs[3] & y_vec>trs[3+1]] & max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal3_dt <- seasonal3_dt[order(season)]

scale3_coef <- 100*sd(dataset_dt[latitude %!in% y_vec[y_vec<trs[3] & y_vec>trs[3+1]] & max_area>0 & actor=="polit"]$price_ch)*seasonal3_dt$max_area/seasonal3_dt$incidents_dum

seasonal4_dt <- dataset_dt[latitude %!in% y_vec[y_vec<trs[4] & y_vec>trs[4+1]] & max_area>0 & actor=="polit",.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),max_area=mean(max_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(season)]
seasonal4_dt <- seasonal4_dt[order(season)]

scale4_coef <- 100*sd(dataset_dt[latitude %!in% y_vec[y_vec<trs[4] & y_vec>trs[4+1]] & max_area>0 & actor=="polit"]$price_ch)*seasonal4_dt$max_area/seasonal4_dt$incidents_dum

coeftab_dt <- coeftab_dt[order(latitudes,variable)]
scale_vec <- c(scale1_coef,scale2_coef,scale3_coef,scale4_coef)

coeftab_dt <- coeftab_dt[,.(latitudes,season=variable,est=value_coef*scale_vec,se=value_se*scale_vec)]

## need to do this to join the end-points of the circular plot
extra_rows <- coeftab_dt[season==0]
extra_rows$season <- as.factor(12)

coeftab_dt <- rbind(coeftab_dt,extra_rows)
coeftab_dt <- coeftab_dt[order(season)]

coeftab_dt$season <- as.numeric(as.character(coeftab_dt$season))
coeftab_dt$latitudes <- factor(coeftab_dt$latitudes,levels=unique(coeftab_dt$latitudes)[c(4,2,3,1)])

## circular plots for illustrating the seasonal effect
gg_coef_white <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.4,color="steelblue")+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.2,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.3,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  coord_polar(start=-pi*2)+
  facet_wrap(~latitudes,ncol=2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

## saving the plot
ggsave("Figures/circular_polit_latitudes.png",gg_coef_white,width=6.5,height=6.5,device="png",dpi=200)


## circular plots for illustrating the seasonal effect
gg_coef_top_white <- ggplot(coeftab_dt[latitudes %in% unique(coeftab_dt$latitudes)[c(4,2)]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.4,color="steelblue")+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.2,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.3,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  coord_polar(start=-pi*2)+
  facet_wrap(~latitudes,ncol=2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_top_black <- ggplot(coeftab_dt[latitudes %in% unique(coeftab_dt$latitudes)[c(4,2)]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.4,color="steelblue")+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.2,linetype=3) +
  geom_hline(yintercept = 0,color="gray55",size=.3,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  coord_polar(start=-pi*2)+
  facet_wrap(~latitudes,ncol=2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

## saving the plot
ggsave("Presentation/circular_polit_latitudes_top.png",gg_coef_top_white,width=6.5,height=3.5,device="png",dpi="retina")
ggsave("Online/circular_polit_latitudes_top.png",gg_coef_top_black,width=6.5,height=3.5,device="png",dpi="retina")


## circular plots for illustrating the seasonal effect
gg_coef_bottom_white <- ggplot(coeftab_dt[latitudes %in% unique(coeftab_dt$latitudes)[c(3,1)]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.4,color="steelblue")+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.2,linetype=3) +
  geom_hline(yintercept = 0,color="gray35",size=.3,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  coord_polar(start=-pi*2)+
  facet_wrap(~latitudes,ncol=2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_white()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

gg_coef_bottom_black <- ggplot(coeftab_dt[latitudes %in% unique(coeftab_dt$latitudes)[c(3,1)]],aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="steelblue",alpha=.25)+
  geom_line(size=.4,color="steelblue")+
  geom_hline(yintercept = seq(-16,12,4),color="gray55",size=.2,linetype=3) +
  geom_hline(yintercept = 0,color="gray55",size=.3,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,8,4))+
  coord_polar(start=-pi*2)+
  facet_wrap(~latitudes,ncol=2)+
  labs(x="months after harvest (H)",y="% change in violence by political militias")+
  theme_black()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank())

## saving the plot
ggsave("Presentation/circular_polit_latitudes_bottom.png",gg_coef_bottom_white,width=6.5,height=3.5,device="png",dpi="retina")
ggsave("Online/circular_polit_latitudes_bottom.png",gg_coef_bottom_black,width=6.5,height=3.5,device="png",dpi="retina")


## some stats
dataset_dt[latitude %in% y_vec[y_vec<trs[2] & y_vec>trs[2+1]] & max_area>0 & actor=="polit",.(incidents=sum(incidents),cropland=mean(max_area))]

dataset_dt[max_area>0 & actor=="polit",.(incidents=sum(incidents),cropland=mean(max_area))]

dataset_dt[latitude %!in% y_vec[y_vec<trs[2] & y_vec>trs[2+1]] & max_area>0 & actor=="polit",.(incidents=sum(incidents),cropland=mean(max_area))]


datasub_dt <- dataset_dt[year==2020 & mo==12 & actor=="polit"]

datasub_dt[latitude %in% y_vec[y_vec<trs[2] & y_vec>trs[2+1]],.(cropland=sum(area_dum))]
datasub_dt[,.(cropland=sum(area_dum))]


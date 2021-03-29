library(data.table)
library(fixest)
library(ggplot2)
library(cowplot)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

## load the data
load("violence.RData")


#------------------------------#
#--  Figure F3: Drop a Year  --#
#------------------------------#

y_vec <- unique(datacomb_dt$year)[order(unique(datacomb_dt$year))]
y_array <- array(dim=c(12,4,length(y_vec)))

for(i in 1:length(y_vec)){
  harv_fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | xy+yearmo, datacomb_dt[year %!in% unique(datacomb_dt$year)[i]],se="cluster",weights=~population_mln)
  y_array[,,i] <- as.matrix(harv_fe$coeftable)
}

coef_y <- data.table(year=unique(datacomb_dt$year),t(y_array[1:12,1,]))
colnames(coef_y) <- c("year",paste0("h",c(0:11)))
coef_y_lg <- melt(coef_y,id.vars="year")

se_y <- data.table(year=unique(datacomb_dt$year),t(y_array[1:12,2,]))
colnames(se_y) <- c("year",paste0("h",c(0:11)))
se_y_lg <- melt(se_y,id.vars="year")

param_y_lg <- merge(coef_y_lg,se_y_lg,by=c("year","variable"),suffixes = c("_coef", "_se"))
param_y_lg <- param_y_lg[order(variable,year)]

param_y_lg$variable <- factor(param_y_lg$variable,levels=c(paste0("h",c(10:11)),paste0("h",c(0:9))))

levels(param_y_lg$variable) <- c(paste0("harvest[m+",10:11,"]"),paste0("harvest[m]"),paste0("harvest[m+",1:9,"]"))

gg_y <- ggplot(param_y_lg[variable %in% c(paste0("harvest[m+",10:11,"]"),paste0("harvest[m]"),paste0("harvest[m+",1:3,"]"))],aes(x=year)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.6,width=NA,alpha=.7,color="indianred") +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=1.2,color="indianred")+
  facet_wrap(~variable,ncol=6,labeller=label_parsed)+
  coord_flip()+
  labs(x="Omitted Year",y="Coefficients")+
  theme_classic()+
  theme(legend.position="top",legend.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),strip.background=element_blank())

ggsave("Paper/conflict_omit_year.png",gg_y,width=6.5,height=6.5,dpi="retina")


#----------------------------------#
#--  Figure F4: Drop a Latitude  --#
#----------------------------------#

l_vec <- unique(datacomb_dt$latitude)[order(unique(datacomb_dt$latitude))]
l_array <- array(dim=c(12,4,length(l_vec)))

for(i in 1:length(l_vec)){
  harv_fe <- feols(incidents_pop~price_ch:max_area:i(season,drop=0) | xy+yearmo, datacomb_dt[latitude %!in% l_vec[i]],se="cluster",weights=~population_mln)
  l_array[,,i] <- as.matrix(harv_fe$coeftable)
}

coef_l <- data.table(latitude=l_vec,t(l_array[1:12,1,]))
colnames(coef_l) <- c("latitude",paste0("h",c(0:11)))
coef_l_lg <- melt(coef_l,id.vars="latitude")

se_l <- data.table(latitude=l_vec,t(l_array[1:12,2,]))
colnames(se_l) <- c("latitude",paste0("h",c(0:11)))
se_l_lg <- melt(se_l,id.vars="latitude")

param_l_lg <- merge(coef_l_lg,se_l_lg,by=c("latitude","variable"),suffixes = c("_coef", "_se"))
param_l_lg <- param_l_lg[order(variable,latitude)]

param_l_lg$variable <- factor(param_l_lg$variable,levels=c(paste0("h",c(10:11)),paste0("h",c(0:9))))

levels(param_l_lg$variable) <- c(paste0("harvest[m+",10:11,"]"),paste0("harvest[m]"),paste0("harvest[m+",1:9,"]"))

gg_l <- ggplot(param_l_lg[variable %in% c(paste0("harvest[m+",10:11,"]"),paste0("harvest[m]"),paste0("harvest[m+",1:3,"]"))],aes(x=latitude)) + 
  geom_hline(yintercept=0,size=0.5,linetype=2,color="gray50",alpha=.5)+
  geom_errorbar(aes(ymin=value_coef-1.96*value_se,ymax=value_coef+1.96*value_se,group=variable),position=position_dodge(.5),size=0.6,width=NA,alpha=.7,color="indianred") +
  geom_point(aes(y=value_coef),position=position_dodge(.5),size=1.2,color="indianred")+
  scale_x_continuous(breaks=seq(-35,35,by=5))+
  facet_wrap(~variable,ncol=6,labeller=label_parsed)+
  coord_flip()+
  labs(x="Omitted Latitude",y="Coefficients")+
  theme_classic()+
  theme(legend.position="top",legend.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),strip.background=element_blank())

ggsave("Paper/conflict_omit_lat.png",gg_l,width=6.5,height=6.5,dpi="retina")

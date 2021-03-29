library(data.table)
library(ggplot2)
library(cowplot)
library(stringr)
library(fixest)
library(backports)
library(stargazer)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

# load the data
load("violence.RData")


#---------------------------------#
#--  Figure F5: Random Harvest  --#
#---------------------------------#

n_sim <- 100

coef_mat <- matrix(nrow=n_sim,ncol=12)
stde_mat <- matrix(nrow=n_sim,ncol=12)

for(i in 1:n_sim){
  
  if(!is.null(datacomb_dt$season_new)){
    datacomb_dt$season_new <- NULL
  }
  
  datacomb_sub <- datacomb_dt[year==2010 & season!=0,.(xy,mo,season)]
  datacomb_sub <- datacomb_sub[order(xy,mo)]
  
  set.seed(i)
  xy <- as.matrix(data.table(xy=sample(unique(datacomb_sub$xy),length(unique(datacomb_sub$xy)))))
  mo <- as.matrix(data.table(mo=unique(datacomb_sub$mo)))
  
  xy_mo <- CJ(xy,mo,sorted=F)
  
  datacomb_new <- cbind(xy_mo,datacomb_sub$season)
  colnames(datacomb_new) <- c("xy","mo","season_new")
  datacomb_new <- datacomb_new[order(xy,mo)]
  
  datacomb_dt <- merge(datacomb_dt,datacomb_new,by=c("xy","mo"),all.x=T)
  datacomb_dt[season_new %!in% paste0(1:12)]$season_new <- "0"
  datacomb_dt$season_new <- as.factor(datacomb_dt$season_new)
  datacomb_dt <- datacomb_dt[order(country,longitude,latitude,date,season)]

  harv_fe <- feols(incidents_pop~price_ch:area_dum:i(season_new,drop=c(0)) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)
  
  coef_mat[i,] <- harv_fe$coeftable[,"Estimate"]
  stde_mat[i,] <- harv_fe$coeftable[,"Std. Error"]
  
}

harv_fe <- feols(incidents_pop~price_ch:area_dum:i(season,drop=c(0)) | xy+yearmo, datacomb_dt,se="cluster",weights=~population_mln)

true_coef_dt <- data.table(id="0",variable=paste0("h+",0:11),value=harv_fe$coeftable[,"Estimate"])
true_stde_dt <- data.table(id="0",variable=paste0("h+",0:11),value=harv_fe$coeftable[,"Std. Error"])

colnames(true_coef_dt) <- c("id","season","point")
colnames(true_stde_dt) <- c("id","season","error")

true_coef_dt$season <- factor(true_coef_dt$season,levels=c(paste0("h+",10:11),paste0("h+",0:9)))
true_stde_dt$season <- factor(true_stde_dt$season,levels=c(paste0("h+",10:11),paste0("h+",0:9)))

true_dt <- merge(true_coef_dt,true_stde_dt,by=c("id","season"))

coef_dt <- data.table(coef_mat)
coef_dt <- coef_dt[complete.cases(coef_dt)]
colnames(coef_dt) <- paste0("h+",0:11)
coef_dt$id <- rownames(coef_dt)
coef_lg <- melt(coef_dt,id.vars = "id")
colnames(coef_lg) <- c("id","season","point")

stde_dt <- data.table(stde_mat)
stde_dt <- stde_dt[complete.cases(stde_dt)]
colnames(stde_dt) <- paste0("h+",0:11)
stde_dt$id <- rownames(stde_dt)
stde_lg <- melt(stde_dt,id.vars = "id")
colnames(stde_lg) <- c("id","season","error")

coef_lg <- coef_lg[order(season,point),.SD,by=season]
coef_lg$id_new <- rep(c(1:n_sim),12)

parameters_dt <- merge(coef_lg,stde_lg,by=c("id","season"))

parameters_dt <- parameters_dt[order(season,id_new)]

parameters_dt$season <- factor(parameters_dt$season,levels=c(paste0("h+",10:11),paste0("h+",0:9)))

levels(parameters_dt$season) <- c(paste0("harvest[m+",10:11,"]"),paste0("harvest[m]"),paste0("harvest[m+",1:9,"]"))

levels(true_dt$season) <- c(paste0("harvest[m+",10:11,"]"),paste0("harvest[m]"),paste0("harvest[m+",1:9,"]"))

gg_false <- ggplot(parameters_dt,aes(x=id_new))+
  geom_hline(yintercept=0,size=0.5,linetype=3,color="gray50")+
  geom_errorbar(aes(ymin=point-1.96*error,ymax=point+1.96*error,group=season),position=position_dodge(.5),size=0.4,width=NA,alpha=.7,color="indianred") +
  geom_point(aes(y=point),position=position_dodge(.5),size=0.8,color="indianred")+
  facet_wrap(~season,ncol=6,labeller=label_parsed)+
  geom_hline(data=true_dt,aes(yintercept=point),size=0.6,linetype=1,color="steelblue")+
  geom_hline(data=true_dt,aes(yintercept=c(point-1.96*error)),size=0.5,linetype=2,color="steelblue")+
  geom_hline(data=true_dt,aes(yintercept=c(point+1.96*error)),size=0.5,linetype=2,color="steelblue")+
  coord_flip()+
  labs(x="Simulations",y="Coefficients")+
  theme_classic()+
  theme(legend.position="top",legend.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),strip.background=element_blank())

ggsave("Paper/season_falsification.png",gg_false,width=6.5,height=6.5,dpi="retina")
  
  
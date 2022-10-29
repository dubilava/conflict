library(data.table)
library(fixest)
library(modelsummary)
library(splines)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
library(ggplot2)
library(ggstar)
library(haven)
library(cowplot)
library(fastDummies)
library(kableExtra)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

theme_white <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    panel.grid=element_blank(),
    plot.background=element_rect(fill="transparent",color=NA),
    legend.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="gray35"),
    axis.title=element_text(size=10,colour="gray35"),
    axis.text=element_text(size=8,colour="gray35",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line.x=element_line(colour="gray35"),
    axis.line.y=element_line(colour="gray35"),
    axis.ticks=element_line(colour="gray35"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="gray35"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="gray35",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}


africa <- ne_countries(continent = "africa",returnclass = "sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## load the data
load("data_violence_acled.RData")


dum_dt <- dummy_cols(datacomb_dt[,.(xy,date,season)],select_columns = "season")
dum_dt$season <- NULL
dum_dt$season_0 <- NULL

datacomb_dt <- merge(datacomb_dt,dum_dt,by=c("xy","date"))

datacomb_dt[,`:=`(season_2=season_2-season_1,season_3=season_3-season_1,season_4=season_4-season_12,season_5=season_5-season_12,season_6=season_6-season_12,season_7=season_7-season_12,season_8=season_8-season_12,season_9=season_9-season_12,season_10=season_10-season_12,season_11=season_11-season_12)]


dum_dt <- dummy_cols(dataset_dt[,.(actor,xy,date,season)],select_columns = "season")
dum_dt$season <- NULL
dum_dt$season_0 <- NULL

dataset_dt <- merge(dataset_dt,dum_dt,by=c("actor","xy","date"))

dataset_dt[,`:=`(season_2=season_2-season_1,season_3=season_3-season_1,season_4=season_4-season_12,season_5=season_5-season_12,season_6=season_6-season_12,season_7=season_7-season_12,season_8=season_8-season_12,season_9=season_9-season_12,season_10=season_10-season_12,season_11=season_11-season_12)]


pstars <- function(ps){
  p_stars <- ifelse(ps<.01,"***",ifelse(ps<.05,"**",ifelse(ps<.1,"*","")))
  return(p_stars)
}

impact <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}


# Appendix Tables 2-3: Fixed Effects ----

## country-trend ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^trend+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^trend+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^trend+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^trend+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^trend+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^trend+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust2.docx")


impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^trend+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}


c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## year-month ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+yearmo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+yearmo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+yearmo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+yearmo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+yearmo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+yearmo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust3.docx")




impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+yearmo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}


c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 4-7: Inference ----

## conley(200) ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(200,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(200,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(200,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(200,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(200,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(200,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust4.docx")


impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(200,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}


c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))




## conley(800) ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(800,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(800,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(800,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(800,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(800,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(800,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust5.docx")


impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(800,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}


c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))




## cell & country-year ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=~xy+country^year)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=~xy+country^year)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=~xy+country^year)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=~xy+country^year)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=~xy+country^year)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=~xy+country^year)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust6.docx")


impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=~xy+country^year)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))



## cell ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=~xy)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=~xy)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=~xy)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=~xy)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=~xy)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=~xy)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust7.docx")


impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=~xy)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 8-11: Years ----

## 1997-2008 ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[year %in% 1997:2008],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & year %in% 1997:2008],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & year %in% 1997:2008],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & year %in% 1997:2008],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & year %in% 1997:2008],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & year %in% 1997:2008],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust8.docx")


c_state <- impact(dataset_dt[actor=="state" & year %in% 1997:2008])
c_rebel <- impact(dataset_dt[actor=="rebel" & year %in% 1997:2008])
c_polit <- impact(dataset_dt[actor=="polit" & year %in% 1997:2008])
c_ident <- impact(dataset_dt[actor=="ident" & year %in% 1997:2008])
c_milit <- impact(dataset_dt[actor=="milit" & year %in% 1997:2008])
c_comb <- impact(datacomb_dt[year %in% 1997:2008])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## 2001-2012 ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[year %in% 2001:2012],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & year %in% 2001:2012],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & year %in% 2001:2012],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & year %in% 2001:2012],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & year %in% 2001:2012],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & year %in% 2001:2012],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust9.docx")


c_state <- impact(dataset_dt[actor=="state" & year %in% 2001:2012])
c_rebel <- impact(dataset_dt[actor=="rebel" & year %in% 2001:2012])
c_polit <- impact(dataset_dt[actor=="polit" & year %in% 2001:2012])
c_ident <- impact(dataset_dt[actor=="ident" & year %in% 2001:2012])
c_milit <- impact(dataset_dt[actor=="milit" & year %in% 2001:2012])
c_comb <- impact(datacomb_dt[year %in% 2001:2012])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## 2005-2016 ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[year %in% 2005:2016],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & year %in% 2005:2016],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & year %in% 2005:2016],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & year %in% 2005:2016],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & year %in% 2005:2016],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & year %in% 2005:2016],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust10.docx")


c_state <- impact(dataset_dt[actor=="state" & year %in% 2005:2016])
c_rebel <- impact(dataset_dt[actor=="rebel" & year %in% 2005:2016])
c_polit <- impact(dataset_dt[actor=="polit" & year %in% 2005:2016])
c_ident <- impact(dataset_dt[actor=="ident" & year %in% 2005:2016])
c_milit <- impact(dataset_dt[actor=="milit" & year %in% 2005:2016])
c_comb <- impact(datacomb_dt[year %in% 2005:2016])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## 2009-2020 ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[year %in% 2009:2020],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & year %in% 2009:2020],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & year %in% 2009:2020],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & year %in% 2009:2020],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & year %in% 2009:2020],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & year %in% 2009:2020],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust11.docx")


c_state <- impact(dataset_dt[actor=="state" & year %in% 2009:2020])
c_rebel <- impact(dataset_dt[actor=="rebel" & year %in% 2009:2020])
c_polit <- impact(dataset_dt[actor=="polit" & year %in% 2009:2020])
c_ident <- impact(dataset_dt[actor=="ident" & year %in% 2009:2020])
c_milit <- impact(dataset_dt[actor=="milit" & year %in% 2009:2020])
c_comb <- impact(datacomb_dt[year %in% 2009:2020])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 12-15: Latitudes ----

## omit North of Cancer ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[latitude <=23],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & latitude <=23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & latitude <=23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & latitude <=23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & latitude <=23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & latitude <=23],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust12.docx")


c_state <- impact(dataset_dt[actor=="state" & latitude <=23])
c_rebel <- impact(dataset_dt[actor=="rebel" & latitude <=23])
c_polit <- impact(dataset_dt[actor=="polit" & latitude <=23])
c_ident <- impact(dataset_dt[actor=="ident" & latitude <=23])
c_milit <- impact(dataset_dt[actor=="milit" & latitude <=23])
c_comb <- impact(datacomb_dt[latitude <=23])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# check_dt <- datacomb_dt[(latitude < 23 & latitude > 0),.(incidents=sum(incidents),crop_area=mean(crop_area)),by=.(xy)]
# 
# check_dt[crop_area>0]
# 
# 
# check_dt <- datacomb_dt[,.(incidents=sum(incidents),crop_area=mean(crop_area)),by=.(xy)]
# 
# check_dt[crop_area>0]

## omit Cancer to Equator ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[(latitude >23 | latitude <= 0)],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & (latitude >23 | latitude <= 0)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & (latitude >23 | latitude <= 0)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & (latitude >23 | latitude <= 0)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & (latitude >23 | latitude <= 0)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & (latitude >23 | latitude <= 0)],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust13.docx")



c_state <- impact(dataset_dt[actor=="state" & (latitude >23 | latitude <= 0)])
c_rebel <- impact(dataset_dt[actor=="rebel" & (latitude >23 | latitude <= 0)])
c_polit <- impact(dataset_dt[actor=="polit" & (latitude >23 | latitude <= 0)])
c_ident <- impact(dataset_dt[actor=="ident" & (latitude >23 | latitude <= 0)])
c_milit <- impact(dataset_dt[actor=="milit" & (latitude >23 | latitude <= 0)])
c_comb <- impact(datacomb_dt[(latitude >23 | latitude <= 0)])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))



## omit Equator to Capricorn ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[(latitude >0 | latitude <= -23)],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & (latitude >0 | latitude <= -23)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & (latitude >0 | latitude <= -23)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & (latitude >0 | latitude <= -23)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & (latitude >0 | latitude <= -23)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & (latitude >0 | latitude <= -23)],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust14.docx")


c_state <- impact(dataset_dt[actor=="state" & (latitude >0 | latitude <= -23)])
c_rebel <- impact(dataset_dt[actor=="rebel" & (latitude >0 | latitude <= -23)])
c_polit <- impact(dataset_dt[actor=="polit" & (latitude >0 | latitude <= -23)])
c_ident <- impact(dataset_dt[actor=="ident" & (latitude >0 | latitude <= -23)])
c_milit <- impact(dataset_dt[actor=="milit" & (latitude >0 | latitude <= -23)])
c_comb <- impact(datacomb_dt[(latitude >0 | latitude <= -23)])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))



## omit South of Capricorn ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[latitude > -23],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & latitude > -23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & latitude > -23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & latitude > -23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & latitude > -23],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & latitude > -23],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust15.docx")


c_state <- impact(dataset_dt[actor=="state" & latitude > -23])
c_rebel <- impact(dataset_dt[actor=="rebel" & latitude > -23])
c_polit <- impact(dataset_dt[actor=="polit" & latitude > -23])
c_ident <- impact(dataset_dt[actor=="ident" & latitude > -23])
c_milit <- impact(dataset_dt[actor=="milit" & latitude > -23])
c_comb <- impact(datacomb_dt[latitude > -23])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 16-22: Other Crops/Activities ----

## other crops and activities
load("auxiliary.RData")

auxiliary_dt[,`:=`(cash_area=apply(auxiliary_dt[,.(cocoa_area,coffee_area)],1,max))]
auxiliary_dt[,`:=`(Cocoa=ifelse(cocoa_area==cash_area & cocoa_area>0,cocoa_area,0),Coffee=ifelse(coffee_area==cash_area & coffee_area>0,coffee_area,0))]

datacomb_dt <- merge(datacomb_dt,auxiliary_dt,by=c("longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,auxiliary_dt,by=c("longitude","latitude"),all.x=T)

datacomb_dt[,`:=`(cassava_more=ifelse(cassava_area>crop_area,1,0))]
datacomb_dt[,`:=`(cash_more=ifelse(cash_area>crop_area,1,0))]
datacomb_dt[,`:=`(cocoa_more=ifelse(cocoa_area>crop_area,1,0))]
datacomb_dt[,`:=`(coffee_more=ifelse(coffee_area>crop_area,1,0))]

datacomb_dt[,`:=`(cassava_some=ifelse(cassava_area>0,1,0))]
datacomb_dt[,`:=`(cash_some=ifelse(cash_area>0,1,0))]
datacomb_dt[,`:=`(cocoa_some=ifelse(cocoa_area>0,1,0))]
datacomb_dt[,`:=`(coffee_some=ifelse(coffee_area>0,1,0))]

dataset_dt[,`:=`(cassava_more=ifelse(cassava_area>crop_area,1,0))]
dataset_dt[,`:=`(cash_more=ifelse(cash_area>crop_area,1,0))]
dataset_dt[,`:=`(cocoa_more=ifelse(cocoa_area>crop_area,1,0))]
dataset_dt[,`:=`(coffee_more=ifelse(coffee_area>crop_area,1,0))]

dataset_dt[,`:=`(cassava_some=ifelse(cassava_area>0,1,0))]
dataset_dt[,`:=`(cash_some=ifelse(cash_area>0,1,0))]
dataset_dt[,`:=`(cocoa_some=ifelse(cocoa_area>0,1,0))]
dataset_dt[,`:=`(coffee_some=ifelse(coffee_area>0,1,0))]

## second harvest season
load("second.RData")

colnames(second_dt) <- c("longitude","latitude","area_second","crop_second")

dataset_dt <- merge(dataset_dt,second_dt[,.(longitude,latitude,crop_second)],by=c("longitude","latitude"),all.x=T)
dataset_dt[is.na(crop_second)]$crop_second <- "None"

dataset_dt[,`:=`(major_crop=ifelse(tot_area>0 & crop_area/tot_area>.8,"Unique",ifelse(tot_area>0,"Mixed","None")))]


datacomb_dt <- merge(datacomb_dt,second_dt[,.(longitude,latitude,crop_second)],by=c("longitude","latitude"),all.x=T)
datacomb_dt[is.na(crop_second)]$crop_second <- "None"

datacomb_dt[,`:=`(major_crop=ifelse(tot_area>0 & crop_area/tot_area>.8,"Unique",ifelse(tot_area>0,"Mixed","None")))]


## unique crop ----
###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[major_crop!="Mixed"],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & major_crop!="Mixed"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & major_crop!="Mixed"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & major_crop!="Mixed"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & major_crop!="Mixed"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & major_crop!="Mixed"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust16.docx")


c_state <- impact(dataset_dt[actor=="state" & major_crop!="Mixed"])
c_rebel <- impact(dataset_dt[actor=="rebel" & major_crop!="Mixed"])
c_polit <- impact(dataset_dt[actor=="polit" & major_crop!="Mixed"])
c_ident <- impact(dataset_dt[actor=="ident" & major_crop!="Mixed"])
c_milit <- impact(dataset_dt[actor=="milit" & major_crop!="Mixed"])
c_comb <- impact(datacomb_dt[major_crop!="Mixed"])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## one harvest season ----
###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[crop_second=="None"],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & crop_second=="None"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & crop_second=="None"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & crop_second=="None"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & crop_second=="None"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & crop_second=="None"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust17.docx")


c_state <- impact(dataset_dt[actor=="state" & crop_second=="None"])
c_rebel <- impact(dataset_dt[actor=="rebel" & crop_second=="None"])
c_polit <- impact(dataset_dt[actor=="polit" & crop_second=="None"])
c_ident <- impact(dataset_dt[actor=="ident" & crop_second=="None"])
c_milit <- impact(dataset_dt[actor=="milit" & crop_second=="None"])
c_comb <- impact(datacomb_dt[crop_second=="None"])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))



## cash crops ----
###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[cash_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & cash_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & cash_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & cash_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & cash_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & cash_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust18.docx")


c_state <- impact(dataset_dt[actor=="state" & cash_more==0])
c_rebel <- impact(dataset_dt[actor=="rebel" & cash_more==0])
c_polit <- impact(dataset_dt[actor=="polit" & cash_more==0])
c_ident <- impact(dataset_dt[actor=="ident" & cash_more==0])
c_milit <- impact(dataset_dt[actor=="milit" & cash_more==0])
c_comb <- impact(datacomb_dt[cash_more==0])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## cassava ----
###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[cassava_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & cassava_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & cassava_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & cassava_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & cassava_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & cassava_more==0],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust19.docx")


c_state <- impact(dataset_dt[actor=="state" & cassava_more==0])
c_rebel <- impact(dataset_dt[actor=="rebel" & cassava_more==0])
c_polit <- impact(dataset_dt[actor=="polit" & cassava_more==0])
c_ident <- impact(dataset_dt[actor=="ident" & cassava_more==0])
c_milit <- impact(dataset_dt[actor=="milit" & cassava_more==0])
c_comb <- impact(datacomb_dt[cassava_more==0])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## pastoral ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[pastoral_area>=.3],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & pastoral_area>=.3],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & pastoral_area>=.3],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & pastoral_area>=.3],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & pastoral_area>=.3],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & pastoral_area>=.3],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust20.docx")


c_state <- impact(dataset_dt[actor=="state" & pastoral_area>=.3])
c_rebel <- impact(dataset_dt[actor=="rebel" & pastoral_area>=.3])
c_polit <- impact(dataset_dt[actor=="polit" & pastoral_area>=.3])
c_ident <- impact(dataset_dt[actor=="ident" & pastoral_area>=.3])
c_milit <- impact(dataset_dt[actor=="milit" & pastoral_area>=.3])
c_comb <- impact(datacomb_dt[pastoral_area>=.3])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## agro ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[(pastoral_area<.3)],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & (pastoral_area<.3)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & (pastoral_area<.3)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & (pastoral_area<.3)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & (pastoral_area<.3)],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & (pastoral_area<.3)],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust21.docx")


c_state <- impact(dataset_dt[actor=="state" & pastoral_area<.3])
c_rebel <- impact(dataset_dt[actor=="rebel" & pastoral_area<.3])
c_polit <- impact(dataset_dt[actor=="polit" & pastoral_area<.3])
c_ident <- impact(dataset_dt[actor=="ident" & pastoral_area<.3])
c_milit <- impact(dataset_dt[actor=="milit" & pastoral_area<.3])
c_comb <- impact(datacomb_dt[pastoral_area<.3])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))



## no mines ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[mines_dum==0],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & mines_dum==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & mines_dum==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & mines_dum==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & mines_dum==0],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & mines_dum==0],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust22.docx")


c_state <- impact(dataset_dt[actor=="state" & mines_dum==0])
c_rebel <- impact(dataset_dt[actor=="rebel" & mines_dum==0])
c_polit <- impact(dataset_dt[actor=="polit" & mines_dum==0])
c_ident <- impact(dataset_dt[actor=="ident" & mines_dum==0])
c_milit <- impact(dataset_dt[actor=="milit" & mines_dum==0])
c_comb <- impact(datacomb_dt[mines_dum==0])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 23-25: Hotspots ----

xycomb_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(xy,country)]
xycomb_dt <- xycomb_dt[order(-incidents,country)]

xycomb_dt[,`:=`(incidents_csum=cumsum(incidents))]
xycomb_dt[,`:=`(incidents_pcnt=incidents_csum/sum(incidents))]

xy_comb20incid <- xycomb_dt[1:9]$xy
xy_comb01cells <- xycomb_dt[1:25]$xy
xy_comb50incid <- xycomb_dt[1:65]$xy

countrycomb_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
countrycomb_dt <- countrycomb_dt[order(-incidents,country)]
country_comb06 <- countrycomb_dt[1:6]$country

xy_combcountry <- xycomb_dt[country%in%country_comb06]$xy

## top-20% incidents ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[xy %!in% xy_comb20incid],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & xy %!in% xy_comb20incid],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & xy %!in% xy_comb20incid],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & xy %!in% xy_comb20incid],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & xy %!in% xy_comb20incid],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & xy %!in% xy_comb20incid],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust23.docx")


c_state <- impact(dataset_dt[actor=="state" & xy %!in% xy_comb20incid])
c_rebel <- impact(dataset_dt[actor=="rebel" & xy %!in% xy_comb20incid])
c_polit <- impact(dataset_dt[actor=="polit" & xy %!in% xy_comb20incid])
c_ident <- impact(dataset_dt[actor=="ident" & xy %!in% xy_comb20incid])
c_milit <- impact(dataset_dt[actor=="milit" & xy %!in% xy_comb20incid])
c_comb <- impact(datacomb_dt[xy %!in% xy_comb20incid])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))




## top-1% cells ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[xy %!in% xy_comb01cells],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & xy %!in% xy_comb01cells],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & xy %!in% xy_comb01cells],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & xy %!in% xy_comb01cells],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & xy %!in% xy_comb01cells],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & xy %!in% xy_comb01cells],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust24.docx")



c_state <- impact(dataset_dt[actor=="state" & xy %!in% xy_comb01cells])
c_rebel <- impact(dataset_dt[actor=="rebel" & xy %!in% xy_comb01cells])
c_polit <- impact(dataset_dt[actor=="polit" & xy %!in% xy_comb01cells])
c_ident <- impact(dataset_dt[actor=="ident" & xy %!in% xy_comb01cells])
c_milit <- impact(dataset_dt[actor=="milit" & xy %!in% xy_comb01cells])
c_comb <- impact(datacomb_dt[xy %!in% xy_comb01cells])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))




## top-6 countries ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt[xy %!in% xy_combcountry],vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state" & xy %!in% xy_combcountry],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel" & xy %!in% xy_combcountry],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit" & xy %!in% xy_combcountry],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident" & xy %!in% xy_combcountry],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit" & xy %!in% xy_combcountry],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust25.docx")


c_state <- impact(dataset_dt[actor=="state" & xy %!in% xy_combcountry])
c_rebel <- impact(dataset_dt[actor=="rebel" & xy %!in% xy_combcountry])
c_polit <- impact(dataset_dt[actor=="polit" & xy %!in% xy_combcountry])
c_ident <- impact(dataset_dt[actor=="ident" & xy %!in% xy_combcountry])
c_milit <- impact(dataset_dt[actor=="milit" & xy %!in% xy_combcountry])
c_comb <- impact(datacomb_dt[xy %!in% xy_combcountry])

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 26-27: Lags & Leads ----

datacomb_dt[,`:=`(price_chl2=data.table::shift(price_ch,12,type="lag"),price_chl1=data.table::shift(price_ch,6,type="lag"),price_chf1=data.table::shift(price_ch,6,type="lead"),price_chf2=data.table::shift(price_ch,12,type="lead")),by=.(xy)]

dataset_dt[,`:=`(price_chl2=data.table::shift(price_ch,12,type="lag"),price_chl1=data.table::shift(price_ch,6,type="lag"),price_chf1=data.table::shift(price_ch,6,type="lead"),price_chf2=data.table::shift(price_ch,12,type="lead")),by=.(xy,actor)]


## 6-mo lag ----

datacomb_dt$shock <- datacomb_dt$price_chl1
dataset_dt$shock <- dataset_dt$price_chl1

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust26.docx")

impact1 <- function(x){
  r <- feols(incidents_dum~shock:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$shock,na.rm=T)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["shock:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["shock:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["shock:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["shock:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["shock:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["shock:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## 6-mo lead ----

datacomb_dt$shock <- datacomb_dt$price_chf1
dataset_dt$shock <- dataset_dt$price_chf1

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~shock:crop_area:(i(season,keep=1:12))+price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust27.docx")

impact1 <- function(x){
  r <- feols(incidents_dum~shock:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$shock,na.rm=T)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["shock:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["shock:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["shock:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["shock:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["shock:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["shock:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 28-29: Seasonality ----

load("precipitation.RData")

rain_dt <- rain_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,rain=as.numeric(rain))]

datacomb_dt <- merge(datacomb_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)

datacomb_dt[,`:=`(season1=as.factor(as.numeric(as.character(season))))]
dataset_dt[,`:=`(season1=as.factor(as.numeric(as.character(season))))]


## control rainfall ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+rain+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+rain+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+rain+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+rain+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+rain+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+rain+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust28.docx")

impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+rain+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch,na.rm=T)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))



## calendar year ----

datacomb_dt[,`:=`(season1=as.factor(as.numeric(as.character(mo))))]
dataset_dt[,`:=`(season1=as.factor(as.numeric(as.character(mo))))]

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents_dum~price_ch:crop_area:(i(season1,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust29.docx")


dum_dt <- dummy_cols(datacomb_dt[,.(xy,date,season1)],select_columns = "season1")
dum_dt$season1 <- NULL

datacomb_dt <- merge(datacomb_dt,dum_dt,by=c("xy","date"))

datacomb_dt[,`:=`(season1_2=season1_2-season1_1,season1_3=season1_3-season1_1,season1_4=season1_4-season1_12,season1_5=season1_5-season1_12,season1_6=season1_6-season1_12,season1_7=season1_7-season1_12,season1_8=season1_8-season1_12,season1_9=season1_9-season1_12,season1_10=season1_10-season1_12,season1_11=season1_11-season1_12)]


dum_dt <- dummy_cols(dataset_dt[,.(actor,xy,date,season1)],select_columns = "season1")
dum_dt$season1 <- NULL

dataset_dt <- merge(dataset_dt,dum_dt,by=c("actor","xy","date"))

dataset_dt[,`:=`(season1_2=season1_2-season1_1,season1_3=season1_3-season1_1,season1_4=season1_4-season1_12,season1_5=season1_5-season1_12,season1_6=season1_6-season1_12,season1_7=season1_7-season1_12,season1_8=season1_8-season1_12,season1_9=season1_9-season1_12,season1_10=season1_10-season1_12,season1_11=season1_11-season1_12)]

impact1 <- function(x){
  r <- feols(incidents_dum~price_ch:crop_area:(season1_1+season1_2+season1_3+season1_4+season1_5+season1_6+season1_7+season1_8+season1_9+season1_10+season1_11+season1_12)+rain+log(population_mln) | xy+country^year+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch,na.rm=T)*m$crop_area/m$incidents_dum
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season1_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season1_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season1_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season1_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season1_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season1_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


# Appendix Tables 30-33: Intensity ----

datacomb_dt[,`:=`(incidents1=ifelse(fatalities>=1,incidents,0),incidents10=ifelse(incidents>=10,10,incidents))]
datacomb_dt[,`:=`(incidents2=ifelse(incidents1>=10,10,incidents1))]

dataset_dt[,`:=`(incidents1=ifelse(fatalities>=1,incidents,0),incidents10=ifelse(incidents>=10,10,incidents))]
dataset_dt[,`:=`(incidents2=ifelse(incidents1>=10,10,incidents1))]


## incidents ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust30.docx")

impact1 <- function(x){
  r <- feols(incidents~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^trend+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## incidents capped at 10 ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents10~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents10~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents10~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents10~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents10~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents10~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust31.docx")

impact1 <- function(x){
  r <- feols(incidents10~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^trend+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents10),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## fatal incidents ----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents1~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents1~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents1~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents1~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents1~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents1~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust32.docx")

impact1 <- function(x){
  r <- feols(incidents1~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^trend+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents1),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))



## fatal incidents capped at 10----

###-- ALL ACTORS COMBINED --###
coef0_fe <- feols(incidents2~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
coef1_fe <- feols(incidents2~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef2_fe <- feols(incidents2~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="rebel"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef3_fe <- feols(incidents2~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="polit"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef4_fe <- feols(incidents2~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="ident"],vcov=conley(500,distance="spherical")~longitude+latitude)
coef5_fe <- feols(incidents2~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="milit"],vcov=conley(500,distance="spherical")~longitude+latitude)

## print out the table of results
modelsummary(list(coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#,output="Tables/robust33.docx")

impact1 <- function(x){
  r <- feols(incidents2~price_ch:crop_area:(season_1+season_2+season_3+season_4+season_5+season_6+season_7+season_8+season_9+season_10+season_11+season_12)+log(population_mln) | xy+country^trend+mo,data=x,vcov=conley(500,distance="spherical")~longitude+latitude)
  
  m <- x[crop_area>0,.(incidents=mean(incidents2),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln))]
  
  s <- 100*sd(x[crop_area>0]$price_ch)*m$crop_area/m$incidents
  
  h_coef <- round(r$coeftable["price_ch:crop_area:season_1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["price_ch:crop_area:season_1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["price_ch:crop_area:season_1","Pr(>|t|)"])
  
  p_coef <- round(r$coeftable["price_ch:crop_area:season_12","Estimate"]*s,1)
  
  p_se <- round(r$coeftable["price_ch:crop_area:season_12","Std. Error"]*s,1)
  
  p_stars <- pstars(r$coeftable["price_ch:crop_area:season_12","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(cropland=round(m$crop_area*100,1),incidence=round(m$incidents_dum*100,1)),effect=c(h_est,h_std,p_est,p_std)))
}

c_state <- impact1(dataset_dt[actor=="state"])
c_rebel <- impact1(dataset_dt[actor=="rebel"])
c_polit <- impact1(dataset_dt[actor=="polit"])
c_ident <- impact1(dataset_dt[actor=="ident"])
c_milit <- impact1(dataset_dt[actor=="milit"])
c_comb <- impact1(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,rebel=c_rebel$effect,polit=c_polit$effect,ident=c_ident$effect,milit=c_milit$effect,comb=c_comb$effect)))


## UCDP data ##
load("data_violence_ucdp.RData")

# Appendix Table B1: Descriptive Statistics ----

### combined data ----

## aggregate by cell
xy_dt <- datacomb_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(country,xy)]

## the number of the cells with crop production
xy_dt[crop_area>0] 

## the number of cells by country
xysum_dt <- xy_dt[,.(gridcells=.N),by=.(country)]

datacomb_dt <- merge(datacomb_dt,xysum_dt,by="country",all.x = T)
dataset_dt <- merge(dataset_dt,xysum_dt,by="country",all.x = T)

## number of incidents and incidence inc the combined data
incidents_comb <- datacomb_dt[,.(incidents=sum(incidents),incidence=sum(incidents_dum),incidents_dum=mean(incidents_dum),cropland=mean(crop_area))]

incidents_cond_comb <- datacomb_dt[crop_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(crop_area))]


### actor-specific data ----

## aggregate by cell-actor
xy_actor_dt <- dataset_dt[,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),incidents_pop=mean(incidents_pop),price=mean(price),price_ch=mean(price_ch),crop_area=mean(crop_area),tot_area=mean(tot_area),population_mln=mean(population_mln)),by=.(actor,country,xy)]

## number of incidents and incidence inc the actor-specific data
incidents_actor <- dataset_dt[,.(incidents=sum(incidents),incidence=sum(incidents_dum),incidents_dum=mean(incidents_dum),cropland=mean(crop_area)),by=.(actor)]

incidents_cond_actor <- dataset_dt[crop_area>0,.(incidents=sum(incidents),incidents_dum=mean(incidents_dum),cropland=mean(crop_area)),by=.(actor)]


uncond <- rbind(incidents_actor[,.(incidents,incidence,incidents_dum)],incidents_comb[,.(incidents,incidence,incidents_dum)])

cond <- rbind(incidents_cond_actor[,.(incidents,incidents_dum)],incidents_cond_comb[,.(incidents,incidents_dum)])

incidents <- uncond$incidents
incidents_prop <- round(incidents/incidents[length(incidents)]*100,1)
incidence <- uncond$incidence
incidence_uncond <- round(uncond$incidents_dum*100,1)
incidence_cond <- round(cond$incidents_dum*100,1)

tab1 <- Reduce(rbind,list(incidents,incidents_prop,incidence,incidence_uncond,incidence_cond))

colnames(tab1) <- c("state","rebel","comb")
rownames(tab1) <- c("incidents","proportion","incidence","unconditional","conditional")

kable_styling(kable(tab1))


# Appendix Table 34: Datasets ----

###-- ALL ACTORS COMBINED --###
ucdp0_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln) | xy+country^year+mo, datacomb_dt,vcov=conley(500,distance="spherical")~longitude+latitude)

###-- BY INDIVIDUAL ACTORS --###
ucdp1_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="state"],vcov=conley(500,distance="spherical")~longitude+latitude)
ucdp2_fe <- feols(incidents_dum~price_ch:crop_area:(i(season,keep=1:12))+log(population_mln)  | xy+country^year+mo, dataset_dt[actor=="nonstate"],vcov=conley(500,distance="spherical")~longitude+latitude)


## print out the table of results
modelsummary(list(ucdp1_fe,ucdp2_fe,ucdp0_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),coef_omit = "population_mln")#)#,output="Tables/robust34.docx")


## cumulative impact ----

dum_dt <- dummy_cols(datacomb_dt[,.(xy,date,season)],select_columns = "season")
dum_dt$season <- NULL
dum_dt$season_0 <- NULL

datacomb_dt <- merge(datacomb_dt,dum_dt,by=c("xy","date"))

datacomb_dt[,`:=`(season_2=season_2-season_1,season_3=season_3-season_1,season_4=season_4-season_12,season_5=season_5-season_12,season_6=season_6-season_12,season_7=season_7-season_12,season_8=season_8-season_12,season_9=season_9-season_12,season_10=season_10-season_12,season_11=season_11-season_12)]


dum_dt <- dummy_cols(dataset_dt[,.(actor,xy,date,season)],select_columns = "season")
dum_dt$season <- NULL
dum_dt$season_0 <- NULL

dataset_dt <- merge(dataset_dt,dum_dt,by=c("actor","xy","date"))

dataset_dt[,`:=`(season_2=season_2-season_1,season_3=season_3-season_1,season_4=season_4-season_12,season_5=season_5-season_12,season_6=season_6-season_12,season_7=season_7-season_12,season_8=season_8-season_12,season_9=season_9-season_12,season_10=season_10-season_12,season_11=season_11-season_12)]
c_state <- impact(dataset_dt[actor=="state"])
c_rebel <- impact(dataset_dt[actor=="nonstate"])

c_comb <- impact(datacomb_dt)

kable_styling(kable(data.table(state=c_state$effect,nonstate=c_rebel$effect,comb=c_comb$effect)))






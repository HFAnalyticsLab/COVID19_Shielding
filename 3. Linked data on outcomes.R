##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,Rmisc,DescTools,data.table,
               Hmisc,tibble,leaflet,rgeos,raster,plotly,
               pbapply,pbmcapply,here,rgdal,RColorBrewer,ggthemes,
               ggchicklet,tidyverse,showtext,zoo)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#############################################################################
################### NHS Digital: Linked data on outcomes ####################
#############################################################################

THAO_Covid <- fread(paste0(rawdatadir,"Tracking Healthcare Activity and Outcomes/Tracking Healthcare Activity and Outcomes for Shielded Patients, England, Open Data.csv"), header=TRUE, sep=",", check.names=T)

##################################################
################### Mortality ####################
##################################################

Mortality <- filter(THAO_Covid,Activity_type=="Mortality") %>%
  mutate(.,raw_Rate=100*as.numeric(Numerator)/as.numeric(Denominator))

##### Reproduce rates ####

Mortality$Rate_rep <- NA

Mortality$Rate_rep[which(Mortality$Numerator_type=="Daily")] <- rollapply(Mortality$raw_Rate, 7, mean,fill=NA)

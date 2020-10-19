##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,Rmisc,DescTools,data.table,
               Hmisc,tibble,leaflet,rgeos,raster,plotly,
               pbapply,pbmcapply,here,rgdal,RColorBrewer,ggthemes,
               ggchicklet,tidyverse,showtext,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

##############################################
################### GEO-DATA #################
##############################################

OA_to_higher_geo <- fread(paste0(rawdatadir,"Other data/Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv"), header=TRUE, sep=",", check.names=T)

LSOA_to_higher_geo <- OA_to_higher_geo[, list(LSOA11NM = first(LSOA11NM), MSOA11CD = first(MSOA11CD),
                                              LAD17CD = first(LAD17CD), LAD17NM=first(LAD17NM),
                                              RGN11CD = first(RGN11CD), RGN11NM=first(RGN11NM)), 
                                       by = list(LSOA11CD)]

LA_to_higher_geo <- fread(paste0(rawdatadir,"Other data/Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T)

rm(OA_to_higher_geo)

##############################################
################### POPULATION ###############
##############################################

#England and Wales

# pop_by_LSOA <- fread(paste0(rawdatadir,"Other data/Mid-year population estimates/LSOA/2011-to-2018-pop-post.csv"), header=TRUE, sep=",", check.names=T,
#                      select=c("lsoa11","pop18","children18","adults16plus18","over65_18","mean_age_18")) %>%
#   mutate(.,pct_over65_18=round(over65_18/pop18*100,1))

pop_by_LA <- read_excel(paste0(rawdatadir,"Other data/Mid-year population estimates/LA/ukmidyearestimates20192019ladcodes.xlsx"),
                        sheet = "MYE2 - Persons",skip=4) %>%
  select(.,Code,Name,`All ages`,age65plus) %>%
  rename(.,LAD19CD=Code,LAD19NM=Name,pop19=`All ages`) %>%
  filter(.,!is.na(LAD19NM)) %>%
  mutate(.,pct_over65_18=age65plus/pop19*100) %>%
  as.data.table()

#Join with geo-indicators

# pop_by_LSOA <- dplyr::left_join(pop_by_LSOA,LSOA_to_higher_geo,by=c("lsoa11"="LSOA11CD"))
# sum(is.na(pop_by_LSOA$RGN11NM)) #All matched

pop_by_LA <- dplyr::left_join(pop_by_LA,select(LA_to_higher_geo,"LAD19CD","RGN19CD","RGN19NM"),
                              by=c("LAD19CD"="LAD19CD"))

pop_by_LA <- mutate(pop_by_LA,
                    cat_pct_over65_18=cut(pct_over65_18, breaks=c(0,15,20,25,Inf),
                                          labels=c("15% or less","15-20%","20-25%","25% or more")))

##############################################
################### DEPRIVATION ##############
##############################################

#LA level

IMD2019_LA <- fread(paste0(rawdatadir,"Other data/IMD/Local authority/download1677836969253211150.csv"), header=TRUE, sep=",", check.names=T) %>%
  filter(.,DateCode==2019)

ranks <- select(IMD2019_LA,'Measurement','Value','Indices.of.Deprivation') %>%
  filter(.,Measurement=="Rank of average score"&Indices.of.Deprivation=="a. Index of Multiple Deprivation (IMD)") %>%
  select(.,'Value') %>% unique(.)

#LA level - rank of average score

IMD2019_LA_rank_IMD <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
  rename(.,rank_imd=Value) %>% select(.,FeatureCode,rank_imd) %>%
  mutate(.,decile_imd=cut(rank_imd, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10),
  quintile_imd=cut(rank_imd, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/5))[-1]), labels=1:5))

IMD2019_LA_rank_Health <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                   Indices.of.Deprivation=='e. Health Deprivation and Disability Domain') %>%
  rename(.,rank_health=Value) %>% select(.,FeatureCode,rank_health) %>%
  mutate(.,decile_health=cut(rank_health, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10),
         quintile_health=cut(rank_health, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/5))[-1]), labels=1:5))

IMD2019_LA_rank_Income <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                   Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  rename(.,rank_income=Value) %>% select(.,FeatureCode,rank_income) %>%
  mutate(.,decile_income=cut(rank_income, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10),
         quintile_income=cut(rank_income, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/5))[-1]), labels=1:5))

#LA level - average score

IMD2019_LA_avgscore_IMD <- filter(IMD2019_LA,Measurement=='Average Score'&
                                    Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
  rename(.,avgscore_imd=Value) %>% select(.,FeatureCode,avgscore_imd)

IMD2019_LA_avgscore_Health <- filter(IMD2019_LA,Measurement=='Average Score'&
                                       Indices.of.Deprivation=='e. Health Deprivation and Disability Domain') %>%
  rename(.,avgscore_health=Value) %>% select(.,FeatureCode,avgscore_health)

IMD2019_LA_avgscore_Income <- filter(IMD2019_LA,Measurement=='Average Score'&
                                       Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  rename(.,avgscore_income=Value) %>% select(.,FeatureCode,avgscore_income)

#LA level - % of LSOA among most deprived

IMD2019_LA_pctdep_IMD <- filter(IMD2019_LA,Measurement=='Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'&
                                  Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
  rename(.,pctdep_IMD=Value) %>% mutate(.,pctdep_IMD=100*pctdep_IMD) %>% select(.,FeatureCode,pctdep_IMD)

IMD2019_LA_pctdep_Health <- filter(IMD2019_LA,Measurement=='Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'&
                                     Indices.of.Deprivation=='e. Health Deprivation and Disability Domain') %>%
  rename(.,pctdep_health=Value) %>% mutate(.,pctdep_health=100*pctdep_health) %>% select(.,FeatureCode,pctdep_health)

IMD2019_LA_pctdep_Income <- filter(IMD2019_LA,Measurement=='Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'&
                                     Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  rename(.,pctdep_income=Value) %>% mutate(.,pctdep_income=100*pctdep_income) %>% select(.,FeatureCode,pctdep_income)

#Wide format

IMD2019_LA_wide <- left_join(IMD2019_LA_avgscore_IMD,IMD2019_LA_avgscore_Health,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_pctdep_IMD,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_pctdep_Health,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_avgscore_Income,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_pctdep_Income,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_rank_IMD,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_rank_Health,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_rank_Income,by="FeatureCode")

# 'Rank of average score'
# 'Average Score'
# 'Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'

########################################################################
################### NUMBER OF SHIELDERS BY LA: Wales ################### 
########################################################################

# SPL_by_LA_Wales_All <- read_ods(paste0(rawdatadir,"SPL/Wales/shielded-patient-list-in-wales-during-the-coronavirus-covid-19-pandemic-as-at-27-july-2020-182.ods"), skip=4,sheet="Table_1",col_names=TRUE)
# 
# SPL_by_LA_Wales_All <- dplyr::filter(SPL_by_LA_Wales_All,is.na(SPL_by_LA_Wales_All$'Local Authority Code')==FALSE) %>%
#   rename(.,LA.Code='Local Authority Code',Patient.Count='All Ages',LA.Name='Local Authority Name') %>%
#   select(.,LA.Code,Patient.Count,LA.Name)
# 
# ############ ALl shielders: Wales
# 
# SPL_by_LA_Wales_All$Patient.Count <- as.numeric(SPL_by_LA_Wales_All$Patient.Count)
# 
# SPL_by_LA_Wales_All$Breakdown.Field <- "ALL"

####################################################################################
################### NUMBER OF SHIELDERS BY LA: England and Wales ################### 
####################################################################################

SPL_by_LA <- fread(paste0(rawdatadir,"SPL/England/September/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2020-09-16.csv"), header=TRUE, sep=",", check.names=T)

#SPL_by_LA <- rbind.fill(SPL_by_LA,SPL_by_LA_Wales_All)

############ Merge in population numbers (you will lose some Local Authorities - the way the SPL is structured is non-standard)

#Fix code for England

SPL_by_LA$LA.Code[which(SPL_by_LA$LA.Code=="ENG")] <- "E92000001"

#Merge in population

nrow(SPL_by_LA)
SPL_by_LA <- left_join(SPL_by_LA,pop_by_LA,by=c("LA.Code"="LAD19CD"))
nrow(SPL_by_LA)

#Fill in Region for Wales

# SPL_by_LA$RGN19NM[which(SPL_by_LA$LA.Code %in% SPL_by_LA_Wales_All$LA.Code)] <- "Wales"
# SPL_by_LA$RGN19CD[which(SPL_by_LA$LA.Code %in% SPL_by_LA_Wales_All$LA.Code)] <- "W92000004"

#Manually add in population data for rows that add together 2 LAs

hackney_city_london <- filter(pop_by_LA,LAD19NM=="Hackney")$pop19+filter(pop_by_LA,LAD19NM=="City of London")$pop19
cornwall_scilly <- filter(pop_by_LA,LAD19NM=="Cornwall")$pop19+filter(pop_by_LA,LAD19NM=="Isles of Scilly")$pop19

SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$pop19 <- hackney_city_london
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$RGN19NM <- "London"

SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$pop19 <- cornwall_scilly
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$RGN19NM <- "South West"

############ Save

fwrite(SPL_by_LA, file = here::here("Clean data","SPL_by_LA.csv"), sep = ",")

############ ALl shielders

SPL_by_LA_All <- dplyr::filter(SPL_by_LA,Breakdown.Field=="ALL")

#You lose the following LAs: no exact match with latest population figures
# Bournemouth, Christchurch and Poole
# Dorset
# Buckinghamshire
# East Suffolk
# West Suffolk
# Somerset West and Taunton

SPL_by_LA_All$Patient.Count <- as.numeric(SPL_by_LA_All$Patient.Count)

SPL_by_LA_All <- dplyr::mutate(SPL_by_LA_All,Shielders_pct=Patient.Count/pop19*100) %>%
  arrange(.,desc(Shielders_pct)) %>% as.data.table()

############ Merge in deprivation

nrow(SPL_by_LA_All)
SPL_by_LA_All <- left_join(SPL_by_LA_All,IMD2019_LA_wide,by=c("LA.Code"="FeatureCode"))
nrow(SPL_by_LA_All)

############ Remove rows that don't have a region (e.g.ENGLAND)

SPL_by_LA_All_incl_ENG <- SPL_by_LA_All

SPL_by_LA_All <- dplyr::filter(SPL_by_LA_All, !is.na(RGN19NM))

############ Save

fwrite(SPL_by_LA_All, file = here::here("Clean data","SPL_by_LA_All.csv"), sep = ",")

############ Number of shielders by disease group

#Subset of data
SPL_by_LA_dgroup <- filter(SPL_by_LA,Breakdown.Field=="Disease Group") %>%
  rename(.,Cases.Count=Patient.Count)

#Clean up number variable
SPL_by_LA_dgroup$Cases.Count[which(SPL_by_LA_dgroup$Cases.Count=="*")] <- NA
SPL_by_LA_dgroup$Cases.Count <- as.numeric(SPL_by_LA_dgroup$Cases.Count)

#Simplify groups
SPL_by_LA_dgroup$group <- mapvalues(SPL_by_LA_dgroup$Breakdown.Value,
                                    from = c("Transplants (Solid)","Transplants (Haematological within 6 months)",
                                             "Transplants (Haematological with Immunosuppression)",
                                             "Cancer (Solid with Chemotherapy)","Cancer (Lung with Radical Radiotherapy)",
                                             "Cancer (Haematological within 24 months)","Respiratory (Severe Asthma)",
                                             "Respiratory (Severe COPD)","Respiratory (Severe Permanent)",
                                             "Corticosteroid Safety Card","Rare genetic metabolic and autoimmune diseases",
                                             "Immunosuppression Therapy","Pregnant with Congenital Heart Defect"),
                                    to = c("Transplants","Transplants","Transplants",
                                           "Cancer","Cancer","Cancer",
                                           "Respiratory","Respiratory","Respiratory",
                                           "Steroid","Rare genetic metabolic and autoimmune",
                                           "Immunosuppression Therapy",
                                           "Pregnant with Congenital Heart Defect"))

SPL_by_LA_dgroup <- as.data.table(SPL_by_LA_dgroup)

SPL_by_LA_dgroup <- SPL_by_LA_dgroup[, list(Cases.Count=sum(Cases.Count,na.rm=TRUE),
                                            LA.Name=first(LA.Name),
                                            pop19=first(pop19)),
                                     by = list(LA.Code,group)]

#Merge-in the Total counts
SPL_by_LA_dgroup <- left_join(SPL_by_LA_dgroup,select(SPL_by_LA_All_incl_ENG,LA.Code,Patient.Count),
                              by="LA.Code")

#Statistics to compare 'reasons' between LA and England
SPL_by_LA_dgroup <- within(SPL_by_LA_dgroup, {Cases.Total = ave(Cases.Count,LA.Code,FUN=sum)} )
SPL_by_LA_dgroup <- mutate(SPL_by_LA_dgroup,Cases.Pct=Cases.Count/Cases.Total*100)
SPL_by_LA_dgroup <- mutate(SPL_by_LA_dgroup,Cases.Prev.1000=Cases.Count/pop19*1000)
SPL_by_LA_dgroup <- mutate(SPL_by_LA_dgroup,Patient.Pct=Cases.Count/Patient.Count*100)

filter(SPL_by_LA_dgroup,LA.Name=="Camden")
filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")
filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND")

# #Merge-in the English averages
# 
# SPL_by_LA_dgroup <- left_join(SPL_by_LA_dgroup,England.rates,by="group") %>%
#   mutate(.,Cases.Pct.Differential=Cases.Pct-Cases.Pct.England)

#Merge-in the LA-level shielder rate

SPL_by_LA_dgroup <- left_join(SPL_by_LA_dgroup,dplyr::select(SPL_by_LA_All,LA.Code,RGN19CD,RGN19NM,Shielders_pct),
                              by="LA.Code")

#Flag high rates of certain conditions

# SPL_by_LA_dgroup$over50_pct <- ifelse(SPL_by_LA_dgroup$Cases.Pct>=50,1,0)
# SPL_by_LA_dgroup$over30_pct <- ifelse(SPL_by_LA_dgroup$Cases.Pct>=30,1,0)
# SPL_by_LA_dgroup$over20_pct <- ifelse(SPL_by_LA_dgroup$Cases.Pct>=20,1,0)
# SPL_by_LA_dgroup$levels_pct <- cut(SPL_by_LA_dgroup$Cases.Pct, breaks=c(-Inf,40,50,60,Inf), labels=1:4)
# 
# filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND")
# filter(SPL_by_LA_dgroup,LA.Name=="Camden")
# filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")

############ Save

fwrite(SPL_by_LA_dgroup, file = here::here("Clean data","SPL_by_LA_dgroup.csv"), sep = ",")

############ Number of shielders by disease group and deprivation

SPL_by_LA_dgroup_dep <- left_join(SPL_by_LA_dgroup,select(IMD2019_LA_wide,FeatureCode,decile_income,
                                                          quintile_income,decile_imd,quintile_imd),
                                  by=c("LA.Code"="FeatureCode")) %>%
  filter(.,!is.na(decile_income)) %>%
  select(.,LA.Code,group,Cases.Count,LA.Name,pop19,Cases.Total,Patient.Count,RGN19CD,RGN19NM,
         decile_income,quintile_income,decile_imd,quintile_imd)

fwrite(SPL_by_LA_dgroup_dep, file = here::here("Clean data","SPL_by_LA_dgroup_dep.csv"), sep = ",")

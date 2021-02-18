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
               Hmisc,tibble,pbapply,pbmcapply,here,
               tidyverse,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

##############################################
################### GEO-DATA #################
##############################################

#England

LA_to_higher_geo <- fread(paste0(rawdatadir,"Other data/Lookups/Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv"), header=TRUE, sep=",", check.names=T)

##############################################
################### POPULATION ###############
##############################################

#UK, Local Authority and above

pop_by_LA <- read_excel(paste0(rawdatadir,"Other data/Mid-year population estimates/LA/ukmidyearestimates20192019ladcodes.xlsx"),
                        sheet = "MYE2 - Persons",skip=4) %>%
  select(.,Code,Name,`All ages`,age65plus) %>%
  dplyr::rename(.,LAD19CD=Code,LAD19NM=Name,pop19=`All ages`) %>%
  filter(.,!is.na(LAD19NM)) %>%
  mutate(.,pct_over65_18=age65plus/pop19*100) %>%
  as.data.table()

#Join with geo-indicators to match English local authorities to regions

pop_by_LA <- dplyr::left_join(pop_by_LA,select(LA_to_higher_geo,"LAD19CD","RGN19CD","RGN19NM"),
                              by=c("LAD19CD"="LAD19CD"))

#Save

fwrite(pop_by_LA, file = here::here("Clean data","pop_by_LA.csv"), sep = ",")

##############################################
################### DEPRIVATION ##############
##############################################

#LA level

IMD2019_LA <- fread(paste0(rawdatadir,"Other data/IMD/Local authority/download1677836969253211150.csv"), header=TRUE, sep=",", check.names=T) %>%
  filter(.,DateCode==2019)

ranks <- select(IMD2019_LA,'Measurement','Value','Indices.of.Deprivation') %>%
  filter(.,Measurement=="Rank of average score"&Indices.of.Deprivation=="a. Index of Multiple Deprivation (IMD)") %>%
  select(.,'Value') %>% unique(.) %>% arrange(.,Value)

#LA level - rank of average score

IMD2019_LA_rank_IMD <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
  dplyr::rename(.,rank_imd=Value) %>% select(.,FeatureCode,rank_imd) %>%
  mutate(.,decile_imd=cut(rank_imd, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10),
         quintile_imd=cut(rank_imd, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/5))[-1]), labels=1:5))

IMD2019_LA_rank_Income <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                   Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  dplyr::rename(.,rank_income=Value) %>% select(.,FeatureCode,rank_income) %>%
  mutate(.,decile_income=cut(rank_income, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10),
         quintile_income=cut(rank_income, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/5))[-1]), labels=1:5))

#Wide format

IMD2019_LA_wide <- left_join(IMD2019_LA_rank_IMD,IMD2019_LA_rank_Income,by="FeatureCode")

########################################################################
################### NUMBER OF SHIELDERS BY LA: Wales ################### 
########################################################################

SPL_by_LA_Wales_All <- read_ods(paste0(rawdatadir,"SPL/Wales/shielded-patient-list-in-wales-during-the-coronavirus-covid-19-pandemic-as-at-27-july-2020-182.ods"), skip=4,sheet="Table_1",col_names=TRUE)

SPL_by_LA_Wales_All <- dplyr::filter(SPL_by_LA_Wales_All,is.na(SPL_by_LA_Wales_All$'Local Authority Code')==FALSE) %>%
  dplyr::rename(.,LA.Code='Local Authority Code',Patient.Count='All Ages',LA.Name='Local Authority Name') %>%
  select(.,LA.Code,Patient.Count,LA.Name)

############ All shielders: Wales

SPL_by_LA_Wales_All$Patient.Count <- as.numeric(SPL_by_LA_Wales_All$Patient.Count)

SPL_by_LA_Wales_All$Breakdown.Field <- "ALL"

############ Save

fwrite(SPL_by_LA_Wales_All, file = here::here("Clean data","SPL_by_LA_Wales_All.csv"), sep = ",")

##########################################################################
################### NUMBER OF SHIELDERS BY LA: England ################### 
##########################################################################

SPL_by_LA <- fread(paste0(rawdatadir,"SPL/England/October/Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2020-10-28.csv"), header=TRUE, sep=",", check.names=T)

############ Merge in population numbers (you will lose some Local Authorities - the way the SPL is structured is non-standard)

#Manually fix code for England

SPL_by_LA$LA.Code[which(SPL_by_LA$LA.Code=="ENG")] <- "E92000001"

#Merge in population numbers

nrow(SPL_by_LA)
SPL_by_LA <- left_join(SPL_by_LA,pop_by_LA,by=c("LA.Code"="LAD19CD"))
nrow(SPL_by_LA)

#Manually add in population data for rows that add together 2 local authorities
#These are 'Hackney and City of London' and 'Cornwall and Isles of Scilly'

hackney_city_london <- filter(pop_by_LA,LAD19NM=="Hackney")$pop19+filter(pop_by_LA,LAD19NM=="City of London")$pop19
cornwall_scilly <- filter(pop_by_LA,LAD19NM=="Cornwall")$pop19+filter(pop_by_LA,LAD19NM=="Isles of Scilly")$pop19

SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$pop19 <- hackney_city_london
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$RGN19NM <- "London"

SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$pop19 <- cornwall_scilly
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$RGN19NM <- "South West"

############ Save

fwrite(SPL_by_LA, file = here::here("Clean data","SPL_by_LA.csv"), sep = ",")

############ Reduced dataset for all shielders

SPL_by_LA_All <- dplyr::filter(SPL_by_LA,Breakdown.Field=="ALL")

#You lose the following LAs: no exact match with latest population figures
# Buckinghamshire
#NHS England does not report numbers of shielding patients for ALL local authorities

SPL_by_LA_All$Patient.Count <- as.numeric(SPL_by_LA_All$Patient.Count)

SPL_by_LA_All <- dplyr::mutate(SPL_by_LA_All,Shielders_pct=Patient.Count/pop19*100) %>%
  arrange(.,desc(Shielders_pct)) %>% as.data.table()

############ Merge in deprivation

nrow(SPL_by_LA_All)
SPL_by_LA_All <- left_join(SPL_by_LA_All,IMD2019_LA_wide,by=c("LA.Code"="FeatureCode"))
nrow(SPL_by_LA_All)

############ Remove rows that don't have a region or population (ENGLAND and Buckinghamshire)

SPL_by_LA_All_incl_ENG <- SPL_by_LA_All

SPL_by_LA_All <- dplyr::filter(SPL_by_LA_All, !is.na(RGN19NM))

############ Save

fwrite(SPL_by_LA_All_incl_ENG, file = here::here("Clean data","SPL_by_LA_All_incl_ENG.csv"), sep = ",")
fwrite(SPL_by_LA_All, file = here::here("Clean data","SPL_by_LA_All.csv"), sep = ",")

############ Number of shielders by sex

#Subset of data
SPL_by_LA_gender <- filter(SPL_by_LA,Breakdown.Field=="Gender") %>%
  dplyr::rename(.,Patient.Count.Sex=Patient.Count)

#Clean up number variable
SPL_by_LA_gender$Patient.Count.Sex[which(SPL_by_LA_gender$Patient.Count.Sex=="*")] <- NA
SPL_by_LA_gender$Patient.Count.Sex <- as.numeric(SPL_by_LA_gender$Patient.Count.Sex)

#Merge-in the Total counts
SPL_by_LA_gender <- left_join(SPL_by_LA_gender,select(SPL_by_LA_All_incl_ENG,LA.Code,Patient.Count),
                                by="LA.Code")
#Save
fwrite(SPL_by_LA_gender, file = here::here("Clean data","SPL_by_LA_gender.csv"), sep = ",")

############ Number of shielders by age group

#Subset of data
SPL_by_LA_agegroup <- filter(SPL_by_LA,Breakdown.Field=="Age") %>%
  dplyr::rename(.,Patient.Count.Age=Patient.Count)

#Clean up number variable
SPL_by_LA_agegroup$Patient.Count.Age[which(SPL_by_LA_agegroup$Patient.Count.Age=="*")] <- NA
SPL_by_LA_agegroup$Patient.Count.Age <- as.numeric(SPL_by_LA_agegroup$Patient.Count.Age)

#Merge-in the Total counts
SPL_by_LA_agegroup <- left_join(SPL_by_LA_agegroup,select(SPL_by_LA_All_incl_ENG,LA.Code,Patient.Count),
                              by="LA.Code")

#Percentage of shielded patients shielding for a given reason
SPL_by_LA_agegroup <- mutate(SPL_by_LA_agegroup,Patient.Pct=Patient.Count.Age/Patient.Count*100)

#Save
fwrite(SPL_by_LA_agegroup, file = here::here("Clean data","SPL_by_LA_agegroup.csv"), sep = ",")

############ Number of shielders by disease group

#Subset of data
SPL_by_LA_dgroup <- filter(SPL_by_LA,Breakdown.Field=="Disease Group") %>%
  dplyr::rename(.,Cases.Count=Patient.Count)

#Clean up number variable
SPL_by_LA_dgroup$Cases.Count[which(SPL_by_LA_dgroup$Cases.Count=="*")] <- NA
SPL_by_LA_dgroup$Cases.Count <- as.numeric(SPL_by_LA_dgroup$Cases.Count)

#Simplify groups into 7 broad groups

unique(SPL_by_LA_dgroup$Breakdown.Value)

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

#Percentage of shielded patients shielding for a given reason
SPL_by_LA_dgroup <- mutate(SPL_by_LA_dgroup,Patient.Pct=Cases.Count/Patient.Count*100)

#Save

fwrite(SPL_by_LA_dgroup, file = here::here("Clean data","SPL_by_LA_dgroup.csv"), sep = ",")

############ Number of shielders by disease group and deprivation

SPL_by_LA_dgroup_dep <- left_join(SPL_by_LA_dgroup,select(IMD2019_LA_wide,FeatureCode,decile_income,
                                                          quintile_income,decile_imd,quintile_imd),
                                  by=c("LA.Code"="FeatureCode")) %>%
  filter(.,!is.na(decile_income)) %>%
  select(.,LA.Code,group,Cases.Count,LA.Name,pop19,Patient.Count,
         decile_income,quintile_income,decile_imd,quintile_imd)

fwrite(SPL_by_LA_dgroup_dep, file = here::here("Clean data","SPL_by_LA_dgroup_dep.csv"), sep = ",")

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
               ggchicklet,tidyverse,showtext,ggchicklet)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#One Drive directory for graphs
onedrivegraphs <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/"

##################################################
################### Load clean data ##############
##################################################

SPL_by_LA_dgroup_dep <- fread(here::here("Clean data","SPL_by_LA_dgroup_dep.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA_dgroup <- fread(here::here("Clean data","SPL_by_LA_dgroup.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA <- fread(here::here("Clean data","SPL_by_LA.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA_All <- fread(here::here("Clean data","SPL_by_LA_All.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA_Wales_All <- fread(here::here("Clean data","SPL_by_LA_Wales_All.csv"), header=TRUE, sep=",", check.names=T)

pop_by_LA <- fread(here::here("Clean data","pop_by_LA.csv"), header=TRUE, sep=",", check.names=T)

###########################################################
############## Reasons for shielding dot plot #############
###########################################################

dotplot_condition <- ggplot(filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND"),
                            aes(Patient.Pct, reorder(group, Patient.Pct))) +
  geom_point(size=4,col="springgreen4") +
  geom_text(aes(x=Patient.Pct+2,label=paste0(round(Patient.Pct,1),"%")),
            size=3) +
  theme(panel.background = element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.y=element_blank(),
        text = element_text(size = 10)) +
  labs(title="Conditions",y = " ",x=" ")

windows()
dotplot_condition

#Save chart

ggsave(paste0(gitdir,"/Charts/","dotplot_condition.png"), dotplot_condition, device="png",width = 10, height = 5,dpi=500)

#############################################################################
################### Number of shielders vs. deprivation decile ##############
#############################################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_income_dep <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop19)), 
  by = list(decile_income)] %>%
  filter(.,!is.na(decile_income))

cols <- colorRampPalette(brewer.pal(n = 9, name = "RdYlGn"))(10)
deplabs <- c("1 (most deprived)",2:9,"10 (least deprived)")

by_deprivation <- ggplot(pct_shielding_by_income_dep) +
  geom_chicklet(aes(x=factor(decile_income), y = Shielders_pct,fill=factor(decile_income)),
                radius = grid::unit(5, 'mm'),width = 0.75) +
  geom_text(aes(x = decile_income, y = Shielders_pct + 0.2,
                label = paste0(round(Shielders_pct, 1),"%")),
            size=5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  scale_fill_manual(values=cols,guide=FALSE) +
  scale_x_discrete(labels = deplabs) +
  labs(title="Average % residents shielding",y = " ",x="Income deprivation decile")

windows()
by_deprivation

#Save chart

ggsave(paste0(gitdir,"/Charts/","by_deprivation_dec.png"), by_deprivation, device="png",width = 16, height = 4,dpi=500)

#######################################################################
################### Deprivation vs. reason for shielding ##############
#######################################################################

SPL_by_LA_dgroup_dep <- as.data.table(SPL_by_LA_dgroup_dep)

SPL_by_LA_dgroup_dep_sum <- SPL_by_LA_dgroup_dep[, list(Cases.Count=sum(Cases.Count,na.rm=TRUE),
                                                        Patient.Count=sum(Patient.Count,na.rm=TRUE)),
                                     by = list(decile_income,group)]

SPL_by_LA_dgroup_dep_sum <- mutate(SPL_by_LA_dgroup_dep_sum,Cases.Pct=Cases.Count/Patient.Count*100)

deplabs <- c("1 (most deprived)",2:9,"10 (least deprived)")

plot_reason_dep <- ggplot(SPL_by_LA_dgroup_dep_sum, aes(fill=factor(decile_income), y=Cases.Pct, x=group)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=round(Cases.Pct,0)), position=position_dodge(width=0.9),
            vjust=-0.25,size=3) +
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_brewer(palette="Spectral",name = "IMD income decile (2019)",
                    labels=deplabs) +
  labs(title=" ",y = "%",x="Reason for shielding") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) 

windows()
plot_reason_dep

#Save chart

ggsave(paste0(gitdir,"/Charts/","plot_reason_dep.png"), plot_reason_dep, device="png",width = 16, height = 7,dpi=500)

##############################################################
################### Number of shielders per GOR ##############
##############################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

SPL_by_GOR <- SPL_by_LA_All[, list(Shielders_pct=weighted.mean(Shielders_pct,pop19)), 
                            by = list(RGN19NM)] %>%
  filter(.,RGN19NM!=""&RGN19NM!="Wales")

#Add other countries

#Number of shielders
nr_shielders_Wales <- filter(SPL_by_LA_Wales_All,LA.Name=="Wales")$Patient.Count
nr_shielders_Scotland <- 179997
  #Source: https://www.publichealthscotland.scot/media/2763/covid-19-shielding-programme-scotland-impact-and-experience-survey_sept2020_english.pdf
nr_shielders_NI <- 80000
  #Source: https://patientclientcouncil.hscni.net/covid-19-and-shielding/

#Population numbers
pop_Wales <- filter(pop_by_LA,LAD19NM=="WALES")$pop19
pop_Scotland <- filter(pop_by_LA,LAD19NM=="SCOTLAND")$pop19
pop_NI <- filter(pop_by_LA,LAD19NM=="NORTHERN IRELAND")$pop19

#Create dataframes
Wales <- as.data.frame(t(c(nr_shielders_Wales/pop_Wales*100,"Wales")))
names(Wales) <- names(SPL_by_GOR)[2:1]

Scotland <- as.data.frame(t(c(nr_shielders_Scotland/pop_Scotland*100,"Scotland")))
names(Scotland) <- names(SPL_by_GOR)[2:1]

NI <- as.data.frame(t(c(nr_shielders_NI/pop_NI*100,"Northern Ireland")))
names(NI) <- names(SPL_by_GOR)[2:1]

SPL_by_GOR <- rbind(SPL_by_GOR,Scotland,NI,Wales)

SPL_by_GOR$Shielders_pct <- as.numeric(SPL_by_GOR$Shielders_pct)

#Chart

plot_by_gor <- ggplot(SPL_by_GOR) +
  geom_chicklet(aes(x=reorder(RGN19NM, Shielders_pct), y = Shielders_pct),
                fill = "firebrick",radius = grid::unit(5, 'mm'),width=0.50) +
  geom_text(aes(x = RGN19NM, y = Shielders_pct + 0.3,
                label = paste0(round(Shielders_pct, 1),"%")),
            size=5) +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15)) +
  labs(title="Average % residents shielding",y = " ",x="")


windows()
plot_by_gor

#Save chart

ggsave(paste0(gitdir,"/Charts/","by_gor.png"), plot_by_gor, device="png",width = 15, height = 7,dpi=500)
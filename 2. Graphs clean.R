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

#install.packages("ggchicklet", repos = "https://cinc.rud.is")

#Load fonts

#font_add_google("Roboto Mono", "roboto-mono")
#font_add_google("Roboto", "roboto")
font_add_google("Quicksand", "quicksand")
showtext_auto()

#Clean up the global environment
rm(list = ls())

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#One Drive directory for graphs
onedrivegraphs <- "C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/"

############################################
################### Load data ##############
############################################

SPL_by_LA_dgroup_dep <- fread(here::here("Clean data","SPL_by_LA_dgroup_dep.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA_dgroup <- fread(here::here("Clean data","SPL_by_LA_dgroup.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA <- fread(here::here("Clean data","SPL_by_LA.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA_All <- fread(here::here("Clean data","SPL_by_LA_All.csv"), header=TRUE, sep=",", check.names=T)

############################################################
################### Best and worst performing ##############
############################################################

SPL_by_LA_All <- arrange(SPL_by_LA_All, desc(Shielders_pct))
head(SPL_by_LA_All,n=10)
tail(SPL_by_LA_All,n=10)

###########################################################
############## Reasons for shielding dot plot #############
###########################################################

dotplot_condition <- ggplot(filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND"),
                            aes(Patient.Pct, reorder(group, Patient.Pct))) +
  geom_point(size=4,col="springgreen4") +
  geom_text(aes(x=Patient.Pct+3,label=paste0(round(Patient.Pct,1),"%")),
            family="quicksand",size=25) +
  theme(panel.background = element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.y=element_blank(),
        text = element_text(family = "quicksand", size = 80)) +
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

as_tibble(pct_shielding_by_income_dep)

by_deprivation <- ggplot(pct_shielding_by_income_dep) +
  geom_chicklet(aes(x=factor(decile_income), y = Shielders_pct,fill=factor(decile_income)),
                radius = grid::unit(5, 'mm'),width = 0.75) +
  geom_text(aes(x = decile_income, y = Shielders_pct + 0.2,
                label = paste0(round(Shielders_pct, 1),"%")),
            family="quicksand",size=25) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(family = "quicksand", size = 80),
        axis.text = element_text(size = 80),
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

filter(SPL_by_LA_dgroup_dep,LA.Name=="Liverpool")

SPL_by_LA_dgroup_dep <- as.data.table(SPL_by_LA_dgroup_dep)

SPL_by_LA_dgroup_dep_sum <- SPL_by_LA_dgroup_dep[, list(Cases.Count=sum(Cases.Count,na.rm=TRUE),
                                                        Patient.Count=sum(Patient.Count,na.rm=TRUE)),
                                     by = list(decile_income,group)]

#SPL_by_LA_dgroup_dep_sum <- within(SPL_by_LA_dgroup_dep_sum, {Cases.Total = ave(Cases.Count,decile_income,FUN=sum)} )
SPL_by_LA_dgroup_dep_sum <- mutate(SPL_by_LA_dgroup_dep_sum,Cases.Pct=Cases.Count/Patient.Count*100)

deplabs <- c("1 (most deprived)",2:9,"10 (least deprived)")

plot_reason_dep <- ggplot(SPL_by_LA_dgroup_dep_sum, aes(fill=factor(decile_income), y=Cases.Pct, x=group)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=round(Cases.Pct,0)), position=position_dodge(width=0.9),
            vjust=-0.25, family="quicksand",size=20) +
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_brewer(palette="Spectral",name = "IMD income decile (2019)",
                    labels=deplabs) +
  labs(title=" ",y = "%",x="Reason for shielding") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(family = "quicksand", size = 80),
        axis.text = element_text(size = 80),
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

filter(SPL_by_LA_All,RGN19NM=="London")
filter(SPL_by_LA_All,RGN19NM=="South West")
filter(SPL_by_LA_All,RGN19NM=="Wales")

Wales <- as.data.frame(t(c(132660/3152879*100,"Wales")))
names(Wales) <- names(SPL_by_GOR)[2:1]

Scotland <- as.data.frame(t(c(179997/5463300*100,"Scotland")))
names(Scotland) <- names(SPL_by_GOR)[2:1]

NI <- as.data.frame(t(c(80000/(1893700)*100,"Northern Ireland")))
names(NI) <- names(SPL_by_GOR)[2:1]

SPL_by_GOR <- rbind(SPL_by_GOR,Scotland,NI,Wales)

SPL_by_GOR$Shielders_pct <- as.numeric(SPL_by_GOR$Shielders_pct)

#Chart

plot_by_gor <- ggplot(SPL_by_GOR) +
  geom_chicklet(aes(x=reorder(RGN19NM, Shielders_pct), y = Shielders_pct),
                fill = "firebrick",radius = grid::unit(5, 'mm'),width=0.50) +
  geom_text(aes(x = RGN19NM, y = Shielders_pct + 0.45,
                label = paste0(round(Shielders_pct, 1),"%")),
            family="quicksand",size=25) +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "quicksand", size = 90)) +
  labs(title="Average % residents shielding",y = " ",x="")


windows()
plot_by_gor

#Save chart

ggsave(paste0(gitdir,"/Charts/","by_gor.png"), plot_by_gor, device="png",width = 15, height = 4,dpi=500)
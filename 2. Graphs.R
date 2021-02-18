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

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

##################################################
################### Load clean data ##############
##################################################

SPL_by_LA_dgroup_dep <- fread(here::here("Clean data","SPL_by_LA_dgroup_dep.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_dgroup <- fread(here::here("Clean data","SPL_by_LA_dgroup.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_agegroup <- fread(here::here("Clean data","SPL_by_LA_agegroup.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA <- fread(here::here("Clean data","SPL_by_LA.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_All <- fread(here::here("Clean data","SPL_by_LA_All.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_All_incl_ENG <- fread(here::here("Clean data","SPL_by_LA_All_incl_ENG.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_Wales_All <- fread(here::here("Clean data","SPL_by_LA_Wales_All.csv"), header=TRUE, sep=",", check.names=T)
SPL_by_LA_gender <- fread(here::here("Clean data","SPL_by_LA_gender.csv"), header=TRUE, sep=",", check.names=T)
pop_by_LA <- fread(here::here("Clean data","pop_by_LA.csv"), header=TRUE, sep=",", check.names=T)

#######################################################
################### UK regions shapefile ##############
#######################################################

setwd(paste0(rawdatadir,"/Shapefiles"))

UK_shp <- readOGR(dsn="NUTS_Level_1__January_2018__Boundaries-shp",layer="NUTS_Level_1__January_2018__Boundaries")
UK_shp <- spTransform(UK_shp, CRS(latlong))

###############################################
############## Gender differences #############
###############################################

male_shielders <- filter(SPL_by_LA_gender,LA.Name=="ENGLAND"&Breakdown.Value=="Male")$Patient.Count.Sex
female_shielders <- filter(SPL_by_LA_gender,LA.Name=="ENGLAND"&Breakdown.Value=="Female")$Patient.Count.Sex

men_england <- filter(SPL_by_LA_gender,LA.Name=="ENGLAND"&Breakdown.Value=="Male")$pop19*0.49
women_england <- filter(SPL_by_LA_gender,LA.Name=="ENGLAND"&Breakdown.Value=="Female")$pop19*0.51

rate_men <- round(male_shielders/men_england*100,1)
rate_women <- round(female_shielders/women_england*100,1)

###########################################################
############## Reasons for shielding dot plot #############
###########################################################

reasons.data <- filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND") %>%
  select(.,group,Patient.Pct)

dotplot_condition <- ggplot(reasons.data,
                            aes(Patient.Pct, reorder(group, Patient.Pct))) +
  geom_point(size=4,col="springgreen4") +
  geom_text(aes(x=Patient.Pct+2,label=paste0(round(Patient.Pct,0),"%")),
            size=3) +
  theme(panel.background = element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.y=element_blank(),
        text = element_text(size = 10)) +
  labs(title="Conditions",y = " ",x=" ")

windows()
dotplot_condition

#Save chart

ggsave(paste0(gitdir,"/Charts/","dotplot_condition.png"), dotplot_condition, device="png",width = 10, height = 5,dpi=500)
fwrite(reasons.data, file = paste0(gitdir,"/Charts/","reasons.data.csv"), sep = ",")

################################################################
############## Top 10 and Bottom 10 Shielding Pct ##############
################################################################

top10_highest_shielding <- arrange(SPL_by_LA_All, desc(Shielders_pct)) %>%
  select(.,LA.Code,LA.Name,RGN19NM,Shielders_pct) %>%
  head(.,n=10)

top10_lowest_shielding <- arrange(SPL_by_LA_All, desc(Shielders_pct)) %>%
  select(.,LA.Code,LA.Name,RGN19NM,Shielders_pct) %>%
  tail(.,n=10)

fwrite(top10_highest_shielding, file = paste0(gitdir,"/Charts/","top10_highest_shielding.csv"), sep = ",")
fwrite(top10_lowest_shielding, file = paste0(gitdir,"/Charts/","top10_lowest_shielding.csv"), sep = ",")

#################################################################
############## Age of shielding patients in England #############
#################################################################

pct_age_group_shielding <- filter(SPL_by_LA_agegroup,LA.Name=="ENGLAND") %>%
  mutate(.,x=1) %>% rename(age.group=Breakdown.Value)

cols9 <- colorRampPalette( brewer.pal(n = 9, name = "YlGnBu"))(10)[2:10]

by_age_group <- ggplot(pct_age_group_shielding) +
  geom_chicklet(aes(x=factor(age.group), y = Patient.Pct,fill=factor(age.group)),
                radius = grid::unit(5, 'mm'),width = 0.75) +
  geom_text(aes(x = age.group, y = Patient.Pct + 0.75,
                label = paste0(round(Patient.Pct, 0),"%")),
            size=5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  scale_fill_manual(values=cols9,guide=FALSE) +
  labs(title="% in age group",y = " ",x="Age group")

windows()
by_age_group

#Save chart

ggsave(paste0(gitdir,"/Charts/","by_age_group.png"), by_age_group, device="png",width = 10, height = 5,dpi=500)

#############################################################################
################### Number of shielders vs. deprivation decile ##############
#############################################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_dep <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop19)), 
  by = list(decile_income)] %>%
  filter(.,!is.na(decile_income)) %>%
  dplyr::rename(decile=decile_income)

cols <- colorRampPalette(brewer.pal(n = 9, name = "RdYlGn"))(10)
deplabs <- c("1 (most deprived)",2:9,"10 (least deprived)")

by_deprivation <- ggplot(pct_shielding_by_dep) +
  geom_chicklet(aes(x=factor(decile), y = Shielders_pct,fill=factor(decile)),
                radius = grid::unit(5, 'mm'),width = 0.75) +
  geom_text(aes(x = decile, y = Shielders_pct + 0.2,
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

ggsave(paste0(gitdir,"/Charts/","by_all_deprivation_dec.png"), by_deprivation, device="png",width = 16, height = 4,dpi=500)

#############################################################################
################### Number of shielders vs. deprivation quintile ############
#############################################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_dep_quintile <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop19)), 
  by = list(quintile_income)] %>%
  filter(.,!is.na(quintile_income)) %>%
  dplyr::rename(quintile=quintile_income)

cols5 <- brewer.pal(n = 5, name = "RdYlGn")
deplabs5 <- c("1 (most deprived)",2:4,"5 (least deprived)")

by_deprivation_quintile <- ggplot(pct_shielding_by_dep_quintile) +
  geom_chicklet(aes(x=factor(quintile), y = Shielders_pct,fill=factor(quintile)),
                radius = grid::unit(5, 'mm'),width = 0.75) +
  geom_text(aes(x = quintile, y = Shielders_pct + 0.2,
                label = paste0(round(Shielders_pct, 1),"%")),
            size=5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  scale_fill_manual(values=cols5,guide=FALSE) +
  scale_x_discrete(labels = deplabs5) +
  labs(title="Average % residents shielding",y = " ",x="Income deprivation quintile")

windows()
by_deprivation_quintile

#Save chart

ggsave(paste0(gitdir,"/Charts/","by_all_deprivation_quint.png"), by_deprivation_quintile, device="png",width = 9, height = 4,dpi=500)

##############################################################################
################### Deprivation decile vs. reason for shielding ##############
##############################################################################

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

##############################################################################
################### Deprivation quintile vs. reason for shielding ############
##############################################################################

SPL_by_LA_dgroup_dep <- as.data.table(SPL_by_LA_dgroup_dep)

SPL_by_LA_dgroup_depquint_sum <- SPL_by_LA_dgroup_dep[, list(Cases.Count=sum(Cases.Count,na.rm=TRUE),
                                                        Patient.Count=sum(Patient.Count,na.rm=TRUE)),
                                                 by = list(quintile_income,group)]

SPL_by_LA_dgroup_depquint_sum <- mutate(SPL_by_LA_dgroup_depquint_sum,Cases.Pct=Cases.Count/Patient.Count*100)

deplabs5 <- c("1 (most deprived)",2:4,"5 (least deprived)")

plot_reason_depquint <- ggplot(SPL_by_LA_dgroup_depquint_sum, aes(fill=factor(quintile_income), y=Cases.Pct, x=group)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=round(Cases.Pct,0)), position=position_dodge(width=0.9),
            vjust=-0.25,size=5) +
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_brewer(palette="Spectral",name = "IMD income quintile (2019)",
                    labels=deplabs5) +
  labs(title=" ",y = "%",x="Reason for shielding") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) 

windows()
plot_reason_depquint

#Save chart

ggsave(paste0(gitdir,"/Charts/","plot_reason_depquint.png"), plot_reason_depquint, device="png",width = 18, height = 7,dpi=500)

##############################################################
################### Number of shielders per GOR ##############
##############################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

# NW_shielders <- filter(SPL_by_LA_All,RGN19NM=="North West") %>%
#   select(.,Patient.Count) %>% sum(.)
# NW_pop <- filter(SPL_by_LA_All,RGN19NM=="North West") %>%
#   select(.,pop19) %>% sum(.)
# NW_rate <- round(NW_shielders/NW_pop*100,1)
  
# SPL_by_GOR <- SPL_by_LA_All[, list(Shielders_pct=weighted.mean(Shielders_pct,pop19)), 
#                             by = list(RGN19NM)] %>%
#   filter(.,RGN19NM!=""&RGN19NM!="Wales")

SPL_by_GOR <- SPL_by_LA_All[, list(Patient.Count=sum(Patient.Count,na.rm=TRUE),
                                   pop19=sum(pop19,na.rm=TRUE),
  Shielders.pct=weighted.mean(Shielders_pct,pop19)), 
                            by = list(RGN19NM)] %>%
  filter(.,RGN19NM!=""&RGN19NM!="Wales")

#Add other countries

#Number of shielders
nr_shielders_England <- filter(SPL_by_LA_All_incl_ENG,LA.Name=="ENGLAND")$Patient.Count
nr_shielders_Wales <- filter(SPL_by_LA_Wales_All,LA.Name=="Wales")$Patient.Count
nr_shielders_Scotland <- 179997
  #Source: https://www.publichealthscotland.scot/media/2763/covid-19-shielding-programme-scotland-impact-and-experience-survey_sept2020_english.pdf
nr_shielders_NI <- 80000
  #Source: https://patientclientcouncil.hscni.net/covid-19-and-shielding/
nr_UK <- nr_shielders_England+nr_shielders_Wales+nr_shielders_Scotland+nr_shielders_NI

#Population numbers
pop_Wales <- filter(pop_by_LA,LAD19NM=="WALES")$pop19
pop_Scotland <- filter(pop_by_LA,LAD19NM=="SCOTLAND")$pop19
pop_NI <- filter(pop_by_LA,LAD19NM=="NORTHERN IRELAND")$pop19
pop_England <- filter(pop_by_LA,LAD19NM=="ENGLAND")$pop19
pop_UK <- pop_England+pop_Wales+pop_Scotland+pop_NI

#Create dataframes
Wales <- as.data.frame(t(c("Wales",nr_shielders_Wales,pop_Wales,nr_shielders_Wales/pop_Wales*100)))
names(Wales) <- names(SPL_by_GOR)

Scotland <- as.data.frame(t(c("Scotland",nr_shielders_Scotland,pop_Scotland,nr_shielders_Scotland/pop_Scotland*100)))
names(Scotland) <- names(SPL_by_GOR)

NI <- as.data.frame(t(c("Northern Ireland",nr_shielders_NI,pop_NI,nr_shielders_NI/pop_NI*100)))
names(NI) <- names(SPL_by_GOR)

Eng <- as.data.frame(t(c("England",nr_shielders_England,pop_England,nr_shielders_England/pop_England*100)))
names(Eng) <- names(SPL_by_GOR)

SPL_by_GOR <- rbind(SPL_by_GOR,Scotland,NI,Wales)
SPL_by_GOR_wEng <- rbind(SPL_by_GOR,Eng)

SPL_by_GOR$Shielders.pct <- as.numeric(SPL_by_GOR$Shielders.pct)

#Chart

plot_by_gor <- ggplot(SPL_by_GOR) +
  geom_chicklet(aes(x=reorder(RGN19NM, Shielders.pct), y = Shielders.pct),
                fill = "firebrick",radius = grid::unit(5, 'mm'),width=0.50) +
  geom_text(aes(x = RGN19NM, y = Shielders.pct + 0.3,
                label = paste0(round(Shielders.pct, 1),"%")),
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
fwrite(SPL_by_GOR_wEng, file = paste0(gitdir,"/Charts/","SPL_by_GOR_wEng.csv"), sep = ",")

##################################################################
################### Number of shielders per GOR map ##############
##################################################################

  #Merge data into shapefile
UK_shp@data$nuts118nm <- gsub(" (England)", "", UK_shp@data$nuts118nm, fixed=TRUE)
UK_shp@data <- left_join(UK_shp@data,SPL_by_GOR,by=c("nuts118nm"="RGN19NM"))

  #Colors and palette
colors <- brewer.pal(4, "YlOrRd")
pal.function <- colorBin(colors, UK_shp$Shielders_pct, bins=c(0,3.5,4,4.5,Inf))

  #Labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g percent",
  UK_shp$nuts118nm, round(UK_shp$Shielders_pct,1)
) %>% lapply(htmltools::HTML)

legend_label <- c("<3.5%","3.5 to 4%","4 to 4.5%",">4.5%")

  #Map
leaflet(UK_shp, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  setView(lat=51.5095,lng=-0.1245,zoom = 5) %>%
  addPolygons(
    fillColor = ~pal.function(Shielders_pct),weight = 1,opacity = 0.1,color = "black",
    dashArray = "3",fillOpacity = 0.5,
    highlight = highlightOptions(weight = 5,color = "#666",dashArray = "",
                                 fillOpacity = 0.7,bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% addLegend(
        position = "topleft",
        colors = colors,
        labels = legend_label, opacity = 1
      )
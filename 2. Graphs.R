##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Gender disparity: cannot look at reasons by gender using this data
#Why are people shielding and does this vary across regions?

#Isolate worst 10% and see why reasons vary (% cancer, % respiratory)
#Do the same by Region
#Bar chart (lowest to highest) for 'Respiratory' with horizontal line for England, and
#bars coloured by shielders per capita

#Same approach for why women have higher rates
#Clustering: what type of shielders per area?

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

SPL_by_LA_dgroup <- fread(here::here("Clean data","SPL_by_LA_dgroup.csv"), header=TRUE, sep=",", check.names=T)

SPL_by_LA_All <- fread(here::here("Clean data","SPL_by_LA_All.csv"), header=TRUE, sep=",", check.names=T)

############################################################
################### Local authority shapefile ##############
############################################################

LAD_2019_shp <- readOGR(dsn=paste0(rawdatadir,"Shapefiles/Local_Authority_Districts__December_2016__Boundaries_UK-shp"), layer="Local_Authority_Districts__December_2016__Boundaries_UK")
LAD_2019_shp <- spTransform(LAD_2019_shp, CRS(latlong)) #Set to the same projection

#################################################################
################### Histogram of % shielder per LA ##############
#################################################################

ggplot(SPL_by_LA_All, aes(Shielders_pct, fill = cut(Shielders_pct, 100))) +
  geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% shielders", y = "n") +
  ggtitle("Histogram") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

###########################################################
############## Gender differences in shielding ############
###########################################################

SPL_by_LA_Gender <- filter(SPL_by_LA,Breakdown.Field=="Gender"&LA.Name=="ENGLAND")

men_england <-  56286961*0.49
women_england <-  56286961*0.51

gender <- c("Men","Women")
rate_shielding <- c(as.numeric(filter(SPL_by_LA_Gender,Breakdown.Value=="Male")$Patient.Count)/men_england*100,
                    as.numeric(filter(SPL_by_LA_Gender,Breakdown.Value=="Female")$Patient.Count)/women_england*100)

by_gender <- cbind.data.frame(gender,rate_shielding)

dotplot_gender <- ggplot(by_gender,
                            aes(rate_shielding, reorder(gender, rate_shielding))) +
  xlim(3, 5) +
  geom_point(aes(col=gender),size=4) +
  geom_text(aes(x=rate_shielding+0.2,label=paste0(round(rate_shielding,1),"%")),
            family="quicksand",size=25) +
  theme(panel.background = element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.y=element_blank(),
        text = element_text(family = "quicksand", size = 80),
        legend.position = "none") +
  labs(title="Likelihood of shielding by gender",y = " ",x=" ")

windows()
dotplot_gender

#Save chart

ggsave(paste0(onedrivegraphs,"dotplot_gender.png"), dotplot_gender,device="png",width = 5, height = 5,dpi=500)

###########################################################
############## Reasons for shielding dot plot #############
###########################################################

dotplot_condition <- ggplot(filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND"),
                            aes(Cases.Pct, reorder(group, Cases.Pct))) +
  geom_point(size=4,col="springgreen4") +
  geom_text(aes(x=Cases.Pct+3,label=paste0(round(Cases.Pct,1),"%")),
            family="quicksand",size=25) +
  theme(panel.background = element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.y=element_blank(),
        text = element_text(family = "quicksand", size = 80)) +
  labs(title="Conditions",y = " ",x=" ")

windows()
dotplot_condition

#Save chart

ggsave(paste0(onedrivegraphs,"dotplot_condition.png"), dotplot_condition, device="png",width = 10, height = 5,dpi=500)

###########################################################################
############## Bar chart per condition - national discrepancy #############
###########################################################################

filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND")
filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")
sum(filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")$Cases.Pct)

#####Respiratory (this is driving differences!)

# ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(Cases.Pct, fill = cut(Cases.Pct, 100))) +
#   geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% cases", y = "n") +
#   ggtitle("Respiratory conditions") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

# ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(x=Cases.Pct.Differential, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(x=Shielders_pct, y=Cases.Pct)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(x=Shielders_pct, y=Cases.Prev.1000)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

respiratory <- ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory")) +
  ylim(0, 10) +
  geom_col(aes(x=reorder(LA.Name, Shielders_pct), y = Shielders_pct,fill = factor(levels_pct))) +
  scale_fill_brewer(palette="Spectral",name = "Respiratory cases",direction = -1,
                    labels = c("<40%", "40-50%","50-60%",">60%")) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "quicksand", size = 85)) +
  geom_hline(aes(yintercept=4),col="black", linetype="dashed") +
  annotate(geom="text", label="4% (average)", x=35, y=4, vjust=-1,size=25) +
  labs(title="Shielding patients by local authority",y = "% shielding",x="Local authority")

windows()
respiratory

#Save chart

ggsave(paste0(onedrivegraphs,"respiratory.png"), respiratory, device="png",width = 10, height = 7,dpi=500)

#####Cancer

# ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(Cases.Pct, fill = cut(Cases.Pct, 100))) +
#   geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% cases", y = "n") +
#   ggtitle("Cancer") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(x=Cases.Pct.Differential, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(x=Shielders_pct, y=Cases.Pct)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(x=Shielders_pct, y=Cases.Prev.1000)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

#####Rare genetic and autoimmune

ggplot(filter(SPL_by_LA_dgroup,group=="Rare genetic and autoimmune"), aes(Cases.Pct, fill = cut(Cases.Pct, 100))) +
  geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% cases", y = "n") +
  ggtitle("Rare genetic and autoimmune") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

ggplot(filter(SPL_by_LA_dgroup,group=="Rare genetic and autoimmune"), aes(x=Cases.Pct.Differential, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

######################################################################
################### Number of shielders vs. deprivation ##############
######################################################################

#SPL_by_LA_All <- left_join(SPL_by_LA_All,IMD2019_LA_wide,by=c("LA.Code"="FeatureCode"))

#SPL_by_LA_All$score_income_cat <- cut(SPL_by_LA_All$avgscore_income, breaks=c(0,0.05,0.075,0.1,0.125,0.150,0.2,Inf), labels=1:7)

#General deprivation

# ggplot(SPL_by_LA_All, aes(x=pctdep_IMD, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)
# 
# ggplot(SPL_by_LA_All, aes(x=avgscore_imd, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

#Health deprivation

# ggplot(SPL_by_LA_All, aes(x=pctdep_health, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)
# 
# ggplot(SPL_by_LA_All, aes(x=avgscore_health, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

#Income deprivation

# ggplot(SPL_by_LA_All, aes(x=pctdep_income, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)
# 
# ggplot(SPL_by_LA_All, aes(x=avgscore_income, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_income_dep <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop18)), 
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

ggsave(paste0(onedrivegraphs,"by_deprivation.png"), by_deprivation, device="png",width = 16, height = 4,dpi=500)

######################################################################
################### Number of shielders vs. pct over 65 ##############
######################################################################

# ggplot(SPL_by_LA_All, aes(x=pct_over65_18, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

head(dplyr::arrange(SPL_by_LA_All, pct_over65_18),n=10)
tail(dplyr::arrange(SPL_by_LA_All, pct_over65_18),n=10)

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_ageg <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop18)), 
  by = list(cat_pct_over65_18)]

cols <- brewer.pal(n = 8, name = "GnBu")[5:8]
agelabs <- c("<15% over 65","15-20% over 65","20-25% over 65",">25% over 65")
  
plot_age <- ggplot(pct_shielding_by_ageg) +
  geom_chicklet(aes(x=factor(cat_pct_over65_18), y = Shielders_pct,fill=factor(cat_pct_over65_18)),
                radius = grid::unit(5, 'mm'),width=0.5) +
  geom_text(aes(x = cat_pct_over65_18, y = Shielders_pct + 0.2,
                label = paste0(round(Shielders_pct, 1),"%")),
            family="quicksand",size=15) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(family = "quicksand", size = 45),
        axis.text = element_text(size = 45),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  scale_fill_manual(values=cols,guide=FALSE) +
  scale_x_discrete(labels = agelabs) +
  labs(title="Average % residents shielding",y = " ",x="Local authorities")

windows()
plot_age

#Save chart

ggsave(paste0(onedrivegraphs,"over65.png"), plot_age, device="png",width = 5, height = 3,dpi=500)

##############################################################
################### Number of shielders per GOR ##############
##############################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

SPL_by_GOR <- SPL_by_LA_All[, list(Shielders_pct=weighted.mean(Shielders_pct,pop18),
                                   RGN11NM=first(RGN11NM)), 
                            by = list(RGN11CD)]

#Add other countries

Scotland <- as.data.frame(t(c("Unknown",180000/(5.454*1000000)*100,"Scotland")))
names(Scotland) <- names(SPL_by_GOR)

NI <- as.data.frame(t(c("Unknown",80000/(1.882*1000000)*100,"Northern Ireland")))
names(NI) <- names(SPL_by_GOR)

SPL_by_GOR <- rbind(SPL_by_GOR,Scotland,NI)

SPL_by_GOR$Shielders_pct <- as.numeric(SPL_by_GOR$Shielders_pct)

#chart

plot_by_gor <- ggplot(SPL_by_GOR) +
  geom_chicklet(aes(x=reorder(RGN11NM, Shielders_pct), y = Shielders_pct),
                fill = "firebrick",radius = grid::unit(5, 'mm'),width=0.50) +
  geom_text(aes(x = RGN11NM, y = Shielders_pct + 0.45,
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

ggsave(paste0(onedrivegraphs,"by_gor.png"), plot_by_gor, device="png",width = 15, height = 4,dpi=500)

##############################################################################
################### Waffle chart for number of people shielding ##############
##############################################################################

library(waffle)
library(extrafont)

# fa_font <- tempfile(fileext = ".ttf")
# download.file("http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/fonts/fontawesome-webfont.ttf?v=4.3.0",
#               destfile = fa_font, method = "curl")
# 
# font_import(paths = dirname(fa_font), prompt = FALSE)

loadfonts(device = "win")

fa_list()

shielding <- c(`Category 1 (10)`= 96 , `Category 2 (20)`= 4)

waffle_plot <- waffle(shielding/2, rows=5, use_glyph = "male",
                      colors=c("darkgray","firebrick"),
                      legend_pos="none")

windows()
waffle_plot

#Save chart

ggsave(paste0(onedrivegraphs,"waffle.png"), waffle_plot, device="png",width = 5, height = 5,dpi=500)

#################################################################################
################### Map of number of shielders per 1,000 residents ##############
#################################################################################

# #Add data to shapefile
# 
# LAD_2019_shp@data <- left_join(LAD_2019_shp@data,SPL_by_LA_All,by=c("lad16cd"="LA.Code"))
# 
# #Create map
# 
# #Colors and palette
# colors <- colorRampPalette(brewer.pal(9, "Spectral"))(10)[10:1]
# 
# pal.function <- colorQuantile(colors, LAD_2019_shp$Shielders_pct, n = 10, probs = seq(0, 1, length.out = 10
#                                                                                       + 1), na.color = "transparent", alpha = FALSE, reverse = FALSE,
#                               right = FALSE)
# 
# #Quantiles
# deciles <- round(quantile(LAD_2019_shp$Shielders_pct,
#                           prob = seq(0, 1, length = 11), type = 5,na.rm=TRUE),1)
# 
# #Labels
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g percent",
#   LAD_2019_shp$lad16nm, round(LAD_2019_shp$Shielders_pct,2)
# ) %>% lapply(htmltools::HTML)
# 
# #Map
# leaflet(LAD_2019_shp) %>%
#   addProviderTiles(providers$Wikimedia) %>%
#   setView(lat=51.5095,lng=-0.1245,zoom = 5) %>%
#   addPolygons(
#     fillColor = ~pal.function(Shielders_pct),weight = 1,opacity = 0.1,color = "black",
#     dashArray = "3",fillOpacity = 0.3,
#     highlight = highlightOptions(weight = 5,color = "#666",dashArray = "",
#                                  fillOpacity = 0.7,bringToFront = TRUE),
#     label = labels,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto")) %>% addLegend(
#         position = "bottomright",
#         colors = colors,
#         labels = deciles[1:10], opacity = 1
#       )
	  
# ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory")) +
#   ylim(0, 10) +
#   geom_col(aes(x=reorder(LA.Name, Shielders_pct), y = Shielders_pct,fill = factor(over50_pct))) +
#   scale_fill_manual(values=c("#3288bd","#d53e4f"),name = "Respiratory cases",labels = c("<50%", ">50%")) +
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
#         axis.text.x=element_blank(),axis.ticks.x=element_blank(),
#         panel.background = element_blank()) +
#   geom_hline(aes(yintercept=4),col="black", linetype="dashed") +
#   annotate(geom="text", label="4% (average)", x=35, y=4, vjust=-1,size=3) +
#   labs(title="Shielding patients by local authority",y = "% shielding",x="Local authority")


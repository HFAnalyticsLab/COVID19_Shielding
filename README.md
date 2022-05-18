# Shielding patients

#### Project Status: Completed

## Project Description

This is an exploratory analysis of Open Data on shielding patients, with the purpose of producing a series of charts to be integrated into a blog post or similar. This may support the launch of the Networked Data Lab's pilot study on shielding patients by providing contextual information.

## Data source

This projects pools together different sources of data on Shielding patients:

- The Shielding Patient List ([England](https://digital.nhs.uk/dashboards/shielded-patient-list-open-data-set)) split by local authority, gender, age group and reason for shielding
- The Shielding Patient List ([Wales](https://gov.wales/shielded-patient-list-wales-during-coronavirus-covid-19-pandemic-15-june-2020)) split by local authority

This is linked the local authority-level ONS datasets on:

- Local deprivation ([Index of Multiple Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) domains)
- Population estimates ([mid-year estimates](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland))
- Age structure (% over 65)

## How does it work?

In order to run the first .R code ('1. Importing and cleaning data.R') you will need access to the Open Data Environment where those large datasets listed above are saved. This first R code saves two clean .csv files inside the repository (/Clean data), which are accessed by the second .R code ('2. Graphs.R') to produce the relevant charts.

### Requirements

The following R packages (available on CRAN) are needed: 

- dplyr
- stringr
- sp
- ggplot2
- plyr
- readODS
- gmodels
- Rmisc
- DescTools
- data.table
- Hmisc
- leaflet
- rgeos
- raster
- plotly
               
These scripts were written in R version (4.0.2) and RStudio Version 1.1.383. 

### Getting started

If you would like to merge and clean the input datasets from scratch, run the first script ('1. Importing and cleaning data.R').

If you would like to skip that first stage and directly produce the graphs, you may start with the second script ('2. Graphs.R').

## Authors

* Sebastien Peytrignet - (Github: sg-peytrignet / Twitter: @SebastienPeytr2)

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/README_template/blob/master/LICENSE).

## Acknowledgments

The inspiration for the choropleth maps comes from the NHS Digital Dashboard.

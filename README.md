# Climate change risks to marine ecosystems and fisheries: Future projections from the Fisheries and Marine Ecosystems Model Intercomparison Project

## Overview
This repository was developed by the [Fisheries and Marine Ecosystem Model Intercomparison Project (FishMIP)](https://fishmip.org/) group, and it contains data analysis workflows to produce a subset of the figures included in the *Climate change risks to marine ecosystems and fisheries: Future projections from the Fisheries and Marine Ecosystems Model Intercomparison Project* report prepared for the FAO.  
  
## Accessing outputs from FishMIP models
Due to the size of the input files, the data used in the scripts within this repository have not been included. However, outputs for all FishMIP models included in this analysis can be accessed from the [Inter-Sectoral Impact Model Intercomparison Project (ISIMIP) Data Portal](https://data.isimip.org/). Alternatively, you can contact the [FishMIP coordination team](mailto:fishmip.coordinators@gmail.com) to access the data used as input here.  
  
## Accessing other supporting data
To produce the maps in this repository, we used the following publicly available datasets:  
- Exclusive Economic Zones (EEZs) boundaries (version 11) from the Flanders Marine Institute [[1]](#1)  
- Waters under national jurisdiction (200 nm limits): `fifao:limit_200nm` [[2]](#2)  
- UN-approved world base map: `fifao:country_bounds` [[2]](#2)  
- UN-approved international boundaries: `fifao:UN_intbnd` [[2]](#2)  
- Global fisheries catches (1950-2021) from the FAO [[3]](#3)  
- FAO statistical areas for fishery purposes [[4]](#4)  
- Historic and future country-level population data (1950-2100) from ISIMIP [[5]](#5), [[6]](#6), [[7]](#7)  
- Percentage of sustainable fisheries per countries from the FAO [[8]](#8)  
  
## Requirements to run these notebooks
All scripts included in these notebooks were developed in `R` version 4.3.0. Please ensure you have this `R` version or later installed in your computer for notebooks to run correctly.  
  
You will also need to have the following packages installed prior to running the notebooks. You will need to check not only that each package is installed in your computer, but also that the version installed is the same or higher:  
  
```R
[1] tidyverse_2.0.0         here_1.0.1      sf_1.0-12     data.table_1.14.8   countrycode_1.5.0 rnaturalearth_0.3.2 glue_1.7.0
[7] rnaturalearthdata_0.1.0 patchwork_1.1.2 terra_1.7-18  googlesheets4_1.1.0 readxl_1.4.2      janitor_2.2.0 
[13] ggnewscale_0.4.9       tidyterra_0.4.0 cmocean_0.3-1 cowplot_1.1.1       shiny_1.7.4       
```
  
## How to run these notebooks in your computer
If you are familiar with `git` and GitHub, you can clone this repository to your computer. Alternatively, you can click on the green **<> Code** button on the right hand side at the top of this page to see the options available. Then click on *Download ZIP* option. This will download an exact copy of this repository to your computer. Make sure you uncompress (unzip) the downloaded folder before attempting to open any files.  
  
Since this repository has an open software license, once you have a copy of this repository, feel free to not only run the notebooks as they are, but you can also adapt them to suit your needs. But if you use any of the scripts (partially or in full), make sure you acknowledge this work (this is a license condition). You can find our preferred citation by clicking on *Cite this repository* in the **About** section of this repository (top right panel).  
  
## Estimates of fish biomass change from FishMIP model ensemble (Shiny app)
If you do not have experience in `R`, but would still like to have access to the temporal trends of fish biomass change (plots and data) as produced by the script named [05_plotting_yearly_global_biomass_change](https://github.com/Fish-MIP/FAO_Report/blob/main/scripts/05_plotting_yearly_global_biomass_change.Rmd), we are providing an alternative.  
  
We have developed a shiny app, you can access the beta version [here](https://rstudio.global-ecosystem-model.cloud.edu.au/shiny/FAO_Report/shiny_app/). You can select the Exclusive Economic Zone [[1]](#1) or the FAO area for fishery purposes [[3]](#3) of your interest, which will produce an interactive plot of temporal trends of fish biomass changes in the area you selected. It will also show the entire dataset used to create the interactive plot. Additionally, there is a download button (on the left panel) that allows you to download the data shown in the table as a csv file.  
  
## How to report issues or suggest changes
You can check reported issue or create a new issue [here](https://github.com/Fish-MIP/FAO_Report/issues).  
  
## References
<a id="1">[1]</a> 
Flanders Marine Institute. (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at [https://www.marineregions.org/](https://www.marineregions.org/). [https://doi.org/10.14284/386](https://doi.org/10.14284/386).    
  
<a id="2">[2]</a> 
FAO. (2020). Geo Server. In: Food and Agriculture Organization of the United Nations [online]. Rome. [Cited February 20, 2024]. [https://www.fao.org/figis/geoserver/web/](https://www.fao.org/figis/geoserver/web/).    

<a id="3">[3]</a> 
FAO. (2023). Fishery and Aquaculture Statistics. Global production by production source 1950-2021 (FishStatJ). In: FAO Fisheries and Aquaculture Division [online]. Rome. Updated 2023. [www.fao.org/fishery/en/statistics/software/fishstatj](www.fao.org/fishery/en/statistics/software/fishstatj).  
  
<a id="4">[4]</a> 
FAO. (2020). FAO Statistical Areas for Fishery Purposes. In: FAO Fisheries and Aquaculture Department [online]. Rome. [http://www.fao.org/fishery/area/search/en](http://www.fao.org/fishery/area/search/en).  
  
<a id="5">[5]</a> 
European Commission, Joint Research Centre, Demographic and Human Capital Scenarios for the 21st Century: 2018 assessment for 201 countries, Wolfgang Lutz, Anne Goujon, Samir KC, Marcin Stonawski, Nikolaos Stilianakis (Eds.), Publications Office of the European Union, Luxembourg, 2018, ISBN 978-92-79-78024-0, [https://doi.org/10.2760/41776](https://doi.org/10.2760/41776), EUR 29113.  
  
<a id="6">[6]</a> 
KC S., Lutz W. (2017). The human core of the shared socioeconomic pathways: Population scenarios by age, sex and level of education for all countries to 2100, Global Environmental Change, Volume 42, Pages 181-192, ISSN 0959-3780, [https://doi.org/10.1016/j.gloenvcha.2014.06.004](https://doi.org/10.1016/j.gloenvcha.2014.06.004).  
  
<a id="7">[7]</a> 
KC, S. (2020). Updated demographic SSP4 and SSP5 scenarios complementing the SSP1-3 scenarios published in 2018. IIASA Working Paper. Laxenburg, Austria: WP-20-016  
  
<a id="8">[8]</a> 
FAO. (2022). The State of World Fisheries and Aquaculture. Food and Agriculture Organisation of the United Nations (FAO), Fisheries and Aquaculture Department.

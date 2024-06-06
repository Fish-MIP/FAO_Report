#Code developed for shiny app supporting FAO report
#Author: Denisse Fierro Arcos

# Loading R libraries -----------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(bslib)
library(cowplot)
library(cmocean)
library(scales)
library(sf)
library(rnaturalearth)
library(ggiraph)
library(glue)


#Base folder for project
base_folder <- "/rd/gem/private/users/camillan"

#Defining location of notebook outputs
out_folder <- file.path(base_folder, "FAO_Report")

# Loading data ------------------------------------------------------------
#Loading ensemble biomass change
maps_data <- read_csv("../data/ensemble_perc_bio_change_data_map.csv", 
                      col_select = c(x, y, starts_with("rel_change"), 
                                     NAME_EN, continent, name_merge,
                                     figure_name)) |> 
  mutate(name = case_when(!is.na(continent) ~ continent, 
                          !is.na(NAME_EN) ~ NAME_EN, T ~ NA))


#Ensemble percentage change in biomass by countries
count_bio <- list.files("/rd/gem/private/users/camillan/FAO_Report/", 
                        "ensemble_perc_bio_change_country.csv",
                        recursive = T, full.names = T) |> 
  read_csv()


#Ensemble percentage change in biomass by FAO regions
fao_bio <- list.files("/rd/gem/private/users/camillan/FAO_Report/", 
                      "ensemble_perc_bio_change_fao_region.csv",
                      recursive = T, full.names = T) |> 
  read_csv()

#Ensemble percentage change in biomass by FAO regions
lme_bio <- list.files("/rd/gem/private/users/camillan/FAO_Report/", 
                      "ensemble_perc_bio_change_lme.csv",
                      recursive = T, full.names = T) |> 
  read_csv()

# iso3c lookups
iso_lookup <- read.csv("../data/iso3c_lookup_regions.csv")

#Table of summary statistics
table_stats_admin <- read_csv("../data/table_stats_country_admin.csv") %>% left_join(iso_lookup) %>%
  mutate(fill_category = case_when(
    mean < -30 ~ "Decrease >30%",
    mean <= -20 & mean >= -30 ~ "Decrease 20 to 30%",
    mean <= -10 & mean > -20 ~ "Decrease 10 to 20%",
    mean <= 0 & mean > -10 ~ "Decrease <10%",
    mean >= 0 & mean < 10 ~ "Increase <10%",
    mean >= 10 & mean < 20 ~ "Increase 10 to 20%", 
    mean >= 20 & mean <= 30 ~ "Increase 20 to 30%",
    mean > 30 ~ "Increase >30%"
  ))

#List of countries
country_list <- maps_data |> 
  distinct(figure_name) |> 
  drop_na() |> 
  rename("name"= "figure_name") |> 
  arrange(name)

#List of continents
continent_list <- maps_data |> 
  distinct(continent) |> 
  drop_na() |> 
  rename("name"= "continent") |> 
  arrange(name)

#List of LMEs
lme_list <- maps_data |>
  distinct(name_merge) |>
  drop_na() |>
  rename("name"= "name_merge") |> 
  arrange(name)

#List of FAO regions
fao_list <- maps_data |>
  distinct(NAME_EN) |> 
  drop_na() |> 
  rename("name"= "NAME_EN") |> 
  arrange(name)

#Map of the world
world <- ne_countries(returnclass = "sf", scale = "medium")
world_360 <- read_sf("../data/world_360deg.shp")

# read in shapefiles with countries and eezs
countries_shp <- ne_countries(scale = 110, returnclass = "sf") |> 
  st_transform(crs('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')) # read in countries shapefile

## join admin countries summary with shp file
table_stats_admin_shp <- countries_shp %>%
  dplyr::select(-continent) %>%
  right_join(table_stats_admin, by = c("adm0_a3" = "ISO3_country")) %>%
  rename(ISO3_country = adm0_a3) %>% 
  dplyr::select(colnames(table_stats_admin)) %>%
  filter(!st_is_empty(.))
  

# Supporting information --------------------------------------------------
#Create custom-made color palette
scale_fill_custom <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                              option = "D", values = NULL, space = "Lab", 
                              na.value = "white", guide = "colourbar", 
                              aesthetics = "fill") {
  continuous_scale(aesthetics, scale_name = "custom",
                   palette = gradient_n_pal(c(cmocean("matter", start = 0.1, 
                                                      end = 0.8, 
                                                      direction = -1)(123),
                                              cmocean("delta", start = 0.49, 
                                                      end = 0.5)(20),
                                              cmocean("deep", start = 0.1, 
                                                      end = 0.8)(123)), values,
                                            space), 
                   na.value = na.value, guide = guide, ...)
}


#Define base steps for maps
base_map <- list(geom_tile(),
                 scale_fill_binned(limits = c(-50, 50), n.breaks = 8,
                                   type = scale_fill_custom, oob = oob_squish,
                                   name = "% change in fish biomass"),
                 coord_cartesian(),
                 #Adding world
                 geom_sf(inherit.aes = F, data = world, lwd = 0.25,
                         color = "black", show.legend = F),
                 theme_bw(),
                 theme(axis.title = element_blank(), 
                       panel.border = element_rect(colour = NA),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = "none", 
                       title = element_text(size = 14, face = "bold"),
                       axis.text = element_text(size = 12)))


# Define colors for each fill category for the global summary maps
fill_colors <- c(
  "Decrease >30%" = "#AC390E",
  "Decrease 20 to 30%" = "#C4603E",
  "Decrease 10 to 20%" = "darksalmon",
  "Decrease <10%" = "wheat",
  "Increase <10%" = "honeydew3",
  "Increase 10 to 20%" = "lightblue2",
  "Increase 20 to 30%" = "#4297D3",
  "Increase >30%" = "#1B194B"
)

# order categories for global summary maps so the legend shows in the correct order
all_categories <- c("Decrease >30%", "Increase <10%",  "Decrease 20 to 30%", "Increase 10 to 20%", "Decrease 10 to 20%", "Increase 20 to 30%", "Decrease <10%",  "Increase >30%")
table_stats_admin_shp$fill_category <- factor(table_stats_admin_shp$fill_category, c(all_categories))

#Function to improve map ratios for plotting
scaler <- function(x, type, ratio = F){
  if((x > 0 & type == "min") | (x < 0 & type == "min")){
    x <- ifelse(ratio == T, x-3, x-6)
  }else if((x < 0 & type == "max") | (x > 0 & type == "max")){
    x <- ifelse(ratio == T, x+2, x+5)
  }else if(x == 0 & type == "min"){
    x <- ifelse(ratio == T, x-1, x-2)
  }else{
    x <- ifelse(ratio == T, x+1, x+2)
  }
  return(x)
}

# Defining user interface -------------------------------------------------
ui <- navbarPage(title = "Interactive Tool",
                 fluid = T,
                 tabPanel(title = "About",
                          img(src = "FishMIP_logo.png", height = 150,
                              width = 450, style = "display: block;
                          margin-left: auto; margin-right: auto;"),
                          box(title = "About this website", status = "primary",
                              width = 18,
                              fluidRow(column(width = 11,
                                              "This tool shows estimates of fish biomass change\
                              under two different climate scenarios: low
                              emissions (SSP1-2.6) and high emissions\
                              (SSP5-8.5). Results shown are the mean percentage\
                              across 10 ecosystem models making up the FishMIP\
                              ensemble.",
                                              br(),
                                              "This tool was developed by FishMIP and supports \
                              the 'xxxxx' report for the FAO and published in \
                              July 2024 and it can be accessed ",
                                              tags$a(href="https://fishmip.org/publications.html",
                                                     "here."),
                                              br(),
                                              br(),
                                              strong("Who is FishMIP?"),
                                              br(),
                                              "The Fisheries and Marine Ecosystem Model \
                              Intercomparison Project (FishMIP) is a network of \
                              more than 100 marine ecosystem modellers and \
                              researchers from around the world. Our goal is to \
                              bring together diverse marine ecosystem models to \
                              help better understand and project the long-term \
                              impacts of climate change on fisheries and marine \
                              ecosystems, and to use our findings to help inform\
                              policy.",
                                              br(),
                                              "You can find more information about FishMIP in\
                              our ",
                                              tags$a(href="https://fishmip.org/",
                                                     "website."),
                                              br(),
                                              br(),
                                              strong("Who is this tool directed to?"),
                                              br(),
                                              "Our target audience is xxxx.",
                                              br(),
                                              br(),
                                              strong("How should I use this tool?"),
                                              br(),
                                              "Instructions on how to use this website.",
                                              br(),
                                              br(),
                                              strong("How should I cite data from this site?"),
                                              br(),
                                              "If you use data downloaded from this site, we \
                              suggest the following citation:",
                                              br(),
                                              "FishMIP (2024). FAO Report citation.",
                                              br(),
                                              br(),
                                              strong("Acknowledgments"),
                                              br(),
                                              "The development of this tool was supported by \
                              the Australian Government through the Australian \
                              Research Council (ARC) Centre of Excellence for \
                              XXXXX (project XXXXX).",
                                              br(),
                                              br(),
                                              card(img(src = "IMAS_logo.png", height = 150,
                                                       width = 300, style = "display: block;
                                       margin-left: auto; margin-right:auto"))
                              )))
                 ),
                 tabPanel(title = "Global Overview",
                          img(src = "FishMIP_logo.png", height = 150,
                              width = 450, style = "display: block;
                              margin-left: auto; margin-right: auto;"),
                          "Map showing fish biomass change under four different \
                          scenarios will show here. Please allow up to two \
                          minutes for the maps to appear on your screen.",
                          br(),
                          br(),
                          "This tab will show circle plots from Gage and a map\
                          of the world. Aiming at having an interactive map \
                          here. We will also have a download button.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "world_scenario",
                                          label = "Choose emissions scenario",
                                          choices = c("SSP1-2.6 (low emissions)",
                                                      "SSP5-8.5 (high emissions)"
                                          )
                              ),
                              selectInput(inputId = "world_decade",
                                          label = "Choose decade of projected \
                                          change",
                                          choices = c("2041-2050 (medium term)",
                                                      "2091-2100 (long term)"
                                                      )
                                          ),
                              p("Click the 'Download' button below to get the \
                                data used to create this map."),
                              #Download button
                              downloadButton(outputId = "download_world",
                                             label = "Download"
                              )
                            ),
                            mainPanel(

                            #  fluidRow(plotlyOutput(outputId = "plot_world")) # change to plotlyOutput if we want plotly interactive plots instead
                              girafeOutput("plot_world"), # using girafe for the interactive maps
                              br(),
                              br(), 
                              br(),
                              dataTableOutput(outputId = "world_df")
                            )
                          )),
                 tabPanel(title = "Maps",
                          img(src = "FishMIP_logo.png", height = 150,
                              width = 450, style = "display: block;
                              margin-left: auto; margin-right: auto;"),
                          titlePanel("Maps of projected fish biomass change"),
                          br(),
                          "Select your area of interest to see how fish biomass\
                          is projected to change for the decades between 2041 \
                          and 2050, and between 2091 and 2100 under two \
                          emissions scenarios: SSP1-2.6 and SSP5-8.5.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("sectors_maps",
                                           "Choose group you would like to \
                                           visualise",
                                           choiceNames =
                                             c("Exclusive Economic Zone (EEZ) \
                                               by continent",
                                               "FAO regions (Outside EEZs)",
                                               "Large Marine Ecosystems (LMEs)"
                                             ),
                                           choiceValues = c("cont", "FAO", 
                                                            "LME"),
                                           selected = NULL
                              ),
                              selectInput(inputId = "region_maps",
                                          label = "Choose your area of interest",
                                          choices = NULL
                              )
                            ),
                            mainPanel(
                              fluidRow(
                                column(6, plotOutput(outputId = "plot_maps1")),
                                column(6, plotOutput(outputId = "plot_maps2"))
                              ),
                              fluidRow(
                                column(6, plotOutput(outputId = "plot_maps3")),
                                column(6, plotOutput(outputId = "plot_maps4"))
                              ),
                              fluidRow(plotOutput(outputId = "legend", 
                                                  height = "90px"))
                            )
                          )
                 ),
                 tabPanel(title = "Time Series",
                          img(src = "FishMIP_logo.png", height = 150,
                              width = 450, style = "display: block;
                              margin-left: auto; margin-right: auto;"),
                          titlePanel("Time series of fish biomass change"),
                          br(),
                          "Select the area of your interest to see how fish \
                          biomass is estimated to change until 2100 under two \
                          emissions scenarios: SSP1-2.6 and SSP5-8.5.",
                          br(),
                          br(),
                          "The estimated change shown in the plot is the mean \
                          percentage change for FishMIP model ensemble in \
                          relation to the historical reference period (mean for \
                          the decade between 2005 and 2014). The shaded areas \
                          show the standard deviation across the 10 ecosystem \
                          models that form the FishMIP ensemble.",
                          br(),
                          br(),
                          "The horizontal grey dashed line shows no difference \
                          between a particular year and the reference decade \
                          (2005-2014). The vertical grey line shows the end of \
                          the historical period and the different emissions \
                          scenarios.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("sectors_ts",
                                           "Choose group you would like to \
                                           visualise",
                                           choiceNames =
                                             c("Exclusive Economic Zones (EEZs)",
                                               "FAO regions (Outside EEZs)",
                                               "Large Marine Ecosystems (LMEs)"
                                             ),
                                           choiceValues = c("EEZ", "FAO",
                                                            "LME"),
                                           selected = NULL
                              ),
                              selectInput(inputId = "region_ts",
                                          label = "Choose your area of interest",
                                          choices = NULL
                              ),
                              p("Click the 'Download' button below to get the \
                                data used to create this time series plot."),
                              #Download button
                              downloadButton(outputId = "download_ts",
                                             label = "Download"
                              )
                            ),
                            mainPanel(
                              plotlyOutput(outputId = "plot_ts"),
                              br(),
                              br(), 
                              br(),
                              dataTableOutput(outputId = "table_ts")
                            ))
                 ))


server <- function(input, output, session) {
  
  ########## Global overview tab

  output$download_world <- downloadHandler(
    filename = function() {

        "summary_stats_country.csv"
    },
    content = function(file) {
        write_csv(table_stats_admin, file)
    }
  )
  
  world_map_data <- reactive({
        if(str_detect(input$world_scenario, "low") & 
              str_detect(input$world_decade, "medium")) {
      
      data <- table_stats_admin_shp |> 
        filter(scenario == "ssp126",
               decade == "2041-2050") %>%
        dplyr::select(-spatial_scale, -min, -max, -wilcox_p) %>% 
        mutate(country_map = 1) %>%
        mutate(mean_tooltip = ifelse(mean > 0, glue("+{round(mean, 2)}%"), glue("{round(mean, 2)}%"))) %>%
        mutate(tooltip = glue("Country name: {Country_FAO}\n Biomass change = {mean_tooltip}\n Model agreement = {agreement}%"))            
        
        
    } else if(str_detect(input$world_scenario, "low") & 
             str_detect(input$world_decade, "long")){
      
      data <- table_stats_admin_shp |> 
        filter(scenario == "ssp126",
               decade == "2091-2100") %>%
        dplyr::select(-spatial_scale, -min, -max, -wilcox_p) %>% 
        mutate(country_map = 1) %>%
        mutate(mean_tooltip = ifelse(mean > 0, glue("+{round(mean, 2)}%"), glue("{round(mean, 2)}%"))) %>%
        mutate(tooltip = glue("Country name: {Country_FAO}\n Biomass change = {mean_tooltip}\n Model agreement = {agreement}%"))    

    } else if(str_detect(input$world_scenario, "high") & 
              str_detect(input$world_decade, "medium")){
      
      data <- table_stats_admin_shp |> 
        filter(scenario == "ssp585",
               decade == "2041-2050") %>%
        dplyr::select(-spatial_scale, -min, -max, -wilcox_p) %>% 
        mutate(country_map = 1) %>%
        mutate(mean_tooltip = ifelse(mean > 0, glue("+{round(mean, 2)}%"), glue("{round(mean, 2)}%"))) %>%
        mutate(tooltip = glue("Country name: {Country_FAO}\n Biomass change = {mean_tooltip}\n Model agreement = {agreement}%"))
      
      
    } else if(str_detect(input$world_scenario, "high") & 
              str_detect(input$world_decade, "long")){
      
      data <- table_stats_admin_shp |> 
        filter(scenario == "ssp585",
               decade == "2091-2100") %>%
        dplyr::select(-spatial_scale, -min, -max, -wilcox_p) %>% 
        mutate(country_map = 1) %>%
        mutate(mean_tooltip = ifelse(mean > 0, glue("+{round(mean, 2)}%"), glue("{round(mean, 2)}%"))) %>%
        mutate(tooltip = glue("Country name: {Country_FAO}\n Biomass change = {mean_tooltip}\n Model agreement = {agreement}%"))
      
    }

  })
  
  
  output$plot_world <-
    renderGirafe({

    # if(any(str_detect(colnames(world_map_data()), "values"))){
    #   
    #   # p1 <- ggplot() +
    #   #   geom_sf_interactive(data = world_map_data(),
    #   #                       aes(fill = fill_category, tooltip = tooltip, data_id = values), show.legend = TRUE) +
    #   #   scale_fill_manual(values = fill_colors, breaks = all_categories, labels = all_categories, drop = F) +  # Assign fill colors manually
    #   #   theme_bw() +
    #   #   theme(
    #   #     panel.border = element_rect(colour = NA),
    #   #     plot.title = element_text(hjust = 0.5),
    #   #     legend.position = "bottom",
    #   #     title = element_text(size = 11, face = "bold"),
    #   #     axis.title = element_blank(),
    #   #     legend.key.height = unit(2, "mm"),
    #   #     legend.key.width = unit(2, "mm")
    #   #   )  +
    #   #   labs(fill = "% change in fish biomass") +
    #   #   guides(fill = guide_legend(
    #   #     title.position = "top",
    #   #     title.hjust = 0.5,
    #   #     title.vjust = 1,
    #   #     nrow = 2,
    #   #     label.position = "bottom",
    #   #     label.hjust = 0.5,
    #   #     label.vjust = 1))
    #   # 
    #   # return(girafe(code = print(p1)) %>%
    #   #          girafe_options(opts_zoom(max = 5),
    #   #                         opts_toolbar(hidden = c("zoom_rect"))))
    # 
    # 
    # }else 
      if(any(str_detect(colnames(world_map_data()), "country_map"))){

      p1 <- ggplot() +
        geom_sf_interactive(data = world_map_data(),
                                 aes(fill = fill_category, tooltip = tooltip, data_id = mean), show.legend = TRUE) +
        scale_fill_manual(values = fill_colors, breaks = all_categories, labels = all_categories, drop = F) +  # Assign fill colors manually
        theme_bw() +
        theme(
              panel.border = element_rect(colour = NA),
              plot.title = element_text(hjust = 0.5),
              legend.position = "bottom",
              title = element_text(size = 11, face = "bold"),
              axis.title = element_blank(),
              legend.key.height = unit(2, "mm"),
              legend.key.width = unit(2, "mm")
              )  +
        labs(fill = "% change in fish biomass") +
        guides(fill = guide_legend(
          title.position = "top",
          title.hjust = 0.5,
          title.vjust = 1,
          nrow = 2,
          label.position = "bottom",
          label.hjust = 0.5,
          label.vjust = 1))
        
          return(girafe(code = print(p1)) %>%
                   girafe_options(opts_zoom(max = 5),
                                  opts_toolbar(hidden = c("zoom_rect"))))

    }
      
  })
  
  output$world_df <- renderDataTable(world_map_data() %>% st_drop_geometry() %>% 
                                       dplyr::select(-tooltip, -fill_category, -country_map, -mean_tooltip, -Country_FAO_new) %>%
                                       mutate(mean = round(mean, 2), 
                                              median = round(median, 2), 
                                              sd = round(sd, 2)) %>%                
                                       rename(`mean_biomass_change_%` = mean,
                                              `median_biomass_change_%` = median,
                                              `sd_biomass_change_%` = sd,
                                               number_of_models = n_model,
                                               `model_agreement_%` = agreement)) # clean up the datatable a bit
  
  
  ########## Maps tab
  region_list_maps <- reactive({
    if(input$sectors_maps == "FAO"){
      data <- fao_list
    }else if(input$sectors_maps == "cont"){
      data <- continent_list
    }else if(input$sectors_maps == "LME"){
      data <- lme_list
    }
  })
  
  observeEvent(region_list_maps(), {
    choices <- region_list_maps()$name
    updateSelectInput(inputId = "region_maps",
                      choices = choices)})
  
  maps_df <- reactive({
    if(input$sectors_maps != "LME"){
      df <- maps_data |>
        filter(name == input$region_maps)
    }else{
      df <- maps_data |>
        filter(name_merge == input$region_maps)
    }
    
    
    #Adjusting map proportions
    minx <- min(df$x)
    maxx <- max(df$x)
    miny <- min(df$y)
    maxy <- max(df$y)
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))
    if(rangex == 0 & str_detect(input$region_maps, 
                                "Arctic|Americas|Europe|Antarct", 
                                negate = T)){
      df <- df |>
        mutate(x = x%%360)
      minx <- min(df$x)
      maxx <- max(df$x)
      rangex <- abs(abs(maxx)-abs(minx))
      base_map[[4]] <- geom_sf(inherit.aes = F, data = world_360, lwd = 0.25,
                               color = "black", show.legend = F)
    }else{
      base_map[[4]] <- geom_sf(inherit.aes = F, data = world, lwd = 0.25,
                               color = "black", show.legend = F)
    }
    
    if(rangex >= 1.15*rangey){
      ylims <- c(scaler(miny, "min"),
                 scaler(maxy, "max"))
      xlims <- c(scaler(minx, "min", ratio = T),
                 scaler(maxx, "max", ratio = T))
    }else if(rangey >= 1.15*rangex){
      xlims <- c(scaler(minx, "min"),
                 scaler(maxx, "max"))
      ylims <- c(scaler(miny, "min", ratio = T),
                 scaler(maxy, "max", ratio = T))
    }else{
      xlims <- c(scaler(minx, "min"),
                 scaler(maxx, "max"))
      ylims <- c(scaler(miny, "min"),
                 scaler(maxy, "max"))
    }
    return(list(df = df, 
                xlims = xlims, 
                ylims = ylims,
                base_map = base_map))
  })
  
  output$plot_maps1 <- renderPlot({
    p1 <- ggplot(maps_df()$df, aes(x = x, y = y,
                                   fill = rel_change_mean50_ssp126_mean))+
      maps_df()$base_map+
      labs(title = "SSP1-2.6: 2041-2050")+
      coord_sf(maps_df()$xlims, maps_df()$ylims)
    p1
  })
  
  output$plot_maps2 <- renderPlot({
    p2 <- ggplot(maps_df()$df, aes(x = x, y = y,
                                   fill = rel_change_mean50_ssp585_mean))+
      maps_df()$base_map+
      labs(title = "SSP5-8.5: 2041-2050")+
      coord_sf(maps_df()$xlims, maps_df()$ylims)
    p2
  })
  
  output$plot_maps3 <- renderPlot({
    p3 <- ggplot(maps_df()$df, aes(x = x, y = y,
                                   fill = rel_change_mean00_ssp126_mean))+
      maps_df()$base_map+
      labs(title = "SSP1-2.6: 2091-2100")+
      coord_sf(maps_df()$xlims, maps_df()$ylims)
    p3
  })
  
  output$plot_maps4 <- renderPlot({
    p4 <- ggplot(maps_df()$df, aes(x = x, y = y,
                                   fill = rel_change_mean00_ssp585_mean))+
      maps_df()$base_map+
      labs(title = "SSP5-8.5: 2091-2100")+
      coord_sf(maps_df()$xlims, maps_df()$ylims)
    p4
  })
  
  output$legend <- renderPlot({
    p1 <- ggplot(maps_df()$df, aes(x = x, y = y,
                                   fill = rel_change_mean50_ssp126_mean))+
      maps_df()$base_map+
      guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                   barwidth = 40, barheight = 2.5, 
                                   ticks.linewidth = 1, frame.linewidth = 0.5,
                                   ticks.colour = "#444444", 
                                   frame.colour = "#444444",
                                   title.theme = element_text(face = "plain",
                                                              size = 14), 
                                   label.theme = element_text(size = 14)))+
      theme(legend.position = "bottom")
    
    #Get legend for fish biomass
    leg_fish <- get_legend(p1)
    
    p1 <- ggdraw(plot_grid(leg_fish, ncol = 1))
    
    p1
  })
  
  ########## Time series tab
  region_list <- reactive({
    if(input$sectors_ts == "EEZ"){
      data <- country_list
    }else if(input$sectors_ts == "FAO"){
      data <- fao_list
    }else if(input$sectors_ts == "LME"){
      data <- lme_list
    }
  })
  
  observeEvent(region_list(), {
    choices <- region_list()$name
    updateSelectInput(inputId = "region_ts",
                      choices = choices)})
  
  down_name <- reactive({
    region_name <- input$region_ts |> 
      #changing to lower case
      str_to_lower() |> 
      #removing accents
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') |> 
      #Removing parentheses
      str_remove_all("\\(|\\)") |> 
      #Replaces spaces " " with dashes "-"
      str_replace_all(" ", "-") |> 
      #Remove apostrophes in names
      str_replace("'", "")
    region_name <- str_c("mean_ensemble_perc_change_fish_bio_", 
                         region_name, "_1950-2100.csv")
    return(region_name)
  })
  
  
  ts_df <- reactive({
    if(input$sectors_ts == "EEZ"){
      df <- count_bio |> 
        filter(figure_name == input$region_ts)
    }else if(input$sectors_ts == "FAO"){
      df <- fao_bio |> 
        filter(NAME_EN == input$region_ts)
    }else if(input$sectors_ts == "LME"){
      df <- lme_bio |> 
        filter(name_merge == input$region_ts)
    }
    return(df)
  })
  
  output$plot_ts <- renderPlotly({
    ggplotly({
      p <- ggplot(ts_df(), aes(x = year, y = mean_change, colour = scenario))+
        geom_line(linewidth = 0.5)+
        #Adding no change line at 0 for reference 
        geom_hline(yintercept = 0, color = "grey80", linewidth = 0.65, 
                   linetype = 2)+
        #Adding line dividing historical period and future projections
        geom_vline(xintercept = 2015, color = "grey80", linewidth = 0.65)+
        #Adding SD as shading 
        geom_ribbon(aes(ymin = mean_change-sd_change,
                        ymax = mean_change+sd_change, fill = scenario),
                    alpha = 0.3, color = NA)+
        #Manually setting colours to be used in plots
        scale_color_manual(values = c("historical" = "black", 
                                      "ssp126" = "#33bbee", 
                                      "ssp585" = "#ee3377"), 
                           name = "Scenarios",
                           labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
        scale_fill_manual(values = c("historical" = "black", 
                                     "ssp126" = "#33bbee", 
                                     "ssp585" = "#ee3377"), 
                          name = "Scenarios",
                          labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
        guides(color = guide_legend(nrow = 1, title.position = "left"))+
        theme_classic()+
        scale_x_continuous(breaks = seq(1950, 2100, 10))+
        labs(y = "Change in exploitable fish biomass (%)")+
        theme(legend.position = "top", legend.justification = "center", 
              legend.text = element_text(size = 14), 
              legend.title = element_text(size = 14),
              panel.grid.minor.y = element_blank(), 
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 13),
              axis.text.x = element_text(angle = 45, vjust = 0.765, 
                                         hjust = 0.65, size = 12),
              axis.text.y = element_text(size = 12))
    })
    p
  })
  
  output$table_ts <- renderDataTable(ts_df())
  
  output$download_ts <- downloadHandler(
    filename = function(){
      down_name()
    },
    #Creating name of download file based on original file name
    content = function(file){
      write_csv(ts_df(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
#Code developed for shiny app supporting FAO report
#Author: Denisse Fierro Arcos

# Loading R libraries -----------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(bslib)
library(cmocean)
library(scales)
library(sf)
library(rnaturalearth)
library(ggiraph)

# Loading data ------------------------------------------------------------
base_folder <- "/rd/gem/private/users/camillan/FAO_Report/"

#Loading ensemble biomass change
maps_data <- read_csv("../data/ensemble_perc_bio_change_data_map.csv", 
                      col_select = c(x, y, starts_with("rel_change"), 
                                     NAME_EN, name_merge, figure_name))

#Ensemble percentage change in biomass by countries
count_bio <- list.files(base_folder, "ensemble_perc_bio_change_country.csv",
                        recursive = T, full.names = T) |> 
  read_csv() |> 
  rename(name = figure_name)


#Ensemble percentage change in biomass by FAO regions
fao_bio <- list.files(base_folder, "ensemble_perc_bio_change_fao_region.csv",
                      recursive = T, full.names = T) |> 
  read_csv() |> 
  rename(name = NAME_EN)

#Ensemble percentage change in biomass by FAO regions
lme_bio <- list.files(base_folder, "ensemble_perc_bio_change_lme.csv",
                      recursive = T, full.names = T) |> 
  read_csv() |> 
  rename(name = name_merge)

#Table of summary statistics
table_stats_admin <- read_csv("../data/table_stats_country_admin.csv") 

#List of countries
country_list <- maps_data |> 
  distinct(figure_name) |> 
  drop_na() |> 
  rename("name"= "figure_name") |> 
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

#Loading choropleth shapefile
choro_data <- read_csv("../data/global_choro_maps_data.csv")

#Map of the world
world <- ne_countries(returnclass = "sf", scale = "medium")
world_360 <- read_sf("../data/world_360deg.shp")

#Biomass change - World map
levs <- c("Decrease >30%", "Increase <10%",  "Decrease 20 to 30%", 
          "Increase 10 to 20%", "Decrease 10 to 20%", "Increase 20 to 30%", 
          "Decrease <10%",  "Increase >30%", "No data")

#Ensure category column is a factor and ordered
table_stats_admin_shp <- read_sf("../data/biomass_shapefile_projected.shp") |> 
  mutate(category = factor(category, levels = levs, ordered = T))

# Define colors for each fill category for the global summary maps
fill_colors <- c(
  "Decrease >30%" = "#AC390E",
  "Decrease 20 to 30%" = "#C4603E",
  "Decrease 10 to 20%" = "darksalmon",
  "Decrease <10%" = "wheat",
  "Increase <10%" = "honeydew3",
  "Increase 10 to 20%" = "lightblue2",
  "Increase 20 to 30%" = "#4297D3",
  "Increase >30%" = "#1B194B",
  "No data" = "#f7f7f7"
)

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
                 guides(fill = guide_colorbar(title.position = "top", 
                                              title.hjust = 0.5, barwidth = 30, 
                                              barheight = 2, 
                                              ticks.linewidth = 1, 
                                              frame.linewidth = 0.5,
                                              ticks.colour = "#444444",
                                              frame.colour = "#444444",
                                              title.theme = element_text(
                                                face = "plain",size = 14),
                                              label.theme = element_text(
                                                size = 14))),
                 theme(axis.title = element_blank(), 
                       panel.border = element_rect(colour = NA),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = "bottom", 
                       title = element_text(size = 14, face = "bold"),
                       axis.text = element_text(size = 12)))

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
                              "You can download the data used to create the\
                              plots shown in this interactive tool using the\
                              'Donwload' button included under each tab. As a\
                              condition of using these data, you must cite its\
                              use. Please use the following citation:",
                              br(),
                              "FishMIP (2024). FAO Report\
                                              citation.",
                              br(),
                              "When using the data product in a publication,\
                              please include the following citation(s) in\
                              addition to the data product citation provided\
                              above:",
                              br(),
                              "FishMIP (2024). Some other citation.",
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
                          "Here we present the mean estimated changes in fish \
                          biomass across the entire FishMIP ensemble \
                          (including 10 ecosystem models) in relation to our \
                          reference period (mean between 2005-2014) within the\
                          boundaries of a country's exclusive economic zone\
                          (EEZ).",
                          br(),
                          "Choose the scenario and decade of your interest for\
                          the interactive map to appear on your screen. Allow\
                          up to a minute for the map to show on your screen.",
                          br(),
                          "More info about interactive plot.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "world_scenario",
                                           label = "Choose emissions scenario",
                                           choiceNames =
                                             c("SSP1-2.6 (low emissions)",
                                               "SSP5-8.5 (high emissions)"),
                                           choiceValues = c("ssp126", "ssp585"),
                                           selected = NULL
                              ),
                              radioButtons(inputId = "world_decade",
                                           "Choose decade of projected change",
                                           choiceNames = 
                                             c("2041-2050 (medium term)",
                                               "2091-2100 (long term)"),
                                           choiceValues = c("2041-2050",
                                                            "2091-2100"),
                                           selected = NULL
                              ),
                              p("Click the 'Download' button below to get the \
                                data used to create this map."),
                              #Download button
                              downloadButton(outputId = "download_world",
                                             label = "Download"
                              )
                          ),
                          mainPanel(
                            fluidRow(
                              girafeOutput(outputId = "plot_world")
                                     )
                          )
                          )),
                 tabPanel(title = "Maps",
                          img(src = "FishMIP_logo.png", height = 150,
                              width = 450, style = "display: block;
                              margin-left: auto; margin-right: auto;"),
                          titlePanel("Maps of projected fish biomass change"),
                          br(),
                          "Here we present the mean estimated changes in fish \
                          biomass across the entire FishMIP ensemble \
                          (including 10 ecosystem models) in relation to our \
                          reference period (mean between 2005-2014).",
                          br(),
                          br(),
                          "To see changes in the area of your interest, click \
                          on the group you want to visualise and select the \
                          area of your choice from the drop down list. You can \
                          also choose the emissions scenario and decade of \
                          decade of your interest.",
                          br(),
                          br(),
                          "The dotted red line in the map shows the boundaries\
                          of territorial seas (i.e., 200 NM from the coast, \
                          also referred to as exclusive economic zone).",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("sectors_maps",
                                           "Choose group you would like to \
                                           visualise",
                                           choiceNames =
                                             c("Exclusive Economic Zones (EEZs)",
                                               "FAO Major Fishing Areas",
                                               "Large Marine Ecosystems (LMEs)"
                                             ),
                                           choiceValues = c("EEZ", "FAO", 
                                                            "LME"),
                                           selected = NULL
                              ),
                              selectInput(inputId = "region_maps",
                                          label = "Choose your area of interest",
                                          choices = NULL
                              ),
                              radioButtons(inputId = "region_scenario",
                                           label = "Choose emissions scenario",
                                           choiceNames = 
                                             c("SSP1-2.6 (low emissions)",
                                               "SSP5-8.5 (high emissions)"),
                                           choiceValues = c("ssp126", "ssp585"),
                                           selected = NULL
                              ),
                              radioButtons(inputId = "region_decade",
                                           "Choose decade of projected change",
                                           choiceNames = 
                                             c("2041-2050 (medium term)",
                                               "2091-2100 (long term)"),
                                           choiceValues = c("mean50", "mean00"),
                                           selected = NULL
                              ),
                              p("Click the 'Download' button below to get the \
                                data used to create the map shown on the \
                                right."),
                              #Download button
                              downloadButton(outputId = "download_map",
                                             label = "Download"
                              )
                            ),
                            mainPanel(
                              fluidRow(plotOutput(outputId = "plot_maps1"))
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
                                               "FAO Major Fishing Areas",
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
                                             label = "Download")
                            ),
                            mainPanel(
                              plotOutput(outputId = "plot_ts")
                          ))
                 ))


server <- function(input, output, session) {
  
  ########## Global overview tab ----
  output$download_world <- downloadHandler(
    filename = function(){
      "table_stats_country_admin.csv"
    },
    #Creating name of download file based on original file name
    content = function(file){
      write_csv(table_stats, file)
    }
  )
  
  #Select correct data 
  world_map_data <- reactive({
    data <- table_stats_admin_shp |>
      filter(scenario == input$world_scenario, decade == input$world_decade)
  })
  
  #Plot data
  output$plot_world <- renderGirafe({
      p1 <- ggplot()+
        geom_sf_interactive(data = world_map_data(), 
                            aes(fill = category, tooltip = tooltip, 
                                data_id = iso_code), show.legend = TRUE)+
        scale_fill_manual(values = fill_colors, breaks = levs, labels = levs, 
                          drop = F)+
        theme_bw()+
        theme(panel.border = element_rect(colour = NA),
              plot.title = element_text(hjust = 0.5),
              legend.position = "bottom",
              title = element_text(size = 11, face = "bold"),
              axis.title = element_blank(),
              legend.key.height = unit(2, "mm"),
              legend.key.width = unit(2, "mm"))+
        labs(fill = "% change in fish biomass")+
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                                   title.vjust = 1, nrow = 2, label.vjust = 1,
                                   label.position = "bottom", 
                                   label.hjust = 0.5))
      
      return(girafe(code = print(p1)) %>%
               girafe_options(opts_zoom(max = 5),
                              opts_toolbar(hidden = c("zoom_rect"))))
    })
  
  ########## Maps tab ----
  region_list_maps <- reactive({
    if(input$sectors_maps == "FAO"){
      data <- fao_list
    }else if(input$sectors_maps == "EEZ"){
      data <- country_list
    }else if(input$sectors_maps == "LME"){
      data <- lme_list
    }
  })
  
  observeEvent(region_list_maps(), {
    choices <- region_list_maps()$name
    updateSelectInput(inputId = "region_maps",
                      choices = choices)})
  
  maps_df <- reactive({
    if(input$sectors_maps == "LME"){
      df <- maps_data |>
        filter(name_merge == input$region_maps)
    }else if(input$sectors_maps == "FAO"){
      df <- maps_data |>
        filter(NAME_EN == input$region_maps)
    }else if(input$sectors_maps == "EEZ"){
      df <- maps_data |> 
        filter(figure_name == input$region_maps)
    }
    
    df <- df |> 
      select(x, y, contains(input$region_scenario) & 
               contains(input$region_decade)) |> 
      rename_with(~ "change", starts_with("rel_change"))
    
    #Adjusting map proportions
    minx <- min(df$x)
    maxx <- max(df$x)
    miny <- min(df$y)
    maxy <- max(df$y)
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))
    if(rangex == 0 & str_detect(input$region_maps, 
                                "Arctic|Americas|Europe|Antarct|France", 
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
      p1 <- ggplot(maps_df()$df, aes(x = x, y = y, fill = change))+
        maps_df()$base_map+
        coord_sf(maps_df()$xlims, maps_df()$ylims)
      p1
  })
  
  down_name_map <- reactive({
    region_name <- input$region_maps |> 
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
    region_name <- str_c("ensemble_perc_change_fish_bio_", 
                         input$region_scenario, "_", input$region_decade, "_",
                         region_name)
    return(region_name)
  })
  
  output$download_map <- downloadHandler(
    filename = function(){
      down_name_map()
    },
    #Creating name of download file based on original file name
    content = function(file){
      write_csv(maps_df()$df, file)
    }
  )
      
  ########## Time series tab ----
  region_list <- reactive({
    if(input$sectors_ts == "EEZ"){
      data <- country_list
      df <- count_bio
    }else if(input$sectors_ts == "FAO"){
      data <- fao_list
      df <- fao_bio
    }else if(input$sectors_ts == "LME"){
      data <- lme_list
      df <- lme_bio
    }
    return(list(df = df,
                df_list = data))
  })
   
  observeEvent(region_list(), {
    choices <- region_list()$df_list$name
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
    region_name <- str_c("mean_ensemble_perc_change_fish_bio_timeseries_", 
                         region_name, "_1950-2100.csv")
    return(region_name)
  })

  ts_df <- reactive({
    df <- region_list()$df |> 
      filter(name == input$region_ts)
    return(df)
    })
  
  output$plot_ts <- renderPlot({
      p <- ggplot(data = ts_df(), aes(x = year, y = mean_change, colour = scenario))+
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
    p
  })
  
  # output$plot_ts <- renderPlotly({
  #   ggplotly({
  #     p <- ggplot(data = ts_df(), aes(x = year, y = mean_change, colour = scenario))+
  #       geom_line(linewidth = 0.5)+
      # #Adding no change line at 0 for reference 
      # geom_hline(yintercept = 0, color = "grey80", linewidth = 0.65, 
      #            linetype = 2)+
      # #Adding line dividing historical period and future projections
      # geom_vline(xintercept = 2015, color = "grey80", linewidth = 0.65)+
      # #Adding SD as shading 
      # geom_ribbon(aes(ymin = mean_change-sd_change,
      #                 ymax = mean_change+sd_change, fill = scenario),
      #             alpha = 0.3, color = NA)+
      # #Manually setting colours to be used in plots
      # scale_color_manual(values = c("historical" = "black", 
      #                               "ssp126" = "#33bbee", 
      #                               "ssp585" = "#ee3377"), 
      #                    name = "Scenarios",
      #                    labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      # scale_fill_manual(values = c("historical" = "black", 
      #                              "ssp126" = "#33bbee", 
      #                              "ssp585" = "#ee3377"), 
      #                   name = "Scenarios",
      #                   labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      # guides(color = guide_legend(nrow = 1, title.position = "left"))+
      # theme_classic()+
      # scale_x_continuous(breaks = seq(1950, 2100, 10))+
      # labs(y = "Change in exploitable fish biomass (%)")+
      # theme(legend.position = "top", legend.justification = "center", 
      #       legend.text = element_text(size = 14), 
      #       legend.title = element_text(size = 14),
      #       panel.grid.minor.y = element_blank(), 
      #       axis.title.x = element_blank(),
      #       axis.title.y = element_text(size = 13),
      #       axis.text.x = element_text(angle = 45, vjust = 0.765, 
      #                                  hjust = 0.65, size = 12),
      #       axis.text.y = element_text(size = 12))
  #     })
  #   p
  # })
  
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
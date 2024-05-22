
# Loading R libraries -----------------------------------------------------
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(cowplot)
library(cmocean)
library(scales)
library(rnaturalearth)

# Loading data ------------------------------------------------------------
#Loading ensemble biomass change
maps_data <- read_csv("../data/ensemble_perc_bio_change_data_map.csv", 
                      col_select = c(x, y, starts_with("rel_change"), 
                                     NAME_EN, figure_name)) |> 
  mutate(name = case_when(!is.na(figure_name) ~ figure_name, 
                          !is.na(NAME_EN) ~ NAME_EN, T ~ NA))

country_list <- maps_data |> 
  distinct(figure_name) |> 
  drop_na() |> 
  rename("name"= "figure_name") |> 
  arrange(name)

fao_list <- maps_data |>
  distinct(NAME_EN) |> 
  drop_na() |> 
  rename("name"= "NAME_EN") |> 
  arrange(name)

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

world <- ne_countries(returnclass = "sf")

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
                       legend.position = "none"))

scaler <- function(x, type, ratio = F){
  if((x > 0 & type == "min") | (x < 0 & type == "max")){
    x <- ifelse(ratio == T, x*.95, x*.75)
  }else if((x < 0 & type == "min") | (x > 0 & type == "max")){
    x <- ifelse(ratio == T, x*1.05, x*1.25)
  }else if(x == 0 & type == "min"){
    x <- ifelse(ratio == T, x-.05, x-.15)
  }else{
    x <- ifelse(ratio == T, x+.05, x+.15)
  }
  return(x)
}

# Defining user interface -------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "yeti"),
  #Title panel including FishMIP logo and title
  titlePanel(title = div(img(src = "FishMIP_logo.png", height = 150, 
                             width = 450, style = "display: block; 
                                                  margin-left: auto; 
                                                  margin-right: auto;"),
                 h1("(BETA version) Percentage change in fish biomass from FishMIP model ensemble",
                    style = "background-color:#f3f3f3;
                            border:1.5px solid #c9d5ea;
                            padding-left: 15px;
                            padding-bottom: 10px;
                            padding-top: 10px;
                            text-align: center;
                            font-weight: bold")),
             windowTitle = "(BETA version) FishMIP Ensemble biomass change"),
  #Side panel
  sidebarLayout(
    sidebarPanel(
      radioButtons("sectors", "Choose group you would like to visualise",
                   choiceNames = c("Exclusive Economic Zones (EEZs)",
                                   "High seas per FAO regions"),
                   choiceValues = c("EEZ", "FAO"),
                   selected = NULL),
      selectInput(inputId = "region", label = "Choose your area of interest",
                  choices = NULL)),
    mainPanel(
      plotOutput(outputId = "plot1"), br(),
      plotOutput(outputId = "plot2"), br(),
      em("Lines show mean percentage change for model ensemble and shaded areas show model ensemble standard deviation in relation to historical reference period (mean 2005-2014)"),
      br(), br(), br(),
      dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output) {
  region_list <- reactive({
    if(input$sectors == "EEZ"){
      data <- country_list
      }else if(input$sectors == "FAO"){
        data <- fao_list
        }
  })
   
  observeEvent(region_list(), {
    choices <- region_list()$name
    updateSelectInput(inputId = "region",
                      choices = choices)})
  
    maps_df <- reactive({
      df <- maps_data |> 
        filter(name == input$region)
      return(df)
      })
    
    ts_df <- reactive({
      if(input$sectors == "EEZ"){
        df <- count_bio |> 
          filter(figure_name == input$region)
      }else if(input$sectors == "FAO"){
        df <- fao_bio |> 
          filter(NAME_EN == input$region)
      }
      return(df)
    })
  
  output$plot1 <- renderPlot({
    minx <- min(maps_df()$x)
    maxx <- max(maps_df()$x)
    miny <- min(maps_df()$y)
    maxy <- max(maps_df()$y)
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))
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
    
    p1 <- ggplot(maps_df(), aes(x = x, y = y, 
                                fill = rel_change_mean50_ssp126_mean))+
      base_map+
      labs(title = "SSP1-2.6: 2041-2050")+
      guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, 
                                   barwidth = 15))+
      theme(legend.position = "bottom")
    #Get legend for fish biomass
    leg_fish <- get_legend(p1)
    p1 <- p1+
      theme(legend.position = "none")+
      coord_sf(xlims, ylims)
    p2 <- ggplot(maps_df(), aes(x = x, y = y, 
                                fill = rel_change_mean50_ssp585_mean))+
      base_map+
      labs(title = "SSP5-8.5: 2041-2050")+
      coord_sf(xlims, ylims)
    p3 <- ggplot(maps_df(), aes(x = x, y = y, 
                                fill = rel_change_mean00_ssp126_mean))+
      base_map+
      labs(title = "SSP1-2.6: 2091-2100")+
      coord_sf(xlims, ylims)
    p4 <- ggplot(maps_df(), aes(x = x, y = y, 
                                fill = rel_change_mean00_ssp585_mean))+
      base_map+
      labs(title = "SSP5-8.5: 2091-2100")+
      coord_sf(xlims, ylims)
    
    #Plotting everything together
    p <- plot_grid(plot_grid(p1, p2, ncol = 2, nrow = 1, 
                             labels = c("a", "b"), label_x = 0.1),
                   plot_grid(p3, p4, ncol = 2, nrow = 1, 
                             labels = c("c", "d"), label_x = 0.1),
                   leg_fish, ncol = 1, nrow = 3, rel_heights = c(1, 1, 0.4))
    p
  })
  
  output$plot2 <- renderPlot({
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
      labs(y = "Change in exploitable\nfish biomass (%)")+
      theme(legend.position = "top", legend.justification = "center", 
            legend.text = element_text(size = 12),
            panel.grid.minor.y = element_blank(), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 0.765, 
                                       hjust = 0.65))
    p
  })
}

shinyApp(ui = ui, server = server)
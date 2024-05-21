
# Loading R libraries -----------------------------------------------------
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(cowplot)
library(cmocean)
library(scales)

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
base_gg <- list(geom_tile(),
                scale_fill_binned(limits = c(-50, 50), n.breaks = 8,
                                  type = scale_fill_custom, oob = oob_squish,
                                  name = "% change in fish biomass"),
                #Adding world
                # geom_sf(inherit.aes = F, data = world_bound_proj, lwd = 0.25, 
                #         color = "black", show.legend = F),
                theme_bw(),
                theme(axis.title = element_blank(), 
                      panel.border = element_rect(colour = NA),
                      plot.title = element_text(hjust = 0.5),
                      legend.position = "none"))

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
  
    decades_df <- reactive({
      df <- maps_data |> 
        filter(name == input$region)
      return(df)
  })
  
  output$plot1 <- renderPlot({
    xlims <- c(min(decades_df()$x), max(decades_df()$x))
    ylims <- c(min(decades_df()$y), max(decades_df()$y))
    
    p1 <- ggplot(decades_df(), aes(x = x, y = y, 
                                   fill = rel_change_mean50_ssp126_mean))+
      base_gg+
      labs(title = "SSP1-2.6: 2041-2050")+
      guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, 
                                   barwidth = 15))+
      theme(legend.position = "bottom")
    #Get legend for fish biomass
    leg_fish <- get_legend(p1)
    p1 <- p1+
      theme(legend.position = "none")
    p2 <- ggplot(decades_df(), aes(x = x, y = y, 
                                   fill = rel_change_mean50_ssp585_mean))+
      base_gg+
      labs(title = "SSP5-8.5: 2041-2050")
    p3 <- ggplot(decades_df(), aes(x = x, y = y, 
                                   fill = rel_change_mean00_ssp126_mean))+
      base_gg+
      labs(title = "SSP1-2.6: 2091-2100")
    p4 <- ggplot(decades_df(), aes(x = x, y = y, 
                                   fill = rel_change_mean00_ssp585_mean))+
      base_gg+
      labs(title = "SSP5-8.5: 2091-2100")
    
    #Plotting everything together
    p <- plot_grid(plot_grid(p1, p2, ncol = 2, nrow = 1, 
                             labels = c("a", "b"), label_x = 0.1),
                   plot_grid(p3, p4, ncol = 2, nrow = 1, 
                             labels = c("c", "d"), label_x = 0.1),
                   leg_fish, ncol = 1, nrow = 3, rel_heights = c(1, 1, 0.4))
      #   geom_line(alpha = 0.5)+
      #   geom_ribbon(aes(ymin = mean_change-sd_change,
      #                   ymax = mean_change+sd_change,
      #                   fill = scenario),
      #               alpha = 0.2, color = NA)+
      #   #Manually setting colours to be used in plots
      #   scale_color_manual(values = c("historical" = "black",
      #                                 "ssp126" = "#33bbee",
      #                                 "ssp585" = "#ee3377"),
      #                      name = "Scenarios",
      #                      labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      #   scale_fill_manual(values = c("historical" = "black",
      #                                "ssp126" = "#33bbee",
      #                              "ssp585" = "#ee3377"),
      #                     name = "Scenarios",
      #                   labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      #   #Adding line dividing historical period and future projections
      #   geom_vline(xintercept = 2015, color = "#709fcc", linewidth = 0.65)+
      #   geom_hline(yintercept = 0, color = "#709fcc", linewidth = 0.65,
      #              linetype = 2)+
      # theme_bw()+
      # labs(y = "Change in exploitable\nfish biomass (%)")+
      # theme(legend.position = "top", legend.justification = "center",
      #       legend.text = element_text(size = 10),
      #       panel.grid.minor.y = element_blank(), axis.title.x = element_blank(),
      #       axis.title.y = element_text(size = 12),
      #       axis.text.x = element_text(angle = 45, vjust = 0.765, hjust = 0.65))
      p
  })
  # 
  # output$table <- renderDataTable(filtered_data())
  # 
  # #Create name for downloaded data that contains EEZ name
  # file_name <- reactive({
  #   name <- str_remove(input$region, " (e|E)xclusive (e|E)conomic Zone")
  #   glue("perc_change_fish_biomass_{name}.csv")})
  # 
}

shinyApp(ui = ui, server = server)
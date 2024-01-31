
# Loading R libraries -----------------------------------------------------
library(shiny)
library(plotly)
library(tidyverse)
library(glue)

# Loading data ------------------------------------------------------------
#Same dataset produced to create the timeseries plots in report
base_file <- "/rd/gem/private/users/camillan/FAO_Report/ensemble_perc_bio_change_data.csv"
fishmip <- read_csv(base_file) |> 
  #Transform names and scenarios to factors
  mutate(across(c(nm_mrgd, scenario), ~factor(.x)),
         #Keep up to two decimal places for % change values
         across(ends_with("change"), ~round(.x, 2)))

# Defining user interface -------------------------------------------------
ui <- fluidPage(
  #Title panel including FishMIP logo and title
  titlePanel(title = div(img(src = "FishMIP_logo.png",
                             height = 150,
                             width = 450,
                             style = "display: block; 
                                      margin-left: auto; 
                                      margin-right: auto;"),
                 h1("Estimates of fish biomass change from FishMIP model ensemble",
                    style = "background-color:#f3f3f3;
                            border:1.5px solid #c9d5ea;
                            padding-left: 15px;
                            padding-bottom: 10px;
                            padding-top: 10px;
                            text-align: center"))),
  #Side panel
  sidebarLayout(
    sidebarPanel(
      radioButtons("sectors", "Choose area you want to visualise",
                   choiceNames = c("Exclusive Economic Zones (EEZs)",
                               "High seas per FAO regions"),
                   choiceValues = c("EEZ", "FAO"),
                   selected = NULL),
      selectInput(inputId = "region", label = "Choose region to visualise",
                  choices = NULL),
      downloadButton(outputId = "download_data", 
                     label = "Download data (csv file)"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"), br(),
      em("Lines show mean percentage change for model ensemble and shaded areas show model ensemble standard deviation in relation to historical reference period (mean 2005-2014)"),
      br(), br(), br(),
      dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output) {
  subset_fishmip <- reactive({
    req(input$sectors)
    if(input$sectors == "EEZ"){
      data <- fishmip |>
        filter(is.na(OCEAN)) |>
        droplevels() |>
        select(year, scenario, mean_change, sd_change, nm_mrgd, region)
      }else if(input$sectors == "FAO"){
        data <- fishmip |>
          drop_na(OCEAN) |>
          droplevels() |>
          select(year, scenario, mean_change, sd_change, nm_mrgd, OCEAN)
        }
  })
   
  observeEvent(subset_fishmip(), {
    choices <- levels(subset_fishmip()$nm_mrgd)
    updateSelectInput(inputId = "region",
                      choices = choices)})
  
  filtered_data <- reactive({
    subset_fishmip() |>
      filter(str_detect(nm_mrgd, input$region))})

  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes(x = year, y = mean_change,
                                       colour = scenario))+
        geom_line(alpha = 0.5)+
        geom_ribbon(aes(ymin = mean_change-sd_change,
                        ymax = mean_change+sd_change,
                        fill = scenario),
                    alpha = 0.2, color = NA)+
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
        #Adding line dividing historical period and future projections
        geom_vline(xintercept = 2015, color = "#709fcc", linewidth = 0.65)+
        geom_hline(yintercept = 0, color = "#709fcc", linewidth = 0.65,
                   linetype = 2)+
      theme_bw()+
      labs(y = "Change in exploitable\nfish biomass (%)")+
      theme(legend.position = "top", legend.justification = "center",
            legend.text = element_text(size = 10),
            panel.grid.minor.y = element_blank(), axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 0.765, hjust = 0.65))
      p
    })
  })

  output$table <- renderDataTable(filtered_data())

  #Create name for downloaded data that contains EEZ name
  file_name <- reactive({
    name <- str_remove(input$EEZ, " (e|E)xclusive (e|E)conomic Zone")
    glue("perc_change_fish_biomass_{name}.csv")})

  output$download_data <- downloadHandler(
    filename = file_name,
    content = function(file) {
      data <- filtered_data()
      write_csv(data, file)
  }
  )
}

shinyApp(ui = ui, server = server)
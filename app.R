#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(DT)

fishmip <- read.csv("EEZ_tcb.csv", sep = ',')
 
#percentchange only
fishmip <- fishmip[fishmip$dataType=="per",]
#fishmip <- fishmip[fishmip$model=="BOATS",]
fishmip <- fishmip[fishmip$value !="Inf",]


#multimodel mean & sd
fishmip<-fishmip %>% group_by(year,EEZ,esm,ssp) %>%
  summarise(mean = mean(value), sd = sd(value))

  
fishmip$EEZ<-as.factor(fishmip$EEZ)
#fishmip$model<-as.factor(fishmip$model)
fishmip$esm<-as.factor(fishmip$esm)
fishmip$ssp<-as.factor(fishmip$ssp)
fishmip$mean<-fishmip$mean*100
fishmip$sd<-fishmip$sd*100

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("FishMIP Ensemble EEZ Projections"),
      selectInput(inputId = "EEZ", label = "EEZ",
                  choices = levels(fishmip$EEZ),
                  selected = "Australia"),
          # selectInput(inputId = "model", "Model",
          #         choices = levels(fishmip$model),
          #         selected = "BOATS"),
            selectInput(inputId = "esm", "CMIP6 Model",
                              choices = levels(fishmip$esm),
                               # multiple = TRUE,
                               selected = c("gfdl-esm4")),
      downloadButton(outputId = "download_data", label = "Download"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"), br(),
      em("Postive and negative percentages indicate an increase and decrease from the historical reference period (mean 1990-1999)"),
      br(), br(), br(),
      DT::dataTableOutput(outputId = "table")
    )
  )
)


server <- function(input, output) {
  filtered_data <- reactive({
    subset(fishmip,
           EEZ %in% input$EEZ  &  esm %in% input$esm )})
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="year", y="mean",colour="ssp")) +
         geom_line(alpha=0.5) +
        # geom_ribbon(aes(ymin=mean-2*sd, ymax=mean+2*sd), alpha=0.2)+
        # #theme(legend.position = "none") +
        theme_minimal() +
        ylab("Multimodel mean % change exploitable biomass")
      
      p
    })
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
library(shiny)
library(readr)
#library("xlsx")
library(ggplot2)
library(readxl)
library(xml2)
library(dplyr)
library(rvest)

#ojo 
#source('C:/Users/Freddy Tapia/Desktop/Scripts Trabajo/ruta_bcv.R')
source(paste(getwd(),"ruta_bcv.R",sep = "/"))

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Descargar Data BCV"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Elegir un Archivo:",
                  choices = c("0-22", "Caracteristicas",
                              "Absorcion","Inyeccion","Tasas de Interes",
                              "Subasta Letras","Subasta TIF","Subasta VEB")),
      
      # Button
      downloadButton("downloadData", "Descargar")
      
    )
    ,
    
    # Main panel for displaying outputs ----
    mainPanel(
      verbatimTextOutput("p1")
    #  tableOutput("table")
      
    )
    # fluidRow(
    #   DT::dataTableOutput("table")
    # )
  )#final sidebarlayout
 
  
)#final fluidpage ui

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
    #        "0-22" = "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/resumersec0718_2.xls",
    #        "Caracteristicas" = "https://www.bcv.org.ve/sites/default/files/3_1_2_20.xls",
    #        "Absorcion" = "https://www.bcv.org.ve/sites/default/files/Cuadros-Indicadores/resuabs_0.xls",
    #        "Inyeccion" = "https://www.bcv.org.ve/sites/default/files/Cuadros-Indicadores/resuiny_0.xls",
    #        "Tasas de Interes" = "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/1_3_29_33.xls",
    #        "Subasta Letras" = "https://www.bcv.org.ve/sites/default/files/Cuadros-Indicadores/result_0.xls",
    #        "Subasta TIF" = "https://www.bcv.org.ve/sites/default/files/Cuadros-Indicadores/resudpntif.xls",
    #        "Subasta VEB" = "https://www.bcv.org.ve/sites/default/files/Cuadros-Indicadores/resuvebono_0.xls")
            "0-22" = ruta_bcv("0-22"),
            "Caracteristicas" = ruta_bcv("caracteristicas"),
            "Absorcion" = ruta_bcv("abs"),
            "Inyeccion" = ruta_bcv("iny"),
            "Tasas de Interes" = ruta_bcv("tasas"),
            "Subasta Letras" = ruta_bcv("letras"),
            "Subasta TIF" = ruta_bcv("tif"),
            "Subasta VEB" = ruta_bcv("veb"))
      })
  
  output$p1 <- renderPrint({ input$dataset })
  
  #Table of selected dataset ----
  # output$table <- renderTable({
  #   
  #   nombre <- paste("C:/Users/Freddy Tapia/Desktop/Scripts Trabajo/",input$dataset,".xls",sep ="")
  #   data <- read_excel(nombre, sheet = 1)
  #   data
  #   
  # })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xls", sep = "")
    },
    content = function(file) {
      download.file(datasetInput(),file,method = "libcurl",mode="wb")
      #write.xlsx(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
}#final server

# Create Shiny app ----
shinyApp(ui, server)


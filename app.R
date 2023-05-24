#
# libraries ----
library(tidyverse)    # collection of R packages designed for data science
library(sf)           # Used for creating simple features objects
library(scales)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(tidygeocoder) # Used for geocoding
# selectIput data
selectInput_data <- readRDS(file = "www/select_item_data.rds")
icon_tbl <- read_rds("www/icon_tbl.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          br(),
          fileInput("upload", "Upload Reference geodata file"),
          hr(),
          shiny::selectInput("datasetLevel", "Select Dataset Level",
                                  c("National" = "national",
                                    "State" = "state")),
          # Only show this panel if the Agriculture is selected
          shiny::conditionalPanel(
            condition = "input.datasetLevel == 'state'",
            shiny::selectInput(inputId = "mapState",
                               label = "Select State:",
                               choices = c(Choose='', selectInput_data$state_values))
          ),
          shiny::selectInput("sector", "Select Uploaded dataset Sector",
                             c("Administrative Boundaries" = "admin",
                               "Agriculture" = "agriculture",
                               "Commerce" = "commerce",
                               "Education" = "education",
                               "Energy" = "energy",
                               "Health and Safety" = "health_safety",
                               "Population" = "population",
                               "Public Facilities" = "public-facilities",
                               "Religion" = "religion",
                               "Security" = "security",
                               "Water and Sanitation" = "water_sanitation")),
          
          uiOutput("agric_output"),
          uiOutput("commerce_output"),
          uiOutput("edu_output"), 
          uiOutput("energy_output"),
          uiOutput("health_output"), 
          uiOutput("public_output"),
          uiOutput("religion_output"),
          uiOutput("security_output"),
          uiOutput("water_san_output"),
          
          actionButton(inputId = "submitButton",
                       label = "Submit"),
          br()
        ),

        # Show a plot of the generated distribution
        mainPanel(
           uiOutput("lfMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## UI section
  output$agric_output <- renderUI({
    req(input$sector == 'agriculture')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Farmland")
    )
  })
  
  
  output$commerce_output <- renderUI({
    req(input$sector == 'commerce')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Factories/Industrial Sites", "Filling Stations",
        "Market")
    )
  })
  
  
  
  
  output$edu_output <- renderUI({
    req(input$sector == 'education')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Primary Schools", "Private Schools",
        "Public Schools","Secondary Schools",
        "Tertiary Schools")
    )
  })
  
  
  
  output$energy_output <- renderUI({
    req(input$sector == 'energy')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Electricity Sub-stations")
    )
  })
  
  output$health_output <- renderUI({
    req(input$sector == 'health_safety')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Ambulance Emergency Services", "Fire Station",
        "Health Care Facilities (Primary, Secondary, Tertiary)",
        "Laboratories","Pharmaceutical Facilities")
    )
  })
  
  
  output$public_output <- renderUI({
    req(input$sector == 'public-facilities')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Government Buildings", "Post Office",
        "Road")
    )
  })
  
  
  
  output$religion_output <- renderUI({
    req(input$sector == 'religion')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Churches", "Mosques")
    )
  })
  
  
  output$security_output <- renderUI({
    req(input$sector == 'security')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Prison", "Police Stations")
    )
  })
  
  
  output$water_san_output <- renderUI({
    req(input$sector == 'water_sanitation')
    shiny::selectInput(
      "subSector", "Select Sub Sector",
      c("Dump Sites", "Public Water Points",
        "Enviromental Sites","Water Bodies","Waterway")
    )
  })
  
  
  
  userFile <- reactive({
    req(!is.null(input$upload))
    # If no file is selected, don't do anything
    validate(need(input$upload, message = FALSE))
    sf::st_read(input$upload$datapath) |>
      mutate(label=paste("<center>",
                         sep = "<br/>",
                         "<b>",toupper(name),"</b>",
                         "</center>"))
  })
  
  geo_icon <- reactive({
    req(!is.null(input$subSector))
    validate(need(input$upload, message = FALSE))
    ic=icons(
      iconUrl = icon_tbl |>
        filter(sub_sector == input$sub_sector) |>
        pull(icon_url),
      iconWidth = 40,
      iconHeight = 40,
      iconAnchorX = 22,
      iconAnchorY = 30,
      shadowWidth = 50,
      shadowHeight = 50,
      shadowAnchorX = 4,
      shadowAnchorY = 62
    )
    
  })
  
  observeEvent(c(input$subSector, input$submitButton), {
    geo_icon <- reactive({
      ic <-  icons(
        iconUrl = icon_tbl |>
          filter(sub_sector == input$subSector) |>
          pull(icon_url),
        iconWidth = 40,
        iconHeight = 40,
        iconAnchorX = 22,
        iconAnchorY = 30,
        shadowWidth = 50,
        shadowHeight = 50,
        shadowAnchorX = 4,
        shadowAnchorY = 62
      )
      ic
    })
    output$lfMap <- renderUI({
      req(input$submitButton)
      g_map <- leaflet(userFile()) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 7.5248,
                lat = 5.4527,
                zoom = 3) %>%
        addMarkers(
          popup = ~ label,
          icon = geo_icon(),
          clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE)
        )
      g_map
    })
  })



  
  # reactive map update
  # observe({
  #   leafletProxy("lfMap", data = userFile()) |> 
  #     clearMarkerClusters() |> 
  #     clearShapes() |> 
  #     clearMarkers() |>  
  #     addMarkers(
  #       popup = ~label,
  #       icon = geo_icon(),
  #       clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE)
  #     )
  # })
  # 
}

# Run the application 
shinyApp(ui = ui, server = server)

# Dependencies ----
# Shiny
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Data
library(openxlsx)
library(dplyr)

# Viz
library(DT)
library(echarts4r)
library(googleway)

# Application UI ----
ui <-dashboardPage(
    dashboardHeader(title = "Owen's Golf Stats"),
    dashboardSidebar(disable = T),
    dashboardBody(
        includeCSS("www/style.css")
        # Header Box
        , tabBox(width = 12
                 # Course Panel
                 , course_ui("courses")
                 # Round Panel
                 , tabPanel(title = "Rounds")
                 # Bag Panel 
                 , tabPanel(title = "In The Bag")
                 
        )
    )
) 

fluidPage(
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Module Calls 
    course_server("courses")
}

# Run the application 
shinyApp(ui = ui, server = server)

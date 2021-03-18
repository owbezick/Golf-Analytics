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
library(leaflet)

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
                 , rounds_ui("rounds")
                 # Bag Panel 
                 , bag_ui("bag")
                 
        )
    )
) 


# Define server logic required to draw a histogram
server <- function(input, output) {
    # Module Calls 
    course_server("courses")
    rounds_server("rounds")
    bag_server("bag")
}

# Run the application 
shinyApp(ui = ui, server = server)

# Define Course UI ----
course_ui <- function(id){
  tabPanel(title = "Courses"
           , fluidRow(
             # Course Picker 
             column(width = 6
                    , box(width = 12, status = "primary"
                          , uiOutput(NS(id, "course_picker"))
                    )
                    # Course Info
                    , uiOutput(NS(id, "course_info"))
             )
             # Map 
             , column(width = 6
                      , box(width = 12, title = "Satallite Map", status = "primary"
                            , leafletOutput(NS(id, "leaflet_map"), height = "60vh")
                      )
             )
           )
  )
}

# Course Server 
course_server <- function(id, df_courses){
  moduleServer(id, function(input, output, session) {
    # Data 
    df_courses <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "courses") %>%
      mutate(img = paste0("<img src='", course_logo, "' width = 30px> <div class = 'picker-img'>", course_name,"</div> </img>"))
    
    # Filter course name
    output$course_picker <- renderUI({
      pickerInput(NS(id, "course_picker"), "Select Course"
                  , choices = unique(df_courses$course_name)
                  , multiple = F
                  , choicesOpt = list(content = df_courses$img))
    })
    
    # Data 
    r_courses <- reactive({
      req(input$course_picker)
      df_courses %>%
        filter(course_name == input$course_picker)
    })
    
    r_rounds <- reactive({
      req(input$course_picker)
      openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "rounds") %>%
        mutate(date = as.Date(date, origin = "1899-12-30" )) %>%
        filter(course_id == r_courses()$course_id) 
    })
    # Rounds Played 
    output$rounds_table <- renderDT({
      req(input$course_picker)
      df <- r_rounds() %>% 
        mutate(date = as.Date(date, origin = "1899-12-30")) %>%
        select("Date" =date, "Time" = time, "Round Name" = round_name, "Holes Played" = holes)
      datatable(df, rownames = F
                , options = list(sDom  = '<"top">lrt<"bottom">ip'
                                 , paging = FALSE)
      )
    })
    
    # Course Info 
    output$course_info <- renderUI({
      req(input$course_picker)
      box(width = 12, status = "primary", title = div(class = "info-tite", r_courses()$course_name)
          , fluidRow(
            column(width = 12
                   , div(class = "course-info-sub", HTML("Course Notes:")
                         )
                   , HTML(r_courses()$notes)
                   , br()
            )
          )
          , fluidRow(
            column(width = 12
                   , div(class = "course-info-sub", HTML("Rounds Played:"))
                   , DTOutput(NS(id, "rounds_table"))
            )
          )
      )
    })
    
    # Map 
    output$leaflet_map <- renderLeaflet({
      req(input$course_picker)
      leaflet(data = r_courses()) %>% 
        addProviderTiles('Esri.WorldImagery') %>%
        addMarkers(lng = ~ course_long, lat = ~ course_lat, label = ~course_name)
      
    })
   
  })
}

# Define Course UI ----
course_ui <- function(id){
  tabPanel(title = "Courses"
           , fluidRow(
             # Course Picker 
             column(width = 6
                    , box(width = 12, status = "primary"
                          , uiOutput(NS(id, "course_filter"))
                    )
                    # Course Info
                    , uiOutput(NS(id, "course_info"))
             )
             # Map 
             , column(width = 6
                      , box(width = 12, title = "Satallite Map"
                            #TODO: Google way Map ----
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
      mutate(img = paste0("<img src='", course_logo, "' width = 30px> <div class = '.picker-img'>", course_name,"</div> </img>"))
    
    # Filter course name
    output$course_filter <- renderUI({
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
        filter(course_id == r_courses()$course_id)
    })
    # Rounds Played 
    output$rounds_table <- renderDT({
      req(input$course_picker)
      df <- r_rounds() %>% 
        mutate(date = as.Date(date, origin = "1899-12-30")) %>%
        select("Date" =date, "Time" = time, "Holes Played" = holes)
      datatable(df, rownames = F
                , options = list(sDom  = '<"top">lrt<"bottom">ip', scrollY = "10vh")
      )
    })
    
    # Course Info 
    output$course_info <- renderUI({
      req(input$course_picker)
      box(width = 12, status = "primary", title = r_courses()$course_name
          , fluidRow(
            column(width = 6
                   , box(width = 12, title = "Rounds Played"
                         , DTOutput(NS(id, "rounds_table"))
                   )
            )
            ,   column(width = 6
                       , box(width = 12, title = "Course Notes"
                             , HTML(r_courses()$notes)
                       )
            )
          )
          , fluidRow(
            column(width = 12, align = "center"
                   , img(src = r_courses()$course_scorecard, width = "75%")
            )
          )
      )
    })
    
    # Map 
    
  })
}

# Rounds Module UI ----
rounds_ui <- function(id){
  tabPanel(title = "Rounds"
           , fluidRow(
             # Course Picker 
             column(width = 4
                    , box(width = 12, status = "primary"
                          , uiOutput(NS(id, "course_filter"))
                    )
                    # Round Selection Info
                    , box(width = 12, status = "primary"
                          , uiOutput(NS(id, "round_selection"))
                    )
                    
             )
             # Scorecards
             , column(width = 8
                      , tabBox(width = 12
                               , tabPanel(title = "Score Cards", status = "primary"
                                          , fluidRow(
                                            box(width = 12, status = "primary"
                                                , DTOutput(NS(id, "total_score_card"))
                                            )
                                          )
                               )
                               , tabPanel(title = "Summary Statistics"
                                          , uiOutput(NS(id, "summary_statistics"))
                               )
                      )
             )
           )
  )
}

# Rounds Server ----
rounds_server <- function(id){
  moduleServer(id, function(input, output, session){
    # Data 
    df_courses <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "courses") %>%
      mutate(img = paste0("<img src='", course_logo, "' width = 30px> <div class = 'picker-img'>", course_name,"</div> </img>"))
    df_rounds <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "rounds")
    df_holes <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "holes")
    # Filter course name
    output$course_filter <- renderUI({
      pickerInput(NS(id, "course_picker"), "Select Course"
                  , choices = unique(df_courses$course_name)
                  , multiple = T
                  , selected = unique(df_courses$course_name)
                  , choicesOpt = list(content = df_courses$img))
    })
    
    # Data 
    r_courses <- reactive({
      req(input$course_picker)
      df_courses %>%
        filter(course_name %in% input$course_picker)
    })
    
    round_choices <- reactive({
      req(input$course_picker)
      df_rounds %>%
        filter(course_id == r_courses()$course_id) %>%
        mutate(date = as.Date(date, origin = "1899-12-30")) %>%
        select(round_id, round_name, date)
    })
    
    output$round_selection <- renderUI({
      req(input$course_picker)
      checkboxGroupInput(NS(id, "round_boxes")
                         , label = "Rounds Selected"
                         , choiceNames = paste(round_choices()$round_name, "-", round_choices()$date)
                         , choiceValues = round_choices()$round_id
      )
    })
    
    df_score_card <- reactive({
      req(input$round_boxes)
      df_holes %>%
        filter(round_id %in% input$round_boxes) %>%
        left_join(df_rounds, by = "round_id") %>%
        select("Round Name" = round_name, "Hole Number" = hole_number, "Par" = par
               , "Score" = score, "Fairway" = fairway, "Green in Regulation" = green_in_regulation
               , "Putts" = putts, "Hole Notes" = notes.x)
    })
    
    output$total_score_card <- renderDT({
      req(input$round_boxes)
      datatable(df_score_card()
                , rownames = F
                , options = list(sDom  = "tip"
                                 , autoWidth = T
                                 , paging = FALSE
                                 , scrollX = TRUE
                                 , scrollY = "60vh"
                                 , columnDefs = list(
                                   list(width = '250px', targets = c(7))
                                   ,  list(width = '100px', targets = c(0, 1, 5))
                                 )
                )
      )
    })
    df_sum_stats <- reactive({
      df_holes %>%
        filter(round_id %in% input$round_boxes) %>%
        left_join(df_rounds, by = "round_id") %>%
        select(round_id, par, score, fairway, putts, green_in_regulation) %>%
        group_by(round_id)
    })
    # value boxes 
    output$total_over_under_vb <- renderValueBox({
      value <- sum(df_sum_stats()$score) - sum(df_sum_stats()$par)
      valueBox(value, "Total Over Par")
    })
    output$total_fairways_hit_vb <- renderValueBox({
      total_fairways_hit <- df_sum_stats() %>%
        filter(fairway == "hit") %>%
        nrow()
      total_fairways_right <- df_sum_stats() %>%
        filter(fairway == "right") %>%
        nrow()
      total_fairways_left <- df_sum_stats() %>%
        filter(fairway == "left") %>%
        nrow()
      total_fairway_attempt <- sum(total_fairways_hit, total_fairways_right, total_fairways_left)
      fairway_hit_prcnt <- total_fairways_hit/total_fairway_attempt
      valueBox(format(fairway_hit_prcnt, digits = 2), paste("Total Fairways Hit:", total_fairways_hit, "/", total_fairway_attempt))
    })
    
    output$total_green_in_reg_vb <- renderValueBox({
      total_green_in_regulation <- df_sum_stats() %>%
        filter(green_in_regulation == "Yes") %>%
        nrow()
      holes <- nrow(df_sum_stats())
      gir_prcnt <- total_green_in_regulation/holes
      valueBox(format(gir_prcnt, digits = 2), paste("Total Greens in Regulation:", total_green_in_regulation, "/", holes))
    })
    
    output$fairway_right <- renderValueBox({
      total_fairways_hit <- df_sum_stats() %>%
        filter(fairway == "hit") %>%
        nrow()
      total_fairways_right <- df_sum_stats() %>%
        filter(fairway == "right") %>%
        nrow()
      total_fairways_left <- df_sum_stats() %>%
        filter(fairway == "left") %>%
        nrow()
      total_fairway_attempt <- sum(total_fairways_hit, total_fairways_right, total_fairways_left)
      value <- format(total_fairways_right/total_fairway_attempt, digits = 2)
      valueBox(value, paste("Total Fairway Misses Right",total_fairways_right, "/",  total_fairway_attempt))
    })
    output$fairway_left <- renderValueBox({
      total_fairways_hit <- df_sum_stats() %>%
        filter(fairway == "hit") %>%
        nrow()
      total_fairways_right <- df_sum_stats() %>%
        filter(fairway == "right") %>%
        nrow()
      total_fairways_left <- df_sum_stats() %>%
        filter(fairway == "left") %>%
        nrow()
      total_fairway_attempt <- sum(total_fairways_hit, total_fairways_right, total_fairways_left)
      value <- format(total_fairways_left/total_fairway_attempt, digits = 2)
      valueBox(value, paste("Total Fairway Misses Left",total_fairways_left, "/",  total_fairway_attempt))
    })
    output$total_puts <- renderValueBox({
      value <- sum(df_sum_stats()$putts)
      valueBox(value, "Total Putts")
    })
    output$three_plus_putt <- renderValueBox({
      more_than_2_putt <- df_sum_stats() %>%
        filter(putts >= 3) %>%
        nrow()
      valueBox(more_than_2_putt, "3 Put or More Holes")
    })

    output$summary_statistics <- renderUI({
      fluidRow(
        box(width = 12, status = "primary"
            , fluidRow(
              column(width = 12
                     , valueBoxOutput(NS(id, "total_over_under_vb"), width = 12)
              )
            )
            , fluidRow(
              column(width = 6
                     , valueBoxOutput(NS(id, "total_fairways_hit_vb"), width = 12)
              )
              , column(width = 6
                       , valueBoxOutput(NS(id, "total_green_in_reg_vb"), width = 12)
              )
            )
            , fluidRow(
              column(width = 6
                     , valueBoxOutput(NS(id, "fairway_left"), width = 12)
              )
              , column(width = 6
                       , valueBoxOutput(NS(id, "fairway_right"), width = 12)
              )
            )
            , fluidRow(
              column(width = 6
                     , valueBoxOutput(NS(id, "total_puts"), width = 12)
              )
              , column(width = 6
                       , valueBoxOutput(NS(id, "three_plus_putt"), width = 12)
              )
            )
        )
      )      
    })
    
  })
}

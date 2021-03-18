bag_ui <- function(id){
  tabPanel(title = "In The Bag"
           , fluidRow(
             box(width = 12, status = "primary", title = "Clubs"
                 , DTOutput(NS(id, "club_table"))
                 )
           )
  )
}

bag_server <- function(id){
  moduleServer(id, function(input, output, session){
    df_club <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "clubs") %>%
      mutate(club_distance_lower_bound = paste(club_distance_lower_bound, "yds")
             , club_distance_upper_bound = paste(club_distance_upper_bound, "yds")) %>%
      select("Club" = club_name_short
             , "Loft" = loft
             , "Brand/Name" = club_name_long
             , "Shaft" = club_shaft
             , "Grip" = club_grip
             , "Lower Bound Distance" = club_distance_lower_bound
             , "Upper Bound Distance" = club_distance_upper_bound
             )
    output$club_table <- renderDT({
      datatable(df_club
                , rownames = F
                , options = list(sDom  = "tip"
                                 
                                 , paging = FALSE
                                 , scrollX = TRUE
                                 
                                 # , columnDefs = list(
                                 #     list(width = '75px', targets = c(0))
                                 #     ,  list(width = '150px', targets = c(2, 3, 4))
                                 #     ,  list(width = '175px', targets = c(5, 6))
                                 # )
                )
                )
    })
  })
}

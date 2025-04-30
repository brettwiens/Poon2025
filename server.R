#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define server logic required to draw a histogram
function(input, output, session) {

  leaderboard_data <- team_points
  
  # output$Team_Leaderboard <- DT::renderDataTable({
  #   datatable(
  #     team_points,
  #     escape = FALSE,
  #     options = list(dom = 't')
  #   ) %>%
  #     formatStyle(
  #       'Points',
  #       background = styleColorBar(range(team_points$Points, na.rm = TRUE), 'darkred'),
  #       backgroundSize = '100% 80%',
  #       backgroundRepeat = 'no-repeat',
  #       backgroundPosition = 'center'
  #     )
  # })
  
  output$Team_Leaderboard <- DT::renderDataTable({
    team_points$Team <- gsub("/", "", team_points$Team)
    team_points$Avatar <- paste0("<img src='", team_points$Team, ".png' height='40'>")
    # names(team_points) <- c('Team','Goals','Assists','Points','Points Today','Position','Avatar')
        # Add an explicit index to apply colour by visual row position
    
    team_points <- team_points %>%
      arrange(desc(Points))
    
    # print(names(team_points))
    #reorder columns on team_points so Remaining is last
    team_points$Position <- 1:nrow(team_points)
    team_points <- team_points[, c('Avatar', 'Team', 'Goals', 'Assists', 'Points', 'Pts Today', 'Remaining', 'Playing')]
    
    # Gradient from green (top) to red (bottom)
    row_colors <- colorRampPalette(c('#003300', 'white', '#8B0000'))(nrow(team_points))
    
    datatable(
      team_points,
      escape = FALSE,
      options = list(
        dom = 't',
        pageLength = 11
      )
    ) %>%
      # Apply gradient by original row index
      formatStyle(
        columns = names(team_points),
        target = 'row',
        # backgroundColor = styleEqual(rownames(team_points), row_colors),
        color = 'white',
        textShadow = '1px 1px 4px black, -1px -1px 4px black, -1px 1px 4px black, 1px -1px 4px black'
      ) %>%
      # Style Points column bar overlay
      formatStyle(
        'Points',
        background = styleColorBar(range(0:max(team_points$Points), na.rm = TRUE), 'rgba(125,0,0,0.9)'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Pts Today',
        background = styleColorBar(range(team_points$`Pts Today`, na.rm = TRUE), 'rgba(125,125,0,0.9)'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Remaining',
        background = styleColorBar(range(0:max(team_points$`Remaining`), na.rm = TRUE), 'rgba(120,120,150,0.9)'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Playing',
        background = styleColorBar(range(team_points$`Playing`, na.rm = TRUE), 'rgba(125,125,59,0.9)'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  
    
    observe({
      updateSelectizeInput(session, "team", choices = c("All", team_points$Team), selected = team_points$Team[1])
    })
    
    observeEvent(input$team, {
        # Filter the data based on the selected team

        # If "All" is selected, show all data
        if (input$team == "All") {
          filtered_data <- long_picks
        } else {
          filtered_data <- long_picks[long_picks$Team == input$team, ]
        }
        # filtered_data <- long_picks[long_picks$Team == input$team, ]
        # Sort filtered_data by points
  
        filtered_data <- filtered_data[order(-filtered_data$`points_total`), ]
        #I want to add a column to display the team logo for each player using the team name .gif (in the www folder)
        # Add a column for the team logo
        # filtered_data$team_logo <- paste0('<img src="', filtered_data$team, '.gif" width="20%" height="20%">')
        filtered_data$team_logo <- paste0(
          "<img src='https://assets.nhle.com/logos/nhl/svg/",filtered_data$team,"_light.svg' height='40'>")
        filtered_data$team <- NULL
        # filtered_data$Team <- NULL
        filtered_data$id <- NULL
        # print(filtered_data)
        
        if (input$team == 'All') {
          filtered_data <- filtered_data[, c('Team','Round', 'team_logo', 'player_name', 'goals_total', 'assists_total', 'points_total', 'points_today', 'player_remaining')]
          names(filtered_data) <- c('Pick','Round','Team','Player','Goals','Assists','Points', 'Points Today', 'Remaining')
        } else {
          filtered_data$Team <- NULL
           filtered_data <- filtered_data[, c('Round', 'team_logo', 'player_name', 'goals_total', 'assists_total', 'points_total', 'points_today', 'player_remaining')]
          names(filtered_data) <- c('Round','Team','Player','Goals','Assists','Points', 'Points Today', 'Remaining')
        }
        filtered_data$Remaining <- ifelse(filtered_data$Remaining == 0, "N", "Y")
        
        # Update the table output with the filtered data
        output$Team_Table <- DT::renderDataTable({
          datatable(
            filtered_data,
            escape = FALSE,
            options = list(
              dom = 'tip'
              ) 
          ) %>% 
            formatStyle(
              columns = names(filtered_data),  # apply style across all columns
              target = 'row',
              color = styleEqual("N", 'rgba(70,70,70,70)'),   # when Remaining == 0, font color = grey
              backgroundColor = styleEqual("N", 'rgba(20,20,20,20)'),
              valueColumns = 'Remaining'       # <- this is the key line
            ) %>%
            formatStyle(
              'Remaining',
              textAlign = 'right'
            )
        })
    })

    # Handle click event from Team links
    observeEvent(input$team_click, {
      updateSelectizeInput(session, "team", selected = input$team_click)
      updateTabsetPanel(session, "Tabs", selected = "Teams")
    })
    
    
    output$gameBoxes <- renderUI({
      tagList(
        lapply(1:nrow(LiveGames), function(i) {
          game <- LiveGames[i]
          div(class = "score-box",
              div(class = "score-row",
                  HTML(paste(game$AwayScore, game$AwayLogo, game$AwayTeam))
              ),
              div(class = "score-row",
                  HTML(paste(game$HomeScore, game$HomeLogo, game$HomeTeam))
              ),
              div(style = "font-size: 12px;",
                  if (game$GameState == 'FUT' | game$GameState == 'PRE') {
                    # convert game$startTimeUTC to local time
                    print(game)
                    game$startTimeUTC <- as.POSIXct(game$V11, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
                    game$startTimeUTC <- format(game$startTimeUTC, tz = "America/Edmonton", usetz = TRUE)
                    game$startTimeUTC
                  } else if (game$GameState == 'LIVE') {
                    paste(game$GameTime, "-", game$Period)
                  } else if (game$GameState == 'CRIT') {
                    "Overtime/Critical Time"
                    } else {
                    "Final"
                  }
              )
          )
        })
      )
    })
    
    output$Progress_Plot <- renderPlot(
      ggplot(long_progress, aes(x = Date, y = Points, color = Team)) +
        geom_line(size = 1.0, alpha = 0.9) +
        geom_text_repel(
          data = end_points,
          aes(label = paste(Team, Points, sep = " - ")),
          vjust = -1,
          size = 4,
          color = "white",
          # fontface = "normal",
          box.padding = 0.5,
          segment.color = "white"
        ) +
        geom_text(
          data = long_progress,
          aes(label = Points, color = Team),
          vjust = -1,
          size = 3,
          # color = "lightgray",
          fontface = "bold"
        ) +
        scale_color_manual(values = c(
          "Al" = "#FF6B6B",
          "Brett" = "#4ECDC4",
          "Colin" = "#FFD93D",
          "Harvey" = "#A29BFE",
          "Ian F" = "#E17055",
          "Ian M" = "#00B894",
          "Matt" = "#74B9FF",
          "Morrie" = "#FDCB6E",
          "Norm" = "#55EFC4",
          "Ryan" = "#D63031",
          "Tierney" = "#0984E3"
        )) +
        theme_dark(base_size = 14) +
        labs(
          title = "Points Progression by Team",
          subtitle = "Up to Latest Date",
          x = "Date",
          y = "Points",
          color = "Team"
        ) +
        theme(
          plot.background = element_rect(fill = "#1C1C1C", color = "white", size = 1),
          panel.background = element_rect(fill = "#1C1C1C", color = NA),
          panel.grid.major = element_line(color = "#444444"),
          panel.grid.minor = element_line(color = "#333333"),
          legend.background = element_rect(fill = "#1C1C1C"),
          legend.key = element_rect(fill = "#1C1C1C"),
          plot.title = element_text(face = "bold", hjust = 0.5, color = "white", size = 16),
          plot.subtitle = element_text(hjust = 0.5, color = "gray70", size = 12),
          legend.text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white"),
          strip.background = element_rect(fill = "#1C1C1C"),
          strip.text = element_text(color = "white")
        )
    )
    
    # Refresh everything in the app every minute
    autoInvalidate <- reactiveTimer(60 * 1000) # 1 minute
    
}

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  tags$head(
    tags$title("Poon Invitational 2025"),  # Sets the browser tab title
    tags$style(HTML("
      body {
        background-color: #2a2a2e;
        color: #ecf0f1;
      }
      h3 {
        color: #ecf0f1;
      }
      .dataTable {
        background-color: #1a1a1c;
        color: #FFFFFF;
      }
      div.dataTables_wrapper {
      box-shadow: 0 0 20px 2px lightgrey;  /* white halo */
      border-radius: 10px;
      }
      .score-box {
        border: 2px solid #ccc;
        border-radius: 10px;
        padding: 2px;
        margin: 2px;
        background-color: #000000;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        font-size: 16px;
        display: inline-block;
        vertical-align: top;
        color: #FFFFFF;
        min-width: 300px;
        text-align: center;
      }
      .score-row {
        font-weight: bold;
        font-size: 18px;
        margin: 5px 0;
      }
      #main-title {
      font-family: 'Orbitron', sans-serif;
      font-size: 36px;
      color: black;
      text-shadow: 1px 1px 3px #fff;
      }
      /* Pagination controls */
  .dataTables_paginate,
  .dataTables_paginate a,
  .dataTables_paginate .paginate_button {
    color: white !important;
  }

  .dataTables_paginate .paginate_button.current {
    background-color: #444 !important;
    color: white !important;
  }

  .dataTables_paginate .paginate_button:hover {
    background-color: #666 !important;
    color: white !important;
  }

  /* Info text */
  .dataTables_info {
    color: white !important;
  }
  
  .modal-content {
  background-color: #1e1e1e !important;
  color: white !important;
  border: 1px solid #444;
}

.modal-header, .modal-footer {
  border-color: #444;
}

.modal-title {
  color: white;
}

.btn-close {
  filter: invert(1);
}
      "))),
  
    # Application title
    titlePanel(div("Poon Invitational 2025", id = 'main-title')),
    fluidRow(
      # column(1,''),
             
             column(10, 
                    uiOutput("gameBoxes")
                    ),
             column(2,h5(paste0("Data as of ", Sys.time())))
             ),
  tabsetPanel(
    tabPanel("Main", 
   

       # column(1,''),
       column(5,
              h3("Leaderboard"),
              hr(),
              DT::dataTableOutput(outputId = 'Team_Leaderboard')
       ), 
    column(2,
           div(
             style = "text-align: center;",
             br(),
             br(),
             img(src = 'Trophy.png', height = 500)
             )),
               column(5,
                      br(),
                      # h3("Team Stats"),
                      # hr(),
                      fluidRow(selectizeInput("team", "Select a team:", choices = NULL)),
                      fluidRow(DT::dataTableOutput(outputId = 'Team_Table'))),
               column(1,'')
    
    ),
    tabPanel("Progress",
             column(2,
                    div(
                      style = "text-align: center;",
                      br(),
                      br(),
                      img(src = 'Trophy.png', height = 500)
                    )),
             column(10,
                    plotOutput("Progress_Plot", width = "100%", height = "600px"),
    )
  )
)
)

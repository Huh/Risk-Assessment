#  Admin ui, companion of the expert opinion app
#  Josh Nowak
#  11/2015

library(shiny)

shinyUI(fluidPage(
  tags$head(
    tags$style(HTML(".shiny-progress .progress{ height:25px }")),
    tags$style(HTML('#refresh{background-color:#0044CC;
                        color:white;
                        margin-top:40px;
                        float:right;
                        margin-bottom:200px}')),
    tags$style(HTML('#refresh_raw{background-color:#808080;
                        color:white;
                        margin-top:15px}'))
  ),


  headerPanel(a(h2("Expert Opinion Admin Dashboard", style = "color:black"),
        a(href = "http://popr.cfc.umt.edu/",
            h4("Powered by PopR")))),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
        tags$div(title = "Use this box to set the disease number",
            selectInput("disease", "Disease Number", choices = 1:50)
        ),
        tags$div(title = "Use this box to set the current scenario number",
            selectInput("scenario", "Scenario Number", choices = 1:50)
        ),
        tags$div(title = "Click here to check for updated data",
            actionButton("refresh", "Refresh Data"))
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Setup",
                fluidRow(
                    column(6, h4("Number of users who submitted")),
                    column(6, h4(textOutput("n_users")))
                ),
                hr(),
                DT::dataTableOutput("user_responses")
            ),
            tabPanel("Data",
                tags$div(title = "Click here to check for updated data",
                    actionButton("refresh_raw", "Refresh Raw Data")),
                hr(),
                DT::dataTableOutput("raw_data")
            )
        , type = "pills")
    )
    ), title = "PopR", collapsible = T)
)


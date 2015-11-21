library(shiny)

# Define UI for random distribution application
shinyUI(fluidPage(
  tags$head(
    tags$style(HTML('#fitgo{background-color:#0044CC;
                        color:white;
                        margin-top:40px}'))
  ),

  # Application title
  headerPanel(a(h2("Expert Opinion", style = "color:black"),
        a(href = "http://popr.cfc.umt.edu/",
            h4("Powered by PopR")))),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
        tags$div(title = "Use this box to choose the user number that the
            facilitator assigned you at the beginning of the session",
            selectInput("userid", "Your User Number", choices = 1:50)
        ),
        tags$div(title = "Use this box to set the disease number",
            selectInput("disease", "Disease Number", choices = 1:50)
        ),
        tags$div(title = "Use this box to set the current scenario number",
            selectInput("scenario", "Scenario Number", choices = 1:50)
        ),
        tags$div(title = "You may use this box to give the disease by scenario
            combination a unique name, but this information is optional.",
            textInput("ds_name", "Scenario Name (Optional)")
        ),
        tags$div(title = "Use this box to set the facilitator's number",
            selectInput("facilitator", "Facilitator Number", choices = 1:10)
        ),
        tags$div(title = "Use this box to select the type of distribution to use
            for a scenario.  The facilitator will provide guidance on how to
            make this selection.",
            selectInput("distr_type", "Distribution Type",
                choices = list("Continuous", "Discrete"))
        )
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
        tabsetPanel(
            tabPanel("Setup",
                conditionalPanel("input.distr_type == 'Continuous'",
                    plotOutput("cont_plot"),
                    fluidRow(
                        column(6,
                            sliderInput("mu_slider", "Expected Value",
                                min = 0.05, max = 0.95, value = 0.5,
                                step = 0.05, width = "100%", sep = "")
                        ),
                        column(6,
                            sliderInput("sd_slider", "Uncertainty",
                                min = 0.000005, max = 0.009, value = 0.000005,
                                step = 0.000005, width = "100%", sep = "")
                        )
                    ),
                    fluidRow(
                        column(4, h6(textOutput("min_val"))),
                        column(4, h6(textOutput("mean_val"))),
                        column(4, h6(textOutput("max_val")))
                    )
                ),
                conditionalPanel("input.distr_type == 'Discrete'",
                    sliderInput("n_groups", "Number of Groups", min = 1,
                        max = 10, value = 2, step = 1, width = "100%"),
                    uiOutput("g_boxes1"),
                    conditionalPanel("input.n_groups > 6",
                        uiOutput("g_boxes2")
                    ),
                    hr(),
                    fluidRow(
                        column(6, h4("Current Groups Sum")),
                        column(6, h4(textOutput("sum20")))
                    ),
                    h5(textOutput("warn20"), style = "color:red"),
                    hr(),
                    plotOutput("disc_plot")
                ),
                tags$div(title = "Click here to submit your opinion",
                    shiny::actionButton("fitgo", "Submit",
                        width = "100%",
                        icon = icon("exchange")),
                    style = "padding-bottom:200px")

            ),
            tabPanel("My Results",
                fluidRow(
                    column(4,
                        tags$div(title = "If you provided a unique name for
                            each scenario you can use this box to display the
                            results of those excercises.",
                            selectInput("ds_name_recall", "Scenario Name",
                                choices = NULL)
                        )
                    ),
                    column(4,
                        tags$div(title = "Choose the disease number you would
                            like to display",
                            selectInput("disease_recall", "Disease Number",
                                choices = 1:50)
                        )
                    ),
                    column(4,
                        tags$div(title = "Choose the scenario number you would
                            like to display",
                            selectInput("scenario_recall", "Scenario Number",
                                choices = 1:50)
                        )
                    )
                )
            )
        )
    )
  )
))
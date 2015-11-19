library(shiny)

# Define UI for random distribution application
shinyUI(fluidPage(

  # Application title
  titlePanel("Risk Tolerance"),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
        textInput("userid", "Last Name", "Enter Last Name", width = "100%"),
        textInput("org", "Organization", "Provide Affiliation", width = "100%")
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
        selectInput("param", "Select Parameter",
            choices = c("Survival", "Extinction")
        ),
        radioButtons("expert", "Level of Expertise",
            choices = c("Low", "High"), selected = "High"
        ),
        plotOutput("plot"),
        fluidRow(
            column(6,
                sliderInput("mu_slider", "Mean", min = 0, max = 1, step = 0.1,
                    value = 0.5)
            ),
            column(6,
                sliderInput("sd_slider", "Uncertainty", min = 0.0001, max = 1,
                    step = 0.01, value = 1)
            )
        ),
        h6(textOutput("min_val")),
        h6(textOutput("mean_val")),
        h6(textOutput("max_val"))
    )
  )
))
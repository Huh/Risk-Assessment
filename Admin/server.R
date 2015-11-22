#  Admin ui, companion of the expert opinion app
#  Josh Nowak
#  11/2015

library(shiny)
library(ggplot2)
library(googlesheets)
library(DT)
library(dplyr)

data_url <- "https://docs.google.com/spreadsheets/d/1S2yrIGxIu4O8AoGondi3Iqj-QXoeGHlzXLkyprwj8Pg/pub?output=csv"

#  Define server logic
shinyServer(function(input, output, session){

    #  Get information about google sheet and store for later
    sheet <- gs_url(data_url)

    #  Report the number of users
    output$n_users <- renderText({
        #  Setup googlesheet
        out <- sheet %>%
            gs_read_csv(.) %>%
            filter(Disease == input$disease,
                Scenario == input$scenario) %>%
            summarise(Users = length(unique((ID))))

        out$Users
    })

    #  Table the number of responses per user
    output$user_responses <- DT::renderDataTable({
        input$refresh

        out <- sheet %>%
            gs_read_csv(.) %>%
            filter(Disease == input$disease,
                Scenario == input$scenario)

        DT::datatable(out,
            escape = T,
            style = 'bootstrap',
            selection = 'multiple',
            rownames = FALSE,
            filter = list(position = 'top', clear = F),
            extensions = c('ColReorder', 'ColVis', 'FixedHeader',
                'TableTools'),
		    options = list(
		        pageLength = -1,
                dom = 'CTRltir',
                lengthMenu = list(c(20, 50, 100, -1),
                                  c('20', '50', '100', 'All')),
                tableTools = list(sSwfPath = copySWF())))
    })

    #  All of the raw data
    output$raw_data <- DT::renderDataTable({
        input$refresh_raw

        rawd <- sheet %>%
            gs_read_csv(.)

        DT::datatable(rawd,
            escape = T,
            style = 'bootstrap',
            selection = 'multiple',
            rownames = FALSE,
            filter = list(position = 'top', clear = F),
            extensions = c('ColReorder', 'ColVis', 'FixedHeader',
                'TableTools'),
		    options = list(
		        pageLength = -1,
                dom = 'CTRltir',
                lengthMenu = list(c(20, 50, 100, -1),
                                  c('20', '50', '100', 'All')),
                tableTools = list(sSwfPath = copySWF())))
    })


})
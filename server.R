library(shiny)
library(ggplot2)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {

    observeEvent(input$expert, {
        if(input$expert == "Low"){
            updateSliderInput(session, "mu_slider", label = "Lower Boundary",
                min = 0, max = .9, value = .5, step = .1)
            updateSliderInput(session, "sd_slider", label = "Upper Boundary",
                min = 0.1, max = 1, value = .5, step = .1)
        }
        if(input$expert == "High"){
            updateSliderInput(session, "mu_slider", label = "Mean", min = 0.1,
                max = 0.99, value = .5, step = 0.1)
            updateSliderInput(session, "sd_slider", label = "Uncertainty",
                min = 0.01, max = 0.99, value = 0.5, step = 0.1)
        }
    })

    #  Container to hold data
    dat <- reactive({
        if(input$expert == "Low"){
            out <- data.frame(val = runif(10000, input$mu_slider,
                input$sd_slider))
        }
        if(input$expert == "High"){
            out <- data.frame(val = rnorm(10000, mean = input$mu_slider,
                sd = input$sd_slider))
        }
    return(out)
    })

    output$plot <- renderPlot({
        ggplot(dat(), aes(x = val)) +
            geom_density(alpha = .4, fill = "dodgerblue",
                colour = "dodgerblue") +
            theme_bw() +
            xlab("Risk") +
            theme(legend.position = "none") +
			theme(panel.border = element_blank(),
			    axis.line = element_line(color = "black"))

    })

    output$min_val <- renderText({
        paste("Minimum value likely to occur =",
            round(quantile(dat()$val, 0.025), 2))
    })
    output$mean_val <- renderText({
        paste("Most likely value =", input$mu_slider)
    })
    output$max_val <- renderText({
        paste("Maximum value likely to occur =",
            round(quantile(dat()$val, 0.975), 2))
    })
})
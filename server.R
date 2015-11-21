library(shiny)
library(ggplot2)

estBetaParams <- function(mu, var) {
    alpha <- mu * (((mu * (1 - mu))/var) - 1)
    beta <- (1 - mu) * (((mu * (1 - mu))/var) - 1)
    return(params = list(alpha = alpha, beta = beta))
}

iter <- 0

# Define server logic for random distribution application
shinyServer(function(input, output, session){

    #  Observe mean slider and update SD slider to appropriate range when mu
    #  changes
    observeEvent(input$mu_slider, {
        if(input$distr_type == "Continuous"){
            cond_var <- input$mu_slider * (1 - input$mu_slider)
            updateSliderInput(session, "sd_slider", "Uncertainty",
                min = 0.000005, max = cond_var - 0.01, value = 0.000005,
                step = 0.000005)
        }
    })

    #  Create container to hold data because it is used in at least two places
    dat_in <- reactiveValues()

    output$cont_plot <- renderPlot({
        #  Estimate beta parameters from input sliders
        beta_parms <- estBetaParams(input$mu_slider, input$sd_slider)

        #  Assign parameters to data container for later
        dat_in$parms <- data.frame(
            id = input$userid,
            disease = input$disease,
            scenario = input$scenario,
            name = ifelse(is.null(input$ds_name), NA, input$ds_name),
            facilitator = input$facilitator,
            alpha = beta_parms$alpha,
            beta = beta_parms$beta)

        #  Create data frame of 10,000 random draws from beta distribution
        #  parameterized by slider values, but this will only occur when
        #  the distribution choice is continuous.  Data is stored in reactive
        #  object to make it available later.
        dat_in$dat <- data.frame(val = rbeta(30000, beta_parms$alpha,
            beta_parms$beta))

        #  Plot the resulting data so the user can see it and respond
        ggplot(dat_in$dat, aes(x = val)) +
            geom_density(fill = rgb(0, 132, 204, 200, maxColorValue = 255),
                colour = NA) +
            theme_bw() +
            xlab("") +
            ylab("") +
            theme(legend.position = "none") +
			theme(panel.border = element_blank(),
			    axis.line = element_line(color = "black")) +
            xlim(c(0, 1))
    })

    #  Generate a user input number of boxes for discrete analysis
    output$g_boxes1 <- renderUI({
        fluidRow(
            lapply(1:ifelse(input$n_groups <= 6, input$n_groups, 6), function(i){
                column(floor(12/ifelse(input$n_groups <= 6, input$n_groups, 6)),
                    textInput(paste0("txt", i), paste("Group", i),
                        width = "80px", value = ifelse(i == 1, 20, 0))
                )
            })
        )
    })

    output$g_boxes2 <- renderUI({
        if(input$n_groups > 6){
            tmp <- input$n_groups - 6
            fluidRow(
                lapply(7:(6 + tmp), function(i){
                    column(floor(10/tmp),
                        textInput(paste0("txt", i), paste("Group", i),
                            width = "80px", value = ifelse(i == 1, 20, 0))
                    )
                })
            )
        }
    })

    catVals <- reactive({
        if(is.null(input$n_groups))
            return()

        #  Recover user inputs
        index <- grep("^txt", names(input))

        if(length(index) < 1){
            return()
        }else{

            n_vals <- length(index)
            user_vals <- numeric(as.numeric(n_vals))

            for(i in 1:n_vals){
                user_vals[i] <- as.numeric(input[[paste0("txt", i)]])
            }

        return(user_vals)
        }
    })

    output$sum20 <- renderText({
        sum(catVals())
    })

    output$warn20 <- renderText({

        if(sum(catVals()) == 20){
            ""
        }else{
            "Sum of the groups must equal 20!"
        }

    })

    #  Generate discrete data and plot
    output$disc_plot <- renderPlot({
        if(is.null(input$n_groups))
            return()

        #  Recover user inputs
        dat <- try(data.frame(
            group = 1:input$n_groups,
            vals = catVals(),
            sum20 = sum(catVals()) == 20), silent = T)

        if(is.data.frame(dat)){
            ggplot(dat, aes(x = group, y = vals)) +
                geom_bar(fill = rgb(0, 132, 204, 200, maxColorValue = 255),
                    binwidth = 1, origin = -0.5, stat = "identity") +
                theme_bw() +
                xlab("") +
                ylab("Count") +
                theme(legend.position = "none") +
    			theme(panel.border = element_blank(),
    			    axis.line = element_line(color = "black")) +
                scale_x_continuous(breaks = dat$group,
                    limits = c(0.5, (as.numeric(input$n_groups) + 0.5)))
        }

    })

    #  Update text that reports the quantiles
    output$min_val <- renderText({
        paste("Lower probable bound =",
            round(quantile(dat_in$dat$val, 0.025), 2))
    })
    output$mean_val <- renderText({
        paste("Most likely value =", round(quantile(dat_in$dat$val, 0.5), 2))
    })
    output$max_val <- renderText({
        paste("Upper probable bound =",
            round(quantile(dat_in$dat$val, 0.975), 2))
    })


})
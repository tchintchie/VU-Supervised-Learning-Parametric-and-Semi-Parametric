# ---------------------------------------------
# Name:        server.R
# Author:      Thorsten Simon
# Date:        2020-03-26
# Description: Server for Scatter Smoothing App.
# License:     CC-BY
# ---------------------------------------------

library("shiny")
source("scatter-smooth-app-master/scatter-smooth-functions.R")

true_effect <- function(x) sin(2 * (4 * x - 2)) + 2 * exp(-256 * (x - .5)^2)
seed <- 333

function(input, output) {

	output$scatter_smooth <- renderPlot({
		d <- simdata(input$n, input$sd,
			true_effect = true_effect,
			seed = seed,
			equidist = input$equidist
		)
		f <- switch(input$basis,
			"poly_raw" = substitute(y ~ 1 + poly(x, k, raw = TRUE), list(k = input$k)),
			"poly"     = substitute(y ~ 1 + poly(x, k), list(k = input$k)),
			"tp"       = substitute(y ~ 0 + tp(x, k, m), list(k = input$k, m = input$m)),
			"bs"       = substitute(y ~ 0 + bsDesign(x, k, m), list(k = input$k, m = input$m))
		)
		f <- as.formula(f)
		
		scatter_smooth(f, d, true_effect = true_effect)
	})

	output$selected_formula <- renderText({
		f <- switch(input$basis,
			"poly_raw" = sprintf("y ~ poly(x, degree = %d, raw = TRUE)", input$k),
			"poly"     = sprintf("y ~ poly(x, degree = %d)", input$k),
			"tp"       = sprintf("y ~ 0 + tp(x, degree = %d, knots = %d)", input$k, input$m),
			"bs"       = sprintf("y ~ 0 + bsDesign(x, degree = %d, knots = %d)", input$k, input$m)
		)
		paste("Formula: ", f)
	})
}




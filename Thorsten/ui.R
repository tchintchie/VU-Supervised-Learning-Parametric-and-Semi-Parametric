# ---------------------------------------------
# Name:        ui.R
# Author:      Thorsten Simon
# Date:        2020-03-26
# Description: User Interface for Scatter Smoothing App.
# License:     CC-BY
# ---------------------------------------------

library("shiny")
library("shinythemes")

fluidPage(
	theme = shinytheme("lumen"),
	titlePanel("Scatter Smoothing"),
	sidebarLayout(
    	sidebarPanel(

			strong("Set up parameters of toy data"),
    		# Select sample size
      		numericInput(
				inputId = "n",
				label = "Sample size",
            	value = 200,
            	min = 25,
				max = 2000,
				step = 25
			),

    		# Select standard deviation
      		numericInput(
				inputId = "sd",
				label = "Standard deviation",
            	value = 0.2,
            	min = 0.1,
				max = 2,
				step = 0.1
			),

			checkboxInput(
				inputId = "equidist",
				label = "Equidistant x",
				value = TRUE
			),

			strong("Set up parameters of smoother"),	
			# Select basis type
			selectInput(
				inputId = "basis",
				label = "Basis type",
				choices = c(
					"B-Splines" = "bs",
					"Raw Polynoms" = "poly_raw",
					"Orthogonal Polynom" = "poly",
					"Truncated Powers" = "tp"
				),
				selected = "tp"
			),

    		# Select degree of polynoms
      		numericInput(
				inputId = "k",
				label = "Degree of Polynoms",
            	value = 3L,
            	min = 1L,
				max = 40L,
				step = 1L
			),

    		# Select number of knots
      		numericInput(
				inputId = "m",
				label = "Number of knots",
            	value = 13L,
            	min = 2L,
				max = 40L,
				step = 1L
			)

		),

		# Output: Description, lineplot, and reference
		mainPanel(strong("Resulting scatter smoothing"),
			textOutput(outputId = "selected_formula"),
			plotOutput(outputId = "scatter_smooth")
		)
	)
)



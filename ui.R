# User interface for optimal CRXO design Shiny app
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

library(shiny)
library(plotly)

shinyUI(fluidPage(
  
  # App title
  titlePanel("Optimal CRXO design with decaying correlation"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      h4("Correlation parameters"),
      
      # Input: Interval, base correlation rho0
      sliderInput("rho0", "Base correlation",
                  min = 0, max = 0.2,
                  value = 0.036, step = 0.001),
      
      # Input: Interval, decay in correlation over trial
      sliderInput("d", "Decay in correlation over trial",
                  min = 0, max = 1,
                  value = 0.25, step = 0.01),
      
      hr(),
      
      h4("Trial configuration"),
      
      # Input: Numeric, number of subjects per cluster
      numericInput("M", "Number of subjects per cluster:",
                  min = 2, max = 2000, value = 100, step=2),
      
      # Input: Interval, maximum number of clusters
      sliderInput("maxN", "Maximum number of clusters",
                  min = 10, max = 100,
                  value = 100, step = 2),
      
      hr(),
      
      h4("Cost components"),
      
      # Input: Interval, cost per cluster
      sliderInput("c", "Cost per cluster",
                  min = 100, max = 10000,
                  value = 5000, step = 100),
      
      # Input: Interval, cost per subject
      sliderInput("s", "Cost per subject",
                  min = 100, max = 5000,
                  value = 500, step = 100),
      
      # Input: Interval, cost per crossover
      sliderInput("x", "Cost per crossover",
                  min = 100, max = 5000,
                  value = 400, step = 100),
      
      # Input: Interval, total budget
      sliderInput("B", "Maximum trial budget",
                  min = 1000000, max = 10000000,
                  value = 2000000, step = 100000),
      
      # Update button to defer the rendering of output until user
      # clicks the button
      actionButton("update", "Update")
    ),
    
    # Main panel for displaying output
    mainPanel(
      
      tabsetPanel(
        tabPanel("Relative efficiency",
                 checkboxInput('logscale', "Display x-axis on log scale"),
                 plotlyOutput("plot1")),
        
        tabPanel("Results table", DT::dataTableOutput("dtresults"))
      )
    )
  )
))

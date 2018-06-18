# Underlying calculations and plot generation for
# optimal CRXO design Shiny app
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

library(shiny)
library(plotly)
library(scales)

source('optimal_design.R')

shinyServer(function(input, output) {
  
  getresults <- eventReactive(input$update, {
    
    res <- optimal_N_T_fixedM(r=1-input$d, rho0=input$rho0, M=input$M,
                              maxN=input$maxN, B=input$B, c=input$c,
                              s=input$s, x=input$x)
    return(res)
  })
  
  output$tableheader1 <- eventReactive(input$update, {
    header1()
  })
  
  header1 <- renderPrint({
    tags$h3("Optimal configuration")
  })
  
  output$optimal <- renderTable({
    res <- getresults()
    res$N <- as.integer(res$N)
    res$Tp <- as.integer(res$Tp)
    res$cost <- paste0("$", comma(res$cost))
    opt <- res[which.min(res$variance),]
    opt$variance <- format(opt$variance, digits=3)
    names(opt) <- c("N", "T", "Cost", "Variance")
    return(opt)
  })
  
  output$tableheader2 <- eventReactive(input$update, {
    header2()
  })
  
  header2 <- renderPrint({
    tags$h3("All admissible designs")
  })
  
  output$results <- renderTable({
    res <- getresults()
    res$N <- as.integer(res$N)
    res$Tp <- as.integer(res$Tp) 
    res$cost <- paste0("$", comma(res$cost))
    ressorted <- res[order(res$variance),]
    ressorted$variance <- format(ressorted$variance, digits=3)
    names(ressorted) <- c("N", "T", "Cost", "Variance")
    return(ressorted)
  })
})

# Underlying calculations and plot generation for
# optimal CRXO design Shiny app
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

library(shiny)
library(plotly)
library(scales)
library(DT)

source('optimal_design.R')

shinyServer(function(input, output) {
  
  getresults <- eventReactive(input$update, {
    
    res <- optimal_N_T_fixedM(r=1-input$d, rho0=input$rho0, M=input$M,
                              maxN=input$maxN, B=input$B, c=input$c,
                              s=input$s, x=input$x)
    res$N <- as.integer(res$N)
    res$Tp <- as.integer(res$Tp)
    res$releff <- min(res$variance)/res$variance
    res$releff <- as.numeric(format(res$releff, digits=3))
    res$variance <- as.numeric(format(res$variance, digits=3))
    return(res)
  })
  
  output$plot1 <- renderPlotly({
    res <- getresults()
    if (input$logscale){
      type <- "log"
    }else{
      type <- "-"
    }
    p <- plot_ly(res, height=800, x=~Tp, y=~N, 
                 type="scatter", mode="markers", hoverinfo="text",
#                 hoverlabel=list(bordercolor=NULL, font=list(size=16)),
                 text=~paste("T: ", Tp, "<br>N: ", N,
                             "<br>Relative efficiency: ", format(releff, digits=3),
                             "<br>Cost: ", paste0("$", comma(cost))),
                 color=~releff, marker=list(size=14)) %>%
         layout(xaxis=list(title="Number of periods (T)", titlefont=list(size=18), tickfont=list(size=16), type=type),
                yaxis=list(title="Number of clusters (N)", titlefont=list(size=18), tickfont=list(size=16)))
    p$elementId <- NULL # Workaround to suppress warning due to an incompatility between shiny and plotly
    p
  })

  output$dtresults <- DT::renderDataTable(
    DT::datatable(getresults(), options=list(
      searching=FALSE, paging=FALSE,
      order=list(3, 'asc'),
      columnDefs=list(list(targets=2, class="dt-right"))),
      colnames=c("N", "T", "Cost", "Variance", "Relative efficiency"),
      rownames=FALSE)
  )
})

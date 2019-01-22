# Underlying calculations and plot generation for
# optimal CRXO design Shiny app
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

library(shiny)
library(plotly)
library(scales)
library(DT)

source('optimal_design.R')

releff_N_T <- function(r, rho0, M, maxN, B, c, s, x){
  # Returns relative efficiencies for all possible numbers of clusters and periods
  # for given:
  #   - correlation values
  #   - total trial budget
  #   - cluster cost
  #   - subject cost
  #   - crossover cost
  #   - number of subjects per cluster
  #   _ maximum number of clusters
  
  Ns <- seq(2, maxN, 2) # all possible numbers of clusters
  dM <- divisors(M)
  Tps <- dM[dM %% 2 == 0] # all even divisors of M
  # Determine combinations of N and T that stay within budget
  all <- data.frame(N=rep(Ns, each=length(Tps)), Tp=rep(Tps, times=length(Ns)), cost=NA)
  all$cost <- total_cost(all$N, all$Tp, M, c, s, x)
  if(sum(all$cost <= B) == 0){
    stop('No admissible designs within budget')
  }else{
    underbudget <- all[all$cost <= B,]
    V <- contdecayVi(r=r, rho0=rho0, M=M)
    Vi_inv <- TrenchInverse(V)
    underbudget$variance <- mapply(vartheta, underbudget$N, underbudget$Tp, MoreArgs=list(Vi_inv=Vi_inv))
    underbudget$RE <- min(underbudget$variance)/underbudget$variance
    return(underbudget)
  }
}

shinyServer(function(input, output) {
  
  Mselection <- reactive({
    validate(
      need(input$M%%2==0, "Number of subjects per cluster must be even")
    )
    input$M
  })
  
  getresults <- eventReactive(input$update, {
    
    res <- releff_N_T(r=1-input$d, rho0=input$rho0, Mselection(),
                              maxN=input$maxN, B=input$B, c=input$c,
                              s=input$s, x=input$x)
    res$N <- as.integer(res$N)
    res$Tp <- as.integer(res$Tp)
    res$RE <- min(res$variance)/res$variance
    res$RE <- as.numeric(format(res$RE, digits=5))
    res$variance <- as.numeric(format(res$variance, digits=5))
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
                 text=~paste("T: ", Tp, "<br>N: ", N,
                             "<br>Relative efficiency: ", format(RE, digits=3),
                             "<br>Cost: ", paste0("$", comma(cost))),
                 color=~RE, marker=list(size=14)) %>%
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

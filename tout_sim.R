# Simple Shiny App Prototype: The Cost of Buying Picks from Touts (Expanded)
# Author: Adam Wickwire - Bettor Analysis

library(shiny)
library(dplyr)
library(ggplot2)
library(reactable)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("📉 The Real Cost of Buying Picks from Touts"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput("bankroll", "Starting Bankroll ($):", 1500, min = 100, step = 50),
      numericInput("package_cost", "Pick Package Cost ($):", 150, min = 0, step = 10),
      numericInput("num_picks", "Number of Picks in Package:", 10, min = 1, step = 1),
      numericInput("unit_size", "Tout's Unit Size ($):", 100, min = 10, step = 10),
      sliderInput("slippage", "Line Slippage (% worse for bettor):", min = 1, max = 15, value = 7),
      sliderInput("missed_pct", "Chance Bettor Misses a Pick (%):", min = 0, max = 100, value = 20),
      sliderInput("tout_winrate", "Tout's Claimed Win Rate (%):", min = 50, max = 70, value = 55),
      numericInput("num_simulations", "Number of Simulations:", 1000, min = 1, step = 1),
      actionButton("runSim", "Run Simulation")
    ),
    
    mainPanel(
      h4("Simulation Results"),
      div(style = "height: 300px; overflow-y: auto; margin-bottom: 20px;",
          reactableOutput("results")
      ),
      h4("Bankroll Comparison (One Package View)"),
      plotOutput("bankrollPlot"),
      h4("Summary"),
      verbatimTextOutput("summaryCard"),
      h4("Aggregate Results from Simulated Packages"),
      plotOutput("histogramPlot"),
      verbatimTextOutput("distributionSummary")
    )
  )
)

server <- function(input, output, session) {
  simulate_once <- function() {
    prob_win <- input$tout_winrate / 100
    outcomes <- sample(c(1, 0), input$num_picks, replace = TRUE, prob = c(prob_win, 1 - prob_win))
    tout_odds <- sample(c(seq(-140, -105, 1), seq(105, 140, 1)), input$num_picks, replace = TRUE)
    adjusted_odds <- ifelse(tout_odds < 0, tout_odds * (1 + input$slippage / 100), tout_odds * (1 - input$slippage / 100))
    missed_bets <- rbinom(input$num_picks, 1, input$missed_pct / 100)
    
    american_to_decimal <- function(odds) {
      ifelse(odds > 0, 1 + odds / 100, 1 + 100 / abs(odds))
    }
    
    decimal_tout <- american_to_decimal(tout_odds)
    decimal_bettor <- american_to_decimal(adjusted_odds)
    
    profit_tout <- ifelse(outcomes == 1, (decimal_tout - 1) * input$unit_size, -input$unit_size)
    profit_tout[1] <- profit_tout[1] + input$package_cost
    profit_bettor <- ifelse(missed_bets == 1, 0, ifelse(outcomes == 1, (decimal_bettor - 1) * input$unit_size, -input$unit_size))
    
    bankroll_tout <- input$bankroll + input$package_cost + cumsum(c(0, profit_tout))
    bettor_bankroll <- numeric(input$num_picks + 1)
    bettor_bankroll[1] <- input$bankroll
    extra_funds <- 0
    
    for (i in 1:input$num_picks) {
      if (i == 1) {
        bettor_bankroll[i] <- bettor_bankroll[i] - input$package_cost
      }
      if (missed_bets[i] == 1) {
        bettor_bankroll[i + 1] <- bettor_bankroll[i]
        next
      }
      if (bettor_bankroll[i] < input$unit_size) {
        extra_funds <- extra_funds + (input$unit_size - bettor_bankroll[i])
        bettor_bankroll[i] <- input$unit_size
      }
      bettor_bankroll[i + 1] <- bettor_bankroll[i] + profit_bettor[i]
    }
    
    result_df <- data.frame(
      Pick = 1:input$num_picks,
      Tout_Odds = round(tout_odds),
      Bettor_Odds = round(adjusted_odds),
      Win = outcomes,
      Missed = missed_bets,
      Profit_Tout = round(cumsum(profit_tout), 2), # includes package cost in first pick
      Profit_Bettor = round(bettor_bankroll[-1] - input$bankroll, 2) # fully reflects package + bets
    )
    
    list(
      result_df = result_df,
      bankroll_tout = bankroll_tout,
      bankroll_bettor = bettor_bankroll,
      picks = 0:input$num_picks,
      final_tout = tail(bankroll_tout, 1),
      final_bettor = tail(bettor_bankroll, 1),
      extra_funds = extra_funds,
      missed_total = sum(missed_bets)
    )
  }
  
  single_sim <- reactiveVal()
  multi_sims <- reactiveVal()
  
  observeEvent(input$runSim, {
    single_sim(simulate_once())
    multi_sims(replicate(input$num_simulations, simulate_once(), simplify = FALSE))
  })
  
  output$results <- renderReactable({
    sim <- single_sim()
    if (!is.null(sim)) {
      reactable(
        sim$result_df,
        searchable = FALSE,
        striped = TRUE,
        highlight = TRUE,
        defaultPageSize = 10,
        bordered = TRUE,
        resizable = TRUE,
        pagination = FALSE
      )
    }
  })
  
  output$bankrollPlot <- renderPlot({
    sim <- single_sim()
    if (!is.null(sim)) {
      data <- data.frame(
        Pick = sim$picks,
        Tout = sim$bankroll_tout,
        Bettor = sim$bankroll_bettor
      )
      ggplot(data, aes(x = Pick)) +
        geom_line(aes(y = Tout, color = "Tout"), size = 1.2) +
        geom_line(aes(y = Bettor, color = "Bettor"), size = 1.2, linetype = "dashed") +
        labs(y = "Bankroll ($)", color = "Legend") +
        scale_color_manual(values = c("Tout" = "#0073C2FF", "Bettor" = "#EFC000FF")) +
        theme_minimal(base_size = 14)
    }
  })
  
  output$summaryCard <- renderPrint({
    sim <- single_sim()
    if (!is.null(sim)) {
      tout_roi <- round(((sim$final_tout - input$bankroll) / input$bankroll) * 100, 2)
      bettor_roi <- round(((sim$final_bettor - input$bankroll) / input$bankroll) * 100, 2)
      diff <- round(sim$final_tout - sim$final_bettor, 2)
      
      cat("Final Bankroll (Tout): $", sim$final_tout, "\n")
      cat("Final Bankroll (Bettor): $", sim$final_bettor, "\n")
      cat("Tout ROI: ", tout_roi, "%\n")
      cat("Bettor ROI: ", bettor_roi, "%\n")
      cat("Difference in Profit: $", diff, "\n")
      cat("Total Missed Bets by Bettor: ", sim$missed_total, "\n")
      if (sim$extra_funds > 0) {
        cat("Additional Funds Required by Bettor to Cover Bets: $", sim$extra_funds, "\n")
      }
    }
  })
  
  output$histogramPlot <- renderPlot({
    sims <- multi_sims()
    if (!is.null(sims)) {
      bettor_end <- sapply(sims, function(x) x$final_bettor)
      tout_end <- sapply(sims, function(x) x$final_tout)
      df <- data.frame(
        Profit = c(bettor_end - input$bankroll, tout_end - input$bankroll),
        Role = rep(c("Bettor", "Tout"), each = input$num_simulations)
      )
      ggplot(df, aes(x = Profit, fill = Role, color = Role)) +
        geom_density(alpha = 0.3, size = 1.2) +
        theme_minimal(base_size = 14) +
        labs(title = "Profit Distribution over Simulated Pick Packages",
             x = "Profit ($)", y = "Density") +
        scale_fill_manual(values = c("Bettor" = "#F9A602", "Tout" = "#1877F2")) +
        scale_color_manual(values = c("Bettor" = "#F9A602", "Tout" = "#1877F2"))
    }
  })
  
  output$distributionSummary <- renderPrint({
    sims <- multi_sims()
    if (!is.null(sims)) {
      bettor_end <- sapply(sims, function(x) x$final_bettor)
      tout_end <- sapply(sims, function(x) x$final_tout)
      bettor_roi <- round(((bettor_end - (input$bankroll - input$package_cost)) / (input$bankroll - input$package_cost)) * 100, 2)
      tout_roi <- round(((tout_end - (input$bankroll + input$package_cost)) / (input$bankroll + input$package_cost)) * 100, 2)
      cat("Mean Final Bankroll (Tout): $", round(mean(tout_end), 2), "\n")
      cat("Mean Final Bankroll (Bettor): $", round(mean(bettor_end), 2), "\n")
      cat("Median Final Bankroll (Tout): $", round(median(tout_end), 2), "\n")
      cat("Median Final Bankroll (Bettor): $", round(median(bettor_end), 2), "\n")
      cat("Mean ROI (Tout): ", round(mean(tout_roi), 2), "%\n")
      cat("Mean ROI (Bettor): ", round(mean(bettor_roi), 2), "%\n")
      
      
      
    }
  })
}

shinyApp(ui = ui, server = server)

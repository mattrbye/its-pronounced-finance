library(shiny)
library(quantmod)
library(DT)
library(ggplot2)

calculate_volatility <- function(symbol, start_date, end_date) {
  # Load historical stock data
  getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
  
  # Extract adjusted closing prices
  stock_data <- get(symbol)
  prices <- Cl(stock_data)
  
  # Calculate daily log returns
  log_returns <- diff(log(prices))
  
  # Calculate historical volatility (annualized)
  daily_volatility <- sd(log_returns, na.rm = TRUE)  # Standard deviation of returns
  annual_volatility <- daily_volatility * sqrt(252)  # Scale to annual (252 trading days)
  
  # Get the last closing price
  last_close <- as.numeric(tail(prices, 1))
  
  return(list(volatility = annual_volatility, last_close = last_close))
}

calculate_option_probability <- function(current_price, strike_price, volatility, risk_free_rate, time_to_expiry, option_type) {
  # Calculate d2
  d2 <- (log(current_price / strike_price) + (risk_free_rate - 0.5 * volatility^2) * time_to_expiry) / (volatility * sqrt(time_to_expiry))
  
  # Calculate probability based on option type
  if (option_type == "put") {
    probability <- pnorm(-d2)
  } else if (option_type == "call") {
    probability <- pnorm(d2)
  } else {
    stop("Invalid option type. Use 'call' or 'put'.")
  }
  
  return(round(probability,6))
}

do_that_thang <- function(ticker,last_close,volatility, start_date, end_date,risk_free_rate, time_to_expiry, lo_prob, hi_prob, strike_max, type){
  strikes <- 1:strike_max
  vol <- calculate_volatility(ticker, start_date, end_date)
  result <- lapply(strikes, function(func){ 
    pr <- calculate_option_probability(last_close, func, volatility, risk_free_rate, time_to_expiry, type)
  })
  df <- data.frame(do.call(rbind, result))
  df <- cbind(df, strikes)
  df <- round(df,5)
  colnames(df) <- c("prob", "strike")
  df <- df %>%
    filter(last_close*.8 < strike & strike < last_close*1.2)
}

ui <- fluidPage(
  titlePanel("Option Probability Calculator"),
  
  # Top bar for inputs
  fluidRow(
    column(2, textInput("symbol", "Stock/ETF Symbol", value = "VEA"), 
           dateInput("start_date", "Start Date", value = Sys.Date() - 365)),
    column(3, dateInput("end_date", "End Date", value = Sys.Date()), 
           numericInput("strike_price", "Strike Price", value = 45, min = 0)),
    column(4, numericInput("risk_free_rate", "Risk-Free Rate (e.g., 0.05 for 5%)", value = 0.05, min = 0),
           numericInput("days_to_expiry", "Days to Expiry", value = 10, min = 1)),
    column(3, selectInput("option_type", "Option Type", choices = c("call", "put")),
           actionButton("calculate", "Calculate Probability"))
  ),
  
  # Main content below the top bar
  fluidRow(
    column(8, plotOutput("probability_plot")),  # Plot takes up more room
    column(4, DTOutput("prob_table"))          # Table takes up less room
  )
)


server <- function(input, output, session) {
  observeEvent(input$symbol, {
    # Fetch last close price when symbol is entered
    req(input$symbol)
    tryCatch({
      volatility_data <- calculate_volatility(input$symbol, input$start_date, input$end_date)
      last_close <- volatility_data$last_close
      output$last_close <- renderText({ paste("Last Close Price:", round(last_close, 2)) })
    }, error = function(e) {
      output$last_close <- renderText({ "Error fetching data. Please check the symbol." })
    })
  })
  
  observeEvent(input$calculate, {
    # Calculate volatility and last close price
    req(input$symbol)
    tryCatch({
      volatility_data <- calculate_volatility(input$symbol, input$start_date, input$end_date)
      volatility <- volatility_data$volatility
      last_close <- volatility_data$last_close
      
      # Convert time to expiry to years
      time_to_expiry <- input$days_to_expiry / 365
      
      # Calculate probability of option being exercised
      probability <- calculate_option_probability(
        current_price = last_close,
        strike_price = input$strike_price,
        volatility = volatility,
        risk_free_rate = input$risk_free_rate,
        time_to_expiry = time_to_expiry,
        option_type = input$option_type
      )
      
      # Output results
      output$volatility <- renderText({ paste("Annualized Volatility:", round(volatility, 4)) })
      output$probability <- renderText({ paste("Probability of Option Being Exercised:", round(probability, 4)) })
      
      # Create probability plot
prob_df <- do_that_thang(input$symbol,last_close,volatility,input$start_date, input$end_date,
                         input$risk_free_rate, input$days_to_expiry,
                         0, .99,1000, input$option_type)

        output$probability_plot <- renderPlot({
        ggplot(prob_df, aes(x = strike, y = prob)) +
          geom_line(color = "blue", linewidth = 1) +
          geom_vline(aes(xintercept = input$strike_price, color = "Strike"), linetype = "dashed") +
          geom_vline(aes(xintercept = last_close, color = "Latest"), linetype = "dashed") +
          labs(title = "Probability of Option Being Exercised",
               x = "Stock Price",
               y = "Probability") +
            xlab(c("strike price")) +
            ylab(c("Probability of assignment")) +
            ylim(c(0,1)) +
          theme_minimal(base_size = 15) +
          scale_color_manual(name = "Prices", values = c(Latest = "blue", Strike = "red"))
      })
           
          prob_df$prob <- round(prob_df$prob,2)
          
          output$prob_table <- renderDT({
            prob_df <- prob_df[, c("strike", "prob")]
            datatable(prob_df, 
                      options = list(pageLength = 40,
                                     dom = 'tp'),
                      rownames = FALSE )               
          })
    }, error = function(e) {
      output$volatility <- renderText({ "Error calculating values. Please check inputs." })
      output$probability <- renderText({ "Error calculating values. Please check inputs." })
    })
  })
}

shinyApp(ui = ui, server = server)

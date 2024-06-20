# Install and load necessary packages
install.packages("quantmod")
install.packages("prophet")
library(quantmod)
library(prophet)

# Load historical stock prices for Apple Inc. (AAPL)
getSymbols("AAPL", src = "yahoo", from = "2010-01-01", to = Sys.Date())

# Inspect the structure of the data
str(AAPL)
summary(AAPL)

# Convert the data to a data frame
stock_data <- data.frame(date = index(AAPL), coredata(AAPL))
head(stock_data)

# Select the necessary columns and rename them
stock_data <- stock_data[, c("date", "AAPL.Open", "AAPL.High", "AAPL.Low", "AAPL.Close", "AAPL.Volume", "AAPL.Adjusted")]
colnames(stock_data) <- c("date", "open", "high", "low", "close", "volume", "adjusted")

# Install and load ggplot2
install.packages("ggplot2")
library(ggplot2)

# Plot the closing prices over time
ggplot(stock_data, aes(x = date, y = close)) +
  geom_line(color = "blue") +
  labs(title = "AAPL Closing Prices Over Time", x = "Date", y = "Closing Price") +
  theme_minimal()

# Calculate moving averages
stock_data$ma50 <- rollapply(stock_data$close, width = 50, FUN = mean, align = "right", fill = NA)
stock_data$ma200 <- rollapply(stock_data$close, width = 200, FUN = mean, align = "right", fill = NA)

# Plot closing prices with moving averages
ggplot(stock_data, aes(x = date)) +
  geom_line(aes(y = close, color = "Close")) +
  geom_line(aes(y = ma50, color = "50-Day MA")) +
  geom_line(aes(y = ma200, color = "200-Day MA")) +
  labs(title = "AAPL Closing Prices with Moving Averages", x = "Date", y = "Price") +
  scale_color_manual(values = c("Close" = "blue", "50-Day MA" = "red", "200-Day MA" = "green")) +
  theme_minimal()

# Prepare the data for prophet
prophet_data <- stock_data[, c("date", "close")]
colnames(prophet_data) <- c("ds", "y")

# Fit the prophet model
m <- prophet(prophet_data)

# Make future dataframe for predictions
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# Plot the forecast
plot(m, forecast) +
  labs(title = "AAPL Stock Price Forecast")

# Plot the forecast components
prophet_plot_components(m, forecast)

# Merge the forecasted data with the actual data
forecasted_data <- merge(stock_data, forecast[, c("ds", "yhat")], by.x = "date", by.y = "ds", all.x = TRUE)

# Plot actual vs. forecasted values
ggplot(forecasted_data, aes(x = date)) +
  geom_line(aes(y = close, color = "Actual")) +
  geom_line(aes(y = yhat, color = "Forecasted")) +
  labs(title = "AAPL Actual vs. Forecasted Prices", x = "Date", y = "Price") +
  scale_color_manual(values = c("Actual" = "blue", "Forecasted" = "red")) +
  theme_minimal()

# Install and load shiny
install.packages("shiny")
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("AAPL Stock Price Prediction"),
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Select Date:", value = Sys.Date())
    ),
    mainPanel(
      plotOutput("forecastPlot"),
      textOutput("pricePrediction")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$forecastPlot <- renderPlot({
    plot(m, forecast) +
      labs(title = "AAPL Stock Price Forecast")
  })
  
  output$pricePrediction <- renderText({
    selected_date <- as.Date(input$date)
    prediction <- forecast[forecast$ds == selected_date, "yhat"]
    if (length(prediction) == 0) {
      return("No prediction available for the selected date.")
    } else {
      return(paste("Predicted Closing Price on", selected_date, ":", round(prediction, 2)))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)



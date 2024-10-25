#Step 1: Install and Load Required Libraries
install.packages("quantmod")
library(quantmod)
#Step 2: Specify the Ticker Symbols and Date Range
stocks <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
start_date <- "2021-01-04"
end_date <- Sys.Date()  # Use Sys.Date() for today's date
#Step 3: Download Historical Prices
getSymbols(stocks, src = "yahoo", from = start_date, to = end_date)
#Step 4: Extract Adjusted Closing Prices
prices <- NULL
for (stock in stocks) {
  prices <- cbind(prices, Cl(get(stock)))
}
colnames(prices) <- stocks
#Step 5: Data Visualization (Optional)
install.packages("ggplot2")
library(ggplot2)
install.packages("xts")
library(xts)
ggplot(data = prices, aes(x = index(prices))) +
  geom_line(aes(y = AAPL, color = "AAPL")) +
  geom_line(aes(y = MSFT, color = "MSFT")) +
  geom_line(aes(y = GOOGL, color = "GOOGL")) +
  geom_line(aes(y = AMZN, color = "AMZN")) +
  geom_line(aes(y = TSLA, color = "TSLA")) +
  labs(title = "Historical Stock Prices", y = "Closing Price", x = "Date") +
  scale_color_manual(values = c("AAPL" = "blue", "MSFT" = "red", "GOOGL" = "green", "AMZN" = "purple", "TSLA" = "orange"))
#Step 6: Export Prices to Excel
install.packages("openxlsx")
library(openxlsx)
# Assuming 'prices' is your data frame
dates <- index(prices)
# Create a data frame with dates and stock prices
data_to_export <- data.frame(Date = as.Date(dates), coredata(prices))
# Export to Excel
write.xlsx(data_to_export, "stock_prices_with_dates.xlsx")


###################################################################
# AAPL returns
aapl<-data.frame(data_to_export[,2])
n <- nrow(aapl)
aapl_returns <- ((aapl[1:(n-1), 1] - aapl[2:n, 1])/aapl[2:n, 1])

# Print or view AAPL returns
aapl_returns

# MSFT returns
msft <- data.frame(data_to_export[, "MSFT"])
n_msft <- nrow(msft)
msft_returns <- ((msft[1:(n_msft-1), 1] - msft[2:n_msft, 1]) / msft[2:n_msft, 1])

# Print or view MSFT returns
print(msft_returns)

# GOOGL returns
googl <- data.frame(data_to_export[, "GOOGL"])
n_googl <- nrow(googl)
googl_returns <- ((googl[1:(n_googl-1), 1] - googl[2:n_googl, 1]) / googl[2:n_googl, 1])

# Print or view GOOGL returns
print(googl_returns)

# AMZN returns
amzn <- data.frame(data_to_export[, "AMZN"])
n_amzn <- nrow(amzn)
amzn_returns <- ((amzn[1:(n_amzn-1), 1] - amzn[2:n_amzn, 1]) / amzn[2:n_amzn, 1])

# Print or view AMZN returns
print(amzn_returns)

# TSLA returns
tsla <- data.frame(data_to_export[, "TSLA"])
n_tsla <- nrow(tsla)
tsla_returns <- ((tsla[1:(n_tsla-1), 1] - tsla[2:n_tsla, 1]) / tsla[2:n_tsla, 1])

# Print or view TSLA returns
print(tsla_returns)
#############################################################

# MSFT volatility
msft <- data.frame(data_to_export[, "MSFT"])
n_msft <- nrow(msft)
msft_returns <- ((msft[1:(n_msft-1), 1] - msft[2:n_msft, 1]) / msft[2:n_msft, 1])
msft_volatility <- sd(msft_returns, na.rm = TRUE)

# Print or view MSFT volatility
print(msft_volatility)

# GOOGL volatility
googl <- data.frame(data_to_export[, "GOOGL"])
n_googl <- nrow(googl)
googl_returns <- ((googl[1:(n_googl-1), 1] - googl[2:n_googl, 1]) / googl[2:n_googl, 1])
googl_volatility <- sd(googl_returns, na.rm = TRUE)

# Print or view GOOGL volatility
print(googl_volatility)

# AMZN volatility
amzn <- data.frame(data_to_export[, "AMZN"])
n_amzn <- nrow(amzn)
amzn_returns <- ((amzn[1:(n_amzn-1), 1] - amzn[2:n_amzn, 1]) / amzn[2:n_amzn, 1])
amzn_volatility <- sd(amzn_returns, na.rm = TRUE)

# Print or view AMZN volatility
print(amzn_volatility)

# TSLA volatility
tsla <- data.frame(data_to_export[, "TSLA"])
n_tsla <- nrow(tsla)
tsla_returns <- ((tsla[1:(n_tsla-1), 1] - tsla[2:n_tsla, 1]) / tsla[2:n_tsla, 1])
tsla_volatility <- sd(tsla_returns, na.rm = TRUE)

# Print or view TSLA volatility
print(tsla_volatility)
#######################################################

# Create a data frame with returns
returns_data <- data.frame(
  AAPL = aapl_returns,
  MSFT = msft_returns,
  GOOGL = googl_returns,
  AMZN = amzn_returns,
  TSLA = tsla_returns
)

# Calculate correlations
correlation_matrix <- cor(returns_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Install and load necessary packages
install.packages("corrplot")
library(corrplot)

# Plot a heatmap of the correlation matrix
corrplot(correlation_matrix, method = "number")
##########################################################

#Question2:
covariance_matrix <- cov(returns_data, use = "complete.obs")
print(covariance_matrix)

#######################
# export covariance into excel
covariance_df <- as.data.frame(covariance_matrix)

# Specify the file path for Excel export
excel_file <- "covariance_matrix.xlsx"

# Write the data frame to Excel
write.xlsx(covariance_df, excel_file, sheetName = "CovarianceMatrix", rowNames = TRUE)

######################

install.packages("openxlsx")
library(openxlsx)
write.xlsx(covariance_matrix, "covariance_matrix.xlsx")

# Calculate the mean (average) of daily returns for each stock
average_daily_ret <- data.frame(
  AAPL = mean(aapl_returns, na.rm = TRUE),
  MSFT = mean(msft_returns, na.rm = TRUE),
  GOOGL = mean(googl_returns, na.rm = TRUE),
  AMZN = mean(amzn_returns, na.rm = TRUE),
  TSLA = mean(tsla_returns, na.rm = TRUE)
)

# Print the average daily returns
print(average_daily_ret)



# Calculate the daily variance of the assets
variance_daily<-data.frame(
  var(aapl_returns),var(msft_returns),var(googl_returns),var(amzn_returns), var(tsla_returns))
names(variance_daily)<-c("aapl_returns","msft_returns","googl_returns","amzn_returns", "tsla_returns")
print(variance_daily)

## To compute the annualized returns of the assets
average_annual_ret<-data.frame(mean(aapl_returns)*252,mean(msft_returns)*252,mean(googl_returns)*252,mean(amzn_returns)*252, mean(tsla_returns)*252)
names(average_annual_ret)<-c("aapl_returns","msft_returns","googl_returns","amzn_returns", "tsla_returns")
print(average_annual_ret)

## To compute the annualized variance of the assets
variance_annual<-data.frame(var(aapl_returns)*252,mean(msft_returns)*252,mean(googl_returns)*252,mean(amzn_returns)*252, mean(tsla_returns)*252)
names(variance_annual)<-c("aapl_returns","msft_returns","googl_returns","amzn_returns", "tsla_returns")
print(variance_annual)
#############################################################################
# Assuming 'covariance_matrix' is your calculated covariance matrix
# Assuming 'returns_data' is your data frame with daily returns

# Extract the covariance matrix
covariance_matrix <- cov(returns_data[, -1], use = "complete.obs")

# Extract the vector of mean returns
mean_returns <- colMeans(returns_data[, -1], na.rm = TRUE)

# Target annual return
target_return <- 0.25  # Replace with your desired target return

# Number of assets
num_assets <- length(mean_returns)

# Set up the parameters for quadratic programming
Dmat <- 2 * as.matrix(covariance_matrix)  # Covariance matrix
dvec <- numeric(num_assets)
Amat <- cbind(rep(1, num_assets), mean_returns)
bvec <- c(target_return, 1)

# Solve the quadratic programming problem
library(quadprog)
portfolio_weights <- solve.QP(Dmat, dvec, Amat, bvec = bvec, meq = 1)$solution

# Print the optimal portfolio weights
print(portfolio_weights)

# Calculate the expected portfolio return and risk
expected_portfolio_return <- sum(portfolio_weights * mean_returns)
portfolio_risk <- sqrt(t(portfolio_weights) %*% Dmat %*% portfolio_weights)

# Print the results
cat("Optimal Portfolio Weights:", portfolio_weights, "\n")
cat("Expected Portfolio Return:", expected_portfolio_return, "\n")
cat("Portfolio Risk (Standard Deviation):", portfolio_risk, "\n")
########################################################
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")


portfolioPrices <- NULL
for(ticker in stock_tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2021-01-04', periodicity = 'daily', auto.assign=FALSE)[,4])
}

pportfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type="box", min=.10, max=.40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")

library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)

weights(optPort)
chart.Weights(optPort)
ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)
######################################################################################################################

# Load necessary libraries for ARCH
library(quantmod)
library(rugarch)
# Function to fit and summarize an ARCH model
fit_arch_model <- function(returns, order = c(1, 0)) {
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = order),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
  fit <- ugarchfit(spec = spec, data = returns)
  return(fit)
}
# Load the data for a single stock (replace "AAPL" with the stock symbol)
symbol <- "AAPL"
getSymbols(symbol, src = "yahoo", from = "2020-01-01", to = Sys.Date())
stock_data <- Cl(get(symbol))
# Calculate returns
returns <- diff(log(stock_data), lag = 1)
# Remove NA values
returns <- na.omit(returns)
# Fit different ARCH models
arch11_fit <- fit_arch_model(returns, order = c(1, 0))
arch22_fit <- fit_arch_model(returns, order = c(2, 0))
arch33_fit <- fit_arch_model(returns, order = c(3, 0))
# Compare models using AIC
aic_values <- c(arch11 = infocriteria(arch11_fit)[1],
                arch22 = infocriteria(arch22_fit)[1],
                arch33 = infocriteria(arch33_fit)[1])
best_model_name <- names(which.min(aic_values))
best_model_fit <- switch(best_model_name,
                         arch11 = arch11_fit,
                         arch22 = arch22_fit,
                         arch33 = arch33_fit)
# Print results
cat("AIC Values:\n", aic_values, "\n")
cat("Best Model:", best_model_name, "\n")
print(summary(best_model_fit))
# Extract conditional volatility from the best-fitted model
volatility <- sigma(best_model_fit)
# Plot the volatility
plot(volatility, type = "l", col = "blue", lwd = 2, main = paste("Conditional Volatility (", best_model_name, ")"))
##################################################################################
# Load necessary libraries for GARCH
library(rugarch)
# Function to fit and summarize a GARCH model
fit_garch_model <- function(returns, model_type, order = c(1, 1)) {
  spec <- ugarchspec(variance.model = list(model = model_type, garchOrder = order),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
  fit <- ugarchfit(spec = spec, data = returns)
  return(fit)
}
# Load the data for a single stock (replace "AAPL" with the stock symbol)
symbol <- "AAPL"
getSymbols(symbol, src = "yahoo", from = "2020-01-01", to = Sys.Date())
stock_data <- Cl(get(symbol))
# Calculate returns
returns <- diff(log(stock_data), lag = 1)
# Remove NA values
returns <- na.omit(returns)
# Fit different GARCH models
garch11_fit <- fit_garch_model(returns, "sGARCH")
egarch11_fit <- fit_garch_model(returns, "eGARCH")
tgarch11_fit <- fit_garch_model(returns, "gjrGARCH")
# Compare models using AIC
aic_values <- c(garch11 = infocriteria(garch11_fit)[1],
                egarch11 = infocriteria(egarch11_fit)[1],
                tgarch11 = infocriteria(tgarch11_fit)[1])
best_model_name <- names(which.min(aic_values))
best_model_fit <- switch(best_model_name,
                         garch11 = garch11_fit,
                         egarch11 = egarch11_fit,
                         tgarch11 = tgarch11_fit)
# Print results
cat("AIC Values:\n", aic_values, "\n")
cat("Best Model:", best_model_name, "\n")
print(summary(best_model_fit))
# Extract conditional volatility from the best-fitted model
volatility <- sigma(best_model_fit)
# Plot the volatility
plot(volatility, type = "l", col = "blue", lwd = 2, main = "Conditional Volatility")
plot(volatility, type = "l", col = "blue", lwd = 2, main = paste("Conditional Volatility (", best_model_name, ")"))
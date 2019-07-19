library(tidyr)
library(ggplot2)
options(scipen = 1e9)

set.seed(123)

price <- 100
tradingdays <- 252
length_years <- 10
n_simulations <- 10000
expected_return <- 0.087
monthly_reinvestment <- 100

# Make data frames for holding the simulation results, temp keeps the
# simulations temporarily and is replaced after every simulation
temp <- as.data.frame(matrix(nrow = tradingdays * length_years, ncol = 1))
twenty <- thirty <- as.data.frame(matrix(nrow = n_simulations, ncol = 1))

# Set the starting price for each simulation
temp[1, ] <- price
# Monte Carlo simulation of returns using 20% volatility
volatility <- 0.2
for (i in 1:n_simulations) {
    for (j in 1:I(tradingdays * length_years - 1)) {
        # If month start, then invest more by monthly_reinvestment
        temp[j + 1, 1] <- (j %% 22 == 0 | j == 1) *
            monthly_reinvestment +
            temp[j, 1] +
            temp[j, 1] *
            (expected_return / tradingdays +
                 sqrt(volatility / tradingdays) *
                 qnorm(runif(1), 0, 1))
    }
    twenty[i, 1] <- as.numeric(tail(temp, 1))
}
twenty <- unname(unlist(twenty))

# Monte Carlo simulation of returns using 30% volatility
volatility <- 0.3
for (i in 1:n_simulations) {
    for (j in 1:I(tradingdays * length_years - 1)) {
        # If month start, then invest more by monthly_reinvestment
        temp[j + 1, 1] <- (j %% 22 == 0 | j == 1) *
            monthly_reinvestment +
            temp[j, 1] +
            temp[j, 1] *
            (expected_return / tradingdays +
                 sqrt(volatility / tradingdays) *
                 qnorm(runif(1), 0, 1))
    }
    thirty[i, 1] <- as.numeric(tail(temp, 1))
}
thirty <- unname(unlist(thirty))

# Combine & label data
results <- means <- as.data.frame(cbind(twenty, thirty))
colnames(results) <- c("Volatility 20%", "Volatility 30%")

# Gather and rename
to_plot <- gather(results)
colnames(to_plot) <- c("Volatility", "Return")

vlines <- data.frame(Volatility = c("Volatility 20%", "Volatility 30%"),
                     Means = c(mean(means[, 1]), mean(means[, 2])))

# Plot the results
ggplot(to_plot, aes(x = Return)) +
    geom_histogram(aes(y = ..count.., fill = Volatility)) +
    facet_grid(Volatility ~.) +
    # Add lines for the average returns of each volatility
    geom_vline(data = vlines, aes(xintercept = Means),
               color = "#00000080",
               size = 2) +
    xlim(0, 50000) +
    ylab("Count") +
    ggtitle("Monte Carlo simulation of investment returns for different volatilities") +
    labs(subtitle = paste0("Investing $", monthly_reinvestment,
                           " monthly with a mean return of ", expected_return * 100,
                           "% for ", length_years, " years, ", n_simulations,
                           " simulations"),
         caption = paste0("Gray bars mark mean ending value of investment, ", 
                         "outliers with extremely high values are not plotted\n",
         "Source: databasedinvesting.blogspot.com")) +
    theme(plot.caption = element_text(hjust = 0), legend.position = "none")
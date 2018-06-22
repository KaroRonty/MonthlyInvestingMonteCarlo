library(ggplot2)
library(reshape2) # melt function
options(scipen = 10000000)
set.seed(123)

price <- 100
tradingdays <- 252
length_years <- 10
n_simulations <- 1000
expected_return <- 0.087
monthy_reinvestment <- 100

# Make data frames y and z for the simulation results
# x keeps the simulations temporarily and is replaced after every simulation
x <- as.data.frame(matrix(nrow = 252 * length_years, ncol = 1))
y <- as.data.frame(matrix(nrow = n_simulations, ncol = 1))
z <-y

# Set the starting price for each simulation
x[1,] <- price

# Monte Carlo simulation of returns using 20% volatility
volatility <- 0.2
for (i in 1:n_simulations){
	for (j in 1:I(tradingdays * length_years - 1)){
		# If month start, then invest more by monthy_reinvestment
		x[j+1,1] <- (j %% 22 == 0 | j == 1) * monthy_reinvestment + x[j,1] + x[j,1] *
		(expected_return / 252 + sqrt(volatility / 252) * qnorm(runif(1),0,1))
	}
	y[i,1] <- as.numeric(tail(x,1))
}
y <- unname(unlist(y))

# Monte Carlo simulation of returns using 30% volatility
volatility <- 0.3
for (i in 1:n_simulations){
	for (j in 1:I(tradingdays * length_years - 1)){
		# If month start, then invest more by monthy_reinvestment
		x[j+1,1] <- (j %% 22 == 0 | j == 1) * monthy_reinvestment + x[j,1] + x[j,1] *
		(expected_return / 252 + sqrt(volatility / 252) * qnorm(runif(1),0,1))
	}
	z[i,1] <- as.numeric(tail(x,1))
}
# Combine & format data
z <- unname(unlist(z))
z <- cbind(y,z)
colnames(z) <- c("Volatility 20%", "Volatility 30%")
# To calculate mean returns
temp <- z
# Filter simulation results for better plotting
z[z > 50000] <- NA
d <- melt(z)
d$Var1 <- NULL
colnames(d) <- c("Volatility", "Return")

# Plot the results
ggplot(d, aes(x = Return)) +
    geom_histogram(aes(y = ..count.., fill = Volatility)) +
    facet_grid(Volatility~.) +
	# Add lines for the average returns of each volatility
	geom_vline(xintercept=mean(temp[,1]),size=2,color=rgb(248/255,117/255,108/255,0.5)) +
	geom_vline(xintercept=mean(temp[,2]),size=2,color=rgb(1/255,191/255,196/255,0.5))

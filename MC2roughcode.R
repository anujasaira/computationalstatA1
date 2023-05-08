library(ggplot2)

# Function to sample X from Pareto distribution
sample_X<- function(n, gamma){
  u<- runif(n)
  x<-1/((1-u)^(1/gamma))
  return(x)
}


# Function to sample Y = log(X)
sample_Y <- function(size,gamma) {
  X <- sample_X(gamma, size)
  y<- log(x)
  return(y)
}

# Set the gamma values to explore
gamma_values <- c(1, 2, 5)

# Set the sample size
sample_size <- 10000
x1=sample_X(10000,40)
y1=sample_Y(10000,40)

# Generate samples for different gamma values and plot histograms
for (gamma in gamma_values) {
  # Generate samples of Y = log(X)
  Y <- sample_Y(gamma, sample_size)
  
  # Create a histogram plot of Y
  hist_plot <- ggplot(data = data.frame(Y), aes(x = Y)) +
    geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
    labs(title = paste0("Histogram of Y (gamma =", gamma, ")"),
         x = "Y", y = "Frequency")
  
  # Plot the histogram
  print(hist_plot)
  
  # Create a density plot of Y
  density_plot <- ggplot(data = data.frame(Y), aes(x = Y)) +
    geom_density(fill = "steelblue", color = "white") +
    labs(title = paste0("Density Plot of Y (gamma =", gamma, ")"),
         x = "Y", y = "Density")
  
  # Plot the density
  print(density_plot)
}

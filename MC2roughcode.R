library(ggplot2)
library(gridExtra)

# Define the parameter values for gamma
gamma_values <- c(1, 2, 5)

# Create an empty list to store the plots
plots <- list()

# Loop over the gamma values
for (gamma in gamma_values) {
  # Generate samples for X from Pareto distribution
  n <- 1000
  X <- 1 / rgamma(n, shape = gamma)
  
  # Transform X to Y = log(X)
  Y <- log(X)
  
  # Create a data frame
  df <- data.frame(X = X, Y = Y)
  
  # Create the histogram plot for Y
  hist_plot <- ggplot(df, aes(x = Y)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.2, color = "black", fill = "skyblue") +
    geom_density(color = "red") +
    labs(title = paste0("Histogram and Density (γ =", gamma, ")"), x = "Y", y = "Density") +
    theme_minimal()
  
  # Create the histogram plot for X
  density_plot <- ggplot(df, aes(x = X)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "skyblue") +
    geom_density(color = "red") +
    labs(title = paste0("Histogram and Density (γ =", gamma, ")"), x = "X", y = "Density") +
    theme_minimal()
  
  # Store the plots in the list
  plots[[gamma]] <- list(hist_plot, density_plot)
}


plots[1]+plots[2]

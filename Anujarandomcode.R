# Set the seed for reproducibility
set.seed(42)

# Generate random variable Z using Box-Muller transform
u1 <- runif(1000)
u2 <- runif(1000)
z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)

# Calculate the corresponding true normal distribution values
quantiles <- seq(0.001, 1, by = 0.001)
true_normal <- qnorm(quantiles)
quantiles
# Calculate the empirical cumulative distribution function (ECDF) for the generated Z values
ecdf_z <- ecdf(z)
empirical_probs <- ecdf_z(z)

length(quantiles)
# Calculate the true cumulative distribution function (CDF) values
true_probs <- pnorm(z)

data <- data.frame(Quantiles = quantiles,
                   Empirical_Probabilities = empirical_probs,
                   True_Probabilities = true_probs)

# Plotting
ggplot(data, aes(x = Quantiles)) +
  geom_line(aes(y = Empirical_Probabilities, color = "Box-Muller")) +
  geom_line(aes(y = True_Probabilities, color = "True Normal")) +
  labs(title = "Comparison of Box-Muller and True Normal Distributions",
       x = "Quantiles",
       y = "Probability") +
  scale_color_manual(values = c("Box-Muller" = "blue", "True Normal" = "red")) +
  theme_minimal()



ggplot(data, aes(x = Quantiles)) +
  geom_line(aes(y = True_Probabilities, color = "True Normal"))





# Set the parameter for Pareto distribution
gamma <- 2

# Function to sample X from Pareto distribution
sample_X <- function() {
  return(rpareto(1, gamma))
}

# Function to sample Y = log(X)
sample_Y <- function() {
  x <- sample_X()
  return(log(x))
}

# Sample X and Y
x <- sample_X()
y <- sample_Y()

# Print the sampled values
print(paste("X:", x))
print(paste("Y:", y))





# Required packages
library(ggplot2)
library(ggthemes)
library(ggpubr)

# Function to sample from Pareto distribution
pareto_sampler <- function(n, gamma) {
  u <- runif(n)
  x <- (1/u)^(1/gamma)
  return(x)
}

# Function to calculate Y = log(X)
y_transform <- function(x) {
  y <- log(x)
  return(y)
}




# Required packages
library(ggplot2)
library(ggthemes)
library(ggpubr)

# Function to sample from Pareto distribution
pareto_sampler <- function(n, gamma) {
  u <- runif(n)
  x <- (1/u)^(1/gamma)
  return(x)
}

# Function to calculate Y = log(X)
y_transform <- function(x) {
  y <- log(x)
  return(y)
}

# Generate samples and transform
gamma_values <- c(1, 2, 5)  # Example gamma values
n_samples <- 1000  # Number of samples to generate

sample_data <- data.frame()

for (gamma in gamma_values) {
  x <- pareto_sampler(n_samples, gamma)
  y <- y_transform(x)
  data <- data.frame(x = x, y = y, gamma = as.factor(gamma))
  sample_data <- rbind(sample_data, data)
}

# Plot histogram and density
histogram_plot <- ggplot(sample_data, aes(x = y, fill = gamma)) +
  geom_histogram(binwidth = 0.5, alpha = 0.7) +
  facet_wrap(~gamma, ncol = 1) +
  labs(title = "Histogram of Y = log(X) for different gamma values",
       x = "Y", y = "Count") +
  theme_bw()

density_plot <- ggplot(sample_data, aes(x = y, fill = gamma)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~gamma, ncol = 1) +
  labs(title = "Density Plot of Y = log(X) for different gamma values",
       x = "Y", y = "Density") +
  theme_bw()

# Combine plots
combined_plot <- ggarrange(histogram_plot, density_plot,
                           nrow = 2, ncol = 1,
                           common.legend = TRUE,
                           legend = "right")

# Display the combined plot
print(combined_plot)





# Load required library
library(ggplot2)

# Function to generate samples from Pareto distribution
generate_samples <- function(gamma, n) {
  return(1 / (runif(n)^(1/gamma)))
}

# Generate samples and compute Y = log(X)
gamma_values <- c(1, 2, 5)  # Different values of gamma
num_samples <- 10000        # Number of samples to generate

# Create a list to store the samples and their transformed values
samples_list <- list()

for (gamma in gamma_values) {
  X <- generate_samples(gamma, num_samples)
  Y <- log(X)
  samples_list[[as.character(gamma)]] <- Y
}

# Plot histogram and density for each gamma value
for (gamma in gamma_values) {
  # Create a new plot
  plot_title <- paste("Histogram and Density Plot (γ =", gamma, ")", sep=" ")
  p <- ggplot(data.frame(x = samples_list[[as.character(gamma)]]), aes(x = x)) +
    theme_minimal() +
    ggtitle(plot_title) +
    labs(x = "Y", y = "Density") +
    geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white", alpha = 0.7) +
    geom_density(color = "red", size = 0.7)
  
  # Display the plot
  print(p)
}



library(ggplot2)

# Create sample data
data <- matrix(runif(100), nrow = 10)

# Convert data to a data frame
df <- as.data.frame(data)

# Add row and column names
df$row <- paste0("Row", 1:10)
df$column <- paste0("Column", 1:10)

# Reshape the data to long format
df_long <- reshape2::melt(df, id.vars = c("row", "column"))

# Set up the plot
ggplot(df_long, aes(x = column, y = row, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_hline(yintercept = 1:(nrow(df) + 1), color = "gray80", size = 0.5) +
  geom_vline(xintercept = 1:(ncol(df) + 1), color = "gray80", size = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())


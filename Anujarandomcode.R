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
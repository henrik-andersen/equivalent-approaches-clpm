# Simulated example: Multiple time series 
# Henrik Andersen 
# 26.04.2021
# --- 

# Packages
library(ggplot2)

# Clear global environment
rm(list = ls())

# Set a seed for replicability 
set.seed(1234)

# Set sample parameters 
n <- 10000
rho <- 0.5

# Initial realization is y0 ~ N(20, 1)
y0 <- rnorm(n = n, mean = 0, sd = 1)

# Individual effects are eta ~ N(5, 1)
eta <- rnorm(n = n, mean = 5, sd = 1)

# Create the errors separately, distributed as nu_t ~ N(0, sd = 0.5)  
v1 <- rnorm(n = n, mean = 0, sd = 0.5)
v2 <- rnorm(n = n, mean = 0, sd = 0.5)
v3 <- rnorm(n = n, mean = 0, sd = 0.5)
v4 <- rnorm(n = n, mean = 0, sd = 0.5)
v5 <- rnorm(n = n, mean = 0, sd = 0.5)
v6 <- rnorm(n = n, mean = 0, sd = 0.5)
v7 <- rnorm(n = n, mean = 0, sd = 0.5)
v8 <- rnorm(n = n, mean = 0, sd = 0.5)
v9 <- rnorm(n = n, mean = 0, sd = 0.5)
v10 <- rnorm(n = n, mean = 0, sd = 0.5)
v11 <- rnorm(n = n, mean = 0, sd = 0.5)
v12 <- rnorm(n = n, mean = 0, sd = 0.5)
v13 <- rnorm(n = n, mean = 0, sd = 0.5)
v14 <- rnorm(n = n, mean = 0, sd = 0.5)
v15 <- rnorm(n = n, mean = 0, sd = 0.5)

# Generate the observed variables based on 
# y_t = eta + rho*y_t-1 + nu_t
y1 <- eta + rho*y0 + v1
y2 <- eta + rho*y1 + v2
y3 <- eta + rho*y2 + v3
y4 <- eta + rho*y3 + v4
y5 <- eta + rho*y4 + v5
y6 <- eta + rho*y5 + v6
y7 <- eta + rho*y6 + v7
y8 <- eta + rho*y7 + v8
y9 <- eta + rho*y8 + v9
y10 <- eta + rho*y9 + v10
y11 <- eta + rho*y10 + v11
y12 <- eta + rho*y11 + v12
y13 <- eta + rho*y12 + v13
y14 <- eta + rho*y13 + v14
y15 <- eta + rho*y14 + v15

# Put the observed variables together into a dataframe
df <- data.frame(id = 1:n, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15)
# Make a second dataframe with eta 
df2 <- data.frame(id = 1:n, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, eta)

# Make alpha 
alpha <- (1 - rho)^-1*eta

# Make epsilon_t 
e1 <- y1 - alpha
e2 <- y2 - alpha
e3 <- y3 - alpha
e4 <- y4 - alpha
e5 <- y5 - alpha
e6 <- y6 - alpha
e7 <- y7 - alpha
e8 <- y8 - alpha
e9 <- y9 - alpha
e10 <- y10 - alpha
e11 <- y11 - alpha
e12 <- y12 - alpha
e13 <- y13 - alpha
e14 <- y14 - alpha
e15 <- y15 - alpha

# Means, variances, covariances at different stages 
mean(df$y0); mean(df$y5); mean(df$y14)
var(df$y0); var(df$y5); var(df$y14)
cov(df$y0, df$y1); cov(df$y5, df$y6); cov(df$y14, df$y15)
cov(df2$y0, df2$eta); cov(df2$y5, df2$eta); cov(df2$y15, df2$eta)

# Theoretical values  
(1 - rho)^-1*5                          # Mean yt
(1 - rho)^-2*1 - (0.25/(rho^2 - 1))     # Variance yt
(1 - rho)^-2*1 - rho*(0.25/(rho^2 - 1)) # Covariance yt, yt+1
(1 - rho)^-1*1                          # Covariance yt, eta

# Sample 5 cases randomly
samp_n <- 5
urn <- sample(1:n, size = samp_n, replace = FALSE)

# Subset only those randomly chosen case 
df_samp <- subset(df, rownames(df) %in% urn)

# Plot individual trajectories 
library(reshape2)

# Turn df_samp into a long format dataframe for ggplot  
df_sampl <- melt(df_samp, id.vars = "id")

# Rename columns
names(df_sampl) <- c("id", "t", "y")

# Recode time 
df_sampl$t <- rep(0:15, each = samp_n)

# Plot the cases 
ggplot(df_sampl, aes(x = t, y = y, color = factor(id), shape = factor(id))) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(name = "y", breaks = seq(0, 30, 2)) + 
  scale_x_continuous(name = "Time", breaks = 0:15) + 
  scale_color_discrete(name = "Case #") + 
  scale_shape_discrete(name = "Case #")

# Make aggregate plots
yt_means <- colMeans(df)[2:length(df)]; yt_means
yt_vars  <- sapply(df[2:length(df)], FUN = var, na.rm = TRUE); yt_vars
df3 <- df[2:length(df)]
yt_covs  <- c(rep(NA, length(df3)))
for(i in 2:16) {
  yt_covs[i] <- cov(df3[i], df3[i - 1])
}; yt_covs
yt_eta_covs <- c(cov(df[2:length(df)], eta)); yt_eta_covs

df_agg <- data.frame(t = 0:15, yt_means, yt_vars, yt_covs, yt_eta_covs); df_agg

# --- Plot each 

# Set parameters for plots 
mu_eta <- 5
sigma2_eta <- 1
# mu_y0 <- 1
# sigma2_y0 <- 1
# mu_nu_t <- 0
sigma2_nu_t <- 0.25
rho <- 0.5

# Means
ggplot(df_agg, aes(x = t, y = yt_means)) + 
  geom_point(size = 2.5, shape = 1) + 
  geom_abline(aes(intercept = (1 - rho)^-1*mu_eta, slope = 0)) + 
  scale_x_continuous(name = "Time", limits = c(0, 15), breaks = 0:15) + 
  scale_y_continuous(name = "mean(y)", limits = c(0, 12), breaks = 0:12) +
  theme(axis.line = element_line(size = 0.5, color = "black"), 
        text = element_text(size = 20))

# Variances 
ggplot(df_agg, aes(x = t, y = yt_vars)) + 
  geom_point(size = 2.5, shape = 1) + 
  geom_abline(aes(intercept = (1 - rho)^-2*sigma2_eta - (sigma2_nu_t/(rho^2 - 1)), slope = 0)) + 
  scale_x_continuous(name = "Time", limits = c(0, 15), breaks = 0:15) + 
  scale_y_continuous(name = "var(y)", limits = c(0, 6), breaks = 0:6) +
  theme(axis.line = element_line(size = 0.5, color = "black"), 
        text = element_text(size = 20))

# Covariances
ggplot(df_agg, aes(x = t, y = yt_covs)) + 
  geom_point(size = 2.5, shape = 1) + 
  geom_abline(aes(intercept = (1 - rho)^-2*sigma2_eta - rho*(sigma2_nu_t/(rho^2 - 1)), slope = 0)) + 
  scale_x_continuous(name = "Time", limits = c(0, 15), breaks = 0:15) + 
  scale_y_continuous(name = "cov(yt,yt-1)", limits = c(0, 6), breaks = 0:6) +
  theme(axis.line = element_line(size = 0.5, color = "black"), 
        text = element_text(size = 20))

# Covariances, yt - eta
ggplot(df_agg, aes(x = t, y = yt_eta_covs)) + 
  geom_point(size = 2.5, shape = 1) + 
  geom_abline(aes(intercept = (1 - rho)^-1*sigma2_eta, slope = 0)) + 
  scale_x_continuous(name = "Time", limits = c(0, 15), breaks = 0:15) + 
  scale_y_continuous(name = "cov(yt,eta)", limits = c(0, 3), breaks = 0:3) +
  theme(axis.line = element_line(size = 0.5, color = "black"), 
        text = element_text(size = 20))

# ----
# Test function for calculating autocovariance 
# 
# x1 <- rnorm(100, 0, 1)
# x2 <- rnorm(100, 0, 1)
# x3 <- rnorm(100, 0, 1)
# 
# dftest <- data.frame(x1, x2, x3)
# 
# cov_vec <- c(NA, NA, NA)
# 
# for(i in 2:3) {
#   cov_vec[i] <- cov(dftest[i], dftest[i - 1])
# }
# cov(dftest$x1, dftest$x2)
# cov(dftest$x2, dftest$x3)
# cov_vec


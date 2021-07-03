# ---  
# Simulation: compare DPM and RI-CLPM for non-stationary data
# Henrik Andersen
# 12.06.2021 
# ---

rm(list = ls())

library(lavaan)
library(reshape2)
library(ggplot2)

# Description: 
# The following function simulates a 25 wave longitudinal dataset on a single variable.
# The process is nonstationary as the effect changes over time.
# The effect at each point in time is a random variable distributed as ~ N(mu_rho, sd_rho)
# The function runs a kind of DPM and RI-CLPM on the dataset and returns the "true" effects along with each of the estimates.

sim_func <- function(n, mu_rho, sd_rho, phase) {

  # Create the individual effects
  a <- rnorm(n, 0, 1)
  
  # Make an empty vector to hold the time-varying autoreg. effect
  b <- double(length = 26L)
  
  # Don't need the first one
  b[1] <- 0
  
  # Each b[i] is a random variable distributed as ~ N(mu_rho, sd_rho)
  for(i in 2:26) {
    b[i] <- rnorm(1, mu_rho, sd_rho)
  }
  
  # Make an empty dataframe to hold y
  y <- matrix(nrow = n, ncol = 26)
  y <- data.frame(y)
  names(y) <- paste0("y", 0:25)
  
  # Some arbitrary starting point for y0 
  y[1] <- rnorm(n, 0, 1)
  
  # Fill in the rest of the columns 
  for(i in 2:26) {
    y[i] <- b[i] * y[i - 1] + a + rnorm(n, 0, 1)
  } 
  
  # Keep the 'true' coefficients for comparison
  true_rho <- b[22:25]
  
  if(phase == "early") {
    # Run the DPM
    dpm <- '
      eta =~ 1*y2 + 1*y3 + 1*y4 + 1*y5
      y2 ~ rho2*y1
      y3 ~ rho3*y2
      y4 ~ rho4*y3
      y5 ~ rho5*y4
      eta ~~ y1
    '
    dpm.fit <- sem(dpm, y)
    # summary(dpm.fit, fit.measures = TRUE)
    
    # Run the RI-CLPM
    riclpm <- '
      alpha =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
      e1 =~ 1*y1
      e2 =~ 1*y2
      e3 =~ 1*y3 
      e4 =~ 1*y4
      e5 =~ 1*y5
      y1 ~~ 0*y1
      y2 ~~ 0*y2
      y3 ~~ 0*y3
      y4 ~~ 0*y4
      y5 ~~ 0*y5
      e2 ~ rho2*e1
      e3 ~ rho3*e2 
      e4 ~ rho4*e3 
      e5 ~ rho5*e4
      alpha ~~ 0*e1
    '
    riclpm.fit <- sem(riclpm, y)
    # summary(riclpm.fit, fit.measures = TRUE)
  } else {
    # Run the DPM
    dpm <- '
      eta =~ 1*y22 + 1*y23 + 1*y24 + 1*y25
      y22 ~ rho22*y21
      y23 ~ rho23*y22
      y24 ~ rho24*y23
      y25 ~ rho25*y24
      eta ~~ y21
    '
    dpm.fit <- sem(dpm, y)
    # summary(dpm.fit, fit.measures = TRUE)
    
    # Run the RI-CLPM
    riclpm <- '
      alpha =~ 1*y21 + 1*y22 + 1*y23 + 1*y24 + 1*y25
      e21 =~ 1*y21
      e22 =~ 1*y22
      e23 =~ 1*y23 
      e24 =~ 1*y24
      e25 =~ 1*y25
      y21 ~~ 0*y21
      y22 ~~ 0*y22
      y23 ~~ 0*y23
      y24 ~~ 0*y24
      y25 ~~ 0*y25
      e22 ~ rho22*e21
      e23 ~ rho23*e22 
      e24 ~ rho24*e23 
      e25 ~ rho25*e24
      alpha ~~ 0*e21
    '
    riclpm.fit <- sem(riclpm, y)
    # summary(riclpm.fit, fit.measures = TRUE)
  }
  
  # Save the estimates for rho from DPM 
  dpm_rho <- double(length = 4L)
  dpm_rho <- lavInspect(dpm.fit, "list")[5:8, 14]
  
  # Save the estimates for rho from RI-CLPM 
  riclpm_rho <- double(length = 4L)
  riclpm_rho <- lavInspect(riclpm.fit, "list")[16:19, 14]
  
  # Return true and estimated rhos
  return(list(true_rho = true_rho, dpm_rho = dpm_rho, riclpm_rho = riclpm_rho))
}

# Test
# sim_func(n = 1000, mu_rho = 0.2, sd_rho = 0.1)


# Running the simulation --------------------------------------------------

# One can change 
# n:      the sample size per simulated dataset 
# mu_rho: the average autoregressive effect over time
# sd_rho: the sd of the autoregressive effect, makes it time-varying (set to zero for constant effects)
# phase:  "early" runs the models on waves 1-5, "late" on waves 21-25 

# Note that the number of replications here is set to 200. Be aware that this is time-intensive
# I have not measured it, but 1000 replications takes quite a long time, for example

# Replicate the simulation 1000 times
res <- replicate(200, sim_func(n = 1000, mu_rho = 0.2, sd_rho = 0.5, phase = "early"), simplify = TRUE)

# Get the resulting effects as simple vectors
true_rho <- unlist(res["true_rho", ])
dpm_rho <- unlist(res["dpm_rho", ])
riclpm_rho <- unlist(res["riclpm_rho", ])

# Calculate the distances from the estimates to the "true" values
diff_dpm_rho <- dpm_rho - true_rho
diff_riclpm_rho <- riclpm_rho - true_rho

# Compare the DPM and RI-CLPM 
hist(diff_dpm_rho)
hist(diff_riclpm_rho)

mean(diff_dpm_rho)
mean(diff_riclpm_rho)

var(diff_dpm_rho)
var(diff_riclpm_rho)

max(diff_dpm_rho)
min(diff_dpm_rho)

max(diff_riclpm_rho)
min(diff_riclpm_rho)

# Put the resulting deviations into a dataframe for ggplot
dfres <- data.frame(diff_dpm_rho = diff_dpm_rho, diff_riclpm_rho = diff_riclpm_rho)
dfres <- melt(dfres)
head(dfres)

ggplot(dfres, aes(value, fill = variable)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 0.5)) + 
  geom_vline(xintercept = mean(subset(dfres$value, dfres$variable == "diff_riclpm_rho")), color = "#00BFC4", size = 0.75) + 
  geom_vline(xintercept = mean(subset(dfres$value, dfres$variable == "diff_dpm_rho")), color = "#F8766D", size = 0.75) 

# Remove massive outliers to see what the distribution is like 
dfres <- dfres[dfres$value > -100, ]

ggplot(dfres, aes(value, fill = variable)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 0.5)) + 
  geom_vline(xintercept = mean(subset(dfres$value, dfres$variable == "diff_riclpm_rho")), color = "#00BFC4", size = 0.75) + 
  geom_vline(xintercept = mean(subset(dfres$value, dfres$variable == "diff_dpm_rho")), color = "#F8766D", size = 0.75) 

mean(subset(dfres$value, dfres$variable == "diff_dpm_rho"))
mean(subset(dfres$value, dfres$variable == "diff_riclpm_rho"))

var(subset(dfres$value, dfres$variable == "diff_dpm_rho"))
var(subset(dfres$value, dfres$variable == "diff_riclpm_rho"))


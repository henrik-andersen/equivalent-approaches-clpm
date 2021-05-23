# --- 
# Equivalent Approaches?
# Examples: Cigarette use, alcohol use, parental monitoring in adolescents
# Henrik K. Andersen 
# 21.04.2021 
# --- 

# Packages 
library(lavaan)

# Load data 
getwd()
setwd("C:/Users/Henrik/github_projects/equivalent-approaches/r-files")
# df.Rda is a prepared dataframe 
df <- readRDS("df.Rda")

# head(df)
# colMeans(df[, c("cig97", "cig98", "cig99", "cig00", "cig01", "cig02")], na.rm = TRUE)
# sapply(df[, c("cig97", "cig98", "cig99", "cig00", "cig01", "cig02")], var, na.rm = TRUE)

# df[, c("cig97", "cig98", "cig99", "cig00", "cig01", "cig02")] <- scale(df[, c("cig97", "cig98", "cig99", "cig00", "cig01", "cig02")], scale = FALSE)
# mean(df$cig97, na.rm = TRUE)

# Simple AR(1) model with individual effects ------------------------------

# --- Residual-level 
m1a <- '
# Identify individual effects
alpha =~ 1*cig97 + 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
# Identify structured residuals
e97 =~ 1*cig97
e98 =~ 1*cig98
e99 =~ 1*cig99
e00 =~ 1*cig00
e01 =~ 1*cig01
e02 =~ 1*cig02
# Constrain variances of original variables to zero 
cig97 ~~ 0*cig97
cig98 ~~ 0*cig98
cig99 ~~ 0*cig99
cig00 ~~ 0*cig00
cig01 ~~ 0*cig01
cig02 ~~ 0*cig02
# Regressions
e98 ~ rho*e97
e99 ~ rho*e98
e00 ~ rho*e99
e01 ~ rho*e00
e02 ~ rho*e01
# Covariances, notice Cov(alpha, e97) = 0
alpha ~~ 0*e97
'
m1a.fit <- sem(m1a, data = df, missing = "ML")
summary(m1a.fit, fit.measures = TRUE, standardized = TRUE)

# --- Constrained observation-level 
m1b <- '
# Identify individual effects
etac =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02 + a*cig97
# Regressions 
cig98 ~ rho*cig97
cig99 ~ rho*cig98
cig00 ~ rho*cig99
cig01 ~ rho*cig00
cig02 ~ rho*cig01
# Constraints 
a == (1 - rho)^-1
'
m1b.fit <- sem(m1b, data = df, missing = "ML")
summary(m1b.fit, fit.measures = TRUE, standardized = TRUE)

# --- Predetermined observation-level 
m1c <- '
# Identify individual effects
etap =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
# Regressions 
cig98 ~ rho*cig97
cig99 ~ rho*cig98
cig00 ~ rho*cig99
cig01 ~ rho*cig00
cig02 ~ rho*cig01
# Covariances 
etap ~~ cig97
'
m1c.fit <- sem(m1c, data = df, missing = "ML")
summary(m1c.fit, fit.measures = TRUE, standardized = TRUE)

# --- Likelihood ratio test
anova(m1a.fit, m1b.fit, m1c.fit)

# Extended models with time-varying and invariant covariates --------------

# --- Residual-level 
m2a <- '
# Identify individual effects
alpha =~ 1*cig97 + 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
# Identify structured residuals 
e97 =~ 1*cig97
e98 =~ 1*cig98
e99 =~ 1*cig99
e00 =~ 1*cig00
e01 =~ 1*cig01
e02 =~ 1*cig02
# Constrain variances of original variables to zero 
cig97 ~~ 0*cig97
cig98 ~~ 0*cig98
cig99 ~~ 0*cig99
cig00 ~~ 0*cig00
cig01 ~~ 0*cig01
cig02 ~~ 0*cig02
# Regressions 
e98 ~ rho*e97 + beta*drink97 + theta*mon97
e99 ~ rho*e98 + beta*drink98 + theta*mon97
e00 ~ rho*e99 + beta*drink99 + theta*mon97
e01 ~ rho*e00 + beta*drink00 + theta*mon97
e02 ~ rho*e01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(alpha, e97) = Cov(alpha, mon97) = 0
alpha ~~ 0*e97 + drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
e97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
'
m2a.fit <- sem(m2a, data = df, missing = "ML")
summary(m2a.fit, fit.measures = TRUE, standardized = TRUE)

# --- Residual-level with covariance between mon and alpha
m2ax <- '
# Identify individual effects
alpha =~ 1*cig97 + 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
# Identify structured residuals 
e97 =~ 1*cig97
e98 =~ 1*cig98
e99 =~ 1*cig99
e00 =~ 1*cig00
e01 =~ 1*cig01
e02 =~ 1*cig02
# Constrain variances of original variables to zero 
cig97 ~~ 0*cig97
cig98 ~~ 0*cig98
cig99 ~~ 0*cig99
cig00 ~~ 0*cig00
cig01 ~~ 0*cig01
cig02 ~~ 0*cig02
# Regressions 
e98 ~ rho*e97 + beta*drink97 + theta*mon97
e99 ~ rho*e98 + beta*drink98 + theta*mon97
e00 ~ rho*e99 + beta*drink99 + theta*mon97
e01 ~ rho*e00 + beta*drink00 + theta*mon97
e02 ~ rho*e01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(alpha, e97) = 0
alpha ~~ 0*e97 + drink97 + drink98 + drink99 + drink00 + drink01 + mon97
e97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
'
m2ax.fit <- sem(m2ax, data = df)
summary(m2ax.fit, fit.measures = TRUE, standardized = TRUE)

# --- Constrained observation-level 
m2b <- '
# Individual effects
etac =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02 + a*cig97
# Regressions 
cig98 ~ rho*cig97 + beta*drink97 + theta*mon97
cig99 ~ rho*cig98 + beta*drink98 + theta*mon97
cig00 ~ rho*cig99 + beta*drink99 + theta*mon97
cig01 ~ rho*cig00 + beta*drink00 + theta*mon97
cig02 ~ rho*cig01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(etac, mon97) = 0
etac ~~ drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
cig97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
# Constraints 
a == (1 - rho)^-1
etac ~~ cig97
'
m2b.fit <- sem(m2b, data = df, missing = "ML")
summary(m2b.fit, fit.measures = TRUE, standardized = TRUE)

# --- Constrained observation-level with covariance between etac and mon97
m2bx <- '
# Individual effects
etac =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02 + a*cig97
# Regressions 
cig98 ~ rho*cig97 + beta*drink97 + theta*mon97
cig99 ~ rho*cig98 + beta*drink98 + theta*mon97
cig00 ~ rho*cig99 + beta*drink99 + theta*mon97
cig01 ~ rho*cig00 + beta*drink00 + theta*mon97
cig02 ~ rho*cig01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(etac, mon97) = 0
etac ~~ 0*cig97 + drink97 + drink98 + drink99 + drink00 + drink01 + mon97
cig97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
# Constraints 
a == (1 - rho)^-1
'
m2bx.fit <- sem(m2bx, data = df)
summary(m2bx.fit, fit.measures = TRUE, standardized = TRUE)

# --- Predetermined observation-level 
m2c <- '
# Individual effects
etap =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
# Regressions 
cig98 ~ rho*cig97 + beta*drink97 + theta*mon97
cig99 ~ rho*cig98 + beta*drink98 + theta*mon97
cig00 ~ rho*cig99 + beta*drink99 + theta*mon97
cig01 ~ rho*cig00 + beta*drink00 + theta*mon97
cig02 ~ rho*cig01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(etap, mon97) = 0
etap ~~ cig97 + drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
cig97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
'
m2c.fit <- sem(m2c, data = df, missing = "ML")
summary(m2c.fit, fit.measures = TRUE, standardized = TRUE)

# --- Predetermined observation-level with covariance etap and mon97
m2cx <- '
# Individual effects
etap =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
# Regressions 
cig98 ~ rho*cig97 + beta*drink97 + theta*mon97
cig99 ~ rho*cig98 + beta*drink98 + theta*mon97
cig00 ~ rho*cig99 + beta*drink99 + theta*mon97
cig01 ~ rho*cig00 + beta*drink00 + theta*mon97
cig02 ~ rho*cig01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(etap, mon97) = 0
etap ~~ cig97 + drink97 + drink98 + drink99 + drink00 + drink01 + mon97
cig97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
'
m2cx.fit <- sem(m2cx, data = df, missing = "ML")
summary(m2cx.fit, fit.measures = TRUE, standardized = TRUE)

# --- Likelihood ratio test
anova(m2a.fit, m2b.fit, m2c.fit)


# Extended models with latent slope ---------------------------------------

# --- Residual-level 
m3a <- '
# Identify individual effects
alpha1 =~ 1*cig97 + 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
alpha2 =~ 0*cig97 + 1*cig98 + 2*cig99 + 3*cig00 + 4*cig01 + 5*cig02
# Identify structured residuals 
e97 =~ 1*cig97
e98 =~ 1*cig98
e99 =~ 1*cig99
e00 =~ 1*cig00
e01 =~ 1*cig01
e02 =~ 1*cig02
# Constrain variances of original variables to zero 
cig97 ~~ 0*cig97
cig98 ~~ 0*cig98
cig99 ~~ 0*cig99
cig00 ~~ 0*cig00
cig01 ~~ 0*cig01
cig02 ~~ 0*cig02
# Regressions 
e98 ~ rho*e97 + beta*drink97 + theta*mon97
e99 ~ rho*e98 + beta*drink98 + theta*mon97
e00 ~ rho*e99 + beta*drink99 + theta*mon97
e01 ~ rho*e00 + beta*drink00 + theta*mon97
e02 ~ rho*e01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(alpha1, e97) = Cov(alpha2, e97) = Cov(alpha1, mon97) = Cov(alpha2, mon97) = 0
alpha1 ~~ alpha2 + 0*e97 + drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
alpha2 ~~ 0*e97 + drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
e97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
'
m3a.fit <- sem(m3a, data = df, missing = "ML")
summary(m3a.fit, fit.measures = TRUE, standardized = TRUE)

# --- Constrained observation-level 
m3b <- '
# Individual effects
eta1c =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02 + a*cig97
eta2c =~ 1*cig98 + 2*cig99 + 3*cig00 + 4*cig01 + 5*cig02 + b*cig97
# Regressions 
cig98 ~ rho*cig97 + beta*drink97 + theta*mon97
cig99 ~ rho*cig98 + beta*drink98 + theta*mon97
cig00 ~ rho*cig99 + beta*drink99 + theta*mon97
cig01 ~ rho*cig00 + beta*drink00 + theta*mon97
cig02 ~ rho*cig01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(eta1c, mon97) = Cov(eta2c, mon97) = 0
eta1c ~~ eta2c + drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
eta2c ~~ drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
cig97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
# Constraints 
a == (1 - rho)^-1
b == -rho*(1 - rho)^-2
'
m3b.fit <- sem(m3b, data = df, missing = "ML")
summary(m3b.fit, fit.measures = TRUE, standardized = TRUE)

# --- Predetermined observation-level 
m3c <- '
# Individual effects
eta1p =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
eta2p =~ 1*cig98 + 2*cig99 + 3*cig00 + 4*cig01 + 5*cig02
# Regressions 
cig98 ~ rho*cig97 + beta*drink97 + theta*mon97
cig99 ~ rho*cig98 + beta*drink98 + theta*mon97
cig00 ~ rho*cig99 + beta*drink99 + theta*mon97
cig01 ~ rho*cig00 + beta*drink00 + theta*mon97
cig02 ~ rho*cig01 + beta*drink01 + theta*mon97
# Covariances, notice Cov(etap, mon97) = 0
eta1p ~~ eta2p + cig97 + drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
eta2p ~~ cig97 + drink97 + drink98 + drink99 + drink00 + drink01 + 0*mon97
cig97 ~~ drink97 + drink98 + drink99 + drink00 + drink01 + mon97
drink97 ~~ drink98 + drink99 + drink00 + drink01 + mon97
drink98 ~~ drink99 + drink00 + drink01 + mon97
drink99 ~~ drink00 + drink01 + mon97
drink00 ~~ drink01 + mon97
drink01 ~~ mon97
'
m3c.fit <- sem(m3c, data = df, missing = "ML")
summary(m3c.fit, fit.measures = TRUE, standardized = TRUE)

# --- Likelihood ratio test
anova(m3a.fit, m3b.fit, m3c.fit)


# Cross-lagged models -----------------------------------------------------

# --- Residual-level 
m4a <- '
# Identify individual effects
alphay =~ 1*cig97 + 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
alphax =~ 1*drink97 + 1*drink98 + 1*drink99 + 1*drink00 + 1*drink01 + 1*drink02
# Identify structured residuals
e97 =~ 1*cig97
e98 =~ 1*cig98
e99 =~ 1*cig99
e00 =~ 1*cig00
e01 =~ 1*cig01
e02 =~ 1*cig02
d97 =~ 1*drink97
d98 =~ 1*drink98
d99 =~ 1*drink99
d00 =~ 1*drink00
d01 =~ 1*drink01
d02 =~ 1*drink02
# Constrain variances of original variables to zero 
cig97 ~~ 0*cig97
cig98 ~~ 0*cig98
cig99 ~~ 0*cig99
cig00 ~~ 0*cig00
cig01 ~~ 0*cig01
cig02 ~~ 0*cig02
drink97 ~~ 0*drink97
drink98 ~~ 0*drink98
drink99 ~~ 0*drink99
drink00 ~~ 0*drink00
drink01 ~~ 0*drink01
drink02 ~~ 0*drink02
# Regressions 
e98 ~ rho*e97 + beta*d97
e99 ~ rho*e98 + beta*d98
e00 ~ rho*e99 + beta*d99
e01 ~ rho*e00 + beta*d00
e02 ~ rho*e01 + beta*d01
d98 ~ phi*d97 + gamma*e97
d99 ~ phi*d98 + gamma*e98
d00 ~ phi*d99 + gamma*e99
d01 ~ phi*d00 + gamma*e00
d02 ~ phi*d01 + gamma*e01
# Covariances, notice Cov(alphay, e97) = Cov(alphax, e97) = Cov(alphay, d97) = Cov(alphax, d97) = 0
alphay ~~ alphax + 0*e97 + 0*d97  
alphax ~~ 0*e97 + 0*d97        
e97 ~~ d97
e98 ~~ d98
e99 ~~ d99
e00 ~~ d00
e01 ~~ d01
e02 ~~ d02
'
m4a.fit <- sem(m4a, data = df, missing = "ML")
summary(m4a.fit, fit.measures = TRUE, standardized = TRUE)

# --- Constrained observation-level 
m4b <- '
# Identify individual effects
etayc =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02 + a*cig97 + b*drink97
etaxc =~ 1*drink98 + 1*drink99 + 1*drink00 + 1*drink01 + 1*drink02 + c*drink97 + d*cig97
# Regressions 
cig98 ~ rho*cig97 + beta*drink97
cig99 ~ rho*cig98 + beta*drink98
cig00 ~ rho*cig99 + beta*drink99
cig01 ~ rho*cig00 + beta*drink00
cig02 ~ rho*cig01 + beta*drink01
drink98 ~ phi*drink97 + gamma*cig97
drink99 ~ phi*drink98 + gamma*cig98
drink00 ~ phi*drink99 + gamma*cig99
drink01 ~ phi*drink00 + gamma*cig00
drink02 ~ phi*drink01 + gamma*cig01
# Constraints
a == (1 - phi)/((1 - rho)*(1 - phi) - beta*gamma)
c == (1 - rho)/((1 - rho)*(1 - phi) - beta*gamma)
b == gamma/((1 - rho)*(1 - phi) - beta*gamma)
d == beta/((1 - rho)*(1 - phi) - beta*gamma)
# Covariances, notice just within-time error covariances
cig97 ~~ drink97
cig98 ~~ drink98
cig99 ~~ drink99
cig00 ~~ drink00
cig01 ~~ drink01
cig02 ~~ drink02
'
m4b.fit <- sem(m4b, data = df, missing = "ML")
summary(m4b.fit, fit.measures = TRUE, standardized = TRUE)

# --- Predetermined observation-level 
m4c <- '
# Identify individual effects
etayp =~ 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
etaxp =~ 1*drink98 + 1*drink99 + 1*drink00 + 1*drink01 + 1*drink02
# Regressions 
cig98 ~ rho*cig97 + beta*drink97
cig99 ~ rho*cig98 + beta*drink98
cig00 ~ rho*cig99 + beta*drink99
cig01 ~ rho*cig00 + beta*drink00
cig02 ~ rho*cig01 + beta*drink01
drink98 ~ phi*drink97 + gamma*cig97
drink99 ~ phi*drink98 + gamma*cig98
drink00 ~ phi*drink99 + gamma*cig99
drink01 ~ phi*drink00 + gamma*cig00
drink02 ~ phi*drink01 + gamma*cig01
# Covariances, notice Cov(etayp, cig97) <> 0, Cov(etaxp, cig97) <> 0
etayp ~~ etaxp + cig97 + drink97
etaxp ~~ cig97 + drink97
cig97 ~~ drink97
cig98 ~~ drink98
cig99 ~~ drink99
cig00 ~~ drink00
cig01 ~~ drink01
cig02 ~~ drink02
'
m4c.fit <- sem(m4c, data = df, missing = "ML")
summary(m4c.fit, fit.measures = TRUE, standardized = TRUE)

# --- Likelihood ratio test
anova(m4a.fit, m4b.fit, m4c.fit)


# Predetermined RI-CLPM  --------------------------------------------------

# --- Residual-level 
m5a <- '
# Identify individual effects
alphay =~ 1*cig97 + 1*cig98 + 1*cig99 + 1*cig00 + 1*cig01 + 1*cig02
alphax =~ 1*drink97 + 1*drink98 + 1*drink99 + 1*drink00 + 1*drink01 + 1*drink02
# Identify structured residuals
e97 =~ 1*cig97
e98 =~ 1*cig98
e99 =~ 1*cig99
e00 =~ 1*cig00
e01 =~ 1*cig01
e02 =~ 1*cig02
d97 =~ 1*drink97
d98 =~ 1*drink98
d99 =~ 1*drink99
d00 =~ 1*drink00
d01 =~ 1*drink01
d02 =~ 1*drink02
# Constrain variances of original variables to zero 
cig97 ~~ 0*cig97
cig98 ~~ 0*cig98
cig99 ~~ 0*cig99
cig00 ~~ 0*cig00
cig01 ~~ 0*cig01
cig02 ~~ 0*cig02
drink97 ~~ 0*drink97
drink98 ~~ 0*drink98
drink99 ~~ 0*drink99
drink00 ~~ 0*drink00
drink01 ~~ 0*drink01
drink02 ~~ 0*drink02
# Regressions 
e98 ~ rho*e97 + beta*d97
e99 ~ rho*e98 + beta*d98
e00 ~ rho*e99 + beta*d99
e01 ~ rho*e00 + beta*d00
e02 ~ rho*e01 + beta*d01
d98 ~ phi*d97 + gamma*e97
d99 ~ phi*d98 + gamma*e98
d00 ~ phi*d99 + gamma*e99
d01 ~ phi*d00 + gamma*e00
d02 ~ phi*d01 + gamma*e01
# Covariances, notice Cov(alphay, e97) = Cov(alphax, e97) = Cov(alphay, d97) = Cov(alphax, d97) = 0
alphay ~~ alphax + e97 + d97  
alphax ~~ e97 + d97        
e97 ~~ d97
e98 ~~ d98
e99 ~~ d99
e00 ~~ d00
e01 ~~ d01
e02 ~~ d02
'
m5a.fit <- sem(m5a, data = df, missing = "ML")
summary(m5a.fit, fit.measures = TRUE, standardized = TRUE)

# --- Residual-level with time-varying individual effects
m5a <- '
# Identify individual effects
alphay =~ NA*cig98 + cig99 + cig00 + cig01 + 1*cig02 + cig97
alphax =~ NA*drink98 + drink99 + drink00 + drink01 + 1*drink02 + drink97 
# Identify structured residuals
e97 =~ 1*cig97
e98 =~ 1*cig98
e99 =~ 1*cig99
e00 =~ 1*cig00
e01 =~ 1*cig01
e02 =~ 1*cig02
d97 =~ 1*drink97
d98 =~ 1*drink98
d99 =~ 1*drink99
d00 =~ 1*drink00
d01 =~ 1*drink01
d02 =~ 1*drink02
# Constrain variances of original variables to zero 
cig97 ~~ 0*cig97
cig98 ~~ 0*cig98
cig99 ~~ 0*cig99
cig00 ~~ 0*cig00
cig01 ~~ 0*cig01
cig02 ~~ 0*cig02
drink97 ~~ 0*drink97
drink98 ~~ 0*drink98
drink99 ~~ 0*drink99
drink00 ~~ 0*drink00
drink01 ~~ 0*drink01
drink02 ~~ 0*drink02
# Regressions 
e98 ~ rho*e97 + beta*d97
e99 ~ rho*e98 + beta*d98
e00 ~ rho*e99 + beta*d99
e01 ~ rho*e00 + beta*d00
e02 ~ rho*e01 + beta*d01
d98 ~ phi*d97 + gamma*e97
d99 ~ phi*d98 + gamma*e98
d00 ~ phi*d99 + gamma*e99
d01 ~ phi*d00 + gamma*e00
d02 ~ phi*d01 + gamma*e01
# Covariances, notice Cov(alphay, e97) = Cov(alphax, e97) = Cov(alphay, d97) = Cov(alphax, d97) = 0
alphay ~~ alphax + 0*e97 + 0*d97  
alphax ~~ 0*e97 + 0*d97        
e97 ~~ d97
e98 ~~ d98
e99 ~~ d99
e00 ~~ d00
e01 ~~ d01
e02 ~~ d02
'
m5a.fit <- sem(m5a, data = df, missing = "ML")
summary(m5a.fit, fit.measures = TRUE, standardized = TRUE)

# Cross-lagged models on data at equilibrium ------------------------------

# --- Residual-level 
m6a <- '
# Identify individual effects
alphay =~ 1*cig05 + 1*cig06 + 1*cig07 + 1*cig08 + 1*cig09 + 1*cig10
alphax =~ 1*drink05 + 1*drink06 + 1*drink07 + 1*drink08 + 1*drink09 + 1*drink10
# Identify structured residuals
e05 =~ 1*cig05
e06 =~ 1*cig06
e07 =~ 1*cig07
e08 =~ 1*cig08
e09 =~ 1*cig09
e10 =~ 1*cig10
d05 =~ 1*drink05
d06 =~ 1*drink06
d07 =~ 1*drink07
d08 =~ 1*drink08
d09 =~ 1*drink09
d10 =~ 1*drink10
# Constrain variances of original variables to zero 
cig05 ~~ 0*cig05
cig06 ~~ 0*cig06
cig07 ~~ 0*cig07
cig08 ~~ 0*cig08
cig09 ~~ 0*cig09
cig10 ~~ 0*cig10
drink05 ~~ 0*drink05
drink06 ~~ 0*drink06
drink07 ~~ 0*drink07
drink08 ~~ 0*drink08
drink09 ~~ 0*drink09
drink10 ~~ 0*drink10
# Regressions 
e06 ~ rho*e05 + beta*d05
e07 ~ rho*e06 + beta*d06
e08 ~ rho*e07 + beta*d07
e09 ~ rho*e08 + beta*d08
e10 ~ rho*e09 + beta*d09
d06 ~ phi*d05 + gamma*e05
d07 ~ phi*d06 + gamma*e06
d08 ~ phi*d07 + gamma*e07
d09 ~ phi*d08 + gamma*e08
d10 ~ phi*d09 + gamma*e09
# Covariances, notice Cov(alphay, e05) = Cov(alphax, e05) = Cov(alphay, d05) = Cov(alphax, d05) = 0
alphay ~~ alphax + 0*e05 + 0*d05  
alphax ~~ 0*e05 + 0*d05        
e05 ~~ d05
e06 ~~ d06
e07 ~~ d07
e08 ~~ d08
e09 ~~ d09
e10 ~~ d10
'
m6a.fit <- sem(m6a, data = df, missing = "ML")
summary(m6a.fit, fit.measures = TRUE, standardized = TRUE)

# --- Constrained observation-level 
m6b <- '
# Identify individual effects
etayc =~ 1*cig06 + 1*cig07 + 1*cig08 + 1*cig09 + 1*cig10 + a*cig05 + b*drink05
etaxc =~ 1*drink06 + 1*drink07 + 1*drink08 + 1*drink09 + 1*drink10 + c*drink05 + d*cig05
# Regressions 
cig06 ~ rho*cig05 + beta*drink05
cig07 ~ rho*cig06 + beta*drink06
cig08 ~ rho*cig07 + beta*drink07
cig09 ~ rho*cig08 + beta*drink08
cig10 ~ rho*cig09 + beta*drink09
drink06 ~ phi*drink05 + gamma*cig05
drink07 ~ phi*drink06 + gamma*cig06
drink08 ~ phi*drink07 + gamma*cig07
drink09 ~ phi*drink08 + gamma*cig08
drink10 ~ phi*drink09 + gamma*cig09
# Constraints
a == (1 - phi)/((1 - rho)*(1 - phi) - beta*gamma)
c == (1 - rho)/((1 - rho)*(1 - phi) - beta*gamma)
b == gamma/((1 - rho)*(1 - phi) - beta*gamma)
d == beta/((1 - rho)*(1 - phi) - beta*gamma)
# Covariances, notice just within-time error covariances
cig05 ~~ drink05
cig06 ~~ drink06
cig07 ~~ drink07
cig08 ~~ drink08
cig09 ~~ drink09
cig10 ~~ drink10
'
m6b.fit <- sem(m6b, data = df, missing = "ML")
summary(m6b.fit, fit.measures = TRUE, standardized = TRUE)

# --- Predetermined observation-level 
m6c <- '
# Identify individual effects
etayp =~ 1*cig06 + 1*cig07 + 1*cig08 + 1*cig09 + 1*cig10
etaxp =~ 1*drink06 + 1*drink07 + 1*drink08 + 1*drink09 + 1*drink10
# Regressions 
cig06 ~ rho*cig05 + beta*drink05
cig07 ~ rho*cig06 + beta*drink06
cig08 ~ rho*cig07 + beta*drink07
cig09 ~ rho*cig08 + beta*drink08
cig10 ~ rho*cig09 + beta*drink09
drink06 ~ phi*drink05 + gamma*cig05
drink07 ~ phi*drink06 + gamma*cig06
drink08 ~ phi*drink07 + gamma*cig07
drink09 ~ phi*drink08 + gamma*cig08
drink10 ~ phi*drink09 + gamma*cig09
# Covariances, notice Cov(etayp, cig05) <> 0, Cov(etaxp, cig05) <> 0
etayp ~~ etaxp + cig05 + drink05
etaxp ~~ cig05 + drink05
cig05 ~~ drink05
cig06 ~~ drink06
cig07 ~~ drink07
cig08 ~~ drink08
cig09 ~~ drink09
cig10 ~~ drink10
'
m6c.fit <- sem(m6c, data = df, missing = "ML")
summary(m6c.fit, fit.measures = TRUE, standardized = TRUE)

# getwd()
saveRDS(m6a.fit, "m6a_fit.Rda")
saveRDS(m6b.fit, "m6b_fit.Rda")
saveRDS(m6c.fit, "m6c_fit.Rda")


# --- Likelihood ratio test
anova(m6a.fit, m6b.fit, m6c.fit)

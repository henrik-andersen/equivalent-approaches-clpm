
rm(list = ls())

# --- Load data

getwd()
setwd("C:/Users/Henrik/github_projects/equivalent-approaches/r-files")
df <- readRDS("alc-cig-mar.Rda")
head(df)

names(df)[1:9] <- c("id", "cig97", "drink97", "weed97", "sex", "dobm", "doby", "sample_type", "race")
names(df)[10:ncol(df)] <- paste0(c("cig", "drink", "weed"), substr(rep(1998:2011, each = 3), 3, 4))
head(df)

# Load in second monitoring dataset
df2 <- readRDS("monitor.Rda")
head(df2)
names(df2) <- c("id", "sex", "dobm", "doby", "sample_type", "race", "mon97")
df2 <- df2[, c("id", "mon97")]

df <- merge(df, df2, by = "id")
head(df)

# Write a function to eliminate outliers
na_func <- function(x) {
  x <- ifelse(x > mean(x, na.rm = TRUE) + 2*sd(x, na.rm = TRUE), NA, x)
  return(x)
}

hist(df$drink00)
drink_cols <- which(substr(colnames(df), 1, 5) == "drink")
df[drink_cols] <- lapply(df[drink_cols], FUN = na_func)
hist(df$drink00)

hist(df$cig00)
cig_cols <- which(substr(colnames(df), 1, 3) == "cig")
df[cig_cols] <- lapply(df[cig_cols], FUN = na_func)
hist(df$cig00)

hist(df$weed00)
weed_cols <- which(substr(colnames(df), 1, 4) == "weed")
df[weed_cols] <- lapply(df[weed_cols], FUN = na_func)
hist(df$weed00)

# --- Plot variances and means

library(ggplot2)

# Alcohol 
dfvar_alc <- data.frame(t = 1997:2011, 
                        vars = c(var(df$drink97, na.rm = TRUE),
                                 var(df$drink98, na.rm = TRUE),
                                 var(df$drink99, na.rm = TRUE),
                                 var(df$drink00, na.rm = TRUE), 
                                 var(df$drink01, na.rm = TRUE),
                                 var(df$drink02, na.rm = TRUE),
                                 var(df$drink03, na.rm = TRUE),
                                 var(df$drink04, na.rm = TRUE),
                                 var(df$drink05, na.rm = TRUE),
                                 var(df$drink06, na.rm = TRUE), 
                                 var(df$drink07, na.rm = TRUE),
                                 var(df$drink08, na.rm = TRUE),
                                 var(df$drink09, na.rm = TRUE),
                                 var(df$drink10, na.rm = TRUE),
                                 var(df$drink11, na.rm = TRUE)))
plot(dfvar_alc$t, dfvar_alc$vars)

ggplot(dfvar_alc, aes(x = t, y = vars)) + 
  geom_point(shape = 1) + 
  scale_x_continuous(name = "Year", limits = c(1997, 2011), breaks = 1997:2011) + 
  scale_y_continuous(name = "Observed variance", limits = c(0, 15), breaks = 0:15)

dfmean_alc <- data.frame(t = 1997:2011, 
                         means = c(mean(df$drink97, na.rm = TRUE),
                                   mean(df$drink98, na.rm = TRUE),
                                   mean(df$drink99, na.rm = TRUE),
                                   mean(df$drink00, na.rm = TRUE), 
                                   mean(df$drink01, na.rm = TRUE),
                                   mean(df$drink02, na.rm = TRUE),
                                   mean(df$drink03, na.rm = TRUE),
                                   mean(df$drink04, na.rm = TRUE),
                                   mean(df$drink05, na.rm = TRUE),
                                   mean(df$drink06, na.rm = TRUE), 
                                   mean(df$drink07, na.rm = TRUE),
                                   mean(df$drink08, na.rm = TRUE),
                                   mean(df$drink09, na.rm = TRUE),
                                   mean(df$drink10, na.rm = TRUE),
                                   mean(df$drink11, na.rm = TRUE)))
plot(dfmean_alc$t, dfmean_alc$means)

ggplot(dfmean_alc, aes(x = t, y = means)) + 
  geom_point(shape = 1) + 
  scale_x_continuous(name = "Year", limits = c(1997, 2011), breaks = 1997:2011) + 
  scale_y_continuous(name = "Observed mean", limits = c(0, 5), breaks = 0:5)

# Cigarettes 
dfvar_cig <- data.frame(t = 1997:2011, 
                        vars = c(var(df$cig97, na.rm = TRUE),
                                 var(df$cig98, na.rm = TRUE),
                                 var(df$cig99, na.rm = TRUE),
                                 var(df$cig00, na.rm = TRUE), 
                                 var(df$cig01, na.rm = TRUE),
                                 var(df$cig02, na.rm = TRUE),
                                 var(df$cig03, na.rm = TRUE),
                                 var(df$cig04, na.rm = TRUE),
                                 var(df$cig05, na.rm = TRUE),
                                 var(df$cig06, na.rm = TRUE), 
                                 var(df$cig07, na.rm = TRUE),
                                 var(df$cig08, na.rm = TRUE),
                                 var(df$cig09, na.rm = TRUE),
                                 var(df$cig10, na.rm = TRUE),
                                 var(df$cig11, na.rm = TRUE)))
plot(dfvar_cig$t, dfvar_cig$vars)


ggplot(dfvar_cig, aes(x = t, y = vars)) + 
  geom_point(shape = 1) + 
  scale_x_continuous(name = "Year", limits = c(1997, 2011), breaks = 1997:2011) + 
  scale_y_continuous(name = "Observed variance", limits = c(0, 50), breaks = seq(0, 50, 10))

dfmean_cig <- data.frame(t = 1997:2011, 
                         means = c(mean(df$cig97, na.rm = TRUE),
                                   mean(df$cig98, na.rm = TRUE),
                                   mean(df$cig99, na.rm = TRUE),
                                   mean(df$cig00, na.rm = TRUE), 
                                   mean(df$cig01, na.rm = TRUE),
                                   mean(df$cig02, na.rm = TRUE),
                                   mean(df$cig03, na.rm = TRUE),
                                   mean(df$cig04, na.rm = TRUE),
                                   mean(df$cig05, na.rm = TRUE),
                                   mean(df$cig06, na.rm = TRUE), 
                                   mean(df$cig07, na.rm = TRUE),
                                   mean(df$cig08, na.rm = TRUE),
                                   mean(df$cig09, na.rm = TRUE),
                                   mean(df$cig10, na.rm = TRUE),
                                   mean(df$cig11, na.rm = TRUE)))
plot(dfmean_cig$t, dfmean_cig$means)

ggplot(dfmean_cig, aes(x = t, y = means)) + 
  geom_point(shape = 1) + 
  scale_x_continuous(name = "Year", limits = c(1997, 2011), breaks = 1997:2011) + 
  scale_y_continuous(name = "Observed mean", limits = c(0, 10), breaks = 0:10)

# Marijuana 
dfvar_weed <- data.frame(t = 1997:2011, 
                         vars = c(var(df$weed97, na.rm = TRUE),
                                  var(df$weed98, na.rm = TRUE),
                                  var(df$weed99, na.rm = TRUE),
                                  var(df$weed00, na.rm = TRUE), 
                                  var(df$weed01, na.rm = TRUE),
                                  var(df$weed02, na.rm = TRUE),
                                  var(df$weed03, na.rm = TRUE),
                                  var(df$weed04, na.rm = TRUE),
                                  var(df$weed05, na.rm = TRUE),
                                  var(df$weed06, na.rm = TRUE), 
                                  var(df$weed07, na.rm = TRUE),
                                  var(df$weed08, na.rm = TRUE),
                                  var(df$weed09, na.rm = TRUE),
                                  var(df$weed10, na.rm = TRUE),
                                  var(df$weed11, na.rm = TRUE)))
plot(dfvar_weed$t, dfvar_weed$vars)

dfmean_weed <- data.frame(t = 1997:2011, 
                          means = c(mean(df$weed97, na.rm = TRUE),
                                    mean(df$weed98, na.rm = TRUE),
                                    mean(df$weed99, na.rm = TRUE),
                                    mean(df$weed00, na.rm = TRUE), 
                                    mean(df$weed01, na.rm = TRUE),
                                    mean(df$weed02, na.rm = TRUE),
                                    mean(df$weed03, na.rm = TRUE),
                                    mean(df$weed04, na.rm = TRUE),
                                    mean(df$weed05, na.rm = TRUE),
                                    mean(df$weed06, na.rm = TRUE), 
                                    mean(df$weed07, na.rm = TRUE),
                                    mean(df$weed08, na.rm = TRUE),
                                    mean(df$weed09, na.rm = TRUE),
                                    mean(df$weed10, na.rm = TRUE),
                                    mean(df$weed11, na.rm = TRUE)))
plot(dfmean_weed$t, dfmean_weed$means)

# --- Save dataset
saveRDS(df, file = "C:/Users/Henrik/github_projects/equivalent-approaches/r-files/df.Rda")
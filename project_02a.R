# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Raul
# Date: 2022-05-24
# Notes:
#   This script examines the dependent variables
#   specifically, it looks for non-stationarity
#   and for AR(p) or MA(q) processes at work
#   all this done by state
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(plm)
library(tseries)


rm(list = ls())
wd <- read_csv("an_data.csv") 
pdata <- pdata.frame(wd)


# Root Unit tests
# ---------------

# Recall:
# y1 = total_firearm_deaths
# y2 = total_suicides
# y3 = firearm_homicides
# y4 = femchr

test_series <- function(varname, data) {
  pseries <- split(data[[varname]], data$state)
  pp_pval <- sapply(pseries, function(x){pp.test(x)[["p.value"]]})
  adf_pval <- sapply(pseries, function(x){adf.test(x)[["p.value"]]})
  par(mfrow = c(2,1), mar = c(1,1,1,1))
  hist(pp_pval, breaks = 50, main = paste("pdf test for", varname))
  hist(adf_pval, breaks = 50, main = paste("adf test for", varname))
}
test_series("total_firearm_deaths", pdata)
test_series("total_suicides", pdata)
test_series("firearm_homicides", pdata)
test_series("femchr", pdata)

ggplot(pdata, aes(x = year, y = femchr, group = state)) +
  geom_line(alpha = 0.5) +
  theme_bw()

ggplot(pdata, aes(x = year, y = fem_deaths, group = state)) +
  geom_line(alpha = 0.5) +
  theme_bw()

# Conclusion: no signs of non-stationarity


# Autoregression or Moving Average processes
# ------------------------------------------
xlabs <- unique(pdata$year)
y <- "femchr"
for(s in unique(pdata$state)) {
    par(mfrow = c(1,2))
    acf (ts(pdata[[y]][pdata$state == s]), main = "")
    pacf(ts(pdata[[y]][pdata$state == s]), main = "")
    mtext(s, outer = TRUE, side = 3, line = -2)
}  

# After seeing these ACF and PACF plots,
# I have reason to believe in an AR(1) process for a lot of states,
# some even an AR(2) or AR(3), even. 







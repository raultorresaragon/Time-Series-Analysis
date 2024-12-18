# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Raul
# Date: 2022-05-25
# Notes:
#   This script fits 5 models to log(y) ~ d + X:
#   - RE ARIMA(1,0,0) 
#   - RE ARIMA(2,0,0)
#   - FE ARIMA(1,0,0)
#   - FE ARIMA(1,0,0) twoway effects
#   - FE ARIMA(2,0,0)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(plm)
library(tseries)
library(nlme)
library(simcf)
library(lmtest)


rm(list = ls())
source("project_utils.R")
wd <- read_csv("an_data.csv") 
pdata <- pdata.frame(wd)

# creating first 2 lags of outcome
x <- pdata |> arrange(state, year) |> 
       dplyr::select(femchr)  |> 
       `names<-`(NULL) |> unlist()
c <- pdata |> arrange(state, year) |> 
       dplyr::select(state) |> 
       `names<-`(NULL) |> unlist()
t <- pdata |> arrange(state, year) |> 
       dplyr::select(year) |> 
       `names<-`(NULL) |> unlist()

pdata <- pdata |> arrange(state, year) |> 
           mutate(fchr_l1 = lagpanel(x=x, c=c, t=t, lagnum=1) |> as.vector(),
                  fchr_l2 = lagpanel(x=x, c=c, t=t, lagnum=2) |> as.vector())

rm(list = c("x","c","t"))


# RE ARIMA(1,0,0)
# ~~~~~~~~~~~~~~
m1re_arima100 <- lme(fixed = femchr ~ dvro +
                             pct_risk_age + pct_white_nonhispanic + 
                             unemployment_rate + gender_ratio,
                     random = ~ 1 | state,
                     correlation = corARMA(
                       form = ~ year | state,
                       p = 1,
                       q = 0
                     ),
                     data = pdata |> mutate(year = as.integer(year)),
                     na.action = na.omit)


# RE ARIMA(2,0,0)
# ~~~~~~~~~~~~~~
m1re_arima200 <- lme(fixed = femchr ~ dvro +
                             pct_risk_age + pct_white_nonhispanic + 
                             unemployment_rate + gender_ratio,
                     random = ~ 1 | state,
                     correlation = corARMA(
                       form = ~ year | state,
                       p = 2,
                       q = 0
                     ),
                     data = pdata |> mutate(year = as.integer(year)),
                     na.action = na.omit)


# FE ARIMA(1,0,0)
# ~~~~~~~~~~~~~~
mfe_arima100 <-plm(formula =  femchr ~ fchr_l1 + dvro +
                              pct_risk_age + pct_white_nonhispanic + 
                              unemployment_rate + gender_ratio, 
                    data = pdata,
                    model = "within",
                    effect = "individual")


# FE ARIMA(2,0,0)
# ~~~~~~~~~~~~~~
mfe_arima200 <-plm(formula =  femchr ~ 
                              fchr_l1 + fchr_l2 + dvro +
                              pct_risk_age + pct_white_nonhispanic + 
                              unemployment_rate + gender_ratio, 
                    data = pdata,
                    model = "within",
                    effect = "individual")


# FE ARIMA(1,0,0) twoway
# ~~~~~~~~~~~~~~~~~~~~~~
mfe_arima100t <-plm(formula = femchr ~ fchr_l1 + dvro +
                              pct_risk_age + pct_white_nonhispanic + 
                              unemployment_rate + gender_ratio, 
                    data = pdata,
                    model = "within",
                    effect = "twoway")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPARING MODELS (in sample)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m1re_100_res <- make_RE_restable(m1re_arima100, "RE ARIMA(1,0,0)")
m1re_200_res <- make_RE_restable(m1re_arima200, "RE ARIMA(2,0,0)")
RE_table <- m1re_200_res |> 
            left_join(m1re_100_res, by=c("Coeff","type")) |> 
            dplyr::select(-type)
RE_table

mfe_100_res <- make_FE_restable(mfe_arima100, "FE ARIMA(1,0,0)")
mfe_200_res <- make_FE_restable(mfe_arima200, "FE ARIMA(2,0,0)")
mfe_100t_res <-make_FE_restable(mfe_arima100t,"FE ARIMA(1,0,0)t")

FE_table <- mfe_200_res |> 
            left_join(mfe_100_res, by = c("Coeff","type")) |>
            left_join(mfe_100t_res, by = c("Coeff","type")) |>
            dplyr::select(-type)
FE_table























# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Raul
# Date: 2022-05-24
# Notes:
#   This script examines our control variables
#   specifically, it looking for missingness,
#   miscoded values, outliers, and it creates
#   new covariates from the existing ones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(plm)
library(tseries)


rm(list = ls())
wd <- read_csv("wd_gundeaths.csv")
wd$state <- as.factor(wd$state)
pdata <- plm::pdata.frame(wd, index = c("state","year"))

fh <- read_csv("homicides_female.csv") |> dplyr::select(State, Year, Deaths, `Crude Rate`) |>
      `colnames<-`(c("state","year","fem_deaths","femchr")) |>
      filter(!is.na(state)) |>
      filter(year <= 2017 & year >= 1991) |>
      filter(!is.na(fem_deaths))

# fem_deaths fuller states
fem_deaths_tbl <- tibble(state = character(),
                         min_year = numeric(),
                         max_year = numeric(),
                         has_gaps = logical())
for(s in unique(fh$state)) {
    unique_year <- unique(fh$year[fh$state == s])
    min_year <- min(unique_year)
    max_year <- max(unique_year)
    has_gaps <- (max_year - min_year) == length(unique_year)
    row <- as_tibble_row(list("state" = s,
                              "min_year" = min_year,
                              "max_year" = max_year,
                              "has_gaps" = has_gaps))
    fem_deaths_tbl <- bind_rows(fem_deaths_tbl, row)
}
fem_deaths_tbl
keep_fh_states <- unique(fem_deaths_tbl$state[fem_deaths_tbl$has_gaps == FALSE & 
                                              fem_deaths_tbl$min_year == 1991 & 
                                              fem_deaths_tbl$max_year == 2017])
fh <- fh |> filter(state %in% as.factor(keep_fh_states)) 
fh$state <- as.factor(fh$state)
fh$year <- as.factor(fh$year)

# trt
d1 <- "cap14"
d2 <- "ccbackground"
d3 <- "dvro"

# controls
pdata <- pdata |> 
           mutate(pct_risk_age = pct_15_to_19_years + pct_20_to_24_years + pct_25_to_29_years + 
                                 pct_30_to_34_years + pct_35_to_44_years + pct_45_to_54_years +
                                 pct_55_to_59_years)
X <- c("pct_white_nonhispanic", "unemployment_rate", "gender_ratio", "pct_risk_age")



# intervention variables
# ~~~~~~~~~~~~~~~~~~~~~~
variability_tbl <- tibble(state = character(),
                          cap14_varies = numeric(),
                          ccbackground_varies = numeric(),
                          dvro_varies = numeric()
                          )

for(s in unique(pdata$state)) {
  subd <- pdata[pdata$state == s, c("state","year","cap14","ccbackground","dvro")]
  
  d1_varies <- ifelse(min(subd$cap14)        == max(subd$cap14), 0, 1)
  d2_varies <- ifelse(min(subd$ccbackground) == max(subd$ccbackground), 0, 1)
  d3_varies <- ifelse(min(subd$dvro)         == max(subd$dvro), 0, 1)
  
  
  row <- as_tibble_row(list("state" = s,
                            "cap14_varies" = d1_varies,
                            "ccbackground_varies" = d2_varies,
                            "dvro_varies" = d3_varies))
  
  variability_tbl <- bind_rows(variability_tbl, row)
  
}
count(variability_tbl, cap14_varies, ccbackground_varies, dvro_varies)
count(variability_tbl, cap14_varies)
count(variability_tbl, ccbackground_varies)
count(variability_tbl, dvro_varies)

# conclusion: 
# our best bet is to use dvro as it is the one that varies across most states
keep_states <- unique(variability_tbl$state[variability_tbl$dvro_varies == 1])



# control variables
# ~~~~~~~~~~~~~~~~~
for(i in 1:4){

  print(paste("summary pf", X[[i]]))
  summary(pdata[[X[i]]]) |> print()
  
  print(paste("missingness pf", X[[i]]))
  (sum(is.na(pdata[[X[i]]]))/nrow(pdata)) |> print()
  
}


# lax neighbor
# ------------
wide_wd <- read_csv("gun_deaths.csv") |> 
           dplyr::select(State, Year, dvro) |>
           pivot_wider(names_from = State, values_from = dvro)

wide_wd <- wide_wd |> 
  mutate(California = California * Arizona * Nevada * Oregon,
         Alabama = Alabama * Florida * Georgia * Tennessee * Mississippi,
         Arizona = Arizona * California * `New Mexico` * Utah * Nevada,
         Connecticut = Connecticut * `Rhode Island` * `New York` * Massachusetts,
         Florida = Florida * Georgia * Alabama * Mississippi,
         Hawaii = Hawaii,
         Illinois = Illinois * Indiana * Kentucky * Missouri * Iowa * Wisconsin,
         Iowa = Iowa * Illinois * Missouri * Nebraska * `South Dakota` * Minnesota * Wisconsin,
         Kentucky = Kentucky * Virginia * Tennessee * Missouri * Illinois * Indiana * Ohio,
         Maine = Maine * `New Hampshire`,
         Massachusetts = Massachusetts * `New Hampshire` * Vermont * Connecticut * `Rhode Island`,
         Minnesota = Minnesota * Wisconsin * Iowa * `South Dakota` * `North Dakota`,    
         Missouri = Missouri * Kentucky * Tennessee * Arkansas * Kansas * Oklahoma * Nebraska * Iowa,
         Nebraska = Nebraska * Colorado * Kansas * Iowa * Missouri * `South Dakota` * Wyoming,
        `New Hampshire`= `New Hampshire` * Vermont * Massachusetts, 
        `New Mexico` = `New Mexico` * Texas * Arizona * Colorado * Utah,
        `North Carolina` = `North Carolina` * `South Carolina` * Tennessee * Virginia,
         Ohio = Ohio * Pennsylvania *`West Virginia` * Kentucky * Indiana * Michigan,
         Oregon = Oregon * Idaho * Washington * Nevada * California,
        `Rhode Island` = `Rhode Island` * Massachusetts * Connecticut,
        `South Dakota` = `South Dakota` * Minnesota * `North Dakota` * Montana * Wyoming * Nebraska,
         Texas = Texas * Oklahoma * Arkansas * Louisiana * `New Mexico`,
         Vermont = Vermont * `New Hampshire` * `New York` * Massachusetts,
         Washington = Washington * Oregon * Idaho,
         Wisconsin = Wisconsin * Michigan * Minnesota * Iowa * Illinois)

state_cols <- names(wide_wd[, -1])       

dvro_X_neighbor <- wide_wd |> 
                   pivot_longer(state_cols, names_to = "state", values_to = "dvro_X_neighbor") |> 
                   arrange(state, Year) |> 
                   mutate(state = factor(state), year = factor(Year)) |> 
                   dplyr::select(state, year, dvro_X_neighbor)

pdata <- pdata |> left_join(dvro_X_neighbor, by = c("state", "year"))

pdata <- pdata |> left_join(fh, by = c("state","year"))

keep_states <- keep_states[!(keep_states %in% unique(pdata$state[is.na(pdata$fem_deaths)]))]

count(pdata, dvro, dvro_X_neighbor)

# conclusion: 
# no missingness and no out of wack values
an_data <- pdata |> 
           filter(state %in% keep_states) |> 
           select(state, year, 
                  fem_deaths, femchr, total_firearm_deaths, total_suicides, firearm_homicides, 
                  dvro, dvro_X_neighbor,
                  pct_risk_age, pct_white_nonhispanic, unemployment_rate, gender_ratio)
dim(an_data)
write_csv(an_data[an_data$state %in% keep_states,], "an_data.csv")




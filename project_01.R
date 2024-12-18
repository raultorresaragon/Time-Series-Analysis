library(tidyverse)
rm(list = ls())

# load data
wd <- read_csv("gun_deaths.csv") 
lower_names <- str_to_lower(names(wd))
wd <- wd |> `colnames<-`(lower_names)
names(wd)[str_detect(names(wd), "percent.+")] <- 
  str_replace(names(wd)[str_detect(names(wd), "percent.+")], "percent[s]{0,1}","pct")
names(wd) <- str_replace_all(names(wd),"\\.","_")


# keep variables of interest
y1 <- "total_firearm_deaths"
y2 <- "total_suicides"
y3 <- "firearm_homicides"

# trt
d1 <- "cap14"
d2 <- "ccbackground"
d3 <- "dvro"

# controls
X <- c("pct_white_nonhispanic", "unemployment_rate", "gender_ratio", 
       colnames(wd)[str_detect(colnames(wd), "^pct_[1-9].+")])

# i,t
i <- "state"
t <- "year"

# normalize y variable so that it represents "death per 10,000 people"
normalize_yvar <- function(yvar, df){
    df[[yvar]] <- 10000 * df[[yvar]]/df$population
    df
}
for(y in c(y1,y2,y3)) {
  wd <- normalize_yvar(y,wd)
}

wd <- wd[, c(i,t,y1,y2,y3,d1,d2,d3,X)]
write_csv(wd, "wd_gundeaths.csv")

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

library(tidyverse)

setwd("C:/Users/agomper/OneDrive - Emory University/Documents/Year 2/EPI 590R/epi590r-in-class/data/raw")
nlsy <- read_csv("nlsy.csv",
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols)

library(dplyr)
nlsy <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

nlsy <- na.omit(nlsy)

setwd("../clean/")
write_rds(nlsy, "nlsy-complete-cases.rds")

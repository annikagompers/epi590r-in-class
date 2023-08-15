library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Univariate regression

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)


tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)


## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))


## Create a univariate regression table looking at the association between sex (sex_cat) as the x = variable
## and each of nsibs, sleep_wkdy, and sleep_wknd, and income
nlsy %>%
	select(nsibs, sleep_wkdy, sleep_wknd, income, sex_cat) %>%
	tbl_uvregression(
		method = lm,
		x = sex_cat,
		show_single_row = "sex_cat",
		hide_n = TRUE,
		label = list(nsibs ~ "Number of siblings",
								sleep_wkdy ~ "Hours of sleep on weekdays",
								sleep_wknd ~ "Hours of sleep on weekends",
								income ~ "Income")
	) %>%
	modify_header(list(
		label ~ "**Model Outcome**",
		estimate ~ "**Treatment Coef.**"
	))
# a simpler option:
tbl_uvregression(
	nlsy,
	method = lm,
	x = sex_cat,
	include = c(nsibs, starts_with("sleep"), income),
	label = list(nsibs ~ "Number of siblings",
							 sleep_wkdy ~ "Hours of sleep on weekdays",
							 sleep_wknd ~ "Hours of sleep on weekends",
							 income ~ "Income"))

## Fit a Poisson regression (family = poisson()) for the number of siblings, using at least 3 predictors
poisson_model <- glm(nsibs ~ sex_cat + race_eth_cat + eyesight_cat,
											data = nlsy, family = poisson())
## Create a nice table displaying your Poisson regression and its exponentiated coefficients
tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(sex_cat ~ "Sex",
							 race_eth_cat ~ "Race/ethnicity",
							 eyesight_cat ~ "Eyesight"))

## Regress glasses on eyesight_cat sex_cat
log_binomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial(link = "log"))
## Create a table showing the risk ratios and confidence intervals from this regression
tbl_regression(
	log_binomial_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	))

## Fit a Poisson regression instead of the log-binomial regression in the last question
modified_poisson_model <- glm(glasses ~ eyesight_cat + sex_cat,
													data = nlsy, family = poisson(link="logl"))
## Create a table using tidy_fun = partial(tidy_robust, vcov = "HC1")
tbl_regression(
	modified_poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"),
	tidy_fun = partial(tidy_robust, vcov = "HC1"))


## Make a table comparing the log-binomial and the log-Poisson results.
tbl_logbinomial <- tbl_regression(
	log_binomial_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	))
tbl_logpoisson <- tbl_regression(
	modified_poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"),
	tidy_fun = partial(tidy_robust, vcov = "HC1"))
tbl_merge(list(tbl_logbinomial, tbl_logpoisson),
					tab_spanner = c("**Log-Binomial Model**", "**Log-Poisson Model**"))

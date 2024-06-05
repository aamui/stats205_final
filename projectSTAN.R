library(dplyr)
library(tidyverse)
library(tidybayes)
library(rstan)
library(rstanarm)
library(bayesplot)
library(bayesrules)

#read data
sal <- read.csv("C:/Users/Steven/Documents/UCI/SPRING/STATS205P/salaries.csv")

table(sal$job_title)
salfilt <- filter(sal,job_title %in% c('Data Engineer','Data Scientist','Machine Learning Engineer','Data Analyst'))

table(sal$company_location)
salfilt2 <- filter(sal,company_location %in% c('US','GB','CA','ES','DE'))
salfilt2stan <- salfilt2 %>% select(salary_in_usd,company_location)
###################################
salmodelstan <- stan_glm(salary_in_usd ~ job_title, data = salfilt,
                         family = gaussian,
                         prior = normal(0,100^2), 
                         chains = 4, iter = 5000*2, seed = 84735)
summary(salmodelstan)
mcmc_trace(salmodelstan)

salmodel <- glm(salary_in_usd~job_title, data = salfilt, family = gaussian)
summary(salmodel)
####################################
salmodelyearstan <- stan_glm(salary_in_usd~work_year,data=salfilt,
                             family = gaussian,
                             prior = normal(0,100^2), 
                             chains = 4, iter = 5000*2, seed = 84735)  
summary(salmodelyearstan)
#####################################
salmodelloc <- stan_glm(salary_in_usd~company_location,data=salfilt2,
                        family = gaussian,
                        prior = normal(0,100^2), 
                        chains = 4, iter = 5000*2, seed = 84735)
summary(salmodelloc)
####################################
modelloc <- '
data {
  int<lower=0> N;  // Number of observations
  int<lower=1> K;  // Number of countries
  int<lower=1, upper=K> country[N];  // Country index for each observation
  real salary[N];  // Salary observations
}

parameters {
  vector[K] mu;  // Mean salary for each country
  vector<lower=0>[K] sigma;  // Standard deviation of salary for each country
}

model {
  for (i in 1:N)
    salary[i] ~ normal(mu[country[i]], sigma[country[i]]);
}
'

salfilt2stan$company_location <- as.numeric(factor(salfilt2stan$company_location))

# Prepare the data for Stan
N <- nrow(salfilt2stan)
K <- length(unique(salfilt2stan$company_location))
country <- as.numeric(salfilt2stan$company_location)
salary <- salfilt2stan$salary_in_usd


modellocmc <- stan(model_code =modelloc, data=list(N = N, K = K, country = country, salary = salary), iter = 5000*2, chains = 4)
summary(modellocmc)
mcmc_trace(modellocmc)
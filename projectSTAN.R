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
salfilt3 <- filter(salfilt2,job_title %in% c('Data Engineer','Data Scientist','Machine Learning Engineer','Data Analyst'))
salfilt2stan <- salfilt2 %>% select(salary_in_usd,company_location)

salfiltfinal <- sal %>%
  mutate(
    grouped_job_title = case_when(
      grepl("Machine Learning|ML", job_title) ~ "Machine Learning Engineer",
      grepl("Data Science|Data Scientist", job_title)~ "Data Scientist",
      grepl("Data Analyst|Data Analytics|Visualization", job_title)  ~ "Data Analyst",
      grepl("AI|Artificial Intelligence", job_title) ~ "AI Engineer",
      grepl("Business Intelligence|BI", job_title) ~ "Business Intelligence",
      grepl("Data Engineer", job_title) ~ "Data Engineer",
      TRUE ~ "Other"
    )
  )

salfiltfinal = salfiltfinal %>% filter(employee_residence %in% c("CA", "DE", "ES", "GB", "US"))
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

salfilt2stan$company_location <- as.numeric(factor(salfilt2stan$company_location)) #Alphabetical Coding: CA, DE, ES, GB, US

# Prepare the data for Stan
N <- nrow(salfilt2stan)
K <- length(unique(salfilt2stan$company_location))
country <- as.numeric(salfilt2stan$company_location)
salary <- salfilt2stan$salary_in_usd


modellocmc <- stan(model_code =modelloc, data=list(N = N, K = K, country = country, salary = salary), iter = 5000*2, chains = 4)
summary(modellocmc) #CA=1, DE=2, ES=3, GB=4, US=5
mcmc_trace(modellocmc)

#########################################
model2 <- stan_glm(salary_in_usd~employee_residence+grouped_job_title+employee_residence*grouped_job_title,data=salfiltfinal,
                   family = gaussian,
                   prior = normal(0,100^2), 
                   chains = 4, iter = 5000*2, seed = 84735)
summary(model2)
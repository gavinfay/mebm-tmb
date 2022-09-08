## Load TMB TRUE
library(TMB)
library(tidyverse)

# read in the data
flounder <- readRDS("data/summer_flounder_catch_per_trip.rds") %>% 
  mutate(state = fct_relevel(state, c("MA","RI","CT","NY","NJ","DE","MD","NC","VA")))
with(flounder, table(state))

## TMB data and parameters
data <- list(count=flounder$catch,
             state = as.integer(flounder$state))
parameters <- list(beta=0, b=rep(0,9), logSigmab = log(1))

## Make C++ file
#Hint: you can use the TMB::template() to get starteed
#
#TMB::template("my_new_tmb_model.cpp")

## write the model
# look at poisson_glm.cpp

## Compile and load the model
compile("poisson_glmm.cpp")
dyn.load(dynlib("poisson_glmm"))

## Make a function object
obj <- MakeADFun(data, parameters, 
                 random = "b",
                 DLL="poisson_glmm")

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
opt
sdr <- sdreport(obj)
sdr
summary(sdr, "fixed")
summary(sdr, "random")
summary(sdr, "report")

#plot the estimated state level catch per trips
report_coefs <- summary(sdr, "report")
state_coefs <- tibble(state = levels(flounder$state),
                      pred = report_coefs[-(1:2),1],
                      se = report_coefs[-(1:2),2]) %>% 
  mutate(lo = qlnorm(0.025, log(pred), se/pred),
         hi = qlnorm(0.975, log(pred), se/pred))

state_coefs %>% 
  mutate(state = fct_relevel(state, c("MA","RI","CT","NY","NJ","DE","MD","NC","VA"))) %>% 
  ggplot() +
  aes(x = state, y = pred, ymin = lo, ymax = hi) +
  geom_errorbar(position="dodge",width=0) +
  geom_point(size=3) +
  theme_bw() + 
  coord_flip() +
  labs(y = "predicted catch per trip")






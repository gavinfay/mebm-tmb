## Load TMB TRUE
library(TMB)
library(tidyverse)

# read in the data
herring <- read_table("data/herring_counts.txt") %>% 
  janitor::clean_names() 
herring

## TMB data and parameters
data <- list(count=herring$count)
parameters <- list(beta=0)

## Make C++ file
#Hint: you can use the TMB::template() to get starteed
#
TMB::template("my_new_tmb_model.cpp")

## write the model
# look at poisson_glm.cpp

## Compile and load the model
compile("poisson_glm.cpp")
dyn.load(dynlib("poisson_glm"))

## Make a function object
obj <- MakeADFun(data, parameters, DLL="poisson_glm")

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr


## Load TMB TRUE
library(TMB)
library(tidyverse)

# read in the data
herring <- read_table("data/herring_counts.txt") %>% 
  janitor::clean_names() 
herring

## TMB data and parameters
data <- list(count=herring$count,
             month=herring$month-3)
parameters <- list(beta=rep(0,3))

## Make C++ file
TMB::template("poisson_glm.cpp")


## write the model

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

# $par
# beta     beta     beta 
# 3.388473 3.732644 3.619079 
# 
# $objective
# [1] 317.6419

# single intercept model
## Make a function object
obj2 <- MakeADFun(data, parameters, DLL="poisson_glm",
                  map = list(beta=factor(c(1,1,1))))

## Call function minimizer
opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)

## Get parameter uncertainties and convergence diagnostics
sdr2 <- sdreport(obj2)
sdr2

## Load TMB
library(TMB)
library(tidyverse)
library(FSAdata)

## Make C++ file
#TMB::template("growthincrement.cpp")

## 
#data(BluegillIL)
#str(BluegillIL)
head(BluegillIL)
#plot((lenRecap-lenMark)~deltaTime,data=BluegillIL)
data <- tibble(len1 = BluegillIL$lenMark,
               len2 = BluegillIL$lenRecap,
               dt = BluegillIL$deltaTime)



## Compile and load the model
compile(  )
dyn.load(dynlib(  ))


## Make a function object
obj <- MakeADFun(   )

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr, control = list(trace=1))

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr
#summary(sdr,select="random")



### first TMB example
library(TMB)
library(tidyverse) 


#linear regression
# Generate some data
set.seed(8675309)
data <- tibble(x = 1:20,
               y = 0.5 + 2*x + rnorm(20,0,2))
# view the data set
data
ggplot(data) +
  geom_point(aes(x=x,y=y)) +
  geom_smooth(aes(x=x,y=y),method="lm") +
  theme_minimal()

#  yhat(i) = b0 + b1*x(i)
lm1 <- lm(y~x, data = data)
lm1

# model parameters
parameters <- list(b0=0, b1=0, logSigma=0)
print(parameters)

#compile the TMB model
#require(TMB)
compile("linear_regression.cpp")

# load the compiled model object
dyn.load(dynlib("linear_regression"))

################################################################################
# make the TMB model
model <- MakeADFun(data = data, 
                   parameters = parameters,
                   #map = list(b0=factor(NA)),
                   DLL="linear_regression")
                  #control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)

# look at the built model
print(attributes(model))

# fit the model using nlminb
fit <- nlminb(model$par, model$fn, model$gr)
fit
# get variance estimates for model parameters
rep <- sdreport(model)
rep

set.seed(3)
bob <- sapply(1:10,function(x)model$simulate(complete=TRUE))
bob[,1]$y
bob[,2]$y
# Sumamrize ALL
print(summary(rep,p.value=T))

#summary(rep,select="random")
summary(rep,select="fixed")
#plot the predictions
ggplot(data) +
  geom_point(aes(x=x,y=y)) +
  geom_abline(intercept = fit$par["b0"],
              slope = fit$par["b1"]) +
  theme_minimal()



model <- MakeADFun(data = data, 
                   parameters = parameters,
                   #map = list(b0=factor(NA)),
                   DLL="linear_regression")

model$simulate()


### TMB with STAN
library(shinystan)
library(tmbstan)
# Run a single chain in serial with defaults
fit <- tmbstan(model, chains=1)

## Run in parallel with a init function
cores <- parallel::detectCores()-1
options(mc.cores = cores)
# init.fn <- function()
#   list(u=rnorm(114), beta=rnorm(2), logsdu=runif(1,0,10), logsd0=runif(1,0,1))
fit <- tmbstan(model, chains=cores, open_progress=FALSE), init=fit$par)

## To explore the fit use shinystan
launch_shinystan(fit)




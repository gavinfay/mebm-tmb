#simpleMVN
## Load TMB TRUE
library(TMB)

p <- 3
n <- 10000
mu <- c(10, 20, 30)
sd <- c(3, 2, 1)
rho <- 0.5
#VCV matrix
Sigma <- matrix(c(sd[1]*sd[1], sd[1]*sd[2]*rho, sd[1]*sd[3]*rho,
                  sd[2]*sd[1]*rho,sd[2]*sd[2], sd[2]*sd[3]*rho,
                  sd[3]*sd[1]*rho,sd[3]*sd[2]*rho, sd[3]*sd[3]),
                nrow=p, ncol = p, byrow=TRUE)
set.seed(42)
X <- MASS::mvrnorm(n=n, mu=mu, Sigma)
#pairs(X, lower.panel = NULL)

data <- list(X=X)
parameters <- list(mu=rep(0,3),logSigma=rep(0,3),rhopar=1)


## Make C++ file
#TMB::template("simpleMVN")

## Compile and load the model
compile("simpleMVN.cpp")
dyn.load(dynlib("simpleMVN"))

## Data and parameters

## Make a function object
obj <- MakeADFun(data, parameters, DLL="simpleMVN")

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr

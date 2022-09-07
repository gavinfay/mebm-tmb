#lingcod

## Load TMB TRUE
library(TMB)
library(tidyverse)

## Make C++ file
#TMB::template("lingcod.cpp")

## Compile and load the model
compile("lingcod_gompertz.cpp")
dyn.load(dynlib("lingcod_gompertz"))

## Data(and parameters
lingcod <- read_table("lingcod.dat",skip=7)
data <-list(obs=log(lingcod$`#data`))
parameters <- list(logr=0, logit_beta=0, log_sigma_proc=0, log_sigma_obs=0,
                   u=rep(0,nrow(lingcod)-1))

## Make a function object
obj <- MakeADFun(data, parameters, DLL="lingcod_gompertz", random = "u")

## Call function minimizer
opt <-nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr
summary(sdr)

b <- tibble(par = rownames(summary(sdr)),
              val = summary(sdr)[,1],
              se = summary(sdr)[,2]) %>% 
  filter(par == "b") %>% 
  mutate(obs = data$obs,
         yr = 1:length(data$obs)) 

ggplot(b) +
  geom_ribbon(aes(x=yr,ymin=val-1.96*se,ymax = val+1.96*se),
              fill = "blue", alpha = 0.2) +
  geom_line(aes(x=yr,y=val),size=1) +
  geom_point(aes(x=yr,y=obs),size=2) +
  theme_minimal() +
  theme(plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = "time", y = "ln(abundance)",
    title = "Fit of linear state-space Gompertz model to Schnute's lingcod",
    subtitle = "Estimated log abundance (line) +/- 1.96 SE (shading)",
    caption = "\n@gavin_fay\nData source: Schnute, J. T. (1994). A general framework for developing sequential fisheries models. CJFAS 51:1676-1688")



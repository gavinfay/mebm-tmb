## state-space model example
## exponential growth, SE AK Steller sea lion pup counts 
## Gavin Fay, September 2022
#tmb_wrapper <- function() {
library(tidyverse)
library(TMB)

## Pup counts
pups <- read_csv("data/SSLpupcounts2016.csv")
se_pups <- pups %>% 
  filter(rookery==1,
         trendsite ==1,
         regionnumb == 5,
         str_detect(sitename, "FORRESTER"))
pre2k <- se_pups %>% 
  filter(str_detect(sitename, "COMPLEX"),
         Year<2000) %>% 
  select(Year, pupcount) %>% 
  group_by(Year) %>% 
  mutate(icount = row_number()) %>% 
  ungroup()
post2k <- se_pups %>% 
  filter(!str_detect(sitename, "COMPLEX"),
         Year>=2000) %>% 
  group_by(sitename, Year) %>% 
  mutate(icount = row_number()) %>% 
  select(sitename, Year, pupcount, icount) %>% 
  ungroup() %>% 
  group_by(Year, icount) %>% 
  summarize(pupcount = sum(pupcount), .groups = "drop")
forrester_pups <- bind_rows(pre2k,post2k) %>% 
  arrange(Year,icount)
forrester_pups
  

 # ggplot(forrester_pups) +
 #   geom_point(aes(x=Year,y=pupcount)) +
 #   labs(title = "Pup Counts") +
 #   ylim(0,4250)

#TMB::template("ssl-pups.cpp")

data <- list(obs = forrester_pups$pupcount,
             year = forrester_pups$Year-min(forrester_pups$Year),
             nyr = max(forrester_pups$Year-min(forrester_pups$Year)))
data
maxyr <- max(data$year)

parameters <- list(log_lamda = 0.05, #growth rate
                   log_sigma = log(0.1), #observation error sd
                   log_tau = log(0.1), #process error sd
                   log_N0 = log(1000), #number of pups in first year
                   u = rep(log(1000),maxyr))   #random effects


## Compile and load the model
compile("ssl_pups.cpp")
dyn.load(dynlib("ssl_pups"))

## Make a function object
obj <- MakeADFun(data, parameters,
                 #map = list(log_sigma = factor(NA)),
                 random = "u", DLL="ssl_pups")

    
#    dyn.load(dynlib("ssl_pups"))
#    obj <- MakeADFun(data, parameters, DLL="ssl_pups")
#    return(obj)
#  }

#callr::r(tmb_wrapper, show = TRUE)


## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
summary(sdr)

summary(sdr,select="random")
summary(sdr,select="fixed")

predicted_pups <- summary(sdr,select="report") %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  filter(str_detect(id,"predN")) %>% 
  rowid_to_column(var = "year") %>% 
  mutate(year = year-1) %>% 
  select(-id) %>% 
  mutate(lo = qlnorm(0.025, log(Estimate), sdlog = `Std. Error`/Estimate),
         hi = qlnorm(0.975, log(Estimate), sdlog = `Std. Error`/Estimate))
  
predicted_pups %>% 
  ggplot() +
  geom_lineribbon(aes(x = year, 
                      y = Estimate, 
                      ymin = lo, 
                      ymax = hi)) +
  geom_point(data = tibble(year = data$year, obs=data$obs),
             aes(x = year, y = obs), col = "orange") +
  ylim(0,5000)


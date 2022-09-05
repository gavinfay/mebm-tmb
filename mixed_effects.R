### STREAMS EXAMPLE ###
library(nlme)
Streams <- scan('data/streams.dat',what=list(Stream=0,Density=0),n=3*18,skip=5)
streams.lm1 <- lm(Density~1,data=Streams)
streams.lm2 <- lm(Density~factor(Stream)-1,data=Streams)
streams.lme1 <- lme(Density~1,random=~1|Stream,data=Streams,meth="ML")
#help function for lme()
?(lme)
#summary of results
summary(streams.lme1)

#compare residuals among models
par(las=0)
boxplot(split(residuals(streams.lm1)/summary(streams.lm1)$sigma,Streams$Stream))
mtext(side=3,"lm, intercept only",line=2)
mtext(side=2,"Residuals",line=2)
abline(h=0)
boxplot(split(residuals(streams.lm2)/summary(streams.lm2)$sigma,Streams$Stream))
mtext(side=3,"lm, stream effect",line=2)
mtext(side=1,"Stream",line=2)
abline(h=0)
boxplot(split(residuals(streams.lme1)/summary(streams.lme1)$sigma,Streams$Stream))
mtext(side=3,"lme, random stream effect",line=2)
abline(h=0) 

# plot the fitted values
library(lattice)
plot(streams.lme1,Density~fitted(.)|Stream, abline = c(0,1)) 

# diagnostic plots of residuals, random effects
par(mfrow=c(2,3))
# QQ plot
qqnorm(streams.lme1$residuals/streams.lme1$sigma,ylab="Quantiles of Residuals")
qqline(streams.lme1$residuals/streams.lme1$sigma)

plot(residuals(streams.lme1)/streams.lme1$sigma,ylab="Standardized Residuals")
hist(residuals(streams.lme1)/streams.lme1$sigma,xlab="Standardized Residuals",main="")

#homogeneity of within group variance
boxplot(split(residuals(streams.lme1)/streams.lme1$sigma,Streams$Stream),ylab="Standardized Residual",xlab="Stream",csi=0.2)
abline(0,0,lwd=3)
#normality of the between-group residuals
re.sigma <- as.numeric(VarCorr(streams.lme1)[1,2])
print(streams.lme1$coefficients$random$Stream)
re<-streams.lme1$coefficients$random$Stream/re.sigma  ##/0.6184
qqnorm(re,ylab="Quantiles of random effects")
qqline(re)
hist(streams.lme1$coefficients$random$Stream/re.sigma,xlab="random effects",main="")


##### to the TMB Robin! ########

## Load TMB TRUE
library(TMB)

## Make C++ file
#TMB::template("streams.cpp")

## Compile and load the model
compile("streams.cpp")
dyn.load(dynlib("streams"))

## Data and parameters
data <- list(density = Streams$Density,
             stream = Streams$Stream)
parameters <- list(mu=0, logSigma=0, logSigmaS=0, u=rep(0,length(unique(Streams$Stream))))

## Make a function object
obj <- MakeADFun(data, parameters, random = "u", DLL="streams")

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr

summary(sdr,select="random")
summary(sdr,select="fixed")





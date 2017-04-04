library(rjags)

data = list(N=162, a=40, Y1=NA, Y2=NA, api=1, bpi=1, aS=4.4,
            bS=13.31, aC=71.25, bC=3.75)
## Initial values
inits = function(){list(pi=0.5, S=0.9, C=0.8, Y1=10, Y2=10)}
## Model specification
jags.m =jags.model(file="~/Dropbox/work/teaching/BIOST531/codes/diagnostic.jag", data=data, n.chains=2,
                   n.adapt=2000, inits=inits())
## Parameters to be monitored
params = c("pi", "S", "C", "PPV", "NPV")
## Sampling
samps <- coda.samples(jags.m, params, n.iter=10000, thin=5)
## Summarize posterior samples and save output results
aux <- summary(samps)
#plotting
plot(samps)
#display output
output <- cbind(aux[[1]][,c(1,2)], aux[[2]][,c(1,3,5)]) 
output

library(rjags)

#change this to your working directory
setwd("~/Dropbox/work/teaching/BIOST531/codes")

gambia<-read.csv("GambiaMissing.csv")


#Bayesian analysis using jags
cat("model {
   for(i in 1:805) {
    Y[i] ~ dbern(p[i])
    logit(p[i]) <- alpha + beta.age*(AGE[i] - mean(AGE[])) + beta.bednet*BEDNET[i] +
    beta.green*(GREEN[i] - mean(GREEN[])) + beta.phc*PHC[i]                                    
    }
    
    # model for missing exposure variable
    for(i in 1:805) {  
    BEDNET[i] ~ dbern(q[i]) 
    logit(q[i]) <- gamma[1] + gamma[2]*PHC[i] + gamma[3]*(AGE[i] - mean(AGE[])) + 
    gamma[4]*(GREEN[i] - mean(GREEN[]))
    }
    for(k in 1:4) { gamma[k] ~ dnorm(0,0.00000001) } 
    
    # vague priors on regression coefficients
    alpha ~ dnorm(0,0.00000001)
    beta.age ~ dnorm(0,0.00000001)    
    beta.bednet ~ dnorm(0,0.00000001)     
    beta.green ~ dnorm(0,0.00000001)     
    beta.phc ~ dnorm(0,0.00000001)     
    
    # calculate odds ratios of interest
    OR.age <- exp(beta.age)  # odds ratio of malaria per year      
    OR.bednet <- exp(beta.bednet)  # odds ratio of malaria for children using bednets vs children 
    # not using bednets
    OR.green <- exp(beta.green)	 # odds ratio of malaria per unit increase in greenness index of village
    OR.phc <- exp(beta.phc)		 # odds ratio of malaria for children living in villages belonging to the 
    # primary health care system versus children living in villages not in
    # the health care system
    
    logit(baseline.prev) <- alpha   # baseline.prev = prevalence of malaria in baseline group (i.e. child
    # in age group 1 (<2yrs), sleeps without bednet, and lives in a
    # village with average greenness index and not in the health care system) 
    }", file="malaria1.jag")

dat<-list(Y=gambia$Y, AGE=gambia$AGE, BEDNET=gambia$BEDNET, GREEN=gambia$GREEN,  PHC=gambia$PHC )

inits<-list(
  list(alpha = -0.51, beta.age = 1, beta.bednet = -2.41, beta.green = -0.23, beta.phc = 1.82, 
       gamma = c(0,1,-1,-1)),
  list(alpha = 1.01, beta.age = -1, beta.bednet = 0.21, beta.green = 1.81, beta.phc = -0.23, 
       gamma = c(1,-1,1,1))
)

jags.m =jags.model(file="malaria1.jag", data=dat, n.chains=2,n.adapt=2000, inits=inits)

params<-c("OR.age", "OR.bednet", "OR.green", "OR.phc")
samps <- coda.samples(jags.m, params, n.iter=10000, thin=5)
aux <- summary(samps)
#plotting
plot(samps)
#display output
output <- cbind(aux[[1]][,c(1,2)], aux[[2]][,c(1,3,5)]) 
output

#compare with complete case
complete.case.fit<-glm(Y~AGE+BEDNET+GREEN+PHC,data=gambia)
summary(complete.case.fit)

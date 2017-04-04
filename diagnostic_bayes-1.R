diag.one <- function(data=list(a=40, b= 122),
                     NPOST=5000, BURN=1000, THIN=5, #npost is how many posterior samples you wanna keep, burn is now many you wanna burn
                     a.pi=1, b.pi=1,
                     a.S=4.4, b.S=13.31,
                     a.C=71.25, b.C=3.75,  #numbers from table, specify initial values of the parameters
                     pi0=.5, S0=.5, C0=.5){
  pi <- pi0
  S <- S0
  C <- C0
  post <- matrix(0, NPOST, 5) #5 rows
  dimnames(post) <- list(NULL, c("pi", "S", "C", "PPV","NPV"))
  j <- 1
  for (i in (1:((NPOST*THIN) + BURN))){
    ## sampling from fc for latent Y1
    prob1 <- pi*S/(pi*S + (1-pi)*(1-C))
    Y1 <- rbinom(1, data$a, prob1)
    ## sampling from fc for latent Y2
    prob2 <- pi*(1-S)/(pi*(1-S) + (1-pi)*C)
    Y2 <- rbinom(1, data$b, prob2)
    ## sampling from fc for pi
    pi <- rbeta(1, Y1+Y2+a.pi, data$a+data$b-Y1-Y2+b.pi)
    ## sampling from fc for S
    S <- rbeta(1, Y1 + a.S, Y2 + b.S)
    ## sampling from fc for C
    C <- rbeta(1, data$b - Y2 + a.C, data$a - Y1 + b.C)
    ## PPV
    PPV<-Y1/data$a
    ##NPV
    NPV<-(data$b-Y2)/data$b
    if (i > BURN && (i %% THIN == 0)){
      post[j,] <- c(pi, S, C, PPV, NPV)
      j <- j+1
    }
  }
  return(post)
}

res<-diag.one()
apply(res,2,mean)  #posterior simulations
apply(res,2,quantile,c(0.025,0.5,0.975)) #interval
plot(res[,1])
plot(res[,1],type = 'l')
plot(res[,2],type = 'l')
plot(res[,3],type = 'l')

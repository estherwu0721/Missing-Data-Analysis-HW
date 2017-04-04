MK.data=as.data.frame(cbind(c(0,1,2,3,4),c(168,32,16,6,1)))
names(MK.data)=c("x","nx")

#initial value
lambda.old=sum(MK.data$x*MK.data$nx)/sum(MK.data$nx)
pa.old=0.5  #assume 0.5 is the probability of susceptable
##you can try different initial values
tol=1              #tolerance level, used to stop the loop
counter=0

s1=sum(MK.data$x*MK.data$nx)   #sum of the first moment, doesn't change throughout the iteration


while (tol>0.0001){
  #E-step
  ntilde = 168*pa.old*exp(-lambda.old)/(pa.old*exp(-lambda.old)+(1-pa.old))+(223-168)  #plug in the formula
  
  #M-step
  pa.new=ntilde/223  #
  lambda.new=s1/ntilde
  
  #the following determines whether the convergence is met
  pa.diff=abs(pa.new-pa.old)
  lambda.diff=abs(lambda.new-lambda.old)
  tol=max(pa.diff,lambda.diff) #check if converged
  pa.old=pa.new    #update
  lambda.old=lambda.new      #update
  counter=counter+1
}

lambda.new
pa.new
counter

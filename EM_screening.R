
p.old=c(40/200,10/200,10/20,140/200)
p.diff=1
count=0

while(p.diff>0.001){
  #E-step
  y11=30*p.old[1]/(p.old[1]+p.old[2])
  y10=30*p.old[2]/(p.old[1]+p.old[2])  
  y01=70*p.old[3]/(p.old[3]+p.old[4])  
  y00=70*p.old[4]/(p.old[3]+p.old[4])
  sumy=y11+y10+y01+y00
  #M-step
  p11=(40+y11)/(200+sumy)
  p10=(10+y10)/(200+sumy)
  p01=(10+y01)/(200+sumy)
  p00=(140+y00)/(200+sumy)
  p.new=c(p11,p10,p01,p00)
  p.diff=max(abs(p.new-p.old))
  p.old=p.new
  count=count+1
}


sensitivity=p11/(p11+p01)
specificity=p00/(p10+p00)

sensitivity
specificity

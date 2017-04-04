#作业改这个code
library(tree)
library(treeMI)
library(mi)
library(mitools)

gambia<-read.csv("~/Downloads/GambiaMissing.csv")

##Multiple imputation using chained equations

gambia<-missing_data.frame(gambia)

#display missing data patterns
image(gambia)

#display data types and other information
show(gambia)

#change data types
gambia<-change(gambia,y=c("AGE"),what="type",to=c("positive-continuous"))


#create multiply imputed data sets
IMP<-mi(gambia)  #if the missing data is scattered throughout columns, it will do imputation for all missing data points

#analysis
mi.fit=pool(Y~AGE+BEDNET+GREEN+PHC,IMP,family=binomial(link="logit"))
display(mi.fit)

summary(mi.fit)

#Multiple imputation via sequential regression tree
#作业里有多少个variable， factorvar就有多少个entry，1 = categorical
g1=treeMI(data.frame(gambia), ITER = 30, factorvar = c(1,0,1,0,1), minCut=5)#minCut is the minimum number of entries in a category
g2=treeMI(data.frame(gambia), ITER = 30, factorvar = c(1,0,1,0,1), minCut=5)
g3=treeMI(data.frame(gambia), ITER = 30, factorvar = c(1,0,1,0,1), minCut=5)
g4=treeMI(data.frame(gambia), ITER = 30, factorvar = c(1,0,1,0,1), minCut=5)
g5=treeMI(data.frame(gambia), ITER = 30, factorvar = c(1,0,1,0,1), minCut=5)

all<-imputationList(list(g1,g2,g3,g4,g5)) #combining the imputation
model1<-with(all,glm(Y~AGE+BEDNET+GREEN+PHC,family=binomial))
summary(MIcombine(model1))


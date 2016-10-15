# lab 6
library(alr4)
# conditional model and marginal model
set.seed(423)
x1 = rnorm(50,1,3)
x2 = rnorm(50,3,2)
x2 = c(x2[1:49],-6.33)
# two predictors are uncorrelated
cor(x1,x2)

y1 = 3*x1 -5 * x2 + rnorm(50) 

lm.x1.margin = lm(y1~x1)
lm.x1.margin.sum = summary(lm.x1.margin)
lm.1.conditional = lm(y1~x1+x2)
lm.1.conditional.summ = summary(lm.1.conditional)

# compare coefficient 
lm.x1.margin.sum$coefficients[,1]
lm.1.conditional.summ$coefficients[,1]

# compare se of beta1_hat
lm.x1.margin.sum$coefficients[,2]
lm.1.conditional.summ$coefficients[,2]

# compare t-statistics 
lm.x1.margin.sum$coefficients[,3]
lm.1.conditional.summ$coefficients[,3]

###
# now take a look at the other coefficient
lm.x2.margin = lm(y1~x2)
lm.x2.margin.sum = summary(lm.x2.margin)

# compare coefficient 
lm.x2.margin.sum$coefficients[,1]
lm.1.conditional.summ$coefficients[,1]

# compare se of beta1_hat
lm.x2.margin.sum$coefficients[,2]
lm.1.conditional.summ$coefficients[,2]

# compare t-statistics 
lm.x2.margin.sum$coefficients[,3]
lm.1.conditional.summ$coefficients[,3]

##############
# now look at the case when x1,x2 are negatively correlated
rm(list=ls())
set.seed(423)
x1 = rnorm(50,1,3)
x2 = -x1 + rnorm(50,2,2)
cor(x1,x2)
y = 2*x1 + x2 + rnorm(50,0,2)

cor(x1,y)
cor(x2,y)

lm.x1.margin=lm(y~x1)
lm.x1.margin.sum = summary(lm.x1.margin)

lm.conditional = lm(y~x1+x2)
lm.conditional.summ = summary(lm.conditional)

# compare coefficient 
lm.x1.margin.sum$coefficients[,1]
lm.conditional.summ$coefficients[,1]

# compare se of beta1_hat
lm.x1.margin.sum$coefficients[,2]
lm.conditional.summ$coefficients[,2]

# compare t-statistics 
lm.x1.margin.sum$coefficients[,3]
lm.conditional.summ$coefficients[,3]



lm.x2.margin=lm(y~x2)
summary(lm.x2.margin)$coef
summary(lm.conditional)$coef

###################
# positively correlated case
set.seed(423)
x1 = rnorm(50,1,3)
x2 = x1 - rnorm(50,2,2)
cor(x1,x2)
y = 2*x1 + rnorm(50,0,2)

cor(x1,y)
cor(x2,y)

lm.x1.margin=lm(y~x1)
summary(lm.x1.margin)$coef

lm.conditional = lm(y~x1+x2)
summary(lm.conditional)$coef

lm.x2.margin=lm(y~x2)
summary(lm.x2.margin)
summary(lm.conditional)

##########################################
# linear regression on catagorical variable
head(salary)
salary$rank
with(salary,boxplot(salary~rank))

lm.rank = with(salary,lm(salary~rank))
plot(lm.rank)
summary(lm.rank)$coef

# calculate the mean salary of each rank
tapply(salary$salary,salary$rank,mean)

## use dummy variable
# asst 0,0
# assoc 1,0
# Prof 0,1

n = dim(salary)[1]
X=matrix(ncol=3,nrow=n)
for(i in 1:n){
  if(salary[i,]$rank == "Asst"){
    X[i,]=c(1,0,0)
  }else if(salary[i,]$rank == "Assoc")
  {
    X[i,] = c(1,1,0)
  }else{
    X[i,] = c(1,0,1)
  }
}

X

Y = matrix(salary$salary,ncol=1)
Y

beta = solve(t(X) %*% X) %*% t(X) %*% Y
beta
tapply(salary$salary,salary$rank,mean)


t(X) %*% X

lm.rank.mean = with(salary,lm(salary ~ -1 + rank))
summary(lm.rank.mean)$coef

## use dummy variable, without intercept
# asst 1,0,0
# assoc 0,1,0
# Prof 0,0,1
X2=matrix(ncol=3,nrow=n)
for(i in 1:n){
  if(salary[i,]$rank == "Asst"){
    X2[i,]=c(1,0,0)
  }else if(salary[i,]$rank == "Assoc")
  {
    X2[i,] = c(0,1,0)
  }else{
    X2[i,] = c(0,0,1)
  }
}
beta2 = solve(t(X2) %*% X2) %*% t(X2) %*% Y
beta2

# linear regression with both continous and catagorical varible
lm.year.rank = with(salary,lm(salary~year+rank))
summary(lm.year.rank)
par(mar=c(3,3,2,2))
with(subset(salary,rank == "Prof"),plot(year,salary,
     xlim=c(0,25),ylim=c(15000,40000),col="red",pch=20))
with(subset(salary,rank == "Assoc"),points(year,salary,col="blue",pch=20))
with(subset(salary,rank == "Asst"),points(year,salary,col="green",pch=20))
legend("topleft",col=c("red","blue","green"),legend=c("Prof","Assoc","Asst"),
      pch=20 )

coefs = summary(lm.year.rank)$coef[,1]
coefs

abline(coefs[1],coefs[2],col="green",lwd=2)
abline(coefs[1]+coefs[3],coefs [2],col="blue",lwd=2)
abline(coefs[1]+coefs[4],coefs [2],col="red",lwd=2)




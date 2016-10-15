rm(list=ls())
library(alr4)
BGSgirls
head(BGSgirls)

# simple linear regression on Soma 
# predictor WT18
# response Soma

n=dim(BGSgirls)[1]
n
x = BGSgirls$WT18
Y = BGSgirls$Soma
plot(x,Y,xlab = "WT18", ylab = "Y")

lm.SLR = lm(Y~x)
abline(lm.SLR)

###########################################################
# calculate the estmate of coeffient 
# and covariance estimate using matrix algebra

# X denote the data matrix
X = cbind(rep(1,n),x)
class(X)

# t() transpose a matrix

t(X)

# %*% matrix product 
t(X) * X

t(X) %*% X
# solve() solve a linear equation system. can be used to inverse a matrix
# calculate the coefficient


beta = solve(t(X) %*% X) %*% t(X) %*% Y
beta

lm.SLR$coefficients

# calculate fitted value using matrix algebra

Y.fitted = X %*% beta
Y.fitted


H = X %*% solve(t(X) %*% X) %*% t(X)
# H is called hat matrix 
H

all.equal(H %*% H,H)
I = diag(rep(1,n))
I - H
(I - H) %*% (I - H)
H %*% (I - H)
Y.fitted2 = H %*% Y

all.equal(Y.fitted,Y.fitted2)

# calculate RSS and sigma^2
RSS = sum((Y - Y.fitted)^2)
RSS
sigma2 = RSS / (n-2)
sigma = sqrt(sigma2)
# calculate covariance matrix of beta
beta.cov = solve(t(X) %*% X) * sigma2
beta.cov

# compare with the output for lm
vcov(lm.SLR)

############################################
# confidence interval and prediction interval 
alpha = 0.05
x.bar = mean(x)
SXX = (n-1) * var(x)
# CI
par(mfrow=c(1,1))
plot(x,Y,xlab = "WT18", ylab = "soma",xlim=c(40,100),ylim=c(2,8))

# for any point x*, calculate the confidence interval 
x.star = seq(40,100,length.out = 50)

X.star = cbind(rep(1,50),x.star) 
X.star
y.star = X.star %*% beta 
y.star
CI.upper = y.star + sqrt(1/n + (x.star-x.bar)^2 / SXX ) * qt(1-alpha/2,n-2) * sigma
CI.lower = y.star - sqrt(1/n + (x.star-x.bar)^2 / SXX ) * qt(1-alpha/2,n-2) * sigma

lines(x.star,y.star,col="red",lwd=2)
lines(x.star,CI.upper,lty=2,col="red",lwd=1)
lines(x.star,CI.lower,lty=2,col="red",lwd=1)

# PI
PI.upper = y.star + sqrt(1 + 1/n + (x.star-x.bar)^2 / SXX ) * qt(1-alpha/2,n-2) * sigma
PI.lower = y.star - sqrt(1 + 1/n + (x.star-x.bar)^2 / SXX ) * qt(1-alpha/2,n-2) * sigma

lines(x.star,PI.upper,lty=2,col="blue",lwd=2)
lines(x.star,PI.lower,lty=2,col="blue",lwd=2)

legend("bottomright", legend = c("CI","PI"),col = c("red","blue"),lty=2,lwd=2)
##################################################
# add variable plot
# use two predictors WT18 HT18
x1 = BGSgirls$WT18
x2 = BGSgirls$HT18
Y = BGSgirls$Soma
# paired scatter plot
pairs(data.frame(WT18=x1,HT18=x2,Soma=Y))

lm.MLR2 = lm(Y ~ x1 + x2)
summary(lm.MLR2)

par(mfrow=c(1,2))
plot(x1,Y,xlab = "WT18", ylab = "soma")
lm.Y.x1 = lm(Y ~ x1)
abline(lm.Y.x1)

lm.x2.x1 = lm(x2 ~ x1)
plot(x1,x2,xlab = "HT18", ylab = "WT18")
abline(lm.x2.x1)

# regression using the residual in the first model and residual in the second model
lm.x2.adjust = lm(lm.Y.x1$residuals~ lm.x2.x1$residuals)
plot(lm.Y.x1$residuals,lm.x2.x1$residuals)
abline(lm.x2.adjust)
# ?? how to intepret the model 

## Multiple regression model 
lm.x1.x2 = lm(Y ~ x1 + x2)

summary(lm.x1.x2)

# compare with added model
lm.x1.x2.summ = summary(lm.x1.x2)
lm.x2.adjust.summ = summary(lm.x2.adjust)
lm.x1.x2.summ$coefficients[3,]
lm.x2.adjust.summ$coefficients[2,]

# compare sigma
lm.x1.x2.summ$sigma
lm.x1.adjust.summ$sigma

# compare RSS
sum(lm.x2.adjust$residuals^2)
sum(lm.x1.x2$residuals^2)

# the different is in sigma from the two model
lm.x1.x2.summ$coefficients[3,2] / lm.x1.x2.summ$sigma
lm.x2.adjust.summ$coefficients[2,2] / lm.x2.adjust.summ$sigma


##########################################
# derive multiple regression formula based on matrix algebra
X = cbind(rep(1,n),x1,x2)
beta = solve(t(X) %*% X) %*% t(X) %*% Y
lm.x1.x2

RSS.x1.x2 = sum((Y - X %*% beta)^2)
sigma.x1.x2 = RSS.x1.x2/(n-3)

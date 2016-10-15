#########
# lm example
# create a dataset
library(alr4)


BGSgirls
head(BGSgirls)
with(BGSgirls,plot(HT2,HT18))

lm.BGS = lm(HT18~HT2,data = BGSgirls)
lm.BGS

# coefficient
lm.BGS$coefficients
coef(lm.BGS)
coefficients(lm.BGS)

df = lm.BGS$df.residual

lm.BGS$
  
# variance of coefficient
vcov(lm.BGS)

# residual 
lm.BGS$residuals
resid(lm.BGS)

# fitted value
lm.BGS$fitted.values
fitted(lm.BGS)
fitted.values(lm.BGS)

plot(lm.BGS)

par(mfrow=c(2,2))
plot(lm.BGS)
plot(lm.BGS,mgp=c(2,1,0.5))


par(mar=c(0.5, 0.5, 0.2, 0.2), mfrow=c(2,2),
    oma = c(2, 2, 0.2, 0.2))

plot(lm.BGS)

#residual plot
par(mfrow=c(1,1),mar=c(4,4,2,1),oma=c(0,0,1,0))
with(lm.BGS,plot(fitted.values,residuals,pch=20,cex=1,xlab="fitted",ylab="residual"))
abline(h=0, lty=2)

# check normality assumption
qqnorm(resid(lm.BGS))
qqline(resid(lm.BGS))

## Summary objects
sum.BGS = summary(lm.BGS)
sum.BGS


sum.BGS$residuals

sum.BGS$coefficients

# estimated intercept and slope beta
beta = sum.BGS$coefficients[,1]

BGS.fitted = beta[0] + beta[1] * BGSgirls$HT2

sum.BGS
BGS.t = beta/sum.BGS$coefficients[,2]
BGS.t
BGS.p = 2 * (1 - pt(abs(BGS.t),df))
BGS.p

# sigma^2

sum.BGS$sigma

sqrt(sum(sum.BGS$residuals^2)/df)

# make prediction
with(BGSgirls,plot(HT2,HT18,pch=20))
abline(lm.BGS)
# predict for the point HT = 84,93,96
predict(lm.BGS)
all.equal(predict(lm.BGS),fitted(lm.BGS))
predict(lm.BGS,newdata=84) # wrong?
predict(lm.BGS,newdata=c(84,93,96)) # wrong?

# use dataframe
predict(lm.BGS,newdata=data.frame(HT2=84))
predict(lm.BGS,newdata=data.frame(HT2=c(84,94,96)))

####################################3

(n.Anscombe <- with(anscombe,length(x1)))  # 11

x.bar.Anscombe.1 <- with(anscombe,mean(x1))
y.bar.Anscombe.1 <- with(anscombe,mean(y1))
SXX.Anscombe.1 <- with(anscombe,(n.Anscombe-1)*var(x1))
SYY.Anscombe.1 <- with(anscombe,(n.Anscombe-1)*var(y1))
SXY.Anscombe.1 <- with(anscombe,(n.Anscombe-1)*cov(x1,y1))

x.bar.Anscombe.2 <- with(anscombe,mean(x2))
y.bar.Anscombe.2 <- with(anscombe,mean(y2))
SXX.Anscombe.2 <- with(anscombe,(n.Anscombe-1)*var(x2))
SYY.Anscombe.2 <- with(anscombe,(n.Anscombe-1)*var(y2))
SXY.Anscombe.2 <- with(anscombe,(n.Anscombe-1)*cov(x2,y2))

x.bar.Anscombe.3 <- with(anscombe,mean(x3))
y.bar.Anscombe.3 <- with(anscombe,mean(y3))
SXX.Anscombe.3 <- with(anscombe,(n.Anscombe-1)*var(x3))
SYY.Anscombe.3 <- with(anscombe,(n.Anscombe-1)*var(y3))
SXY.Anscombe.3 <- with(anscombe,(n.Anscombe-1)*cov(x3,y3))

x.bar.Anscombe.4 <- with(anscombe,mean(x4))
y.bar.Anscombe.4 <- with(anscombe,mean(y4))
SXX.Anscombe.4 <- with(anscombe,(n.Anscombe-1)*var(x4))
SYY.Anscombe.4 <- with(anscombe,(n.Anscombe-1)*var(y4))
SXY.Anscombe.4 <- with(anscombe,(n.Anscombe-1)*cov(x4,y4))

all.equal(x.bar.Anscombe.1,x.bar.Anscombe.2)  # TRUE
all.equal(x.bar.Anscombe.2,x.bar.Anscombe.3)  # TRUE
all.equal(x.bar.Anscombe.3,x.bar.Anscombe.4)  # TRUE

all.equal(y.bar.Anscombe.1,y.bar.Anscombe.2)  # TRUE  
all.equal(y.bar.Anscombe.2,y.bar.Anscombe.4)  # TRUE
all.equal(y.bar.Anscombe.2,y.bar.Anscombe.3)  # "Mean relative difference: 0.0001211974"
y.bar.Anscombe.2  # 7.500909
y.bar.Anscombe.3  # 7.5

all.equal(SXX.Anscombe.1,SXX.Anscombe.2)  # TRUE
all.equal(SXX.Anscombe.2,SXX.Anscombe.3)  # TRUE
all.equal(SXX.Anscombe.3,SXX.Anscombe.4)  # TRUE

SYY.Anscombe.1  # 41.27269
SYY.Anscombe.2  # 41.27629
SYY.Anscombe.3  # 41.2262 
SYY.Anscombe.4  # 41.23249

SXY.Anscombe.1  # 55.01
SXY.Anscombe.2  # 55   
SXY.Anscombe.3  # 54.97
SXY.Anscombe.4  # 54.99

(beta.1.hat.Anscombe.1 <- SXY.Anscombe.1/SXX.Anscombe.1)  # 0.5000909
(beta.1.hat.Anscombe.2 <- SXY.Anscombe.2/SXX.Anscombe.2)  # 0.5      
(beta.1.hat.Anscombe.3 <- SXY.Anscombe.3/SXX.Anscombe.3)  # 0.4997273
(beta.1.hat.Anscombe.4 <- SXY.Anscombe.4/SXX.Anscombe.4)  # 0.4999091

(beta.0.hat.Anscombe.1 <- y.bar.Anscombe.1 - beta.1.hat.Anscombe.1*x.bar.Anscombe.1)  # 3.000091
(beta.0.hat.Anscombe.2 <- y.bar.Anscombe.2 - beta.1.hat.Anscombe.2*x.bar.Anscombe.2)  # 3.000909
(beta.0.hat.Anscombe.3 <- y.bar.Anscombe.3 - beta.1.hat.Anscombe.3*x.bar.Anscombe.3)  # 3.002455
(beta.0.hat.Anscombe.4 <- y.bar.Anscombe.4 - beta.1.hat.Anscombe.4*x.bar.Anscombe.4)  # 3.001727

### overhead II-73 ... Anscombe's first data set

with(anscombe,plot(x1,y1,xlim=c(3,20),ylim=c(2,14),xlab="Predictor", ylab="Response",col="blue"))
abline(beta.0.hat.Anscombe.1,beta.1.hat.Anscombe.1,lwd=2,col="blue")


### overhead II-74 ... Anscombe's second data set

with(anscombe,plot(x2,y2,xlim=c(3,20),ylim=c(2,14),xlab="Predictor", ylab="Response",col="blue"))
abline(beta.0.hat.Anscombe.2,beta.1.hat.Anscombe.2,lwd=2,col="blue")

### overhead II-75 ... Anscombe's third data set

with(anscombe,plot(y3))  # to figure out index of separated point

with(anscombe,plot(x3,y3,xlim=c(3,20),ylim=c(2,14),xlab="Predictor", ylab="Response",col="blue"))
abline(beta.0.hat.Anscombe.3,beta.1.hat.Anscombe.3,lwd=2,col="blue")
with(anscombe,points(x3[3],y3[3],pch=20,col="red"))

### overhead II-76 ... Anscombe's fourth data set

with(anscombe,plot(x4))  # to figure out index of separated point

with(anscombe,plot(x4,y4,xlim=c(3,20),ylim=c(2,14),xlab="Predictor", ylab="Response",col="blue"))
abline(beta.0.hat.Anscombe.4,beta.1.hat.Anscombe.4,lwd=2,col="blue")
with(anscombe,points(x4[8],y4[8],pch=20,col="red"))

### overhead II-78 ... residuals vs. fitted values, data set #1

fitted.Anscombe.1 <- with(anscombe,beta.0.hat.Anscombe.1 + beta.1.hat.Anscombe.1*x1)
residuals.Anscombe.1 <- with(anscombe,y1 - fitted.Anscombe.1)

with(anscombe,plot(fitted.Anscombe.1,residuals.Anscombe.1,xlab="Fitted values", ylab="Residuals",col="blue"))
abline(h=0,lty="dashed")


### overhead II-80 ... residuals vs. fitted values, data set #2

fitted.Anscombe.2 <- with(anscombe,beta.0.hat.Anscombe.2 + beta.1.hat.Anscombe.2*x2)
residuals.Anscombe.2 <- with(anscombe,y2 - fitted.Anscombe.2)

with(anscombe,plot(fitted.Anscombe.2,residuals.Anscombe.2,xlab="Fitted values", ylab="Residuals",col="blue"))
abline(h=0,lty="dashed")

### overhead II-81 ... residuals vs. predictors, data set #2

with(anscombe,plot(x2,residuals.Anscombe.2,xlab="Predictors", ylab="Residuals",col="blue"))
abline(h=0,lty="dashed")

### overhead II-82 ... residuals vs. fitted values, data set #3

fitted.Anscombe.3 <- with(anscombe,beta.0.hat.Anscombe.3 + beta.1.hat.Anscombe.3*x3)
residuals.Anscombe.3 <- with(anscombe,y3 - fitted.Anscombe.3)

with(anscombe,plot(fitted.Anscombe.3,residuals.Anscombe.3,xlab="Fitted values", ylab="Residuals",col="blue"))
abline(h=0,lty="dashed")

### overhead II-83 ... residuals vs. predictors, data set #3

with(anscombe,plot(x3,residuals.Anscombe.3,xlab="Predictors", ylab="Residuals",col="blue"))
abline(h=0,lty="dashed")

### overhead II-84 ... residuals vs. fitted values, data set #4

fitted.Anscombe.4 <- with(anscombe,beta.0.hat.Anscombe.4 + beta.1.hat.Anscombe.4*x4)
residuals.Anscombe.4 <- with(anscombe,y4 - fitted.Anscombe.4)

with(anscombe,plot(fitted.Anscombe.4,residuals.Anscombe.4,xlab="Fitted values", ylab="Residuals",col="blue"))
abline(h=0,lty="dashed")

### overhead II-86 ... fitted values and predictions are linearly related ...

with(anscombe,plot(x1,fitted.Anscombe.1,xlab="Predictors", ylab="Fitted values",col="blue"))
with(anscombe,abline(lm(fitted.Anscombe.1~x1),lwd=2))

with(anscombe,plot(x2,fitted.Anscombe.2,xlab="Predictors", ylab="Fitted values",col="blue"))
with(anscombe,abline(lm(fitted.Anscombe.2~x2),lwd=2))

with(anscombe,plot(x3,fitted.Anscombe.3,xlab="Predictors", ylab="Fitted values",col="blue"))
with(anscombe,abline(lm(fitted.Anscombe.3~x3),lwd=2))

with(anscombe,plot(x4,fitted.Anscombe.4,xlab="Predictors", ylab="Fitted values",col="blue"))
with(anscombe,abline(lm(fitted.Anscombe.4~x4),lwd=2))


##########################


# simulation example
set.seed(423)
#generate x

x = rnorm(50,2,3)

beta0 = numeric(5000)
beta1 = numeric(5000)
t_beta1 = numeric(5000)
for(i in 1:5000){
  y = 2 + 0 * x + rnorm(50,0,1)
  lm.0 = lm(y~x)
  summary.0 = summary(lm.0)
  beta0[i] = lm.0$coefficients[1]
  beta1[i] = lm.0$coefficients[2]
  t_beta1[i] = summary.0$coefficients[2,4]
}

mean(t_beta1 < 0.05)
mean(t_beta1 < 0.1)
hist(t_beta1)

hist(beta0)
hist(beta1)

plot(beta0,beta1)





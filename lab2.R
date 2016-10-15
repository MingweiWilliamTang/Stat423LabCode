# lab2 Linear model, overfitting and cross-validation
# Heights<-read.table("http://www.math.uah.edu/stat/data/Galton.txt",header=T)
set.seed(423)
n=1000
x = runif(1000,55,72)
y= x*0.21 + 90 - x^2*0.01   + rnorm(1000,0,3.5)
plot(x,y)
x= round(x)
y=round(y)
Heights=data.frame(Mother=x,Son=y)
head(Heights)
with(Heights,plot(Mother,Son))
# there are many points overlap, to have better visualization, we can use 
# jatter function
# the location of point will be randomly and slightly changed
jx = jitter(x)
jy = jitter(y)
plot(jx,jy,xlab="Mother",ylab="son")


Height.cond.mean=with(Heights,tapply(Son,Mother,mean))
Height.cond.mean
class(Height.cond.mean)
ux = sort(unique(Heights$Mother))
ux
# match function returns the index of the an element in a vector
match(55,ux)
match(c(56,58,72),ux)

# plot the mean of the first cluster
xs=55
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(jx,jy)
points(jx[x==xs],jy[x==xs],col="blue")
points(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",pch="X",cex=2)
lines(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red") 

# another way to do
names(Height.cond.mean)


# change that into a numerical vector
ux2=as.numeric(names(Height.cond.mean))
ux2

xs=55
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(jx,jy)
points(jx[x==xs],jy[x==xs],col="blue")
points(55:xs,Height.cond.mean[ match(55:xs,ux2) ], col="red",pch="X",cex=2)
lines(55:xs,Height.cond.mean[ match(55:xs,ux2) ], col="red") 

#
xs<-56
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(jx,jy)
points(jx[x==xs],jy[x==xs],col="blue")
points(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",pch="X",cex=2)
lines(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",cex=2,lwd=2) 

# using a for loop to label all the condition mean value
xs=max(x)
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(jx,jy)
points(jx[x==xs],jy[x==xs],col="blue")
points(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",pch="X",cex=2)
lines(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",cex=2,lwd=2) 

# the third way to do, use anova
anova.1=lm(Son~as.factor(Mother)-1,data=Heights)
cond.mean2=anova.1$coefficients
cond.mean2
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(jx,jy)
points(55:xs,cond.mean2[ match(55:xs,ux) ], col="red",pch="X",cex=2)
lines(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",cex=2,lwd=2)


# residual plot 

plot(anova.1$fitted.values,anova.1$residuals)
abline(h=0,lty=2)



# simple linear regression model
plot(jx,jy,xlab="Mother",ylab="Son")
lm.1 = with(Heights,lm(Son~Mother))
abline(lm.1,lwd=2,col="purple")
summary(lm.1)
SXY=sum((x-mean(x))*(y-mean(y)))
SXX=sum((x-mean(x))^2)
beta=SXY/SXX
alpha=mean(y)-beta*mean(x)
c(alpha,beta)
summary(lm.1)$coef
# residual plot
plot(lm.1$fitted.values,lm.1$residuals,ylab="residual",xlab="fitted")
abline(h=0,lty=2)





# now compare the models
plot(jx,jy,xlab="Mother",ylab="Son")
points(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",pch="X",cex=2)
lines(55:xs,Height.cond.mean[ match(55:xs,ux) ], col="red",cex=2,lwd=2) 
abline(lm.1,lwd=2,col="purple")
legend("topleft",legend=c("empirical mean","SLR"),lty=1,col=c("red","purple"),lwd=2)

## compare RSS
RSS.emprical=sum((anova.1$residuals)^2)
RSS.lm = sum((lm.1$residuals)^2)

RSS.emprical
RSS.lm

 # Question: Is empircal conditional a better model than linear regression? 

# Using cross-validation
y_ls<-y_np<-NULL

# leave-one-out-cross valiadation

for(i in 1:n){
  xmi<-x[-i]  
  ymi<-y[-i]    
  
  bmi<-lm(ymi~xmi)$coef
  y_ls<-c(y_ls, bmi[1] + bmi[2]*x[i] )
  
  y_np<-c(y_np,mean(ymi[xmi==x[i]]) )
  
}
  
mean( (y-y_ls)^2 ) 

mean( (y-y_np)^2 )  



# multiple linear regression


lm.2 = with(Heights,lm(Son~Mother + I(Mother^2)))
s ummary(lm.2)
odr = order(Heights$Mother)
plot(jx,jy,xlab="Mother",ylab="Son")
with(Heights[odr,],lines(Mother,predict(lm.2)[odr],type="l",col="red",lwd=2))
abline(lm.1) 


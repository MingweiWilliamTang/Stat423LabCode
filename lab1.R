  # for bivariate data
x<-cars$speed
y<-cars$dist

plot(x,y,xlab="speed",ylab="dist",main="dist vs speed for cars data")

# change point type "pch=1,2,3,...", cex is the size of the points

plot(x,y,pch=20,col="red",xlab="speed",
     ylab="dist",main="dist vs speed for cars data",cex=1.5)

# covariance
# r function

cov.byR = cov(x,y)
cov.byR
cov(x+3,y-1)
cov(2*x,y)

cov(2*x,3*y)

cov(2*x,-3*y)
# by hand

n=length(x)
cov.byHand = sum((x-mean(x))*(y-mean(y)))/(n-1)
all.equal(cov.byR,cov.byHand)

# correlation
# by r
cor.byR = cor(x,y)
cor.byR
cor(2*x+3,3*y-1)
all.equal(cor.byR,cor(2*x+3,3*y-1))
cor(2*x,-3*y-1)

# by hand
cor.byHand = cov.byHand/(sd(x)*sd(y))
all.equal(cor.byR,cor.byHand)

SXX = sum((x-mean(x))^2)
SYY = sum((y-mean(y))^2)
SXY= sum((x-mean(x))*(y-mean(y)))
cor.byHand2 = SXY / sqrt(SXX * SYY)
all.equal(cor.byHand2,cor.byHand)

# more on plot
cars
cartype = as.factor(rep(c(1,2),each = 25))
cartype
newdata = data.frame(speed =x ,dist = y , cartype = cartype)

head(newdata)
tail(newdata)

boxplot(speed~cartype,data = newdata,names = c("compact","sports"))

boxplot(speed~cartype,data = newdata,names = c("compact","sports"),
        col=c("red","green"))


# scatterplot for two types of cars 
plot(newdata$speed,newdata$dist,pch=20)
# you can use the with function to get rid of the "$" sign is dataframe
with(newdata,plot(speed,dist,pch=20,main="dist vs speed"))

# now we draw the two type of cars in different color
with(subset(newdata,cartype==1),plot(speed,dist,pch=20,
                                     main="dist vs speed",col="red"))

with(subset(newdata,cartype==2),plot(speed,dist,pch=20,
                                     main="dist vs speed",col="red"))

# it overlaps the previous plot, the solution is to use points 
with(subset(newdata,cartype==1),plot(speed,dist,pch=20,xlim=c(0,30),ylim=c(0,150),cex=1.5,
                                     main="dist vs speed",col="red"))


with(subset(newdata,cartype==2),points(speed,dist,pch=2,cex=1.5,
                                     main="dist vs speed",col="blue"))


# adding legends to the plot
legend(0,150,legend=c("compact","sports"),pch=c(20,2),col=c("red","blue"))

# or your can use "topleft", "topright", "bottomleft","bottomright" to
# specify the location of the legend

legend("bottomright",legend=c("compact","sports"),pch=c(20,2),col=c("red","blue"))



# now some instructions on linear regression
# the syntax is similar as in 421

cars.lm = lm(dist~speed,data=newdata)
cars.lm

cars.lm$coefficients

# add the regression line to the plot: abline
# for abline, you input the slope and intercept of the line your want to draw
abline(cars.lm$coefficients[1],cars.lm$coefficients[2])

# an easier way
abline(cars.lm,col="green",lwd=2)

# you can also use abline to draw horizontal or vertical lines
# horizontal case
abline(h=50,lty=2)

# vertical casse
abline(v=15,lty=2)

cars.lm$fitted.values

points(newdata$speed,cars.lm$fitted.values,pch=1)

# or you can use 
fitted(cars.lm)
all.equal(fitted(cars.lm),cars.lm$fitted.values)


summary(cars.lm)
# calculating the p-value
2*(1-pt(2.601,df=48))
2*(1-pt(9.464,df=48))

car.lm.sum = summary(cars.lm)
car.lm.sum$




# lab 7 
library(alr4)
head(salary)
colnames(salary)
par(mar=c(3,3,1,1))
n = dim(salary)[1]
############################
# 1. regression model with interaction model 
# predictor: 
# year: continuous 
salary$year
# rank: catagorical 
salary$rank

lm.year.rank = with(salary,lm(salary~year + rank + year:rank))
sum.year.rank = summary(lm.year.rank)
sum.year.rank

lm.year.rank2 = with(salary,lm(salary~year * rank))
sum.year.rank2 = summary(lm.year.rank2)

sum.year.rank$coefficients
sum.year.rank2$coefficients

with(salary,plot(year,salary))
points(salary$year,fitted(lm.year.rank),cex=0.5,col="red",pch=3)


with(subset(salary,rank=="Asst"),plot(year,salary,ylim=c(10000,40000),
                                      pch=20,xlim=c(0,25),col="green"))
with(subset(salary,rank=="Assoc"),points(year,salary,pch=20,col="blue"))
with(subset(salary,rank=="Prof"),points(year,salary,pch=20,col="red"))
coef.1= sum.year.rank$coefficients
coef.1
interAsst = coef.1[1,1]
interAssoc = coef.1[1,1] + coef.1[3,1]
interProf = coef.1[1,1] + coef.1[4,1]
slopAsst = coef.1[2,1]
slopAsso = coef.1[2,1] + coef.1[5,1]
slopProf = coef.1[2,1] + coef.1[6,1]
abline(interAsst,slopAsst,col="green")
abline(interAssoc,slopAsso,col="blue")
abline(interProf,slopProf,col="red")


# compare with three separate regreesion model 
lm.Asst = with(subset(salary,rank=="Asst"),lm(salary~year))
lm.Assoc = with(subset(salary,rank=="Assoc"),lm(salary~year))
lm.Prof = with(subset(salary,rank == "Prof"),lm(salary~year))

lm.Asst$coefficients
c(interAsst,slopAsst)

lm.Assoc$coefficients
c(interAssoc,slopAsso)

lm.Prof$coefficients
c(interProf,slopProf)


# model comparision 
# Anova F-test 

# simple linear regression model 
lm.year = with(salary,lm(salary~year))
lm.year.sum = summary(lm.year)

# mulitple linear regression without interaction 
lm.same.slope = with(salary,lm(salary~ year + rank))
lm.same.slope.sum = summary(lm.same.slope)

# multiple linear regression with interation
lm.interaction = with(salary,lm(salary~year * rank))
lm.interaction.sum = summary(lm.interaction)

# campare R^2
c(lm.year.sum$r.squared,lm.same.slope.sum$r.squared,lm.interaction.sum$r.squared)

# campare RSS
RSS = function(lm.res){
  return(sum(lm.res$resid^2))
}

c(RSS(lm.year),RSS(lm.same.slope),RSS(lm.interaction))

# Anova F-test
anova(lm.year,lm.same.slope)
df1 = lm.year.sum$df[2]
df2 = lm.same.slope.sum$df[2]
F1 = (RSS(lm.year) - RSS(lm.same.slope))/(df1-df2) / (RSS(lm.same.slope)/df2)
1- pf(F1,df1-df2,df2)

anova(lm.same.slope,lm.interaction)
df3 = lm.interaction.sum$df[2]
F2 = (RSS(lm.same.slope) - RSS(lm.interaction))/(df2-df3) / (RSS(lm.interaction)/df3)
F2
1 - pf(F2,df2-df3,df3)

# polynomial regression 

US.pop <- data.frame(year=seq(1790, 2010, 10),
                     c.year=seq(-110, 110, 10),
                     pop=c(3.929214, 5.308483, 7.239881, 9.638453, 12.860702,
                           17.063353, 23.191876, 31.443321, 38.558371, 50.189209,
                           62.979766, 76.212168, 92.228496, 106.021537, 123.202624,
                           132.164569, 151.325798, 179.323175, 203.302031, 226.542203,
                           248.709873, 281.421906, 308.745538))

n = dim(US.pop)[1]
set.seed(423)
shuffle = sample(1:n,n,replace = F)
US.pop = US.pop[shuffle,]
with(US.pop,plot(year, pop, col="blue", xlab="Year", 
                 ylab="US Population (millions)", xlim=c(1790,2020), ylim=c(0,335)))


# quadratic fit with centered data

lm.USpop.2 <- lm(pop ~ c.year + I(c.year^2), data=US.pop)

with(US.pop,plot(year, pop, col="blue", xlab="Year", 
                 ylab="US Population (millions)", xlim=c(1790,2020), ylim=c(0,335)))
# unable to plot the fitted curve by directly using lines
lines(US.pop$year,fitted(lm.USpop.2),col="red",lwd=2)
US.pop$year


# method 1, sort predictor in ascending order
index = order(US.pop$year)
US.pop$year[index]
with(US.pop,plot(year, pop, col="blue", xlab="Year", 
                 ylab="US Population (millions)", xlim=c(1790,2020), ylim=c(0,335)))
lines(US.pop$year[index],lm.USpop.2$fitted[index],col="red")

# method 2, calculate directly 
x = seq(1790,2010,by = 10)
x.c= x - mean(x)
y = cbind(rep(1,length(x)),x.c,x.c^2) %*% matrix(lm.USpop.2$coefficients,ncol=1)
with(US.pop,plot(year, pop, col="blue", xlab="Year", 
                 ylab="US Population (millions)", xlim=c(1790,2020), ylim=c(0,335)))

lines(x,y,col="red")

# quadratic fit with noncentered data
lm.USpop.nc <- lm(pop ~ year + I(year^2), data=US.pop)
all.equal(fitted(lm.USpop.nc),fitted(lm.USpop.2))
summary(lm.USpop.nc)$coef
summary(lm.USpop.2)$coef

round(vcov(lm.USpop.nc),10)
round(vcov(lm.USpop.2),10)
 ### prediction for 2020

pop.2020.2 <- predict(lm.USpop.2, newdata=data.frame(year=2020, c.year=120))
round(pop.2020.2)  # 335

with(US.pop,plot(year, pop, col="blue", xlab="Year", ylab="US Population (millions)", xlim=c(1790,2020), ylim=c(0,335)))
points(2020, pop.2020.2, pch=8)
lines(c(US.pop$year[index],2020),c(fitted(lm.USpop.2)[index],pop.2020.2),col="red",lwd=2)

### overhead V-61 ... with d=10 fit

lm.USpop.10 <- lm(pop ~ year + I(c.year^2) + I(c.year^3) + I(c.year^4) + I(c.year^5) + I(c.year^6) + I(c.year^7) + I(c.year^8) + I(c.year^9) + I(c.year^10), data=US.pop)

with(US.pop,plot(year, pop, col="blue", xlab="Year", ylab="US Population (millions)", xlim=c(1790,2020), ylim=c(0,335)))
lines(US.pop$year[index],fitted(lm.USpop.10)[index],col="red",lwd=2)

### overhead V-62 ... and prediction for 2020

pop.2020.10 <- predict(lm.USpop.10, newdata=data.frame(year=2020, c.year=120))
round(pop.2020.10)  # 330

with(US.pop,plot(year, pop, col="blue", xlab="Year", ylab="US Population (millions)", xlim=c(1790,2020), ylim=c(0,335)))
points(2020, pop.2020.10, pch=8)
lines(c(US.pop$year[index],2020),c(fitted(lm.USpop.10)[index],pop.2020.10),col="red",lwd=2)

anova(lm.USpop.2,lm.USpop.10)


library(alr4)
# model selection using leap package
install.packages("leaps")
library(leaps)
head(BGSboys)
dim(BGSboys)

# X is matrix of predictor
X = BGSboys[,1:11]
predName = colnames(X)
predName

# Y is vector of response
Y = BGSboys[,12]

# use leaps to do model subset selection
BGS.leaps = leaps(X,Y)

BGS.leaps

BGS.leaps$Cp

# select the model with minimum cp
# find the index of the model that has mininum Cp
BGS.best.index = which.min(BGS.leaps$Cp)
BGS.best.index
# find the predictors in the model
BGS.leaps$which[BGS.best.index,]

predName[BGS.leaps$which[BGS.best.index,]]
predId = predName[BGS.leaps$which[BGS.best.index,]]

BGS.best.dataframe = data.frame(Y,X[,predId])

colnames(BGS.best.dataframe)
BGS.best.lm = lm(Y ~ ., data=BGS.best.dataframe)
summary(BGS.best.lm)



data(swiss)
head(swiss)
a<-regsubsets(Fertility~.,data=swiss)
plot(a)
# nbest number of best 3 models at each predictor size
a<-regsubsets(Fertility~.,nbest = 3,data=swiss)
plot(a)
a.sum = summary(a)
a.sum$bic
a.sum$cp

#####################
# logistic regression 
# use function glm
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
dim(mydata)
n = dim(mydata)[1]
# study the relationship between gpa and admitted
# campare admitted gpa with non-admitted gpa

gpa.add = subset(mydata,admit==1,select = "gpa")
gpa.add
gpa.Notadd = subset(mydata,admit==0,select = "gpa")
gpa.Notadd
boxplot(gpa~admit,data=mydata,names=c("not","admitted"))
with(mydata,plot(gpa,admit,pch=20))


mydata.gpa.glm = glm(admit ~ gpa,family = binomial,data = mydata)
mydata.gpa.glm

mydata.gpa.sum = summary(mydata.gpa.glm)
mydata.gpa.sum

# fitted value
fitted(mydata.gpa.glm)

# pearson residual
# (y - y_hat)/(sqrt(yhat*(1-yhat)))

pearson <- residuals(mydata.gpa.glm, type = "pearson")
plot(mydata$gpa,pearson)

# get the variance covariance matrix
vcov(mydata.gpa.sum)

# beta1 is the differences in log odds with x increase by 1
confint(mydata.gpa.glm)

# exp(beta1) is the odds ratio with x increase by 1 
exp(cbind(mydata.gpa.glm$coefficients,confint(mydata.gpa.glm)))

## multiple logistic regression 

mydata.all.lm = glm(admit~.,family = binomial,data = mydata)
mydata.all.sum = summary(mydata.all.lm)
mydata.all.sum

AIC(mydata.all.lm)
BIC(mydata.all.lm)



# also you can use stepwise regression 
 # 1. backward selection by aic
step(mydata.all.lm,direction = "backward")
 # 2 backward selection by bic
step(mydata.all.lm,direction = "backward",k = log(n))

# use forward selection 

mydata.0.lm = glm(admit~1,family = binomial,data = mydata)
 # 3 forward selection by aic
step(mydata.0.lm,scope = ~gre + gpa + rank,direction = "forward")

# 4. forward selection by bic
step(mydata.0.lm,scope = ~gre + gpa + rank,direction = "forward",k = log(n))



# another way to do model selection 
# analysis of deviance 
mydata.lm.rank.gpa = glm(admit~rank + gpa,data = mydata,family = binomial)

anova(mydata.lm.rank.gpa,mydata.all.lm,test = "Chi")



####################################
# logistic regression with binary count data
library(MASS)
data(menarche)
head(menarche)

plot(Menarche/Total ~ Age, data=menarche)

men.glm = glm(cbind(Menarche,Total-Menarche)~Age,family = binomial,data = menarche)
lines(menarche$Age,fitted(men.glm),col="red")

summary(men.glm)

# CI of difference in log odds
cbind(men.glm$coefficients,confint(men.glm))

# CI of odds ratio
exp(cbind(men.glm$coefficients,confint(men.glm)))

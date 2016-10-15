library(alr4)
n=dim(BGSgirls)[1]
n

# use two predictors WT18 HT18 to predict soma

x1 = BGSgirls$WT18
x2 = BGSgirls$LG18
Y = BGSgirls$Soma

plot(x1,Y,xlab = "WT18", ylab = "Y")
plot(x2,Y,xlab = "LG18", ylab = "Y")

# paired scatter plot
# using pairs functions, input a data frame

pairs(data.frame(Soma=Y,WT18=x1,LG18=x2))

# or your can use scatterplot matrix function in car package
?scatterplotMatrix
scatterplotMatrix(data.frame(Soma=Y,WT18=x1,LG18=x2))
scatterplotMatrix(BGSgirls[,c("Soma","WT18","LG18")])

# add variable plot

# this is the way to do multiple linear regression 
lm.MLR2 = lm(Y ~ x1 + x2)
summary(lm.MLR2)

# now we have SLR model with WT18 as predictor and some as response
par(mfrow=c(1,1))
plot(x1,Y,xlab = "WT18", ylab = "soma")
lm.Y.x1 = lm(Y ~ x1)
abline(lm.Y.x1)
sum.Y.x1 = summary(lm.Y.x1)
sum.Y.x1
res.Y.x1 = resid(lm.Y.x1)

# residual plot 
plot(fitted(lm.Y.x1),res.Y.x1,xlab="fitted",ylab="residual")
qqnorm(res.Y.x1);qqline(res.Y.x1)

# now we want to include another predictor "LG18" into our model 
scatterplotMatrix(BGSgirls[,c("Soma","WT18","LG18")])

# from the scatter plot, we can see LG18 has linear relationship with both
# Some and WT18

# use the residual to fit LG18 ? 


# Some of the varibility of LG18 can be explained by WT18 

# we need to eliminate the effect that WT18 has on LG18
# idea: use the residual 
lm.x2.x1 = lm(x2 ~ x1)
plot(x1,x2,xlab = "WT18", ylab = "LG18")
abline(lm.x2.x1)
res.x2.x1 = resid(lm.x2.x1)

# regression using the residual in the first model and residual in the second model
lm.x2.adjust = lm(res.Y.x1~ res.x2.x1)
# added variable plot 
plot(lm.x2.x1$residuals,lm.Y.x1$residuals,main="added variable plot")
abline(lm.x2.adjust)
lm.x2.adjust$coefficients
# ?? how to intepret the slope

# Among girls with the same WH18 in BGS experiment, 
# there's a change of -9.901263e-05 
# in soma with one unit increase in LG18

# Or

# On average, there's a change of -9.901263e-05 
# in soma with one unit increase in LG18 
# after adjusting for WT18

## Compare with Multiple regression model 
lm.Y.x1.x2 = lm(Y ~ x1 + x2)

sum.Y.x1.x2 = summary(lm.Y.x1.x2)

sum.Y.x1.x2

lm.x2.adjust.summ = summary(lm.x2.adjust)

lm.x2.adjust.summ

# compare with added model
# coefficient
sum.Y.x1.x2$coefficients[3,]
lm.x2.adjust.summ$coefficients[2,]

# estimate of the slope of X2
all.equal(sum.Y.x1.x2$coefficients[3,1],
          lm.x2.adjust.summ$coefficients[2,1])

all.equal(sum.Y.x1.x2$coefficients[3,2],
          lm.x2.adjust.summ$coefficients[2,2])


# compare RSS: the same
sum(lm.x2.adjust$residuals^2)
sum(lm.Y.x1.x2$residuals^2)

# compare R^2: not the same 
sum.Y.x1.x2$r.squared
lm.x2.adjust.summ$r.squared

# compare df of residual: difference in 1
lm.Y.x1.x2$df.residual
lm.x2.adjust$df.residual

# compare sigma: different
sum.Y.x1.x2$sigma
lm.x2.adjust.summ$sigma


# the different is in sigma from the two model
sum.Y.x1.x2$coefficients[3,2] / sum.Y.x1.x2$sigma
lm.x2.adjust.summ$coefficients[2,2] / lm.x2.adjust.summ$sigma

# compare the t value and p-value: 
sum.Y.x1.x2$coefficients
lm.x2.adjust.summ$coefficients

sum.Y.x1.x2$coefficients[3,3]
lm.x2.adjust.summ$coefficients[2,3] # t-value

sum.Y.x1.x2$coefficients[3,4]
lm.x2.adjust.summ$coefficients[2,4]


########################################################

# now add WH18 when LG18 is in the model

# Y~x2
lm.Y.x2 = lm(Y~x2)
plot(x2,Y,xlab = "LG18",ylab = "soma")
abline(lm.Y.x2)

# x1~x2
lm.x1.x2 = lm(x1~x2)
plot(x2,x1,xlab="LG18",ylab="soma")
abline(lm.x1.x2)

# added variable plot for x1
res.x1.x2 = resid(lm.x1.x2)
res.Y.x2 = resid(lm.Y.x2)
plot(res.x1.x2,res.Y.x2)
lm.x1.adjusted = lm(res.Y.x2~res.x1.x2)
abline(lm.x1.adjusted)

lm.x1.adjusted.summ = summary(lm.x1.adjusted)

# compare with multiple linear regression 

sum.Y.x1.x2
lm.x1.adjusted.summ

# compare coefficient for x1
sum.Y.x1.x2$coefficients[2,1]
lm.x1.adjusted.summ$coefficients[2,1]

# t-value and p-value: 
sum.Y.x1.x2$coefficients[2,3]
lm.x1.adjusted.summ$coefficients[2,3] # t-value

sum.Y.x1.x2$coefficients[2,4]
lm.x1.adjusted.summ$coefficients[2,4] # p-value


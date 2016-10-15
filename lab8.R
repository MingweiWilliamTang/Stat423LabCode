library(alr4)
head(salary)

###############
# look at the case when we with sex and year as predictor
# sex: catagorical: two levels
# year: continuous: 

lm.year.sex = with(salary,lm(salary~sex+year))
sum.year.sex = summary(lm.year.sex)
sum.year.sex


lm.year.sex.inter = with(salary,lm(salary~sex*year))
sum.year.sex.inter = summary(lm.year.sex.inter)
sum.year.sex.inter

# wrong model
lm.wrong = with(salary,lm(salary~sex : year))
lm.wrong$df.residual
lm.year.sex.inter$df.residual

# model comparison 
# in this example, the difference between two models is a variable 
# with 1 df

# t-test
sum.year.sex.inter$coefficients



# anova F-test

anova(lm.year.sex,lm.year.sex.inter)

# the two test has the same result 

# for more than 1 df difference, only use anova F-test
lm.year.rank = with(salary,lm(salary~year+rank))
sum.year.rank = summary(lm.year.rank)
sum.year.rank


lm.year.rank.iter = with(salary,lm(salary~year*rank))
sum.year.rank.iter= summary(lm.year.rank.iter)
sum.year.rank.iter

# comparison: avova F-test
anova(lm.year.rank,lm.year.rank.iter)

# two catagorical predictors 
lm.sex.rank = with(salary,lm(salary~sex+rank))
summary(lm.sex.rank)$coef

lm.sex.rank.iter = with(salary,lm(salary~sex*rank))
summary(lm.sex.rank.iter)$coef



#################################


set.seed(42)
xs.55 <- c(rep(1,1), rep(2,2), rep(3,3), rep(4,4), rep(5,5),
           rep(6,6), rep(7,7), rep(8,8), rep(9,9), rep(10,10))
ys.55 <- 10 - xs.55 + rnorm(length(xs.55), sd=0.5)
length(ys.55)  # 55
lm.ols.55 <- lm(ys.55 ~ xs.55)

plot(xs.55, ys.55, xlab=expression(x[i]), ylab=expression(y[i]), col="blue", cex=0.75)
abline(reg=lm.ols.55)
abline(coef=c(10,-1), col="red")

### overhead VII-5 ... scatterplot with jittering

set.seed(42)
plot(jitter(xs.55), ys.55, xlab=expression(paste("jittered ", x[i])), ylab=expression(y[i]), col="blue", cex=0.75)
abline(reg=lm.ols.55)
abline(coef=c(10,-1), col="red")

### overhead VII-7 ... scatterplot with daily averages

xs.10 <- 1:10
ys.10 <- c(ys.55[1], mean(ys.55[2:3]), mean(ys.55[4:6]), mean(ys.55[7:10]),
           mean(ys.55[11:15]), mean(ys.55[16:21]), mean(ys.55[22:28]),
           mean(ys.55[29:36]), mean(ys.55[37:45]), mean(ys.55[46:55]))

plot(xs.55, ys.55, xlab=expression(paste(x[i], " and x")), ylab=expression(paste(y[i]," and ", bar (y)[x])), col="blue", cex=0.75)
points(xs.10, ys.10, pch="x", col="red", cex=0.75)

### overhead VII-8 ... scatterplot of daily averages and OLS regression

lm.ols.10 <- lm(ys.10 ~ xs.10)

plot(xs.10, ys.10, xlab="x", ylab=expression(bar (y)[x]), pch="x", col="red", cex=0.75)
abline(reg=lm.ols.10)
abline(coef=c(10,-1), col="red")


### overhead VII-10 ... summary of OLS fit to daily averages

summary(lm.ols.10)

lm.ols.10$coefficients
lm.ols.55$coefficients
### overhead VII-24 (same as overhead VII-8)

### overhead VII-25 ... scatterplot of daily averages and WLS regression

lm.wls.10 <- lm(ys.10 ~ xs.10, weights=1:10)

plot(xs.10, ys.10, xlab="x", ylab=expression(bar (y)[x]), pch="x", col="red", cex=0.75)
abline(reg=lm.wls.10)
abline(coef=c(10,-1), col="red")

### are same for OLS with 55 data points and for WLS with 10 data points

all.equal(as.vector(coef(lm.ols.55)),as.vector(coef(lm.wls.10)))

### overheads VII-28 ... summary for OLS with 55 data points

summary(lm.ols.55)

### overheads VII-29 ... summary for WLS with 10 data points

summary(lm.wls.10)

### another example

as.data.frame(lapply(galtonpeas,rev))

### overhead VII-34 ... scatterplot with OLS and WLS regression lines 

lm.ols <- lm(Progeny ~ Parent, data=galtonpeas)
the.weights <- 1/galtonpeas$SD^2
lm.wls <- lm(Progeny ~ Parent, weights=the.weights, data=galtonpeas)

with(galtonpeas, plot(Parent, Progeny, col="blue", xlab="Parent diameter", ylab="Progeny diameter", xlim=c(15,21), ylim=c(15,21), cex=0.75))
abline(reg=lm.ols)
abline(reg=lm.wls, col="red")
abline(coef=c(0,1), lty="dashed")


summary(lm.ols)

summary(lm.wls)

lm.wls2 = lm(Progeny ~ Parent, weights=2*the.weights, data=galtonpeas)


summary(lm.wls)$coef
summary(lm.wls)$coef
### overhead VII-39 ... t-test
round((t.statistic <- (coef(lm.wls)[2])/sqrt(vcov(lm.wls)[2,2])),5) 


###################
# model selection 
head(BGSgirls)
dim(BGSgirls)
n = 70
predictornames=colnames(BGSgirls)[-c(11,12)]
predictornames

# run a full model
lm.full = lm(Soma~.,data = BGSgirls)

# with addition quadratic predictor WT18^2 and HT18^2
lm.13 = lm(Soma~ . + I(WT18^2) + I(HT18^2),data = BGSgirls)
anova(lm.full,lm.13)

# without any predictors
lm.0 = lm(Soma ~1, data =BGSgirls)


# forward selection
extractAIC(lm.0)[2]
forward.Select = step(lm.0,scope=~WT2 + HT2 + WT9 + HT9 + LG9 + ST9 + 
                        WT18 + HT18 + LG18 + ST18 + I(WT18^2) + I(HT18^2),
                      direction = "forward")
forward.Select$call
forward.Select$coefficients
forward.Select.sum = summary(forward.Select)
forward.Select.sum

# forward selection using BIC
extractAIC(lm.0,k=log(70))[2]
forward.Select2 = step(lm.0,scope=~WT2 + HT2 + WT9 + HT9 + LG9 + ST9 + 
                        WT18 + HT18 + LG18 + ST18 + I(WT18^2) + I(HT18^2),
                      direction = "forward",k=log(length(resid(lm.0))))
extractAIC(forward.Select2,k=log(70))[2]
summary(forward.Select2)$call

forward.Select$call


extractAIC(lm.13,k=2)[2]

# backword selection
step(lm.13,direction = "backward")
step(lm.13,direction = "backward",k=sigmaHat(lm.13)^2)
extractAIC(lm.13,k=sigmaHat(lm.13)^2)[2]

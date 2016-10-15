library(alr4)
# after log transform
with(UN11,pairs(data.frame(fertility,logppgdp=log(ppgdp),pctUrban)))
# log(gdp) and pctUrban has linear relationship
# ferfitily and log(g) has lienar relationship
# fertiliy and pctUrban has linaer relationship, however, we can observe 
# heteroscdacity in the plot(non-constant variance )

# original data 
with(UN11,pairs(data.frame(fertility,ppgdp=ppgdp,pctUrban)))


lm.loggdp.pctUrban = with(UN11,lm(fertility~log(ppgdp)+pctUrban))



# fertility as response, log(gdp) as predictor
lm.fer.loggdp = with(UN11,lm(fertility~log(ppgdp)))
with(UN11,plot(log(ppgdp),fertility))
abline(lm.fer.loggdp)
summary(lm.fer.log)

# pctUrban as response, log(gdp) as prediector
lm.pct.loggdp = with(UN11,lm(pctUrban~log(ppgdp)))
with(UN11,plot(log(ppgdp),pctUrban))
abline(lm.pct.loggdp)

# residual from first linear model as response, residual from second linear model as 
# predictor
lm.added.pcturban = lm(resid(lm.fer.loggdp)~resid(lm.pct.loggdp))
plot(resid(lm.pct.loggdp),resid(lm.fer.loggdp))
abline(lm.added.pcturban)

summary(lm.added.pcturban)
# 3.2.5
# compare residual 
all.equal(resid(lm.added.pcturban),resid(lm.loggdp.pctUrban))

# 3.2.6
summary(lm.added.pcturban)$coef[2,3]
summary(lm.loggdp.pctUrban)$coef[3,3]


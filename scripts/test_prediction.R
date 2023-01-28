########################################################################################
# script to demonstrate why FAA predictions should be done on pre-aggregated data ######
########################################################################################

# simulated relationship
inter <- 10
a <- 1
b <- 0.5
sd <- 0.2
n <- 10000

# training set (pelletier)
set.seed(123)
x1 <- round(runif(n,1,10),0)
x2 <- round(rnorm(n,15,3),2)
y <- exp(inter+a*log(x1)+b*log(x2)+rnorm(n,0,sd))

df <- data.frame(y=y,x1=x1,x2=x2)

par(mfrow=c(1,2))
plot(df$y~df$x2)
plot(df$y~df$x1)

# fit model
m <- lm(log(y)~log(x1)+log(x2),data=df)
coef(m)

# predictions on (bio) data
df2 <- df
df2$y <-NULL

# the following things are NOT the same: 
# 0) true values
t0 <- tapply(y,x1,mean) 
points(t0,pch=16,col='red')

t0l <- t0 -2*tapply(y,x1,sd) 
t0u <- t0 +2*tapply(y,x1,sd) 
lines(t0l,lty='dashed',col='red')
lines(t0u,lty='dashed',col='red')

# 1) option 1 (predict on pre-aggregated data)
t1 <- exp(predict(m,data.frame(x1=sort(unique(df2$x1)),x2=tapply(df2$x2,df2$x1,mean)))) 
lines(t1,col='green')

t1l <- t1 - exp(predict(m,data.frame(x1=sort(unique(df2$x1)),x2=tapply(df2$x2,df2$x1,mean)-2*tapply(df2$x2,df2$x1,sd)))) # can't really do that...
t1u <- t1 + exp(predict(m,data.frame(x1=sort(unique(df2$x1)),x2=tapply(df2$x2,df2$x1,mean)-2*tapply(df2$x2,df2$x1,sd)))) 
lines(t1l,col='green',lty='dashed') # will overestimate the lower bound. see shrinkage!
lines(t1u,col='green',lty='dashed')

# 2) option 2 (predicted and then aggregate)
# model predicts the average on a lognormal distribution or hat(log(y)). Here you subsequently assume a normal one...
df2$logpred <- predict(m,df2)
df2$logpred.sd <- predict(m,df2,se.fit = TRUE)$se.fit
t2 <- exp(tapply(df2$logpred,x1,mean))
lines(t2,col='blue')

t2l <- t2 - 2*tapply(exp(df2$logpred),x1,sd) # INCORRECT. Demo.
t2u <- t2 + 2*tapply(exp(df2$logpred),x1,sd)
lines(t2l,col='blue',lty='dashed')  
lines(t2u,col='blue',lty='dashed')

# 3) option 3: option 2 with correction for tansformation to normal (better).
sd <- tapply(df2$logpred,x1,sd)
t3 <- exp(tapply(df2$logpred,x1,mean)+sd^2/2)
lines(t3,col='darkblue')

sd <- sqrt(t3^2*(exp(tapply(df2$logpred,x1,sd)^2)-1)) # https://stats.stackexchange.com/questions/241187/calculating-standard-deviation-after-log-transformation
t3l <- t3 - 2*sd
t3u <- t3 + 2*sd
lines(t3l,col='darkblue',lty='dashed')  
lines(t3u,col='darkblue',lty='dashed')


# average error across all age classes
round(mean(abs(t0-t1)*100/t0),2)
round(mean(abs(t0-t2)*100/t0),2)
round(mean(abs(t0-t3)*100/t0),2)


#6.5
# part a 
data<-read.table(file="http://www.stat.lsu.edu/exstweb/statlab/datasets/KNNLData/CH06PR05.txt")
names(data)<-c("Y","X1","X2")
attach(data)
cor(data)
plot(data)

# part b 
fit = lm(Y~X1+X2, data=data)
summary(fit)

# part c 
resi = fit$resi
resi
boxplot(resi)
title("box plot for residuals")

# part d 
fit_y<-fit$fitted.values 
plot(fit_y,resi)
abline(h=0)
title("plot of residuals against fitted values")

plot(X1,resi)
abline(h=0)
title("plot of residuals against X1")

plot(X2,resi)
abline(h=0)
title("plot of residuals against X2")


plot(X1*X2,resi)
abline(h=0)
title("plot of residuals against X2*X1")

qqnorm(resi)
qqline(resi)

#6.6 
# part a 
qf(0.99,df1=2,df2=13)

# part c 

qt(1-0.01/4,13)


# 6.8 
newx = data.frame(X1 = 5, X2 = 4)
predict.lm(fit, newx, interval="confidence",level=.99)
predict.lm(fit, newx, interval="prediction",level=.99)



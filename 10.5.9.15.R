# 10.5 Brand Preference 

# Part a 
data<-read.table("http://www.stat.sc.edu/~hitchcock/brandpreference.txt")
names(data)<-c("Y","X1","X2")
attach(data)
fit = lm(Y~X1+X2)   
n = nrow(data)

###Added Variable Plot 
resi = fit$resi
par(mfrow=c(2,2))

fit2 = lm(Y~X2)
fit12 = lm(X1~X2)
fit1 = lm(Y~X1)
fit21 = lm(X2~X1)
plot(X1,resi,main='Residual Plot Against X1')    
plot(fit12$resi,fit2$resi, main='Added Variable Plot for X1',xlab="e(x1|x2)",ylab="e(Y|x2)")
abline(lm(fit2$resi~fit12$resi)) 
plot(X2,resi,main='Residual Plot Against X2')
plot(fit21$resi,fit1$resi, main='Added Variable Plot for X2',xlab="e(x2|x1)",ylab="e(Y|x1)")     
abline(lm(fit1$resi~fit21$resi))

# part c 
summary(fit2)
summary(fit21 )
fit_e = lm(fit1$resi~fit21$resi)
summary(fit_e)
           
#4.14
data<-read.table("http://www.stat.lsu.edu/exstweb/statlab/datasets/KNNLData/CH01PR19.txt")
names(data)<-c("GPA","ACT")
attach(data)
fit<-lm(GPA~ACT-1)
Cconfint(fit,level1=0.95)

x<-ACT
y<-GPA
n=120
xmean=mean(x)
ymean=mean(y)
b1=sum(x*y)/sum(x^2)
Xh=30
Yh=b1*Xh
resi = fit$residuals
SSE = sum(resi^2)
MSE = SSE/(120-2)
sYhsq = MSE*Xh^2/sum(x^2)
qt975 =qt(0.975,df=120-2)
sYh=sqrt(sYhsq)
Yh-sYh*qt975
Yh+sYh*qt975

# 4.15

# part a 
plot(x,y,main = "regression line",xlim=c(0,max(x)+1),ylim=c(0,max(y)+1))
abline(fit)

resi=fit$residuals 
sum(resi)
plot(fit$fitted.values,resi)
abline(0,0)

fit_2<-lm(GPA~ACT)
anova(fit,fit_2)





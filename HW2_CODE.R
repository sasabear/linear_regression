# 2.4 
# part a 
setwd=("/Users/jiahongHu/Desktop/Spring 2015/STAT 4315 Linear Regression/hw/hw2")

data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt")
names(data)<-c("GPA","ACT")
attach(data)
fit<-lm(GPA~ACT)
summary(fit)

confint(fit,level=0.99) # direct computation of 99% CI 

# Indirect method by hand 
x = data[,2]
y = data[,1]
xmean = mean(x)
ymean = mean(y)
b1 = sum((x-xmean)*(y-ymean))/sum((x-xmean)^2)
b0 = ymean - b1 * xmean
resi = fit$residuals
SSE = sum(resi^2)
MSE = SSE/(length(GPA)-2)
sb1sq = MSE/sum((x-xmean)^2)
sb1 = sqrt(sb1sq)
qt995 = qt(0.995, df=length(GPA)-2)
b1 - qt995 * sb1
b1 + qt995 * sb1

# part b 
tstar = b1/sb1
qt995 = qt(0.995, df=length(GPA)-2)
tstar
qt995
2*(1-pt(tstar, df=length(GPA)-2))

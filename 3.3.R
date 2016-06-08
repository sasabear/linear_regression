
# 3.3 
# part a 
setwd("/Users/jiahongHu/Desktop/Spring 2015/Linear Regression 4315/hw/hw4")
data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt")
names(data)<-c("GPA","ACT")
attach(data)
fit<-lm(GPA~ACT)
boxplot(ACT,main="the boxplot of ACT scores")

# part c
resi<-fit$residuals
fit_value<-fit$fitted.values
plot(fit_value,resi,main="residule against fitted value")

# part d 
qqplot<-qqnorm(resi)
qqline(resi)
SSE = sum(resi^2)
MSE = SSE/(length(GPA)-2)
n=120
ExpVals = sapply(1:n, function(k) sqrt(MSE) * qnorm((k-.375)/(n+.25)))
cor(ExpVals,sort(fit$residuals))
#exp<-qqplot$x
#corr_coef<-sum((resi-mean(resi))*(exp-mean(exp)))/sqrt(sum((resi-mean(resi))^2)*sum((exp-mean(exp))^2))

# part e 
ACT_1<-ACT[ACT<26]
ACT_2<-ACT[ACT>=26]
n1<-length(ACT_1)
n2<-length(ACT_2)
data_2<-data.frame(data, fit$residuals)
resi_1<-data_2[ACT<26,]$fit.residuals
resi_2<-data_2[ACT>=26,]$fit.residuals
m_1<-median(resi_1)
m_2<-median(resi_2)
d1<-abs(resi_1-m_1)
d2<-abs(resi_2-m_2)
d1_m<-mean(d1)
d2_m<-mean(d2)
s_power<-(sum((d1-d1_m)^2)+sum((d2-d2_m)^2))/(n1+n2-2)
t_bf<-(d1_m-d2_m)/(sqrt(s_power)*sqrt(1/n1+1/n2))

# part f 
data_3<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%203%20Data%20Sets/CH03PR03.txt")
names(data_3)<-c("GPA","ACT","Intelligence","Rank")
attach(data_3)
plot(Intelligence,resi)
plot(Rank,resi)

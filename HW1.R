# HW1
# 1.19

# part a 
setwd("/Users/jiahongHu/Desktop/Spring 2015/Linear Regression 4315/hw")
data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt")
names(data)<-c("GPA","ACT")
attach(data)
fit<-lm(GPA~ACT)
summary(fit)
attributes(fit)
fit$coefficients[1]
fit$coefficients[[1]]
fit$coefficients[2]
fit$coefficients[[2]]


# part b 

plot(ACT,GPA)
cor(ACT,GPA)
abline(fit,lwd=2) # draw the regression line 
res <- GPA - (fit$coefficients[[2]]*ACT+fit$coefficients[[1]])
pre<-predict(fit)
segments(ACT,GPA,ACT,pre,col ="red")
title("regression line and redline residule")
filename1 ="1.b.1.png"
dev.copy(device=png, file=filename1, height=600, width=800)
dev.off()

summary(res)
plot(ACT,res)
title("ACT and residule")
filename2 ="1.b.2.png"
dev.copy(device=png, file=filename2, height=600, width=800)
dev.off()

# part c 
fit$coefficients[[2]]*30+fit$coefficients[[1]]

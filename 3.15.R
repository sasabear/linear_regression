# 3.15
# PART A

data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%203%20Data%20Sets/CH03PR15.txt")
names(data)<-c("con","time")
attach(data)
fit<-lm(con~time)
summary(fit)

# part b 
reduced<-lm(con~time)
full<-lm(con~0+as.factor(time))
anova(reduced, full) 

#3.16
# part a 
plot(time,con,main="scatter plot of time and concentration")
plot(time,log10(con),main = "scatter plot of time and concentration")

# part b  # box-cox 
n<-length(con)
lamda<-c(-0.2,-0.1,0,0.1,0.2)
W<-vector("list",length(lamda))
K2=(prod(con))^(1/n)
K1=1/(lamda*K2^(lamda-1))

for (i in c(1,2,4,5)){
        W[[i]]=K1[i]*(con^lamda[i]-1)
}

W[[3]]=K2*log(con)


SSE<-vector()
for(i in 1:length(lamda)){
        fit_lamda=lm(W[[i]]~time)
        resi=fit_lamda$residuals
        SSE[i]=sum(resi^2)
}

plot(lamda,SSE,type="b")



# part c 
con_new<-log10(con)
fit_new<-lm(con_new~time)
summary(fit_new)

# part d 
plot(time,con_new)
abline(lm(con_new~time))

# part e 
resi_new<-fit_new$residuals
fitted_new<-fit_new$fitted.values
plot(fitted_new,resi_new)
qqplot<-qqnorm(resi_new)
qqline(resi_new)




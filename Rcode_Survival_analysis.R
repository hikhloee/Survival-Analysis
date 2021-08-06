#R CODE

library(survival)
data(colon)
#help(colon)

dataset<-colon[1:929,]
for (i in 1:929){
  dataset[i,]<-colon[2*i,]
}
head(dataset) #dataset

trt1<-dataset[dataset$rx=="Obs",]
trt2<-dataset[dataset$rx=="Lev",]
trt3<-dataset[dataset$rx=="Lev+5FU",]

########trt1

trt1.sex<-survfit(Surv(trt1$time,trt1$status)~trt1$sex)
plot(trt1.sex,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("Female","Male"))
title("Survival functions of trt1: Sex") #Survival Curve

trt1.obstruct<-survfit(Surv(trt1$time,trt1$status)~trt1$obstruct)
plot(trt1.obstruct,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("No","Yes"))
title("Survival functions of trt1: Obstruct") #Survival Curve

trt1.perfor<-survfit(Surv(trt1$time,trt1$status)~trt1$perfor)
plot(trt1.perfor,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("No","Yes"))
title("Survival functions of trt1: perfor") #Survival Curve

trt1.extent<-survfit(Surv(trt1$time,trt1$status)~trt1$extent)
plot(trt1.extent,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:4)
legend(2000,1,lty=c(1:4),legend=c("submucosa","muscle","serosa","contiguous structures"))
title("Survival functions of trt1: extent") #Survival Curve

########trt2
trt2.sex<-survfit(Surv(trt2$time,trt2$status)~trt2$sex)
plot(trt2.sex,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("Female","Male"))
title("Survival functions of trt2: Sex") #Survival Curve

trt2.obstruct<-survfit(Surv(trt2$time,trt2$status)~trt2$obstruct)
plot(trt2.obstruct,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("No","Yes"))
title("Survival functions of trt2: Obstruct") #Survival Curve

trt2.perfor<-survfit(Surv(trt2$time,trt2$status)~trt2$perfor)
plot(trt2.perfor,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("No","Yes"))
title("Survival functions of trt2: perfor") #Survival Curve

trt2.extent<-survfit(Surv(trt2$time,trt2$status)~trt2$extent)
plot(trt2.extent,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:4)
legend(2000,1,lty=c(1:4),legend=c("submucosa","muscle","serosa","contiguous structures"))
title("Survival functions of trt2: extent") #Survival Curve

########trt3
trt3.sex<-survfit(Surv(trt3$time,trt3$status)~trt3$sex)
plot(trt3.sex,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("Female","Male"))
title("Survival functions of trt3: Sex") #Survival Curve

trt3.obstruct<-survfit(Surv(trt3$time,trt3$status)~trt3$obstruct)
plot(trt3.obstruct,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("No","Yes"))
title("Survival functions of trt3: Obstruct") #Survival Curve

trt3.perfor<-survfit(Surv(trt3$time,trt3$status)~trt3$perfor)
plot(trt3.perfor,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2500,1,lty=c(1:2),legend=c("No","Yes"))
title("Survival functions of trt3: perfor") #Survival Curve

trt3.extent<-survfit(Surv(trt3$time,trt3$status)~trt3$extent)
plot(trt3.extent,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:4)
legend(2000,1,lty=c(1:4),legend=c("submucosa","muscle","serosa","contiguous structures"))
title("Survival functions of trt3: extent") #Survival Curve #그룹별 생존함수 그래프 비교

###### surg survival plot
trt1.surg<-survfit(Surv(trt1$time,trt1$status)~trt1$surg)
plot(trt1.surg,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2000,1,lty=c(1:4),legend=c("0:short","1:long"))
title("Survival functions of trt1: surg") #Survival Curve

trt2.surg<-survfit(Surv(trt2$time,trt2$status)~trt2$surg)
plot(trt2.surg,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2000,1,lty=c(1:4),legend=c("0:short","1:long"))
title("Survival functions of trt2: surg") #Survival Curve

trt3.surg<-survfit(Surv(trt3$time,trt3$status)~trt3$surg)
plot(trt3.surg,conf.int=F,xlab="Time until remisson(day)", ylab="Prob without remission",cex=2,lty=1:2)
legend(2000,1,lty=c(1:4),legend=c("0:short","1:long"))
title("Survival functions of trt3: surg") #Survival Curve

##########
colon.diff<-survdiff(Surv(time,status)~factor(rx),data=dataset)
colon.diff #Log-rank test

colon.fit<-survfit(Surv(dataset$time,dataset$status)~dataset$rx)
summary(colon.fit) #survival function

plot(colon.fit,conf.int=F,xlab="Time until remission (day) ", ylab="Prob without remission",cex=2,lty=1:3)
legend(2500,1,lty=c(1:3),legend=c("Obs","Lev","Lev+5FU"))
title("Survival functions of Treatment groups") #Survival Curve

cox1<-coxph(Surv(time,status==1)~factor(rx)+sex+age+perfor+extent+surg,data=dataset); cox1 #status==1: event, status==0; censored #Cox's proportional hazards model

cox.zph(cox1) #Proportional hazards assumption test
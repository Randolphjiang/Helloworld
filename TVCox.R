library(survival)
###data输出的数据，第一列加列名date
data = read.csv("data.csv")
index = which( data$subject == 19)
datanew = data[c("date","Liquidday")][1:index[length(index)],]
datanew$surv = NA
datanew$Liquidday = datanew$Liquidday/50
date = data[,1]
data = data[,-1]
model = coxph(Surv(TStart, TEnd, Status) ~ Operation1+Operation2+Operation3+Operation6 +WTime + LiquidTem + Oilday +RatioWater+ WaterDay + OilMouth+ OilPre+ TaoPre+ GOratio + WellTem + LiquidTem +SpeendOil +cluster(subject),data=data)
smodel <- step(model)
model2 = survfit(smodel)
plot(model2, mark.time= F, lty=1:2,xlab="Time(day)", ylab="Survival",conf.int = F)
for(i in 1:19){
	index = which( data$subject == i)
	predict = survfit(smodel, newdata=data[index,], id=subject)
      indexnew = index[1]-1 + summary(predict)$time
      datanew$surv[index[1]:(indexnew[1]-1)] = 1
      datanew$surv[indexnew] = summary(predict)$surv
	for(i in (indexnew[1]+1):index[length(index)]){
      	if(is.na(datanew$surv[i])){
      		datanew$surv[i] = datanew$surv[i-1]
      	}
	}

}
plot(predict, ylim=c(0,1),xlab= "Time(day)",conf.int = F,col="red",lwd=2)
write.csv(datanew,"datanew.csv")






index = which(datenew$surv > 0)
datenew$surv[1:index[1]] = 1
for(i in (index[1]+1):length(datenew$surv)){
      if(is.na(datenew$surv[i])){
      	datenew$surv[i] = datenew$surv[i-1]
      }
}


plot(predict, ylim=c(0,3),xlab= "Time(day)",conf.int = F,col="red",lwd=2)
y = (data$Liquidday[index])/50#(max(data$Liquidday[index]))
lines(1:(length(index)),y[1:(length(y))], lty = 2,col="blue",lwd=2)
lines(1:(length(index)),rep(0.5,length(y)), lty = 2,lwd=1)





lines(x=c(0,1:22),y=rep(0.5,23),col="red",lty=2)
lines(x=c(0,1:22),c(1,0.97,0.95,0.93,0.92,0.9,0.9,0.88,0.88,0.85,0.83,0.75,0.70,0.6,0.5,0.4,0.3,0.25,0.2,0.1,0.05,0.02,0),col="blue")
lines(x=c(0,1:22),sort(c(1,0.95,0.9,0.8,0.7,0.6,0.58,0.58,0.56,0.55,0.53,0.52,0.52,0.46,0.45,0.4,0.3,0.25,0.2,0.1,0.05,0.02,0),decreasing = F),col="brown")
legend(x=0,y=0.83,legend=c("风险","阈值","产量","结蜡厚度"),lty=c(1,2,1,1),lwd=c(1,1,1,1),col=c("black","red","blue","brown"))

speed = (summary(predict)$`surv`[length(summary(predict)$`surv`)-1]-summary(predict)$`surv`[length(summary(predict)$`surv`)])/(summary(predict)$`time`[length(summary(predict)$`time`)]-summary(predict)$`time`[length(summary(predict)$`time`)-1])
lefttime = (summary(predict)$`surv`[length(summary(predict)$`surv`)]-0.5)/speed



install.packages("survminer")
library(survminer)
test.ph <- cox.zph(model)
test.ph

ggsurvplot(model2, data = data,conf.int = F)

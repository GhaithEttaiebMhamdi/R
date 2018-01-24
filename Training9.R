global<-read.table(file=file.choose(),header=T,sep=";",dec=",",na.string="")
global

gb=scan(file=file.choose(),skip=0)
gb
class(gb)
global=as.ts(gb)
start(global)
end(global)
ts.global=ts(global, start=c(1856,1),end=c(2005,12),freq=12)
start(ts.global)
end(ts.global)
ts.global=window(ts.global,start=c(1970,1))
start(temp)
temps=time(ts.global)
summary(vt)
end(temp)
names(vt)
vt
2/
mod1=lm(ts.global~temps)
summary(mod1)
AIC(mod1)
temps2=temps^2
mod2=lm(ts.global~temps+temps2)
summary(mod2)
temps_stand=((temps-mean(temps))/(sd(temps)))
3/ mod3=lm(ts.global~temps_stand)
summary(mod3)
temps_stand2=temps_stand^2
mod4=lm(ts.global~temps_stand+temps_stand2)
summary(mod4)

4/

SIN <- COS <- matrix(nr = length(temps), nc = 6)
t<-c(0:(length(temps)-1))

for (i in 1:6)
{COS[, i] <- cos(2 * pi * i *t/12)
	SIN[, i] <- sin(2 * pi * i * t/12)}


mod5=lm(ts.global~temps_stand+SIN+COS)
summary(mod5)


mod6=lm(ts.global~temps_stand+SIN[,1])
summary(mod6)
summary(mod1)

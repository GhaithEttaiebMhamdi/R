apprentissage <-read.table(file=file.choose(),sep=",",header=T,dec=".")
head(apprentissage)
summary(apprentissage)


mod1 <- rpart(objective~. ,data=apprentissage) 
mod1
rpart.plot(mod1, uniform=TRUE, main="Decision Tree - Play?")
asRules(mod1)

entest<-read.table(file=file.choose(),sep=",",header=T,dec=".")

?predict
ypredite1<-predict(mod1,entest,type="class")
table(entest$objective,ypredite1)

mod2 <- rpart(objective~. ,data=apprentissage,control=rpart.control(minsplit=50,cp=0.01)) 
asRules(mod2)
ypredite2<-predict(mod2,entest,type="class")
table(entest$objective,ypredite2)

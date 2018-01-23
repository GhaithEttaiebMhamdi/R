apprentissageCredit <-read.xlsx(file=file.choose(),sheetIndex=1,header=T)
summary(apprentissageCredit)

mod1 <- rpart(Decision~. ,data=apprentissageCredit) 
mod1
rpart.plot(mod1, uniform=TRUE, main="Decision Tree")
asRules(mod1)



testCredit<-read.xlsx(file=file.choose(),sheetIndex=2,header=T)
ypredite1<-predict(mod1,testCredit,type="class")
table(testCredit$Decision,ypredite1)


mod2 <- rpart(Decision~. ,data=apprentissageCredit,control=rpart.control(minsplit=50,cp=0.01))
rpart.plot(mod2, uniform=TRUE, main="Decision Tree")
asRules(mod2)
ypredite2<-predict(mod2,testCredit,type="class")
table(testCredit$Decision,ypredite2)
?tree
?party

mod3<-tree(Decision~. ,data=apprentissageCredit)
plot(mod3)
summary(mod3)
ypredite3<-predict(mod3,testCredit,type="class")
table(testCredit$Decision,ypredite3)

mod4<-ctree(Decision~. ,data=apprentissageCredit)
plot(mod4)
summary(mod4)
ypredite4<-predict(mod4,testCredit,type="node")
table(testCredit$Decision,ypredite4)





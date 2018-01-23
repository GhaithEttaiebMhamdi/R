neuro<-read.table(file=file.choose(), header= T, dec="." )
summary(neuro)
neuro
?nnet
neuro$PERF=as.factor(neuro$PERF)

net<-nnet(neuro$PERF~.,neuro,size=2)
library(nnet)
pred = predict(net,neuro,type="class")
pred2
net2<-nnet(neuro$PERF~.,neuro,size=5)

pred2 = predict(net2,neuro,type="class")

table(neuro$PERF,pred2)
table(neuro$PERF,pred)


net3<-nnet(Species~.,iris,size=3)
K<-50
res<-numeric(K)
for ( k in 2:50) { 
rn<-nnet(iris$Species~.,iris,size=k)
pred<-predict(rn,iris,type="class")
tableau<-table(iris$Species,pred)
error<-1-(sum(diag(tableau))/sum(tableau))
res[k] = error


}
res

plot(res)

##########################################################################
svmiris<-svm(Species~.,iris,kernel="linear")

rpartiris<-rpart(Species~.,iris)

Fonction2=function(model,test,target){
pred<-predict(model,test,type="class")
tableau<-table(target,pred)
error<-1-(sum(diag(tableau))/sum(tableau))
return(error)
}
Fonction2(svmiris,iris,iris$Species)
Fonction2(rn,iris,iris$Species)
Fonction2(rpartiris,iris,iris$Species)

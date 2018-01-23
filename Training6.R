prema <-read.xlsx(file=file.choose(),sheetIndex=1,header=T) 
summary(prema)
nrow(prema)
prema.train=prema[1:250,]
prema.test=prema[251:390,]
?svm

modelbasic<-svm(prema.train$PREMATURE~.,data=prema.train)
summary(modelbasic)
summary(modelbasic)
predictbasic <- predict(modelbasic, newdata=prema.test)
t1<-table(predictbasic,prema.test$PREMATURE)
prop.table(t1)
summary(predictbasic)
head(predictbasic)

modellinear<-svm(PREMATURE~.,data=prema.train,kernel="linear")
modellinear
predictlinear <- predict(modellinear, newdata=prema.test)
t2<-table(predictlinear,prema.test$PREMATURE)
prop.table(t2)

modelpoly<-svm(PREMATURE~.,data=prema.train,kernel="polynom")
modelpoly
predictpoly <- predict(modelpoly, newdata=prema.test)
t3<-table(predictpoly,prema.test$PREMATURE)
prop.table(t3)

modelsig<-svm(PREMATURE~.,data=prema.train,kernel="sigmoid")
modelsig
predictsig <- predict(modelsig, newdata=prema.test)
t4<-table(predictsig,prema.test$PREMATURE)
prop.table(t4)
##############################################################################

?kknn
knn1<-kknn(PREMATURE~.,prema.train ,prema.test, distance =1, k=3)
attributes(knn1)
summary(knn1)
fit <- fitted(knn1)
prop.table(table(fit,prema.test$PREMATURE))

knn5<-kknn(PREMATURE~.,prema.train ,prema.test, distance =1, k=5)
attributes(knn5)
summary(knn5)
fit5 <- fitted(knn5)
prop.table(table(fit5,prema.test$PREMATURE))

knn7<-kknn(PREMATURE~.,prema.train ,prema.test, distance =1, k=7)
attributes(knn7)
summary(knn7)
fit7 <- fitted(knn7)
prop.table(table(fit7,prema.test$PREMATURE))


arbre=rpart(PREMATURE~.,data=prema.train)
rpart.plot(arbre, uniform=TRUE, main="Decision Tree")
predictarbre <- predict(arbre, newdata=prema.test,type= "class")
predictarbre 
prop.table(table(predictarbre ,prema.test$PREMATURE))

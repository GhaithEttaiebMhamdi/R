cancerapp <-read.xlsx(file=file.choose(),sheetIndex=1,header=T)
ncol(cancerapp)
nrow(cancerapp)
summary(cancerapp)
cancertest<-read.xlsx(file=file.choose(),sheetIndex=2,header=T)
ncol(cancertest)
nrow(cancertest)

##################SVM##################

modelbasic<-svm(classe~.,data=cancerapp )
modelbasic
summary(modelbasic)
predictbasic <- predict(modelbasic, newdata=cancertest)
t1<-table(predictbasic,cancertest$classe)
prop.table(t1)
 0.630000000 +0.333333333

modellinear<-svm(classe~.,data=cancerapp,kernel="linear")
modellinear
predictlinear <- predict(modellinear, newdata=cancertest)
t2<-table(predictlinear,cancertest$classe)
prop.table(t2)
0.64000000 + 0.32000000

modelpoly<-svm(classe~.,data=cancerapp ,kernel="polynom")
modelpoly
predictpoly <- predict(modelpoly, newdata=cancertest)
t3<-table(predictpolycancertest$classe)
prop.table(t3)
0.05714286 +0.55000000

modelsig<-svm(classe~.,data=cancerapp ,kernel="sigmoid")
modelsig
predictsig <- predict(modelsig, newdata=cancertest)
t4<-table(predictsig,cancertest$classe)
prop.table(t4)
0.626666667 +0.333333333

#########knn########

knn1cancer<-kknn(classe~.,cancerapp ,cancertest, distance =1, k=1)
attributes(knn1cancer)
summary(knn1cancer)
fit1c <- fitted(knn1cancer)
prop.table(table(fit1c,cancertest$classe))
0.64666667+0.32666667


knn3cancer<-kknn(classe~.,cancerapp ,cancertest, distance =1, k=3)
attributes(knn3cancer)
summary(knn3cancer)
fit3c <- fitted(knn3cancer)
prop.table(table(fit3c,cancertest$classe))
0.64666667 + 0.32666667

knn5cancer<-kknn(classe~.,cancerapp ,cancertest, distance =1, k=5)
attributes(knn5cancer)
summary(knn5cancer)
fit5c <- fitted(knn5cancer)
prop.table(table(fit5c,cancertest$classe))
0.640000000+0.330000000

knn7cancer<-kknn(classe~.,cancerapp ,cancertest, distance =1, k=7)
attributes(knn7cancer)
summary(knn7cancer)
fit7c <- fitted(knn7cancer)
prop.table(table(fit7c,cancertest$classe))
0.640000000 +0.330000000








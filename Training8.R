scoring.test=read.table(file=file.choose(),header=T,sep="\t")
scoring.app=read.table(file=file.choose(),header=T,sep="\t")
library(
data=data(bordeaux)
summary(data)
bordeaux
?linDA
my_lin1 = linDA(bordeaux[,1:5], bordeaux$quality)

fbad= -24324.18232+1956* 25.81131-3044*0.03744  -1.49156 *1200+15*10.57760+372* 0.57891 
summary(scoring.app)

length(scoring.app$CLASS)
library(MASS)
?lda
1)
ldascoring <- lda(scoring.app$CLASS~ ., scoring.app)
?predict
summary(ldascoring)
library(VGAM)
predicta <- predict(ldascoring , newdata=scoring.app)
summary(predicta)
table(predicta$class ,scoring.app$CLASS)
2)
head(predicta$posterior)
3)
?cbind
score=predicta$posterior[,1]
cbindscore=cbind(scoring.app,score)

library(caret)
summary(cbindscore)
?sort
scoretri=sort(score,method="shell")
newscore=cbind(scoring.app,scoretri)

trie=cbindscore[order(cbindscore[,87],decreasing=T),]
summary(trie)
?lift
lift1 <- lift(, data = scoring.app)

lfit.plot=lift(CLASS~trie[,87],trie,plot="lift")
xyplot(lfit.plot)
scoring.app$CLASS=as.factor(scoring.app$CLASS)

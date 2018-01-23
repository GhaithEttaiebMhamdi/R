voiture =read.table(file=file.choose(),header=T,sep="	")
summary(voiture)
voiture <- voiture[,2:7]
summary(voiture)
qualvoit<-voiture[,1:5]





summary(qualvoit)
pairs(voiture)
pairs(qualvoit)

#kmeans
kmvoiture<-kmeans(qualvoit,3)
plot(qualvoit[,1], qualvoit[,2], col=kmvoiture$cluster)
table(kmvoiture$cluster,voiture$origin)

#cah 
distvoit<-dist(qualvoit,method='euclidean')
hcvoit<-hclust(distvoit,method='average')
plot(hcvoit)
classevoit<-cutree(hcvoit,3)
table(classevoit,voiture$origin)


#PCA
v=PCA(voiture,quali.sup=6)
plot.PCA(v,axes=c(1,2),habillage=6)

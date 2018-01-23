#arules 

data(Groceries)
summary(LIST(Groceries))
summary(Groceries)
LIST(Groceries)
?apriori
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5, target = "rules"))
inspect(head(rules))
inspect(tail(rules))
rules <- sort(rules, by="lift") #donne toutes les valeurs
rules <- head(rules, n=10, by="lift") #donne les 6 premieres valeurs
inspect(rules) #donne les regles.
#####################################################################
#viz approche graphique

rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
#affichage de tout les regles
plot(rules)
head(quality(rules)) 
plot(rules, measure=c("support", "lift"), shading="confidence")
sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)
plot(rules, measure=c("support", "lift"), shading="confidence", method="grouped")
plot(rules, measure=c("support", "lift"), shading="confidence", method="graph")

subrules <- rules[quality(rules)$confidence > 0.8] 
subrules
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))

subrules2 <- head(sort(rules, by="lift"), 10)
subrules2
plot(subrules2)

plot(subrules2, method="graph")

plot(subrules2, method="graph", control=list(type="itemsets"))



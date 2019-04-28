grocery=read.csv('Groceries_wide.csv')
View(grocery)

grocery$id=NULL

library(arules)
install.packages('arules')

for(i in 1:length(grocery)){
  grocery[[i]]=as.factor(grocery[[i]])
}
class(grocery$baby.cosmetics)

df_trans=as(grocery,'transactions')
inspect(head(df_trans,6))

rules=apriori(df_trans,parameter=list(supp=0.001,confidence=0.8,minlen=2))
inspect(head(rules,20))

rulesdf=as(rules,'data.frame')
View(rulesdf)

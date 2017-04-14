
#Market Basket Analysis

library(arules)

library(arulesViz)

Groceries <- read.csv(file.choose(), header = TRUE)
str(Groceries)
View(Groceries)
gr_rules <- apriori(Groceries,parameter = list(sup=.001, conf=0.8))
inspect(gr_rules[1:10])
redundant_rules <- is.redundant(gr_rules)
summary(redundant_rules)
gr_rules <- gr_rules[!redundant_rules]
inspect(gr_rules[1:10])
gr_rules_beer <- apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
                                appearance = list(default="lhs",rhs="whole milk"),
                                control = list(verbose=F))

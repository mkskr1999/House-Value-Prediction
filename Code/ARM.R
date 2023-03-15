library(viridis)
library(arules)
library(TSP)
library(data.table)
library(ggplot2)
library(Matrix)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
library(arulesViz)
library(arules)
Housing <- read.transactions("C:/Users/mksai/Downloads/transactions_data.csv",
                                rm.duplicates = FALSE, 
                                format = "basket",  ##if you use "single" also use cols=c(1,2)
                                sep=",",  ## csv file
                                cols=1)
apriori(inspect(Housing))

FrulesK = arules::apriori(Housing, parameter = list(support=.05, 
                                                       confidence=.15, minlen=2))
apriori(inspect(FrulesK))

## Plot of which items are most frequent
itemFrequencyPlot(Housing , topN=10, type="absolute")

## Sort rules by support
SortedRulesK <- sort(FrulesK, by="support", decreasing=TRUE)
apriori(inspect(SortedRulesK[1:15]))

## Sort rules by confidence
SortedRulesK_1 <- sort(FrulesK, by="confidence", decreasing=TRUE)
apriori(inspect(SortedRulesK_1[1:15]))

## Sort rules by lift
SortedRulesK_2 <- sort(FrulesK, by="lift", decreasing=TRUE)
apriori(inspect(SortedRulesK_2[1:15]))

plot(SortedRulesK, method = "graph",main="Top15 Rules sorted by support")
plot(SortedRulesK_1, method = "graph",main="Top15 Rules sorted by conifidence")
plot(SortedRulesK_2, method = "graph",main="Top15 Rules sorted by lift")
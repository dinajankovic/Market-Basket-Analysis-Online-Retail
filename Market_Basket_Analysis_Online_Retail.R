dataset = read.csv("OnlineRetail.csv", na.strings = c("","NA"))
attach(dataset)

#checking if there any missing values, in which column(s) are they missing, and how many of them are missing
any(is.na(dataset))
apply(dataset, 2, function(x) any(is.na(x)))
sum(is.na(CustomerID))
sum(is.na(Description))

#cleaning the data set
dataset$InvoiceNo = as.character(InvoiceNo)
trim = function (x) gsub("^\\s+|\\s+$", "", x)
dataset$InvoiceNo = trim(InvoiceNo)
dataset$Description = trim(as.character(Description))

#a function that checks if a string starts with C
is_C = function (x) startsWith(x,"C")
dataset2 = dataset[which(!is_C(dataset$InvoiceNo)),]
dataset3 = subset(dataset2,!is.na(dataset2$Description))

#function str_detect is located in package "stringr"
library(stringr)


#buzzwords = c("WRONG","LOST", "CRUSHED", "SMASHED", "DAMAGED", "FOUND", "THROWN", "MISSING",
#              "AWAY", "\\?", "CHECK", "POSTAGE", "MANUAL", "CHARGES", "AMAZON", "FEE",
#              "FAULT", "SALES", "ADJUST", "COUNTED", "LABEL", "INCORRECT", "SOLD", "BROKEN",
#              "BARCODE", "CRACKED", "RETURNED", "MAILOUT", "DELIVERY", "MIX UP", "MOULDY",
#              "PUT ASIDE", "ERROR", "DESTROYED", "RUSTY")


#isUndesirable = function(x){
#  c=FALSE

#  for (i in 1:(length(buzzwords))){
#    c = c || ifelse(str_detect(toupper(x),buzzwords[i]),TRUE,FALSE)
#  }

#  return(c)
#}

isUndesirable2 = function(x) {
  str_detect(toupper(x),"WRONG") | str_detect(toupper(x),"LOST") |
    str_detect(toupper(x),"CRUSHED") | str_detect(toupper(x),"DAMAGE") |
    str_detect(toupper(x),"FOUND") | str_detect(toupper(x),"THROWN") |
    str_detect(toupper(x),"SMASHED") |
    str_detect(toupper(x),"\\?") |
    str_detect(toupper(x),"AWAY") | str_detect(toupper(x),"CHARGES") |
    str_detect(toupper(x),"FEE") | str_detect(toupper(x),"FAULT")
  str_detect(toupper(x),"SALES") | str_detect(toupper(x),"ADJUST") |
    str_detect(toupper(x),"COUNTED") |
    str_detect(toupper(x),"INCORRECT") |
    str_detect(toupper(x),"BROKEN") | str_detect(toupper(x),"BARCODE") |
    str_detect(toupper(x),"RETURNED") |
    str_detect(toupper(x),"MAILOUT") | str_detect(toupper(x),"DELIVERY") |
    str_detect(toupper(x),"MIX UP") | str_detect(toupper(x),"MOULDY") |
    str_detect(toupper(x),"PUT ASIDE") | str_detect(toupper(x),"ERROR") |
    str_detect(toupper(x),"DESTROYED") | str_detect(toupper(x),"RUSTY")
}

dataset4 = subset(dataset3, dataset3$Quantity > 0)
dataset5 = dataset4[which(!isUndesirable2(as.character(dataset4$Description))),]
Time = format(as.POSIXct(strptime(dataset5$InvoiceDate,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M:%S")
dataset5$InvoiceDate = as.Date(dataset5$InvoiceDate)
dataset5$Description = as.factor(dataset5$Description)

#data preprocessing is done!

library(plyr)
library(arules)

items = ddply(dataset5,c("InvoiceNo"), function(x)paste(x$Description, collapse = ","))
head(items)

write.csv(items,"Items_List.csv",quote=FALSE, row.names = TRUE)

#creating the baskets
baskets = read.transactions("Items_List.csv", format='basket',sep=",")
summary(baskets)

#generating the rules
basket_rules = apriori(baskets,parameter = list(sup = 0.005, conf = 0.75))
basket_rules2 = apriori(baskets,parameter = list(sup = 0.01, conf = 0.7,maxlen=3))

basket_rules = sort(basket_rules, by='lift', decreasing = TRUE)
basket_rules2 = sort(basket_rules2, by = 'confidence', decreasing = TRUE)

summary(basket_rules)
inspect(basket_rules[1:10])

#visualizing the obtained rules
library(arulesViz)

#scatterplot
plot(basket_rules)

summary(basket_rules2)
inspect(basket_rules2[1:10])

#graph
plot(basket_rules2[1:10],method="graph")

basket_rules3 = apriori(baskets, parameter=list(supp=0.002,conf = 0.8),
               appearance = list(default="lhs",rhs="COFFEE"),
               control = list(verbose=F))
basket_rules3 = sort(basket_rules3, decreasing=TRUE,by="confidence")
summary(basket_rules3)
inspect(basket_rules3[1:5])

basket_rules4 = apriori(baskets, parameter=list(supp=0.01,conf = 0.7),
               appearance = list(default="rhs",lhs="SUGAR"),
               control = list(verbose=F))
basket_rules4 = sort(basket_rules4, decreasing=TRUE,by="confidence")
summary(basket_rules4)
inspect(basket_rules4)

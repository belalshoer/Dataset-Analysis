library("dplyr")
library("arules")
path<- readline("enter the path of your file: ")
x <- read.csv(path, stringsAsFactors = F)
payments <- group_by(x,paymentType)
payments <- summarise(payments,Total_Spent=sum(total))
pie(
  x = payments$Total_Spent,
  labels = payments$paymentType,
  main = "comapre cash and credit"
)
#########
sumtoage <- group_by(x,age)
sumtoage <- summarise(sumtoage, summ=sum(total))
plot(x = sumtoage$age,
     y = sumtoage$summ,
     main = "Age and total spending",
     xlab = "Age",
     ylab = "Total Spending"
)
##########
city_total <- group_by(x,city)
city_total <- summarize(city_total, sum_city=sum(total))
print(city_total)
city_total<- arrange(city_total,desc(city_total$sum_city))
barplot(
  height = city_total$sum_city,
  name = city_total$city,
  las = 2,
  col = "lightblue",
  main = "City and total spending",
)
#######
boxplot(
  x$total,
  main = "Distribution of total spending",
  xlab= "Total spending"
)
#######
par(mfrow=c(2,2))
payments <- group_by(x,paymentType)
payments <- summarise(payments,Total_Spent=sum(total))
pie(
  x = payments$Total_Spent,
  labels = payments$paymentType,
  main = "comapre cash and credit"
)
sumtoage <- group_by(x,age)
sumtoage <- summarise(sumtoage, summ=sum(total))
plot(x = sumtoage$age,
     y = sumtoage$summ,
     main = "Age and total spending",
     xlab = "Age",
     ylab = "Total Spending"
)
city_total <- group_by(x,city)
city_total <- summarize(city_total, sum_city=sum(total))
print(city_total)
city_total<- arrange(city_total,desc(city_total$sum_city))
barplot(
  height = city_total$sum_city,
  name = city_total$city,
  las = 2,
  col = "lightblue",
  legend.text = T,
  main = "City and total spending",
)
boxplot(
  x$total,
  main = "Distribution of total spending",
  xlab= "Total spending"
)
clusters_input<- readline("enter a number of clusters between 2 and 4: ")
if (clusters_input < 2 || clusters_input > 4){
  print("Invalid!")
}else {
  clusters <- as.numeric(clusters_input)
}
datax = x$age
datay=x$total
datapoints <- cbind(datax,datay)
result <- kmeans(datapoints,clusters)
x$cluster<-result$cluster
k_data<-select(x,customer,age,total,cluster)
k_data
#####
transactions <- read.transactions("C:/Users/himas/Desktop/grc/transactions.txt", sep = ",")
inspect(transactions)
input<- readline("enter the minimum support(between 0.001 and 1): ")
if (input < 0.001 || input > 1){
  print("Invalid!")
}else {
  min_support <- as.numeric(input)
}
input<- readline("enter the minimum confidence(between 0.001 and 1): ")
if (input < 0.001 || input > 1){
  print("Invalid!")
}else {
  min_confidence <- as.numeric(input)
}
apr <- apriori(transactions,
               parameter =list(supp = min_support, conf = min_confidence , minlen = 2 ) )
inspect(apr)
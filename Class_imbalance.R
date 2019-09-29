#Load Data
data <- read.csv(".//Class_imbalance_inR//binary.csv")
str(data)
head(data)
data$admit <- as.factor(data$admit)
summary(data)
prop.table(table(data$admit)) %>% barplot()
#Train test
ind <- sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
one <- data[ind==1,]
two <- data[ind==2,]

#Data for developing model

table(one$admit) %>% prop.table()
summary(one)

#prediction
library(randomForest)
library(e1071)
rf1 <- randomForest(admit~.,data=one)

#evaluation
library(caret)
confusionMatrix(predict(rf1,two),two$admit,positive = "1")
#over sampling for better sensitivity
library(ROSE)
#ROSE:randomly over sampling examples
over <- ovun.sample(admit~.,data=one,method="over",N=199*2)$data
table(over$admit)
summary(over)
under <- ovun.sample(admit~.,data=one,method="under",N=92*2)$data
table(under$admit)
summary(under)
both <- ovun.sample(admit~.,data=one,method="both",N=285)$data
table(both$admit)
summary(both)
rose <- ROSE(admit~.,data=one,N=500)$data
summary(rose)
#rf for over & under 
rf_over <- randomForest(admit~.,data=over)
confusionMatrix(predict(rf_over,two),two$admit,positive = "1")
rf_under <- randomForest(admit~.,data=under)
confusionMatrix(predict(rf_under,two),two$admit,positive = "1")
rf_both <- randomForest(admit~.,data=both)
confusionMatrix(predict(rf_both,two),two$admit,positive = "1")
rf_rose <- randomForest(admit~.,data=rose)
confusionMatrix(predict(rf_rose,two),two$admit,positive = "1")

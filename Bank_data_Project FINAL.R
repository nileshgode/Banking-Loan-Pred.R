# program on banking database

getwd()
setwd("D:/Nilesh")
data<-read.csv("banking.csv",sep = ";")
View(data)
#install.packages('mice')
library(mice)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('caret')
library(caret) # Accuracy
#install.packages('e1071')
library(e1071)
#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('rattle')
library(rattle)
#install.packages('randomForest')
library(randomForest)
#install.packages('caTools')
library(caTools)
#install.packages('descr')
library(descr)

#summary of data
dim(data)
names(data)

#summary before cleaning
summary(data)

# Data Cleaning
sum(data == "unknown")

#install.packages('dplyr')
library(dplyr)

#install.packages('stringr')
library(stringr)

str(data)
library(dplyr)
data <- mutate(data, empvarrate = as.factor(emp.var.rate))
plot(data$emp.var.rate)

# Check missing values with Plot Visualisation
#install.packages('VIM')
library(VIM)
mice_plot <- aggr(data, col=c('navyblue','yellow'), number=TRUE,label=names(data),cex.axis=0.7, gap=3, ylab=c("missingData","pattern"))

#data %n% summarise_all(list(~sum(.=="unknown"))) %>% gather(key="variable",value="unknown") %>% arrange(nr_unknown)

# Data Cleaning with mice Package
#install.packages('mice')
library(mice)
md.pattern(data)
View(data)

#install.packages('Hmisc')
library(Hmisc)
impute_arg <- aregImpute(~ marital + education + default + housing + loan + contact + month, data=data , n.impute = 5)

library(descr)
CrossTable(data$y)
sum(data$marital=="unknown")
CrossTable(data$marital)
library(dplyr)
#data$marital[data$marital %n% "unknown"] <-"single"
sum(data$marital)

sum(data$default=='unknown')
CrossTable(data$default)

gsub("unknown", NA, data)
gsub(data$housing,"unknown", NA)

# Data Visualization
# Histogram Plot
par(mfrow=c(2,2))
hist(data[,1], col=rainbow(7),main=names(data[1]))
for(i in 11:14)
{  hist(data[,i], col=rainbow(7),main=names(data[i])) }
for(i in 16:20)
{  hist(data[,i], col=rainbow(7),main=names(data[i])) }

# import library
library(ggplot2)
install.packages('tidyverse')
library(tidyverse)

#Boxplot
par(mfrow=c(1,3))
for(i in 1 : length(data))
{
  if(i==1 | i==11 | i==20)
    boxplot(data[,i],main=names(data[i]),col=rainbow(7))
}
ggplot(data, aes(factor(y),duration)) + geom_boxplot(aes(fill=(factor(y))))
ggplot(data, aes(factor(y),age)) + geom_boxplot(aes(fill=(factor(y))))

hist(data[,1], col=rainbow(7),main=names(data[1]))
for(i in 11:14)
{  hist(data[,i], col=rainbow(7),main=names(data[i])) }
for(i in 16:20)
{  hist(data[,i], col=rainbow(7),main=names(data[i])) }

# Correlation
#install.packages('corrplot')  
library(corrplot) 
par(mfrow=c(1,1))
d <- data[c(1,11,12,13,14,16,18,19,20)]
corm <-round(cor(d),2)
corrplot(corm,method="number")


# for(i in data$education) if(i %in% unknown) data$education -> NA

#data[data$ == "unknown"] <- mode(data$education)
# Check for any missing values:
sum(is.na(data))

summary(data)
d2 <- data

#Converting quantititative values to numeric class

d2$age <- as.numeric(d2$age)
d2$duration <- as.numeric(d2$duration)
d2$campaign <- as.numeric(d2$campaign)
d2$pdays <- as.numeric(d2$pdays)
d2$previous <- as.numeric(d2$previous)
d2$emp.var.rate <- as.numeric(d2$emp.var.rate)
d2$cons.price.idx <- as.numeric(d2$cons.price.idx)
d2$cons.conf.idx <- as.numeric(d2$cons.conf.idx)
d2$nr.employed <- as.numeric(d2$nr.employed)

str(d2)

#checking classes of attributes after transformation
sapply(d2,class)



# Sampling the dataset into training data and test data:
set.seed(2314)
sample <- sample.int(n = nrow(d2), size = floor(.8*nrow(d2)), replace = F)
traindata <- data[sample, ]
testdata  <- data[-sample, ]

sapply(d2,class)


#Analyzing barplots of variables
par(mfrow=c(2,2))
for(i in 1:length(data))
{barplot(prop.table(table(data[,i])) , 
         xlab=names(data[i]), ylab= "Frequency (%)" , col = rainbow(3))}

summary(data)
sapply(d2,class)

##Implementing CART

# Classification and Regression Trees
library(rpart)
bank.cart<-rpart(y ~ ., traindata , method = 'class')

par(mfrow=c(1,1))

library(rattle)
fancyRpartPlot(bank.cart , digits=16 , palettes = c("Purples", "Oranges"))

#predict
cart_pred <- predict( bank.cart , testdata , type = "class")
cart_prob <- predict( bank.cart , testdata , type = "prob")

library(caret)
# Confusion matrix
confusionMatrix(cart_pred , testdata$y)

### Cross table validation for CART
CrossTable(testdata$y, cart_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))



#Implementing C5.0

install.packages('C50')
library(C50)
bank.c50 <- C5.0(y ~ . , traindata , trials=15)
bank.c50.pred<-predict(bank.c50,testdata,type="class" )
bank.c50.prob<-predict(bank.c50,testdata,type="prob" )
# Confusion matrix
confusionMatrix(bank.c50.pred, testdata$y)

### Cross table validation for random forest
CrossTable(testdata$y, bank.c50.pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))



#Implementing KNN

bank.knn <- train(y ~ ., data = traindata, method = "knn", 
                  maximize = TRUE,
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess=c("center", "scale"))

predictedkNN <- predict(bank.knn , newdata = testdata)
confusionMatrix(predictedkNN , testdata$y)

### Cross table validation for KNN
CrossTable(testdata$y, predictedkNN,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
install.packages('tinytex')

# Logistic Regression
library(caTools)
# Spliting the data in training and testing of data
split <- sample.split(data, SplitRatio = 0.80)
split

# Devide data into traning and testing
training <- subset(data,split == TRUE)
testing <- subset(data,split == FALSE)

# Plot the Regression line with given data set
model <- glm(data$y ~.,training , family = "continous" )
summary(model)

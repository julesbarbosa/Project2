data <- read.csv("~/homework/application_train.csv", header = TRUE, stringsAsFactors = FALSE)
credit_card_balance <- read.csv("~/homework/credit_card_balance2.csv", header = TRUE, stringsAsFactors = FALSE)

data <- merge(x = data, y = credit_card_balance, by = "SK_ID_CURR", all.x = TRUE)
data<-data[,-which(names(data)=="SK_ID_CURR")]
data<-data[,-which(names(data)=="ORGANIZATION_TYPE")]
data$TARGET<-factor(data$TARGET)

target<- data$TARGET
chr <- data[,sapply(data, is.character)]
num <- data[,sapply(data, is.numeric)]
chr[is.na(chr)] <- "Not Available"
num[is.na(num)] <- 0
fac<-as.data.frame(apply(chr[,1:15], 2, factor))
data <- cbind(fac, num)
data <- cbind(target,data)


data$more_than_one_loan <- as.factor(data$more_than_one_loan)
data$PayLate <- as.factor(data$PayLate)
is.na(data$MeanInstallments) <- 0
data$target<-factor(data$target)

######### Spliting

set.seed(1000)
TrainingData <- sample(1:nrow(data),0.8*nrow(data)) # row indices for training
train <- data[TrainingData, ] #model for training
test <- data[-TrainingData,]   #test

### Balancing UNDER
library(ROSE)

table(train$target)
prop.table(table(data$target))
train$target <- as.factor(train$target)
train.under <- ovun.sample(target ~., data = train, 
                           method = "under", N= 31828)$data  
prop.table(table(train.under$target))

### Balancing OVER
train.over <- ovun.sample(target ~., data = train, 
                          method = "over", N=361784 )$data

prop.table(table(train.over$target))
#### Balancing BOTH
train.both <- ovun.sample(target ~., data = train, 
                          method = "both", N=361784)$data

prop.table(table(train.both$target))

#### Model

library(randomForest)
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
rf = randomForest(target~., data = train.both, importance=T, ntree = 300)
prediction<-predict(rf, test, type='response')

### Validation
library(caret)
library(ggplot2)

confusionMatrix(prediction, test$target, positive = "1", mode="everything")

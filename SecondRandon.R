data <- read.csv("~/Homework/application_train.csv", header = TRUE, stringsAsFactors = FALSE)
credit_card <- read.csv("~/Homework/credit_card_balance2.csv", header = TRUE)

str(data)
data <- merge(x= data, y= credit_card, by = "SK_ID_CURR", all.x=TRUE, all.y=FALSE)
target<- data$TARGET
chr <- data[,sapply(data, is.character)]
num <- data[,sapply(data, is.numeric)]
chr[is.na(chr)] <- "Not Available"
num[is.na(num)] <- 0
fac <- as.data.frame(apply(chr[,1:15], 2, factor))
data <- cbind(fac, num)
data <- cbind(target, data)
data$target <- as.factor(data$target)

str(data)

data$more_than_one_loan <- as.factor(data$more_than_one_loan)
data$PayLate <- as.factor(data$PayLate)
#is.na(data$MeanInstallments) <- 0
ian <- is.na(data$MeanInstallments) 
data$MeanInstallments <- 

data$target<-factor(data$target)
data$ORGANIZATION_TYPE <- NULL

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
                           method = "under", N= 9656)$data  
prop.table(table(train.under$target))

# sampling

train.sample <- sample(1:nrow(train.under),0.5*nrow(train.under))
train.sample <- train.under[train.sample,]
train.sample$ORGANIZATION_TYPE <- NULL
#### Model

library(randomForest)
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
str(train.under$target)
rf = randomForest(target ~., data = train.sample, importance=T, ntree = 300)
prediction<-predict(rf, test, type='response')

### Validation
library(caret)
library(ggplot2)

confusionMatrix(prediction, test$target, positive = "1", mode="everything")


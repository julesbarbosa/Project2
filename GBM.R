str(data)

chr <- data[,sapply(data, is.character)]
num <- data[,sapply(data, is.numeric)]


str(num)
num[is.na(num)] <- 0
chr[is.na(chr)] <- "Not Available"

newData <- data.frame(data$SK_ID_CURR,data$TARGET,data$NAME_CONTRACT_TYPE,
                    data$CODE_GENDER,data$FLAG_OWN_CAR,data$FLAG_OWN_REALTY,
                    data$AMT_INCOME_TOTAL,data$AMT_CREDIT,data$AMT_GOODS_PRICE,
                    data$NAME_INCOME_TYPE,data$NAME_EDUCATION_TYPE,
                    data$NAME_FAMILY_STATUS,data$DAYS_BIRTH,data$OCCUPATION_TYPE,
                    data$CNT_FAM_MEMBERS,data$REGION_RATING_CLIENT,
                    data$REGION_RATING_CLIENT_W_CITY,data$ORGANIZATION_TYPE,data$FLAG_DOCUMENT_2,
                    data$FLAG_DOCUMENT_3,data$FLAG_DOCUMENT_4,data$FLAG_DOCUMENT_5,data$FLAG_DOCUMENT_6)

newData$data.OCCUPATION_TYPE[is.na(newData$data.OCCUPATION_TYPE)]<- "Laborers"
newData$data.OCCUPATION_TYPE=factor(newData$data.OCCUPATION_TYPE)
meancnt=mean(na.omit(newData$data.CNT_FAM_MEMBERS))
newData$data.CNT_FAM_MEMBERS[is.na(newData$data.CNT_FAM_MEMBERS)]=meancnt
meangp=mean(na.omit(newData$data.AMT_GOODS_PRICE))
newData$data.AMT_GOODS_PRICE[is.na(newData$data.AMT_GOODS_PRICE)]=meangp
str(newData$data.DAYS_BIRTH)
summary(newData$data.DAYS_BIRTH)
na_count <-sapply(newData, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)
newData$data.DAYS_BIRTH=(newData$data.DAYS_BIRTH*(-1))
newData$data.DAYS_BIRTH=(newData$data.DAYS_BIRTH/(365.25))



colnames(newData)[colnames(newData)=="data.SK_ID_CURR"] <- "SK_ID_CURR"
data <- merge(x=newData, y= credit_card_balance, by= "SK_ID_CURR", all.x = TRUE)

str(data)
data$DrawingMeans[is.na(data$DrawingMeans)] <- 0
data$Less_than_limit[is.na(data$Less_than_limit)] <- 0
data$MeanInstallments[is.na(data$MeanInstallments)] <- 0
data$SK_DPD_DEF[is.na(data$SK_DPD_DEF)] <- 0
data$SK_DPD[is.na(data$SK_DPD)] <- 0
data$more_than_one_loan[is.na(data$more_than_one_loan)] <- 0
data$more_than_one_loan <- as.factor(data$more_than_one_loan)
data$Number_of_loans[is.na(data$Number_of_loans)] <- 0
data$PayLate[is.na(data$PayLate)] <- 0
data$PayLate <- as.factor(data$PayLate)
data$X <- NULL
str(data)


colnames(data)[colnames(data)=="data.TARGET"] <- "target"
data<-data[,-which(names(data)=="SK_ID_CURR")]
##### spliting
set.seed(1000)
TrainingData <- sample(1:nrow(data),0.8*nrow(data)) # row indices for training
traingbm <- data[TrainingData, ] #model for training
test <- data[-TrainingData,]   #test

library(caret)

fitControl.gbm <- trainControl(method = "repeatedcv",
                               number = 2,
                               repeats = 2,
                               sampling = "up")

outcomeName <- 'target'
predictorNames <- names(traingbm)[names(traingbm) != outcomeName]

traingbm$target <- as.factor(traingbm$target)
gbm<-train(traingbm[,predictorNames],traingbm[,outcomeName],
           method='gbm',
           trControl=fitControl.gbm)
gbmImp.T<-as.data.frame(gbmImp$importance)
write.csv(gbmImp.T, "factors.csv")

test$target <- as.factor(test$target)
gbm.predict <-predict(gbm,test[,predictorNames], type="raw")
confusionMatrix(gbm.predict, test$target, positive = "1", mode= "everything")
library(pROC)
gbm.probs <- predict(gbm,test[,predictorNames],type="prob")    

gbm.plot<-plot(roc(test$target,gbm.probs[,2]))

band<-as.numeric(gbm.probs$`1`)
scoreband<-cut(band,10)
barplot(prop.table(table(scoreband)))


save(gbm, file="GBMmodel.Rdata1")



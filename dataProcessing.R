data <- read.csv("Homework/previous_applicationclean.csv", header = TRUE)
data <-read.csv("Homework/application_train.csv", header=TRUE)
 

## FEATURE SINGLE
summary(data$NAME_FAMILY_STATUS)
data$single <- as.factor(ifelse(data$NAME_FAMILY_STATUS=="Separated" | data$NAME_FAMILY_STATUS=="Single / not married"| 
                                  data$NAME_FAMILY_STATUS=="Widow"| 
                      data$NAME_FAMILY_STATUS=="Unknown", 1,0))

## remove NA`s in gender
data <- subset(data, data$CODE_GENDER == "F"| data$CODE_GENDER =="M")

## Combine Car and Realty

data$own_car_realty <- ifelse(data$FLAG_OWN_CAR == "Y"| data$FLAG_OWN_REALTY == "Y", 1,0)

### Creating income per person in the house
data$income_per_person <- data$AMT_INCOME_TOTAL/data$CNT_FAM_MEMBERS

## Creating variable ratio of income by anual payment
data$credit_income_ratio <- data$AMT_INCOME_TOTAL/data$AMT_ANNUITY

## Ratio Credit over income and Annuity

data$Ratio_Credit_Income <- data$AMT_INCOME_TOTAL/ data$AMT_CREDIT
data$Ratio_Annuity_Income <- data$AMT_INCOME_TOTAL/data$AMT_ANNUITY

summary(data$Ratio_Annuity_Income) ## DELETE NA`s `

### % of total good price
data$good_credit <- data$AMT_CREDIT/data$AMT_GOODS_PRICE


### Alone in the day

data$alone_or_not <- ifelse(data$NAME_TYPE_SUITE == "Unaccompanied", 1,0)


#### Selecting people with jobs

data$has_job <- ifelse(data$NAME_INCOME_TYPE =="Businessman" |
                         data$NAME_INCOME_TYPE =="Commercial associate" | 
                         data$NAME_INCOME_TYPE =="State servant" |data$NAME_INCOME_TYPE =="Working", 1, 0)
table(data$has_job)

##### has a higher education or not
data$higher_education <- ifelse(data$NAME_EDUCATION_TYPE=="Academic degree" | 
                                  data$NAME_EDUCATION_TYPE=="Higher education" |
                                  data$NAME_EDUCATION_TYPE=="Incomplete higher", 1, 0)

### car ages NA`S is for people that does not have cars


#### NA`S fAmily member put the MEAN`
summary(data$CNT_FAM_MEMBERS)


### FACTORS

data$REG_CITY_NOT_LIVE_CITY <-  as.factor(data$REG_CITY_NOT_LIVE_CITY)
data$REG_REGION_NOT_WORK_REGION <-  as.factor(data$REG_CITY_NOT_WORK_CITY)
data$REGION_RATING_CLIENT <-  as.factor(data$REGION_RATING_CLIENT)
data$REGION_RATING_CLIENT_W_CITY <- as.factor(data$REGION_RATING_CLIENT_W_CITY)
data$REG_REGION_NOT_LIVE_REGION <- as.factor(data$REG_REGION_NOT_LIVE_REGION)
data$REG_REGION_NOT_WORK_REGION <- as.factor(data$REG_REGION_NOT_WORK_REGION)
data$LIVE_REGION_NOT_WORK_REGION <- as.factor(data$LIVE_REGION_NOT_WORK_REGION)
data$REG_CITY_NOT_WORK_CITY <- as.factor(data$REG_CITY_NOT_WORK_CITY)
data$LIVE_CITY_NOT_WORK_CITY <- as.factor(data$LIVE_CITY_NOT_WORK_CITY)

###
data$ORGANIZATION_TYPE <- NULL
head(data)

### SOURCE avarage score
x <- data[,c(1,41,42,43)]
x[is.na(x)] <- 0
x$mean <- apply(x[,2:4],1,mean)
data$EXT_SOURCE_1 <- NULL
data$EXT_SOURCE_2 <- NULL 
data$EXT_SOURCE_3 <- NULL 
data <- merge(x=data, y=x, by = "SK_ID_CURR", all.x = TRUE)
colnames(data)[colnames(data)=="mean"] <- "avarage_source"
#####
### Avarege of avarages Apartment
x <- data[,c(1,41,42,43,44,45,46,47,48,49,50,51,52,53,54)]
x[is.na(x)] <- 0
x$AVG_apart <- apply(x[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)],1,mean)


data <- merge(x=data, y=x, by = "SK_ID_CURR", all.x = TRUE)

colnames(data)[colnames(data)=="mean"] <- "avarage_source"
data[,134:148] <- NULL
str(data)
#### Median OF Median
data$median <- apply(data[,55:68],1,median)
data$median[is.na(data$median)] <- 0
## documents
data$document=ifelse(data$FLAG_DOCUMENT_2==1 |
                       data$FLAG_DOCUMENT_3==1 |
                       data$FLAG_DOCUMENT_4==1 |
                       data$FLAG_DOCUMENT_5==1 |data$FLAG_DOCUMENT_6==1 |data$FLAG_DOCUMENT_7==1 |
                       data$FLAG_DOCUMENT_8==1 |data$FLAG_DOCUMENT_9==1 |
                       data$FLAG_DOCUMENT_10==1 |data$FLAG_DOCUMENT_11==1 |
                       data$FLAG_DOCUMENT_12==1 |data$FLAG_DOCUMENT_13==1 |
                       data$FLAG_DOCUMENT_14==1 |data$FLAG_DOCUMENT_15==1 |
                       data$FLAG_DOCUMENT_16==1 |data$FLAG_DOCUMENT_17==1 |
                       data$FLAG_DOCUMENT_18==1 |data$FLAG_DOCUMENT_19==1 |
                       data$FLAG_DOCUMENT_20==1 ,1,0)

### Bureau
data$AMT_REQ_CREDIT_BUREAU_HOUR=ifelse(data$AMT_REQ_CREDIT_BUREAU_HOUR>0,1,0)
data$AMT_REQ_CREDIT_BUREAU_DAY=ifelse(data$AMT_REQ_CREDIT_BUREAU_DAY>0,1,0)
data$AMT_REQ_CREDIT_BUREAU_WEEK=ifelse(data$AMT_REQ_CREDIT_BUREAU_WEEK>0,1,0)
data$AMT_REQ_CREDIT_BUREAU_MON=ifelse(data$AMT_REQ_CREDIT_BUREAU_MON>0,1,0)
data$AMT_REQ_CREDIT_BUREAU_QRT=ifelse(data$AMT_REQ_CREDIT_BUREAU_QRT>0,1,0)
data$AMT_REQ_CREDIT_BUREAU_YEAR=ifelse(data$AMT_REQ_CREDIT_BUREAU_YEAR>0,1,0)


#### DELITING na`s
meangp=mean(na.omit(data$AMT_GOODS_PRICE))
data$AMT_GOODS_PRICE[is.na(data$AMT_GOODS_PRICE)]=meangp
meanann=mean(na.omit(data$AMT_ANNUITY))
data$AMT_ANNUITY[is.na(data$AMT_ANNUITY)]=meanann
data$OCCUPATION_TYPE[is.na(data$OCCUPATION_TYPE)]<- ""
meancar=mean(na.omit(data$OWN_CAR_AGE))
data$OWN_CAR_AGE[is.na(data$OWN_CAR_AGE)]=meancar
meanext1=mean(na.omit(data$EXT_SOURCE_1))
data$EXT_SOURCE_1[is.na(data$EXT_SOURCE_1)]=meanext1
meanext2=mean(na.omit(data$EXT_SOURCE_2))
data$EXT_SOURCE_2[is.na(data$EXT_SOURCE_2)]=meanext2
meanext3=mean(na.omit(data$EXT_SOURCE_3))
data$EXT_SOURCE_3[is.na(data$EXT_SOURCE_3)]=meanext3
meanamt1=mean(na.omit(data$AMT_REQ_CREDIT_BUREAU_HOUR))
data$AMT_REQ_CREDIT_BUREAU_HOUR[is.na(data$AMT_REQ_CREDIT_BUREAU_HOUR)]=meanamt1
meanamt2=mean(na.omit(data$AMT_REQ_CREDIT_BUREAU_DAY))
data$AMT_REQ_CREDIT_BUREAU_DAY[is.na(data$AMT_REQ_CREDIT_BUREAU_DAY)]=meanamt2
meanamt3=mean(na.omit(data$AMT_REQ_CREDIT_BUREAU_WEEK))
data$AMT_REQ_CREDIT_BUREAU_WEEK[is.na(data$AMT_REQ_CREDIT_BUREAU_WEEK)]=meanamt3
meanamt4=mean(na.omit(data$AMT_REQ_CREDIT_BUREAU_MON))
data$AMT_REQ_CREDIT_BUREAU_MON[is.na(data$AMT_REQ_CREDIT_BUREAU_MON)]=meanamt4
meanamt5=mean(na.omit(data$AMT_REQ_CREDIT_BUREAU_QRT))
data$AMT_REQ_CREDIT_BUREAU_QRT[is.na(data$AMT_REQ_CREDIT_BUREAU_QRT)]=meanamt5
meanamt6=mean(na.omit(data$AMT_REQ_CREDIT_BUREAU_YEAR))
data$AMT_REQ_CREDIT_BUREAU_YEAR[is.na(data$AMT_REQ_CREDIT_BUREAU_YEAR)]=meanamt6
meancnt=mean(na.omit(data$CNT_FAM_MEMBERS))
data$CNT_FAM_MEMBERS[is.na(data$CNT_FAM_MEMBERS)]=meancnt
data$DAYS_BIRTH=(data$DAYS_BIRTH*(-1))
data$DAYS_BIRTH=(data$DAYS_BIRTH/(365.25))
meantype="Unaccompanied"
data$NAME_TYPE_SUITE[is.na(data$NAME_TYPE_SUITE)]=meantype
data[, 34:96][is.na(data[, 34:96])] <- 0
data[is.na(data)]<-0

na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)
View(data)
write.csv(data,"application-nodelete.csv")


data[,c(41,42,43,44,45,46,47,48,49,50,51,52,53,54)] <- NULL
data$EXT_SOURCE_1 <- NULL
data$EXT_SOURCE_2 <- NULL 
data$EXT_SOURCE_3 <- NULL 

data$AMT_REQ_CREDIT_BUREAU_HOUR <- NULL
data$AMT_REQ_CREDIT_BUREAU_DAY <-  NULL
data$AMT_REQ_CREDIT_BUREAU_WEEK <- NULL
data$AMT_REQ_CREDIT_BUREAU_MON <- NULL
data$AMT_REQ_CREDIT_BUREAU_QRT <- NULL
data$AMT_REQ_CREDIT_BUREAU_YEAR <- NULL

data$NAME_FAMILY_STATUS <- NULL
data$NAME_TYPE_SUITE <- NULL
data$NAME_EDUCATION_TYPE <- NULL
data$NAME_INCOME_TYPE <- NULL

data$FLAG_DOCUMENT_10 <- NULL
data$FLAG_DOCUMENT_11 <- NULL
data$FLAG_DOCUMENT_12 <- NULL
data$FLAG_DOCUMENT_13 <- NULL
data$FLAG_DOCUMENT_14 <- NULL
data$FLAG_DOCUMENT_1 <- NULL
data$FLAG_DOCUMENT_2 <- NULL
data$FLAG_DOCUMENT_3 <- NULL
data$FLAG_DOCUMENT_4 <- NULL
data$FLAG_DOCUMENT_5 <- NULL
data$FLAG_DOCUMENT_6 <- NULL
data$FLAG_DOCUMENT_7 <- NULL
data$FLAG_DOCUMENT_8 <- NULL
data$FLAG_DOCUMENT_9 <- NULL
data$FLAG_DOCUMENT_15 <- NULL
data$FLAG_DOCUMENT_16 <- NULL
data$FLAG_DOCUMENT_17 <- NULL
data$FLAG_DOCUMENT_18 <- NULL
data$FLAG_DOCUMENT_19 <- NULL
data$FLAG_DOCUMENT_20 <- NULL
data$FLAG_DOCUMENT_21 <- NULL
data$FLAG_CONT_MOBILE <- NULL
data$FLAG_EMAIL <- NULL
data$FLAG_MOBIL <- NULL
data$FLAG_EMP_PHONE <- NULL
data$FLAG_PHONE <- NULL
data$FLAG_WORK_PHONE <- NULL

data$alone_or_not <- as.factor(data$alone_or_not)
data$has_job <- as.factor(data$has_job)
data$higher_education <- as.factor(data$higher_education)
data$document <- as.factor(data$document)
data$own_car_realty <- as.factor(data$own_car_realty)
str(data)

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
                           method = "under", N= 31922)$data  
prop.table(table(train.under$target))

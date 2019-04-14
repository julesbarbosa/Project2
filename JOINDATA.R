mERGING DATA SETE
meancli<-mean(na.omit(data2$NewClient))
data2$NewClient[is.na(data2$NewClient)]=meancli
data2$NewClient<-ifelse(data2$NewClient>0,1,0)
data2$NewClient<-as.factor(data2$NewClient)


meanap<-mean(na.omit(data2$aproved))
data2$aproved[is.na(data2$aproved)]=meanap
data2$aproved<-ifelse(data2$aproved>0,1,0)
data2$aproved<-as.factor(data2$aproved)
meannl<-mean(na.omit(data2$Number_of_loans))
data2$Number_of_loans[is.na(data2$Number_of_loans)]=meannl
meanone<-mean(na.omit(data2$more_than_one_loan))
data2$more_than_one_loan[is.na(data2$more_than_one_loan)]=meanone
meansk<-mean(na.omit(data2$SK_DPD))
data2$SK_DPD[is.na(data2$SK_DPD)]=meansk
meanskd<-mean(na.omit(data2$SK_DPD_DEF))
data2$SK_DPD_DEF[is.na(data2$SK_DPD_DEF)]=meanskd
meanpl<-mean(na.omit(data2$PayLate))
data2$PayLate[is.na(data2$PayLate)]=meanpl
meanmi<-mean(na.omit(data2$MeanInstallments))
data2$MeanInstallments[is.na(data2$MeanInstallments)]=meanmi
meanless<-mean(na.omit(data2$Less_than_limit))
data2$Less_than_limit[is.na(data2$Less_than_limit)]=meanless
meandm<-mean(na.omit(data2$DrawingMeans))
data2$DrawingMeans[is.na(data2$DrawingMeans)]=meandm

na_count <-sapply(data2, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

data2$more_than_oneApp<-as.factor(data2$more_than_oneApp)
data2$more_than_oneApp <- ifelse(is.na(data2$more_than_oneApp),0,data2$more_than_oneApp)
data2$Number_of_app <- ifelse(is.na(data2$Number_of_app),0,data2$Number_of_app)

data3 <- ifelse(is.na(data2),0,data2)

data2$TARGET=as.factor(data2$TARGET)

data2$X.x<-NULL
data2$X.y<-NULL
data2$X<-NULL




str(data2)
View(data2)
summary(data2)

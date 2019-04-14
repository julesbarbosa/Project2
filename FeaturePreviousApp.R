Previous <- read.csv("~/Downloads/previous_application.csv", header = TRUE, stringsAsFactors = FALSE)

str(Previous)

View(d1)
Previous[Previous==" "]<-NA
which(is.na(Previous), arr.ind=TRUE)
na_count <-sapply(Previous, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

Previous$RATE_INTEREST_PRIMARY <- NULL
Previous$RATE_INTEREST_PRIVILEGED <- NULL
Previous$AMT_DOWN_PAYMENT <- NULL
Previous$RATE_DOWN_PAYMENT <- NULL

chr <- Previous[,sapply(Previous, is.character)]
num <- Previous[,sapply(Previous, is.numeric)]
chr[is.na(chr)] <- "Not Available"
num[is.na(num)] <- 0
fac<-as.data.frame(apply(chr[,1:16], 2, factor))
data <- cbind(fac,num)

na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)
data$HOUR_APPR_PROCESS_START <- NULL
data$WEEKDAY_APPR_PROCESS_START <- NULL

##### FEATURES
library(plyr)
length(unique(data$SK_ID_CURR))

Numberofapp <- count(data, c("SK_ID_PREV", "SK_ID_CURR"))
freqApp <- ddply(Numberofapp, "SK_ID_CURR", nrow )
freqApp$more_than_oneApp <- ifelse(freqApp$V1>1, 1,0)
colnames(freqApp)[colnames(freqApp)=="V1"] <- "Number_of_app"


data$aproved <- ifelse(data$NAME_CONTRACT_STATUS =="aproved", 1, 0)
data$NewClient <- ifelse(data$NAME_CLIENT_TYPE=="New", 1, 0)

factor <- aggregate(data[,33:32], list(data$SK_ID_CURR), sum)
colnames(factor)[colnames(factor)=="Group.1"] <- "SK_ID_CURR"



medians <- aggregate(data[,19:22], 
                     list(data$SK_ID_CURR), mean)

colnames(medians)[colnames(medians)=="Group.1"] <- "SK_ID_CURR"
PreviousApp <- merge(x = medians, y = factor, by ="SK_ID_CURR", all.x = TRUE)
PreviousApp <- merge(x = PreviousApp, y = freqApp, by ="SK_ID_CURR", all.x = TRUE)

write.csv(PreviousApp, "previous_app.csv")

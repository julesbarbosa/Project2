credit_card_balance –> read.csv(~./dowload/"credit_card_balance.csv", HEADER = TRUE)

### previous Loans and frequency of Month Balance

number_of_loans <- count(credit_card_balance, c("SK_ID_PREV", "SK_ID_CURR"))

                         
### criating Frequency of Loans

freqLoam <- ddply(number_of_loans, "SK_ID_CURR", nrow )
freqLoam$more_than_one_loan <- ifelse(freqLoam$V1>1, 1,0)

colnames(freqLoam)[colnames(freqLoam)=="V1"] <- "Number_of_loans"


### Median of days past due

medians <- aggregate(credit_card_balance[,22:23], 
               list(credit_card_balance$SK_ID_CURR), mean)

colnames(medians)[colnames(medians)=="Group.1"] <- "SK_ID_CURR"
medians$PayLate <- ifelse(medians$SK_DPD_DEF>0, 1,0)

### Median Installment

installmentMedian <- aggregate(credit_card_balance[,20], 
                               list(credit_card_balance$SK_ID_CURR), mean)
colnames(installmentMedian )[colnames(installmentMedian)=="Group.1"] <- "SK_ID_CURR"
colnames(installmentMedian )[colnames(installmentMedian)=="x"] <- "MeanInstallments"

### Differenc mean Balance and Limit

credit_card_balance$differenc <-  credit_card_balance$AMT_CREDIT_LIMIT_ACTUAL - credit_card_balance$AMT_BALANCE

MeanOfDiff <- aggregate(credit_card_balance[,24], 
                        list(credit_card_balance$SK_ID_CURR), mean)

### Drawing mean

drawingMean <- aggregate(credit_card_balance[,7], 
                         list(credit_card_balance$SK_ID_CURR), mean)

colnames(drawingMean)[colnames(drawingMean)=="Group.1"] <- "SK_ID_CURR"
colnames(drawingMean)[colnames(drawingMean)=="x"] <- "DrawingMeans"

#### JOIN DATA FRAMES

data <- merge(x = freqLoam, y = medians  , by = "SK_ID_CURR", all.x = TRUE)
data <- merge(x = data, y = installmentMedian  , by = "SK_ID_CURR", all.x = TRUE)
data <- merge(x = data, y = MeanOfDiff  , by = "SK_ID_CURR", all.x = TRUE)
data <- merge(x = data, y = drawingMean  , by = "SK_ID_CURR", all.x = TRUE)

write.csv(data, "credit_card_balance2.csv")

###installing the neccesary packages
install.packages("data.table")
install.packages("ggplot2")
install.packages("ggmosaic")
install.packages("readr")

###loading packages
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)

###Point the filePath to where the datasets are and 
###assign the data files to data.tables
filePath <- "C:/Users/duban/Downloads/"
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv")) 
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))

###Exploratory data analysis
head(transactionData)
str(transactionData)

#### Convert DATE column to a date format
#### A quick search online tells us that CSV and Excel integer dates begin on 30 Dec 1899
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

#### Examine PROD_NAME
transactionData$PROD_NAME

#### Examine the words in PROD_NAME to see if there are any incorrect entries 
#### such as products that are not chips 
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')
head(productWords)

###Remove blank or empty entries from productWords
productWords <- productWords[words != ""]

###Remove words containing digits using grepl
productWords <- productWords[!grepl("\\d", words)]

###Remove words containing special characters like '&', '!', '@', etc.
productWords <- productWords[!grepl("[^a-zA-Z]", words)]

#Count the frequency of each word
wordFrequency <- productWords[, .N, by = words]

###Sort the words by frequency (highest to lowest)
sortedWordFrequency <- wordFrequency[order(-N)]


#### Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))] 
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

#### Summarise the data to check for possible outliers
summaryStatistics <- summary(transactionData)
summaryStatistics

### check for nulls
naCounts <- sapply(transactionData, function(x) sum(is.na(x)))
naCounts

### Filter the dataset to find the transactions where the product quantity is 200
outlierTransactions <- transactionData[PROD_QTY == 200]
outlierTransactions
###only 2 transactions made

### Check if the transactions were made by the same customer
uniqueCustomers <- unique(outlierTransactions$LYLTY_CARD_NBR)
uniqueCustomers
### only 1 customer made the transaction

#### Let's see if the customer has had other transactions
uniqueCustomerTransactions <- unique(transactionData[LYLTY_CARD_NBR == 226000])
uniqueCustomerTransactions
###Customer has no other transactions

#### Filter out the customer based on the loyalty card number
filteredTransactionData <- transactionData[LYLTY_CARD_NBR != 226000]
filteredTransactionData

#### Re-examine transaction data
NewsummaryStatistics <- summary(filteredTransactionData)
NewsummaryStatistics

#### Count the number of transactions by date
transactionsByDate <- transactionData[, .N, by = DATE]
nrow(transactionsByDate)
###364

#### Create a sequence of dates and join this the count of transactions by date
dateSequence <- data.table(DATE = seq.Date(from = as.Date("2018-07-01"), 
                                           to = as.Date("2019-06-30"), 
                                           by = "day"))
fullDateData <- merge(dateSequence, transactionsByDate, by = "DATE", all.x = TRUE)

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Plot transactions over time
ggplot(fullDateData, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Filter to December and look at individual days
DecemberData <- fullDateData[format(DATE, "%Y-%m") == "2018-12"]
ggplot(DecemberData, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
###zero sales on Christmas Day because the of the shops being closed

#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]


####checking if the pack sizes look sensible 
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]
### they are sensible!

####plot a histogram of PACK_SIZE.
ggplot(transactionData, aes(x = as.factor(PACK_SIZE))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Transactions by Pack Size",
       x = "Pack Size",
       y = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

###Create a column which contains the brand of the product,
###by extracting it from the product name.
transactionData[, BRAND := toupper(sapply(strsplit(PROD_NAME, " "), `[`, 1))]

###Display a sample of the dataset to check the new BRAND column
head(transactionData[, .(PROD_NAME, BRAND)], 40)

#### Checking brands
###the frequency of each brand to see if the extraction is reasonable
brandCounts <- transactionData[, .N, by = BRAND][order(-N)]
print(brandCounts)
###Some of the brand names look like they are of the same brands

#### Clean brand names
transactionData[BRAND == "RED", BRAND := "RRD"]

###brand names to clean
###INFUSIONS and INFZNS
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]

###SUNBITES and SNBTS
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]

###SMITHS and SMITH
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]

###DORITOS and DORITO
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]

###WW and WOOLWORTHS
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]

### Check the frequency of each brand again
brandCounts <- transactionData[, .N, by = BRAND][order(-N)]
print(brandCounts)
###it is now reasonable!


#### Examining customer data
summary(customerData)

### Check distribution of important key columns
###Count customers by LIFESTAGE
lifestageDistribution <- customerData[, .N, by = LIFESTAGE][order(-N)]
print(lifestageDistribution)

###Count customers by PREMIUM_CUSTOMER category
premiumCustomerDistribution <- customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]
print(premiumCustomerDistribution)

###Count customers by LIFESTAGE and PREMIUM_CUSTOMER combination
customerSegments <- customerData[, .N, by = .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-N)]
print(customerSegments)


#### Merge transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)

###Check for missing customer details
missingCustomers <- data[is.na(LIFESTAGE) | is.na(PREMIUM_CUSTOMER)]
print(missingCustomers)
### there are zero rows therefore no nulls

###save data for task 2
fwrite(data, paste0(filePath,"QVI_data.csv"))


## Data analysis on customer segments

#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER
salesSummary <- data[, .(Total_Sales = sum(TOT_SALES)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]
salesSummary <- salesSummary[order(-Total_Sales)]
print(salesSummary)
###older families in budget have the highest total sales


###Plot the total sales by LIFESTAGE and PREMIUM_CUSTOMER
ggplot(salesSummary, aes(x = LIFESTAGE, y = Total_Sales, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales by Lifestage and Premium Customer Category",
       x = "Lifestage",
       y = "Total Sales ($)",
       fill = "Premium Customer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customerCountSummary <- data[, .(Customer_Count = uniqueN(LYLTY_CARD_NBR)),
                             by = .(LIFESTAGE, PREMIUM_CUSTOMER)]
customerCountSummary <- customerCountSummary[order(-Customer_Count)]
print(customerCountSummary)

### create a plot of Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
ggplot(customerCountSummary, aes(x = LIFESTAGE, y = Customer_Count, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Customers by Lifestage and Premium Customer Category",
       x = "Lifestage",
       y = "Number of Customers",
       fill = "Premium Customer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
unitsSummary <- data[, .(Total_Units = sum(PROD_QTY), 
                         Customer_Count = uniqueN(LYLTY_CARD_NBR)), 
                     by = .(LIFESTAGE, PREMIUM_CUSTOMER)]
unitsSummary[, Avg_Units_Per_Customer := Total_Units / Customer_Count]
print(unitsSummary)

###Plot the average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
ggplot(unitsSummary, aes(x = LIFESTAGE, y = Avg_Units_Per_Customer, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Number of Units per Customer by Lifestage and Premium Customer Category",
       x = "Lifestage",
       y = "Average Units per Customer",
       fill = "Premium Customer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
priceSummary <- data[, .(Total_Sales = sum(TOT_SALES), 
                         Total_Units = sum(PROD_QTY)), 
                     by = .(LIFESTAGE, PREMIUM_CUSTOMER)]
priceSummary[, Avg_Price_Per_Unit := Total_Sales / Total_Units]
print(priceSummary)

###Plot the average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
ggplot(priceSummary, aes(x = LIFESTAGE, y = Avg_Price_Per_Unit, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price per Unit by Lifestage and Premium Customer Category",
       x = "Lifestage",
       y = "Average Price per Unit ($)",
       fill = "Premium Customer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
### Mainstream midage and young singles and couples are more 
###willing to pay more per packet of chips compared to their budget and premium counterparts

#### Perform an independent t-test between mainstream vs premium and 
### budget midage and young singles and couples
relevantData <- data[LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES")]
mainstreamData <- relevantData[PREMIUM_CUSTOMER == "Mainstream"]
premiumBudgetData <- relevantData[PREMIUM_CUSTOMER %in% c("Premium", "Budget")]
tTestResult <- t.test(mainstreamData$TOT_SALES / mainstreamData$PROD_QTY, 
                      premiumBudgetData$TOT_SALES / premiumBudgetData$PROD_QTY)
print(tTestResult)

### The t-test results in a p-value of 2.2e-16, i.e. the unit price
###for mainstream, young and mid-age singles and couples ARE
###significantly higher than that of budget or premium, young and midage singles 
###and couples.

###Work out of there are brands that these two customer segments 
###prefer more than others. You could use a technique called affinity 
###analysis or a-priori analysis (or any other method if you prefer)

###Filter data for Mainstream, Young Singles/Couples
targetGroup <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"]

### brand frequency calculation in the target group
brandFreqTarget <- targetGroup[, .N, by = .(BRAND)]
setnames(brandFreqTarget, "N", "TargetGroup_Frequency")

###brand frequency calculation in the rest of the data
otherGroup <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream")]
brandFreqOthers <- otherGroup[, .N, by = .(BRAND)]
setnames(brandFreqOthers, "N", "OtherGroup_Frequency")

###Merging brand frequency data to compare
brandComparison <- merge(brandFreqTarget, brandFreqOthers, by = "BRAND", all = TRUE)
brandComparison[is.na(brandComparison)] <- 0 

###Affinity Score calculation 
brandComparison[, Affinity_Score := TargetGroup_Frequency / (OtherGroup_Frequency + 1)]

###Sort by Affinity Score to find preferred brands
sortedBrands <- brandComparison[order(-Affinity_Score)]
print(sortedBrands)

###Visualize the top brands preferred by the target group
topBrands <- head(sortedBrands, 20) 
ggplot(topBrands, aes(x = reorder(BRAND, -Affinity_Score), y = Affinity_Score)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 20 Brands Preferred by Mainstream, Young Singles/Couples",
       x = "Brand",
       y = "Affinity Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###INSIGHTS
#we see that the mainstream young singles/couples have high preference for
#Tyrrells,Twisties, Doritos brands and have very low preference burger,woolworths
#and sunbites brands.reasons could be because of the perceived quality and 
#social influence,brand positioning and image etc

#### Preferred pack size compared to the rest of the population
###Filtering data for the target segment
targetSegment <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"]
otherSegments <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream")]

###Summarizing pack size preferences for target and other segments
packSizeTarget <- targetSegment[, .N, by = .(PACK_SIZE)]
packSizeOther <- otherSegments[, .N, by = .(PACK_SIZE)]

### Renaming columns for clarity
setnames(packSizeTarget, "N", "TargetGroup_Frequency")
setnames(packSizeOther, "N", "OtherGroup_Frequency")

###Merge to compare distributions
packSizeComparison <- merge(packSizeTarget, packSizeOther, by = "PACK_SIZE", all = TRUE)
packSizeComparison[is.na(packSizeComparison)] <- 0  

### Calculating percentage preference within each group
packSizeComparison[, `:=`(
  Target_Percentage = TargetGroup_Frequency / sum(TargetGroup_Frequency) * 100,
  Other_Percentage = OtherGroup_Frequency / sum(OtherGroup_Frequency) * 100
)]

###plot pack size preferences
ggplot(packSizeComparison, aes(x = as.factor(PACK_SIZE))) +
  geom_bar(aes(y = Target_Percentage, fill = "Target Group"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Other_Percentage, fill = "Other Groups"), stat = "identity", position = "dodge") +
  labs(title = "Pack Size Preference: Mainstream Young Singles/Couples vs Others",
       x = "Pack Size (grams)",
       y = "Percentage of Purchases",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### INSIGHTS
#Mainstream Young Singles/Couples seem to prefer both larger and smaller packs,
#larger packs might be because they purchase for social gatherings or want 
#better value for money, whilst for smaller packs it could indicate more impulse
#buys or individual snacking rather than sharing.


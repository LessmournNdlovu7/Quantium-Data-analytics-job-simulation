###Load necessary libraries

library(data.table)
library(ggplot2)
library(tidyr)

### Point the filePath to where the datasets are and 
### assign the data files to data.tables

filePath <- "C:/Users/duban/Downloads/"
transactionData <- fread(paste0(filePath,"QVI_data(1).csv"))
data <- transactionData
#### Setting themes for plots

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Add a new month ID column in the data with the format yyyymm
data[, YEARMONTH := format(as.Date(DATE), "%Y%m")]

#For each store and month calculate total sales, number of 
#customers, transactions per customer, chips per customer and the average price per unit.

measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),                     
  nCustomers = uniqueN(LYLTY_CARD_NBR),       
  nTxnPerCust = .N / uniqueN(LYLTY_CARD_NBR), 
  nChipsPerTxn = mean(PROD_QTY),              
  avgPricePerUnit = mean(TOT_SALES / PROD_QTY)    
), by = .(STORE_NBR, YEARMONTH)][order(STORE_NBR, YEARMONTH)]

#### Filter to the pre-trial period and stores with full observation 
### periods 

storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR]) 
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]

### Create a function to calculate correlation for a measure,
### looping through each control store.
### Let's define inputTable as a metric table with potential 
### comparison stores, metricCol as the store metric used to calculate 
### correlation on, and storeComparison as the store number of the trial store.

calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  
  calcCorrTable <- data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  storeNumbers <- unique(inputTable[STORE_NBR != storeComparison, STORE_NBR])
  trialStoreData <- inputTable[STORE_NBR == storeComparison, .(eval(metricCol))]
  
  for (controlStore in storeNumbers) {
    controlStoreData <- inputTable[STORE_NBR == controlStore, .(eval(metricCol))]
    
    if (nrow(controlStoreData) == nrow(trialStoreData)) {
      corr_measure <- cor(trialStoreData[[1]], controlStoreData[[1]], use = "complete.obs")
      
      calculatedMeasure <- data.table(
        Store1 = storeComparison,
        Store2 = controlStore,
        corr_measure = corr_measure
      )
      
      calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
    }
  }
  
  setorder(calcCorrTable, -corr_measure)
  return(calcCorrTable)
}


#### function to calculate a standardised magnitude distance for a measure, 
#### looping through each control store 

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) { 
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), 
                             YEARMONTH = numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH],
                                   "measure" = abs(inputTable[STORE_NBR == storeComparison, 
                                                              eval(metricCol)]
                                                   - inputTable[STORE_NBR == i, eval(metricCol)])
  )
  calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  #### Standardise the magnitude distance so that the measure ranges from 0 to 1 
  
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), 
                              by = c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))   
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = 
                                .(Store1, Store2)]
  return(finalDistTable)
}

### calculate correlations against store 77 using total sales and number of customers.

trial_store <- 77 # Define the trial store number

corr_nSales <- calculateCorrelation(preTrialMeasures, preTrialMeasures$totSales, trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, preTrialMeasures$nCustomers, trial_store)
print(corr_nSales)
print(corr_nCustomers)

### total sales and number of customers of control stores closely follows the same trend as the trial store
### when using the correlation function



### Use the magnitude distance function for total sales and number of customers

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)
print(magnitude_nSales)
print(magnitude_nCustomers)

########

###  merging the correlations table with the magnitude table
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))[
  , scoreNSales := corr_weight * corr_measure + (1 - corr_weight) * mag_measure
]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))[
  , scoreNCust := corr_weight * corr_measure + (1 - corr_weight) * mag_measure
]
print(score_nSales)
print(score_nCustomers)


### Combine scores across the drivers by first merging our sales scores and 
### customer scores into a single table

score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]
print(score_Control)


### Select the most appropriate control store for trial store 77

control_store <- score_Control[Store1 == trial_store][order(-finalControlScore)][1, Store2]

score_Control[order(-finalControlScore)][1, Store2]


# store 233 is the most appropriate!

#### Visual checks on trends based on the drivers

measureOverTimeSales <- measureOverTime
measureOverTimeSales[, YEARMONTH := as.numeric(YEARMONTH)]
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, 
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

pastSales[Store_type %in% c("Control", "Trial"), c("TransactionMonth", 
                                                   "STORE_NBR", "totSales", "Store_type")]


ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

############

#### visual checks on customer count trends by comparing the trial store to the control 
### store and other stores.

measureOverTimeCusts <- measureOverTime 
pastCustomers <- measureOverTimeCusts[, 
                                      Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                           ifelse(STORE_NBR == control_store, 
                                                                  "Control", "Other stores"))
][, nCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type") 
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 
                                      1, sep = "-"), "%Y-%m-%d")  
][YEARMONTH < 201903, ] 

ggplot(pastCustomers, aes(TransactionMonth, nCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of Customers", title = "Customer Count Trends by Month")

#########

#### Scale pre-trial control sales to match pre-trial trial store sales

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store 
& YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store & 
YEARMONTH < 201902, sum(totSales)]
print(scalingFactorForControlSales)

# scalingFactorForControlSales = 1.023617

#### Apply the scaling factor

measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ , 
                      controlSales := totSales * scalingFactorForControlSales]
scaledControlSales

### Calculate the percentage difference between scaled control sales and trial sales

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")], 
                        measureOverTime[STORE_NBR == trial_store77, c("YEARMONTH", "totSales")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales)/controlSales]


#YEARMONTH trialSales scaledControlSales percentageDiff
#<num>      <num>              <num>          <num>
#  1:    201902      235.0           571.9772      -58.91445
#2:    201903      278.5           628.7375      -55.70488
#3:    201904      263.5           604.4758      -56.40851

# the trial store underperformed relative to what we would expect
# based on the control store's scaled sales.

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

#### Note that there are 8 months in the pre-trial period 
#### hence 8 - 1 = 7 degrees of freedom
degreesOfFreedom <- 7 

### Calculate the t-values for the trial months.

percentageDiff[, tValue := (percentageDiff - 0)/stdDev
][,TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                     YEARMONTH %% 100, 1, 
                                     sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

# TransactionMonth   tValue
# <Date>    <num>
#  1:       2019-02-01 11.38305
# 2:       2019-03-01 10.78682
# 3:       2019-04-01 10.91753


### calculate 95th percentile of the t distribution

qt(0.95, df = degreesOfFreedom) 
# 1.894579

# We can observe that the t-value is much larger than the 
# 95th percentile value of the t-distribution for March and April
# - i.e. the increase in sales in the trial store in March and April is
# statistically greater than in the control store.

### Compute a scaling factor to align control store customer counts to our trial store.
#### Then, apply the scaling factor to control store customer counts.
#### Finally, calculate the percentage difference between scaled control store customers 
### and trial customers

scalingFactorForControlCust <- preTrialMeasures[
  STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)
] / preTrialMeasures[
  STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)
]

measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[
  STORE_NBR == control_store
][, controlCustomers := nCustomers * scalingFactorForControlCust
][, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))]

percentageDiffCust <- merge(
  scaledControlCustomers[, .(YEARMONTH, controlCustomers)],
  measureOverTimeCusts[STORE_NBR == trial_store, .(nCustomers, YEARMONTH)],
  by = "YEARMONTH"
)[, percentageDiff := abs(controlCustomers - nCustomers) / controlCustomers]

### Let's again see if the difference is significant visually
### As our null hypothesis is that the trial period is the same as the 
### pre-trial period, let's take the standard deviation based on the scaled
###percentage difference in the pre-trial period 

stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])  

degreesOfFreedom <- 7 

#### Trial and control store number of customers

pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = 
                                        c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile

pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
                                          ][,nCusts := nCusts * (1 + stdDev * 2)
                                          ][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile

pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
                                         ][, nCusts := nCusts * (1 - stdDev * 2)
                                           ][, Store_type := "Control 5th % confidence interval"]
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, 
                         pastCustomers_Controls5)

### Plot everything into one nice graph

ggplot(trialAssessment, aes(x = TransactionMonth, y = nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
                ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() + labs(x = "Month of operation", y = "Total number of customers", 
                     title = "Total no. of customers per month")

###############


## Trial store 86

### Calculate the metrics below as we did for the first trial store

measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)
), by = c("STORE_NBR", "YEARMONTH")
][order(STORE_NBR, YEARMONTH)]

trial_store <- 86

corr_nSales86 <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store) 
corr_nCustomers86 <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

magnitude_nSales86 <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomer86 <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

#### create a combined score composed of correlation and magnitude

corr_weight <- 0.5
score_nSales86 <- merge(corr_nSales86, 
                        magnitude_nCustomer86, 
                        by = c("Store1", "Store2"))[, scoreNSales := (corr_measure * corr_weight) + 
                                                      (mag_measure * (1-corr_weight))]

score_nCustomer86 <- merge(corr_nCustomers86, magnitude_nCustomer86, 
                           by = c("Store1", "Store2"))[, scoreNCust := (corr_measure * corr_weight) +
                                                         (mag_measure * (1-corr_weight))]

#### Combine scores across the drivers using a simple average
score_Control86 <- merge(score_nSales86, 
                         score_nCustomer86, 
                         by = c("Store1", "Store2"))[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store
#### (closest to 1 but not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 86

control_store86 <- score_Control86[Store1 == trial_store86, ][order(-finalControlScore)][1, Store2]
control_store86

## control score will be 155

#### Conduct visual checks on trends based on the drivers

measureOverTimeSales <- measureOverTime
measureOverTimeSales[, YEARMONTH := as.numeric(YEARMONTH)]
pastSales86 <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                           ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
][, totSales := mean(totSales), 
  by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
                                "%Y-%m-%d")][YEARMONTH < 201903, ]

#### Plot
ggplot(pastSales86, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month", y = "Total sales", 
       title = "Total sales by month for stores pre-trial")


########


### number of customers.
#### Conduct visual checks on trends based on the drivers

measureOverTimeCusts <- measureOverTime
pastCustomers86 <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                               ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), 
                                "%Y-%m-%d")][YEARMONTH < 201903, ]

#### Plot
ggplot(pastCustomers86, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month", y = "Number of customers", title = "Average number of customers by month for the stores")

#####################

### assess the impact of the trial on sales.
#### Scale pre-trial control sales to match pre-trial trial store sales

scalingFactorForControlSales_86 <- preTrialMeasures[STORE_NBR == trial_store & 
                                                     YEARMONTH < 201902, 
                                                   sum(totSales) ]/preTrialMeasures[
                                                     STORE_NBR == control_store86 & 
                                                       YEARMONTH < 201902, sum(totSales)]


#### Apply the scaling factor

measureOverTimeSales <- measureOverTime
scaledControlSales86 <- measureOverTimeSales[STORE_NBR == control_store86, 
][, controlSales := totSales * scalingFactorForControlSales_86]

### Calculate the percentage difference between scaled control sales and trial sales

percentageDiff86 <- merge(scaledControlSales86[, c("YEARMONTH", "controlSales")],
                          measureOverTime[STORE_NBR == trial_store, c("YEARMONTH", "totSales")],
                          by = "YEARMONTH"
)[, percentageDiff := abs(controlSales - totSales)/controlSales]
print(percentageDiff86)

#Key: <YEARMONTH>
 # YEARMONTH controlSales totSales percentageDiff
#<num>        <num>    <num>          <num>
 # 1:    201807     896.9222   892.20    0.005264934
#2:    201808     759.2700   764.05    0.006295532
#3:    201809     984.0341   914.60    0.070560652
#4:    201810     934.9488   948.40    0.014387109
#5:    201811     871.8946   918.00    0.052879611
#6:    201812     824.3614   841.20    0.020426281
#7:    201901     848.4190   841.40    0.008273010
#8:    201902     864.5221   913.20    0.056306186
#9:    201903     780.3204  1026.80    0.315869729
#10:    201904     819.3170   848.20    0.035252503
#11:    201905     895.2246   889.30    0.006618028
#12:    201906     831.5398   838.00    0.007768906

### Calculate the standard deviation of percentage differences during the pre-trial period

stdDevSales_86 <- sd(percentageDiff86[, percentageDiff])

degreesOfFreedom <- 7


### Create a table with sales by store type and month

measureOverTimeSales <- measureOverTime
pastSales86 <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", 
                                                           ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

### Control store 95th percentile
pastSales86_Controls95 <- pastSales86[Store_type == "Control",
][, totSales := totSales * (1 + stdDevSales_86 * 2)
][, Store_type := "Control 95th % confidence interval"]


### Control store 5th percentile
pastSales86_Controls5 <- pastSales86[Store_type == "Control",
][, totSales := totSales * (1 - stdDevSales_86 * 2)
][, Store_type := "Control 5th % confidence interval"]

### Then, create a combined table with columns from pastSales
trialAssessmentSales86 <- rbind(pastSales86, pastSales86_Controls5, pastSales86_Controls95)


### Plot these in one nice graph

ggplot(trialAssessmentSales86, aes(TransactionMonth, totSales, color = Store_type)) +
  annotate("rect", xmin = as.Date("2019-02-01"), xmax = as.Date("2019-04-30"), 
           ymin = 0, ymax = Inf, fill = "blue", alpha = 0.2) +

  geom_line(aes(linetype = Store_type)) +
  # Labels
  labs(x = "Month", y = "Average total sales", title = "Average Total Sales Per Month")

#############

### The results show that the trial in store 86 is not significantly different to 
### its control store in the trial period as the trial store performance lies 
### inside the 5% to 95% confidence interval of the control store in two of the 
### three trial months.

#### Scale pre-trial control customers to match pre-trial trial store customers

scalingFactorForControlCust_86 <- preTrialMeasures[STORE_NBR == trial_store & 
                                                    YEARMONTH < 201902, sum(nCustomers)
]/preTrialMeasures[STORE_NBR == control_store86 & YEARMONTH < 201902, 
                  sum(nCustomers)]

#### Apply scaling factor to the control customer numbers

measureOverTimeCusts <- measureOverTime
scaledControlCustomers86 <- measureOverTimeCusts[STORE_NBR == control_store86, 
][, controlCustomers := nCustomers * scalingFactorForControlCust_86
][, Store_type := ifelse(Store_type == trial_store, "Trial", 
                         ifelse(Store_type == control_store86, 
                                "Control", "Other stores"))]

#### Calculate the percentage difference between scaled control sales and trial sales

percentageDiffCust86 <- merge(scaledControlCustomers86[, c("YEARMONTH", "controlCustomers")],
                              measureOverTime[STORE_NBR == trial_store, c("YEARMONTH", "nCustomers")],
                              by = "YEARMONTH"
)[, percentageDiff := abs(controlCustomers - nCustomers) / controlCustomers]

### As our null hypothesis is that the trial period is the same as the pre-trial
### period, let's take the standard deviation based on the scaled percentage difference
### in the pre-trial period 

stdDevCust_86 <- sd(percentageDiffCust86[YEARMONTH < 201902, percentageDiff])

degreesOfFreedom <- 7

#### Trial and control store number of customers

pastCustomers86 <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]


#### Control 95th percentile

pastCustomers86_Controls95 <- pastCustomers86[Store_type == "Control",
][, nCusts := nCusts * (1 + (stdDevCust_86 * 2))
][, Store_type := "Control 95th % confidence interval"]

#### Control 5th percentile 

pastCustomers86_Controls5 <- pastCustomers86[Store_type == "Control",
][, nCusts := nCusts * (1 - (stdDevCust_86 * 2))
][, Store_type := "Control 5th % confidence interval"]

#### Row bind 
trialAssessmentCust86 <- rbind(pastCustomers86, pastCustomers86_Controls5, pastCustomers86_Controls95)

#### Plotting these in one nice graph

ggplot(trialAssessmentCust86, aes(TransactionMonth, nCusts, color = Store_type)) + 
  geom_rect(data = trialAssessmentCust86[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, fill = "blue"), show.legend = FALSE) +
  geom_line() + 
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")

####################

### It looks like the number of customers is significantly higher in all of the three
### months. This seems to suggest that the trial had a significant impact on increasing
### the number of customers in trial store 86 but as we saw, sales were not significantly
### higher. We should check with the Category Manager if there were special deals in 
### the trial store that were may have resulted in lower prices, impacting the results.


## Trial store 88

measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)),
                        by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Use the functions to calculate correlation
trial_store <- 88

corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

#### Use the functions to calculate magnitude

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)


#### Create a combined score composed of correlation and magnitude

corr_weight <- 0.5

score_NSales88 <- merge(corr_nSales, magnitude_nSales, 
                        by = c("Store1", "Store2")
)[, scoreNSales := (corr_measure*corr_weight) +
    (mag_measure * (1 - corr_weight))]


score_NCustomers88 <- merge(corr_nCustomers, magnitude_nCustomers, 
                            by = c("Store1", "Store2")
)[, scoreNCust :=(corr_measure * corr_weight) +
    (mag_measure * (1 - corr_weight))]


#### Combine scores across the drivers by merging sales scores and 
### customer scores, and compute a final combined score

score_Control88 <- merge(score_NSales88, score_NCustomers88, by = c("Store1", "Store2"))
score_Control88[, finalControlScore := (scoreNSales * 0.5) + (scoreNCust * 0.5)]

#### Select control stores based on the highest matching store (closest to 1 but not exactly 1)
#### Select control store for trial store 88

control_store88 <- score_Control88[order(-finalControlScore)][1, Store2]

control_store88

# control store = 237

### check visually if the drivers are indeed similar in the period before the trial.
### look at total sales first.

#### Visual checks on trends based on the drivers


measureOverTimeSales <- measureOverTime
measureOverTimeSales[, YEARMONTH := as.numeric(YEARMONTH)]
pastSales88 <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == 88, "Trial",
                                                           ifelse(STORE_NBR == control_store88, "Control","Other Stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100,YEARMONTH %% 100, 1, 
                                      sep = '-'), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastSales88, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Transaction Month", y = "Total sales per month", 
       title = "Total sales per month for the stores")

################


### for number of customers.
#### Visual checks on trends based on the drivers

measureOverTimeCusts <- measureOverTime

pastCustomers88 <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == 88, "Trial",
                                                               ifelse(STORE_NBR == control_store88, "Control", "Other Stores"))
][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TrsancationMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903]

ggplot(pastCustomers88, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Transaction Month", y = "Total customers per month", 
       title = "Total customers per month for the Stores ")


###########

### the total number of customers of the control 88 and trial 237 are also similar.

### assess the impact of the trial on sales.
### Scale pre-trial control store sales to match pre-trial trial store sales


scalingFactorForControlSales_88 <- preTrialMeasures[STORE_NBR == 88 & 
                                                     YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[
                                                       STORE_NBR == control_store88 & YEARMONTH < 201902, 
                                                       sum(totSales)]

### Apply the scaling factor

measureOverTimeSales <- measureOverTime

scaledControlSales88 <- measureOverTimeSales[STORE_NBR == control_store88, 
][, controlSales := totSales * scalingFactorForControlSales_88]

### Calculate the absolute percentage difference between scaled control 
### sales and trial sales

percentageDiff88 <- merge(scaledControlSales88[, c("YEARMONTH", "controlSales")], 
                          measureOverTime[STORE_NBR == 88, c("YEARMONTH", "totSales")], by = "YEARMONTH")

percentageDiff88[, percentageDiff := abs(controlSales - totSales)/controlSales]

### As our null hypothesis is that the trial period is the same as the pre-trial 
### period, let's take the standard deviation based on the scaled percentage 
### difference in the pre-trial period 

stdDevSales_88 <- sd(percentageDiff88[YEARMONTH < 201902, percentageDiff])

degreesOfFreedom <- 7

#### Trial and control store total sales

measureOverTimeSales <- measureOverTime

pastSales88 <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == 88, "Trial",
                                                           ifelse(STORE_NBR == control_store88, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, 
                                      YEARMONTH %% 100, 1, sep = "-"),
                                "%Y-%m-%d")
][Store_type %in% c("Trial", "Control")]


#### Control store 95th percentile

pastSales88_Controls95 <- pastSales88[Store_type == "Control",
][, totSales := totSales * (1 + stdDevSales_88*2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile

pastSales88_Controls5 <- pastSales88[Store_type == "Control",
][, totSales := totSales * (1 - stdDevSales_88*2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment88 <- rbind(pastSales88, pastSales88_Controls95, pastSales88_Controls5)

#### Plot these in one nice graph

ggplot(trialAssessment88, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment88[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, fill = "blue"), show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#############


### The results show that the trial in store 88 is significantly different 
### to its control store in the trial period as the trial store performance 
### lies outside of the 5% to 95% confidence interval of the control store 
### in two of the three trial months.


### assessing this for number of customers as well.
### This would be a repeat of the steps before for total sales
### Scale pre-trial control store customers to match pre-trial trial store customers 

ScalingFactorForControlCust_88 <- preTrialMeasures[STORE_NBR == 88 & 
                                                    YEARMONTH <  201902, sum(nCustomers)]/preTrialMeasures[
                                                      STORE_NBR == control_store88 & YEARMONTH < 201902,
                                                      sum(nCustomers)
                                                    ]

### Apply the scaling factor

measureOverTimeCusts <- measureOverTime
scaledControlCustomers88 <- measureOverTimeCusts[STORE_NBR == control_store88, 
][, controlCustomers := nCustomers * ScalingFactorForControlCust_88
][, Store_type := ifelse(STORE_NBR == 88, "Trial",
                         ifelse(STORE_NBR == control_store88, "Control", "Other stores"))]


### Calculate the percentage difference between scaled control sales, and trial sales

percentageDiffCust88 <- merge(scaledControlCustomers88[, c("YEARMONTH", "controlCustomers")],
                              measureOverTime[STORE_NBR == 88, c("YEARMONTH", "nCustomers")],
                              by = "YEARMONTH")[, percentageDiff := 
                                                  abs(controlCustomers - nCustomers) / 
                                                  controlCustomers]


### As our null hypothesis is that the trial period is the same as the 
### pre-trial period, let's take the standard deviation based on the scaled 
### percentage difference in the pre-trial period

stdDevCust_88 <- sd(percentageDiffCust88[YEARMONTH < 201902, percentageDiff])
degreesOfFreedom <- 7

#### Trial and control store number of customers

pastCustomers88 <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]


#### Control store 95th percentile

pastCustomers88_Controls95 <- pastCustomers88[Store_type == "Control", 
][, nCusts := nCusts * (1 + stdDevCust_88 * 2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile

pastCustomers88_Controls5 <- pastCustomers88[Store_type == "Control", 
][, nCusts := nCusts * (1 - stdDevCust_88 * 2)
][, Store_type := "Control 5th % confidence interval"]


trialAssessmentCust88 <- rbind(pastCustomers88, pastCustomers88_Controls5, pastCustomers88_Controls95)

#### Plot them in a nice graph

ggplot(trialAssessmentCust88, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessmentCust88[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
                ymin = 0, ymax = Inf, fill = "blue"), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total 
       number of customers by month of the stores")

##############


### Total number of customers in the trial period for the trial store is 
### significantly higher than the control store for two out of three months, 
### which indicates a positive trial effect.


## Conclusion

# The results for trial stores 77 and 88 during the trial period show a significant
# difference in at least two of the three trial months but this is not the case for 
# trial store 86. We can check with the client if the implementation of the trial was 
# different in trial store 86 but overall, the trial shows a significant increase in sales.
# Now that we have finished our analysis, we can prepare our presentation to the Category 
# Manager.
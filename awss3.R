#Clears environment
rm(list = ls(all.names = TRUE))
#-------------------------------------------------------------------------------
#-----------------------------------LIBRARIES-----------------------------------
#-------------------------------------------------------------------------------
library(aws.s3)
library(rjson)
library(jsonlite)
library(dplyr)
library(lubridate)

#-------------------------------------------------------------------------------
#-----------------------------ENVIRONMENT VARIABLES-----------------------------
#-------------------------------------------------------------------------------
Sys.setenv(
  AWS_ACCESS_KEY_ID = "",
  AWS_SECRET_ACCESS_KEY = "",
  AWS_SESSION_TOKEN = "",
  AWS_DEFAULT_REGION = ""
)

#THIS IS DOUBLED BECAUSE OF OWNER FIELD
df2 <- data.table::rbindlist(get_bucket("", prefix="", max = Inf))
df2 <- df2[as.Date(df2$LastModified) >= as.Date("2022-01-01"),]

#isolate file names
files = subset(df2
             , select = c("Key"))

uniqueFiles = unique(files)

uniqueSample = uniqueFiles[ c(1, 2), ]

Moderntreasury <- NULL
for (row in 1:nrow(uniqueFiles)) {
  print(paste('Searching file ', row, ' out of ', nrow(uniqueFiles), sep=""))
  url <- paste('s3://', uniqueFiles[row, "Key"], sep= "") 
  s3Vector <- get_object(url, max = Inf)
  s3Value <- rawToChar(s3Vector)
  s3JSON <- fromJSON(s3Value)
  s3Clean = subset(s3JSON
                 , select = c("transactionId"
                              , "transactionDatetime"
                              , "transactionSource"
                              , "transactionType"
                              , "fee"
                              , "feeCurrency"
                              , "functionalCurrencyValue"
                              , "receivedCurrency"
                              , "receivedQuantity"
                              , "sentCurrency"
                              , "sentQuantity"
                              , "blockchainHash"
                              , "tradeId"
                              , "tradingBook"
                              , "childTransaction"
                              , "counterparty"
                              , "sourceName"
                              , "destinationName"
                              , "note"
                              , "sourceTransactionType"
                              , "legalEntity"
                              , "paymentType"
                              , "interestRate"
                              , "maturityDate"
                              , "walletSource"
                              , "walletDestination"
                              , "spotPrice"
                              , "notionalAmount"))
  #if(any(s3Clean['transactionId']=='')){
   # print("TRANSACTION FOUND")
    #foundResult <- s3Clean[s3Clean$transactionId=='',]
    #break
  #}

  Moderntreasury <- rbind(Moderntreasury, s3Clean)
  #s3Data <- c(s3Data, s3Value)
}

#Reads in raw data file
wd <- ""

#Set output file here
outputFile <- paste(wd, "O.csv", sep='')

write.csv(Moderntreasury, outputFile, row.names = F)

#testTrans <- fromJSON(s3Data)

#rawFile <- get_object("s3://.json", max = Inf)
#charFile <- rawToChar(rawFile)

#transactions <- fromJSON(charFile)

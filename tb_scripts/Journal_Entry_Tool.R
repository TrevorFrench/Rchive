# tool for creating journal entries 
#Clears environment
rm(list = ls(all.names = TRUE))
#-------------------------------------------------------------------------------
#-----------------------------------LIBRARIES-----------------------------------
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(writexl)
library(tidyverse)
library(lubridate)
library(data.table)
#-------------------------------------------------------------------------------
#---------------------------------DATA SOURCES----------------------------------
#-------------------------------------------------------------------------------

#Reads in raw data file
wd <- "C:/Users/Trevor French/Desktop/R Scripts/Files/"
dataSource <- paste(wd, "Input/journalEntries.xlsx", sep='')
rawData <- read_excel(dataSource)

#-------------------------------------------------------------------------------
#----------------------------------DATA FRAMES----------------------------------
#-------------------------------------------------------------------------------
df = subset(rawData
            , select = c("JE ID"
                         , "Date & Time"
                         , "Account"
                         , "Notional Debit"
                         , "Notional Credit"
                         , "Inventory Asset"
                         , "Inventory In"
                         , "Inventory Out"
                         , "Transaction Type"))

#Keep only Trades
df2 <- filter(df, df$`Transaction Type` == "TRADE")

#Create Month/Day/Year columns
df2 <- df2 %>%
  mutate("Year" = year(df2$`Date & Time`))

df2 <- df2 %>%
  mutate("Month" = month(df2$`Date & Time`))

df2 <- df2 %>%
  mutate("Day" = day(df2$`Date & Time`))

#Fill in missing dates/JE IDs
for (row in 1:nrow(df2)) {
  prevYear <- df2[row - 1, "Year"]
  prevMonth <- df2[row - 1, "Month"]
  prevDay <- df2[row - 1, "Day"]
  
  thisYear <- df2[row, "Year"]
  thisMonth <- df2[row, "Month"]
  thisDay <- df2[row, "Day"]
  
  
  JEID <- df2[row, "JE ID"]
  prevJEID <- df2[row - 1, "JE ID"]
  
  if(is.na(JEID)){
    df2[row, "JE ID"] <- prevJEID
  }
  
  if(is.na(thisYear)){
    df2[row, "Year"] <- prevYear
    df2[row, "Month"] <- prevMonth
    df2[row, "Day"] <- prevDay
  }
}


df2 <- filter(df2, df2$`Account` != "5101 - Digital Currencies" & is.na(df2$`Inventory In`))
#Add New Columns
df3 <- df2 %>%
  mutate("BegBal" = 0)

df3 <- df2 %>%
  mutate("Number" = 1)

df3 <- df3 %>%
  mutate("Cost Basis" = 0)

df3 <- df3 %>%
  mutate("G/L" = 0)

df3 <- df3 %>%
  mutate("Quantity" = 0)

df3 <- df3 %>%
  mutate("Asset" = "")

df3[1, "BegBal"] <- 1

for (row in 1:nrow(df3)) {
  Account <- df3[row, "Account"]
  invOut <- df3[row, "Inventory Out"]
  asset <- df3[row, "Inventory Asset"]
  nCredit <- df3[row, "Notional Credit"]
  nDebit <- df3[row, "Notional Debit"]
  debCred <- if_else(is.na(nCredit$`Notional Credit`), nDebit$`Notional Debit` * -1, nCredit$`Notional Credit`)
  #if_else(is.na(nDebit$`Notional Debit`), debCred <- nCredit$`Notional Credit`, debCred <- nDebit$`Notional Debit` * -1)
  
  if(Account == "1150 - Reserve Assets") {
    df3[row, "Quantity"] <- invOut
    df3[row, "Asset"] <- asset
    df3[row, "Cost Basis"] <- nCredit
  }
  
  if(Account == "5875.1 - Digital Currencies") {
    df3[row, "G/L"] <- debCred
  }
  
}

dfCount <- df3 %>% 
  group_by(JEID = df3$`JE ID`) %>%
  summarize("Count" = sum(`Number`))

#MINIMUMS
dfNA <- filter(df3, is.na(df3$`Inventory Asset`))

#dfNA <- aggregate(dfNA$`G/L` ~ dfNA$`JE ID`, df3, function(x) min(abs(x)))
DTNA <- data.table(dfNA)
dfNA <- DTNA[ , .SD[which.min(`G/L`)], by = `JE ID`]

dfAsset <- filter(df3, !is.na(df3$`Inventory Asset`))

#dfAsset <- aggregate(dfAsset$`Cost Basis` ~ dfAsset$`JE ID`, df3, function(x) min(abs(x)))
DTAsset <- data.table(dfAsset)
dfAsset <- DTAsset[ , .SD[which.min(`Cost Basis`)], by = `JE ID`]

#MAXIMUM
dfNAMax <- filter(df3, is.na(df3$`Inventory Asset`))

DTNAMax <- data.table(dfNAMax)
dfNAMax <- DTNAMax[ , .SD[which.max(`G/L`)], by = `JE ID`]

dfAssMax <- filter(df3, !is.na(df3$`Inventory Asset`))

DTAssMax <- data.table(dfAssMax)
dfAssMax <- DTAssMax[ , .SD[which.max(`Cost Basis`)], by = `JE ID`]

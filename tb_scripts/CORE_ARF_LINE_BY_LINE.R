#Clears environment
rm(list = ls(all.names = TRUE))
#-------------------------------------------------------------------------------
#-----------------------------------LIBRARIES-----------------------------------
#-------------------------------------------------------------------------------
library("remotes")   #In case you have not installed it.
remotes::install_github("TrevorFrench/trevoR")
library(trevoR)
library(dplyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(lubridate)
library(reshape2)
library(openxlsx)

#-------------------------------------------------------------------------------
#---------------------------------DATA SOURCES----------------------------------
#-------------------------------------------------------------------------------

#Sets working directory
wd <- "C:/Users/Trevor French/Downloads/Exchange and CORE"

df <- combineFiles(wd)

data <- "C:/Users/Trevor French/Downloads/combined_data_Wyre.csv"
df <- read.csv(data)

rawData <- df %>%
  mutate("Date and Time" = df$transaction_date_time)

rawData <- rawData %>%
  mutate("Sent Currency" = df$sent_currency)

rawData <- rawData %>%
  mutate("Sent Quantity" = df$sent_quantity)

rawData <- rawData %>%
  mutate("Received Currency" = df$received_currency)

rawData <- rawData %>%
  mutate("Received Quantity" = df$received_quantity)

rawData <- rawData %>%
  mutate("Transaction Type" = df$transaction_type)


#-------------------------------------------------------------------------------
#----------------------------------DATA FRAMES----------------------------------
#-------------------------------------------------------------------------------
df = subset(rawData
            , select = c("Date and Time"
                         , "Transaction Type"
                         , "Sent Quantity"
                         , "Sent Currency"
                         , "Received Quantity"
                         , "Received Currency"
            ))

df <- df %>%
  mutate("Month" = as.numeric(substr(df$`Date and Time`, 6, 7)))

df <- df %>%
  mutate("Year" = as.numeric(substr(df$`Date and Time`, 1, 4)))

df <- df[order(df$Year, df$Month),]

df <- subset(df, !is.na(df$Month))

df$`Received Quantity` <- as.numeric(df$`Received Quantity`)

df$`Sent Quantity` <- as.numeric(df$`Sent Quantity`)
##
btc_df <- subset(df, df$`Sent Currency` == 'BTC' | df$`Received Currency` == 'BTC')

btc_df <- btc_df %>%
  mutate("Sent" = if_else(btc_df$`Sent Currency` == "BTC", btc_df$`Sent Quantity`, 0))

btc_df <- btc_df %>%
  mutate("Received" = if_else(btc_df$`Received Currency` == "BTC", btc_df$`Received Quantity`, 0))

btc_df1 <- btc_df[1:1000000,]

btc_df2 <- btc_df[1000001:nrow(btc_df),]

write.csv(btc_df1, "C:/Users/Trevor French/Downloads/btc_core_exchange_1.csv")
write.csv(btc_df2, "C:/Users/Trevor French/Downloads/btc_core_exchange_2.csv")


#dai
dai_df <- subset(df, df$`Sent Currency` == 'DAI' | df$`Received Currency` == 'DAI')

dai_df <- dai_df %>%
  mutate("Sent" = if_else(dai_df$`Sent Currency` == "DAI", dai_df$`Sent Quantity`, 0))

dai_df <- dai_df %>%
  mutate("Received" = if_else(dai_df$`Received Currency` == "DAI", dai_df$`Received Quantity`, 0))

write.csv(dai_df, "C:/Users/Trevor French/Downloads/core_exchange_dai.csv")

i = 2
while (i <= nrow(asset_df)) {
  sent <- if_else(asset_df$`Sent Currency`[i] == "BTC", asset_df$`Sent Quantity`[i], 0)
  received <- if_else(asset_df$`Received Currency`[i] == "BTC", asset_df$`Received Quantity`[i], 0)
  asset_df$Balance[i-1] <- (asset_df$Balance[i-1] + received - sent)
  print(i)
  i = i+1
}


##
asset_inventory <- function(asset) {
  asset_df <- subset(df, df$`Sent Currency` == asset | df$`Received Currency` == asset)
  
  asset_df <- asset_df %>%
    mutate("Balance" = 0)
  
  i = 2
  while (i <= nrow(asset_df)) {
    sent <- if_else(asset_df[asset_df$`Sent Currency`, i] == asset, asset_df[`Sent Quantity`, i], 0)
    received <- if_else(asset_df[`Received Currency`, i] == asset, asset_df[`Received Quantity`, i], 0)
    asset_df[Balance,i-1] <- (asset_df[Balance,i-1] + received - sent)
    i = i+1
  }
  return(asset_df)
}

asset_df <- asset_inventory('BTC')
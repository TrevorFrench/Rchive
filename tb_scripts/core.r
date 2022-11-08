#Clears environment
rm(list = ls(all.names = TRUE))
#-------------------------------------------------------------------------------
#-----------------------------------LIBRARIES-----------------------------------
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(stringr)
library(chron)
#-------------------------------------------------------------------------------
#---------------------------------DATA SOURCES----------------------------------
#-------------------------------------------------------------------------------
#Sets working directory
wd <- "C:/Users/Trevor French/Downloads/again/"
setwd(wd)
files <- list.files(path=wd, pattern="*.csv")
for (file in files) {
  print(file)
  df <- read.csv(file)
  df2 <- df%>%
    mutate("received_currency_type" = if_else(df$received_currency =="", "",(if_else(toupper(df$received_currency) %in% c("CAD", "USD", "EUR", "ARS", "BRL", "CHF", "CLP", "CNY", "COP", "CZK", "DKK", "GBP", "HKD", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "ZAR", "THB", "VND", "ISK"), "fiat", "crypto"))))
  df2 <- df2%>%
    mutate("sent_currency_type" = if_else(df$sent_currency =="", "",(if_else(toupper(df$sent_currency) %in% c("CAD", "USD", "EUR", "ARS", "BRL", "CHF", "CLP", "CNY", "COP", "CZK", "DKK", "GBP", "HKD", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "ZAR", "THB", "VND", "ISK"), "fiat", "crypto"))))
  df2$date = substr(df$transaction_date_time, 1, 19)
  df2$transaction_date_time = gsub(" ", "", paste(df2$date,".000Z"))
  df2$transaction_id = gsub(" ", "_", paste(df$transaction_id, df$line_item))
  
  df2$received_quantity %>% replace_na("")
  df2$sent_quantity %>% replace_na("")
  
  df2$sent_quantity[is.na(df2$sent_quantity)] <- ""
  df2$received_quantity[is.na(df2$received_quantity)] <- ""
  
  #df2<-df2%>%
  #  mutate("sent_quantity" = df$sent_amount)
  #df2<-df2%>%
  #  mutate("received_quantity" = df$received_amount)
  df2<-df2%>%
    mutate("ID" = df2$X.)
  #View(df2)
  df2 = subset(df2
               , select = c("ID"
                            , "transaction_date_time"
                            , "transaction_id"
                            , "received_currency"
                            , "received_currency_type"
                            , "received_quantity"
                            , "sent_currency"
                            , "sent_currency_type"
                            , "sent_quantity"
                            , "transaction_type"
               ))
  #Set output file here
  outputFile <- paste(wd, "output/", file, sep='')
  #Writes output file
  write.csv(df2, outputFile, row.names = FALSE)
}
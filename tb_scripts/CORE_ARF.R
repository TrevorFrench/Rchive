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
wd <- "C:/Users/Trevor French/Downloads/CORE"

df <- combineFiles(wd)

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
  mutate("Month" = month(df$`Date and Time`))

df <- df %>%
  mutate("Year" = year(df$`Date and Time`))
#-------------------------------------------------------------------------------
#-----------------------------------GROUP BYS-----------------------------------
#-------------------------------------------------------------------------------
dfOut <- df %>% 
  group_by(Currency = df$`Sent Currency`
           , Year = df$Year
           , Month = df$Month
           , Type=df$`Transaction Type`) %>%
  summarize(Sent = sum(`Sent Quantity`))

dfIn <- df %>% 
  group_by(Currency = df$`Received Currency`
           , Year = df$Year
           , Month = df$Month
           , Type=df$`Transaction Type`) %>%
  summarize(Received = sum(`Received Quantity`))

#-------------------------------------------------------------------------------
#----------------------------------MERGE GROUPS---------------------------------
#-------------------------------------------------------------------------------
dfMerge <- merge(dfOut, dfIn, by=c("Currency", "Year", "Month", "Type"), all=TRUE)
dfFinal <- dfMerge

#-------------------------------------------------------------------------------
#-------------------------------COLUMN PREPARATION------------------------------
#-------------------------------------------------------------------------------

#Replace NAs with zeros
dfFinal[is.na(dfFinal)] <- 0

#Add Amount Column
dfFinal <- dfFinal %>%
  mutate("Amount" = Received - Sent)

#Subset dataframe
dfFinal= subset(dfFinal
                , select = c("Currency",
                             "Year",
                             "Month",
                             "Type",
                             "Amount"))

#Transform rows and columns (by Amount)
dfFinal <- dcast(dfFinal, dfFinal$Currency + dfFinal$Year + dfFinal$Month ~ dfFinal$Type)

df3 <- dfFinal %>%
  mutate("Currency" = `dfFinal$Currency`)

df3 <- df3 %>%
  mutate("BegBal" = 0)

df3 <- df3 %>%
  mutate("BegForm" = "")

df3 <- df3 %>%
  mutate("EndForm" = "")

#Get rid of currencies without a value
df3 <- filter(df3, Currency != 0)

#-------------------------------------------------------------------------------
#-------------------------------PREPARE VARIABLES-------------------------------
#-------------------------------------------------------------------------------
begBalCol <- which( colnames(df3)=="BegBal" )
Col <- letters[begBalCol - 1]
endCol <- letters[begBalCol + 2]
begCol <- letters[begBalCol + 1]
begNum <- begBalCol + 1
#endNum <- begBalCol + 2

df3[1, "BegBal"] <- "1"
df3[1, "BegForm"] <- "0"
df3[1, "EndForm"] <- paste("=",begCol,2,"+sum(E",2,":",Col, 2,")", sep="")

## Create Workbook object and add worksheets
wb <- createWorkbook()

## Add worksheets
addWorksheet(wb, "Asset Roll Forward")

# Row styles
greenStyle <- createStyle(fontColour = "#000000", fgFill = "#C4D79B")

#apply style to beginning Amount
addStyle(wb, "Asset Roll Forward", cols = begNum, rows = 2, style = greenStyle)

#-------------------------------------------------------------------------------
#----------------LOOP THROUGH TO FIND AND FORMAT BEGINNING ENTRY----------------
#-------------------------------------------------------------------------------

for (row in 2:nrow(df3)) {
  #Set currency variables
  Currency <- df3[row, "Currency"]
  prevCurr <- df3[row - 1, "Currency"]
  
  #Set formulas for beginning/ending values
  df3[row, "BegForm"] <- paste("=",endCol,row, sep="")
  df3[row, "EndForm"] <- paste("=",begCol,row +1,"+sum(E",row+1,":",Col, row+1,")", sep="")
  
  #Check for currency change
  if(Currency != prevCurr) {
    #Signal that currency has changed & set beginning balance to 0
    df3[row, "BegBal"] <- 1
    df3[row, "BegForm"] <- "0"
    
    #apply style to beginning Amount
    addStyle(wb, "Asset Roll Forward", cols = begNum, rows = row + 1, style = greenStyle)
  }
}

#-------------------------------------------------------------------------------
#---------------------------------COLUMN CLEANUP--------------------------------
#-------------------------------------------------------------------------------

#Drop currency column
df3 = subset(df3, select = -c(Currency))

#Change column classes to 'formula'
class(df3$BegForm) <- c(class(df3$BegForm), "formula")

class(df3$EndForm) <- c(class(df3$EndForm), "formula")

#Rename Column Headers
df3 <- rename(df3, "Currency" = "dfFinal$Currency")

df3 <- rename(df3, "Year" = "dfFinal$Year")

df3 <- rename(df3, "Month" = "dfFinal$Month")

df3 <- rename(df3, "First?" = "BegBal")

df3 <- rename(df3, "Beginning Balance" = "BegForm")

df3 <- rename(df3, "Ending Balance" = "EndForm")

#-------------------------------------------------------------------------------
#----------------------------------CLOSING ITEMS--------------------------------
#-------------------------------------------------------------------------------

# Create a heading style
Heading <- createStyle(textDecoration = "bold", border = "Bottom")

#Write dataframe three to the workbook object
writeData(wb, "Asset Roll Forward", df3, startCol = 1, startRow = 1, rowNames = TRUE)

# Apply header style
addStyle(wb, "Asset Roll Forward", cols = 1:ncol(df3)+1, rows = 1, style = Heading)

#Opens workbook without saving it
openXL(wb)

#Set output file here
outputFile <- paste(wd, "/Output/Asset_Roll_Custom_Output.xlsx", sep='')

#Writes output file
saveWorkbook(wb, outputFile, overwrite = TRUE)

#Closes output log file
sink()
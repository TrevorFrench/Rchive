#Clears environment
rm(list = ls(all.names = TRUE))
#-------------------------------------------------------------------------------
#-----------------------------------LIBRARIES-----------------------------------
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(writexl)
library(readr)
#-------------------------------------------------------------------------------
#---------------------------------DATA SOURCES----------------------------------
#-------------------------------------------------------------------------------

#Reads in raw data file
wd <- "C:/Users/Trevor French/Desktop/R Scripts/Files/"
dataSource <- paste(wd, "Input/POLOAK.xlsx", sep='')
rawData <- read_excel(dataSource)

View(rawData)

#-------------------------------------------------------------------------------
#------------------------------COUNT UNIQUE VALUES------------------------------
#-------------------------------------------------------------------------------
df <- rawData %>%
  filter(rawData$action == "borrow-open" | rawData$action == "borrow-close")

df <- df %>%
  mutate("Count" = 1)

dfCounts <- df %>%
  group_by(#timestamp = timestamp,
           #action = action,
           amount = amount) %>%
  summarise("Count" = sum(`Count`))

dfCounts <- dfCounts %>%
  mutate("ID" = 0)

id <- 1
for (row in 1:nrow(dfCounts)) {
  if(dfCounts[row, "Count"] == 2) {
    dfCounts[row, "ID"] <- id
  }
  
  id <- id + 1
}

dfID <- merge(df, dfCounts, by="amount")
#-------------------------------------------------------------------------------
#----------------------------------OPEN AND CLOSE-------------------------------
#-------------------------------------------------------------------------------
dfID1 <- dfID %>%
  filter(dfID$Count.y == 2)

for (row in 1:nrow(dfID1)) {
  tempID <- dfID1[row, "ID"]
  
  dfTemp <- dfID1 %>%
    filter(dfID1$ID == tempID)
  
  open <- dfTemp[1, "action"]
  close <- dfTemp[2, "action"]
  
  if(open == close) {
    dfID1[which(dfID1$ID == tempID), "ID"] <- 0
  }
}

#-------------------------------------------------------------------------------
#--------------------------------OPEN BEFORE CLOSE------------------------------
#-------------------------------------------------------------------------------
dfID2 <- dfID1 %>%
  filter(dfID1$ID != 0)

for (row in 1:nrow(dfID2)) {
  tempID <- dfID2[row, "ID"]
  
  dfTemp <- dfID2 %>%
    filter(dfID2$ID == tempID)
  
  open <- dfTemp[which(dfTemp$action == "borrow-open"), "timestamp"]
  close <- dfTemp[which(dfTemp$action == "borrow-close"), "timestamp"]
  
  if(open > close) {
    dfID2[which(dfID2$ID == tempID), "ID"] <- 0
  }
}

#dfMatched <- dfCounts %>%
#  filter(dfCounts$Count == 2)
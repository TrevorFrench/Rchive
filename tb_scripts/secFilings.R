#Clears environment
rm(list = ls(all.names = TRUE))
#-------------------------------------------------------------------------------
#-----------------------------------LIBRARIES-----------------------------------
#-------------------------------------------------------------------------------
library('edgar')
library(rjson)
library(jsonlite)
library(dplyr)




word.list = c('cryptocurrency')

useragent = 'email'

output <- searchFilings(cik.no='ALL', form.type="ALL", filing.year = c(2021), word.list, useragent)

word.list = c('derivative','hedging','currency forwards','currency futures')
output <- searchFilings(cik.no = c('1000180', '38079'),
                        form.type = c("10-K", "10-K405","10KSB", "10KSB40"),
                        filing.year = c(2005, 2006), word.list, useragent)

library('edgarWebR')
Sys.setenv(
  EDGARWEBR_USER_AGENT = "email"
)

df <- full_text("Crypto")

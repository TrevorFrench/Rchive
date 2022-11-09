library("remotes")   #In case you have not installed it.
remotes::install_github("TrevorFrench/trevoR")
library(trevoR)

library(purrr)

combineFiles2 <- function(input) {
  
  setwd(input)
  
  files <- list.files(pattern="*.csv")
  
  df <- files %>% map_dfr(read.csv, quote = "\"", comment.char = "\"", row.names = NULL)
  
  return(df)
  
}

wd <- ""
tvr_1 <- combineFiles2(paste(wd, "tvr_1", sep=''))

tvr_2 <- combineFiles2(paste(wd, "tvr_2", sep=''))

stat_1 <- tvr_1$tinVerificationStatus
freq_1 <- as.data.frame(table(stat_1))

stat_2 <- tvr_2$tinVerificationStatus
freq_2 <- as.data.frame(table(stat_2))

write.csv(freq_1, "tin_verification_report_1.csv")
write.csv(freq_2, "tin_verification_report_2.csv")
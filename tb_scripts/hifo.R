# TRANSFERS ARE NOT TAXABLE
# USD is not a disposition
# Can't dispose of an asset before it is acquired
library(dplyr)
library(stringi)
options(scipen=999)

df = read.csv("hifo.csv")

df <- df %>%
  mutate("ID" = 1:nrow(df)) %>%
  mutate("ID" = paste0("T", ID, sep=""))

in_df = subset(df, select = c(ID, Date.and.Time, Received.Currency, Received.Quantity, Market.Value))

out_df = subset(df, select = c(ID, Date.and.Time, Sent.Currency, Sent.Quantity, Market.Value))

disposed_assets <- stri_remove_empty(unique(out_df$Sent.Currency), na_empty = TRUE)
disposed_assets <- disposed_assets[disposed_assets != "USD"]
#disposed_assets <- c("SOL")
output <- data.frame(ID_SELL_TRADE = character(),
                     ID_COST = character(),
                     QUANTITY_DISPOSED = integer(),
                     CRYPTO = character(),
                     ACQUISITION_DATE = character(),
                     DISPOSAL_DATE = character(),
                     GROSS_PROCEEDS = integer(),
                     COST_BASIS = integer())

for (asset in disposed_assets) {
  print(asset)

  in_calcs <- in_df[which(in_df$Received.Currency==asset),]
  in_calcs <- in_calcs %>%
    mutate("Spot" =  Market.Value / Received.Quantity) %>%
    arrange(desc(Spot), Date.and.Time)
  out_calcs <- out_df[which(out_df$Sent.Currency==asset),]
  out_calcs <- out_calcs %>%
    mutate("Spot" =  Market.Value / Sent.Quantity) %>%
    arrange(Date.and.Time)
  
  balance = 0
  
  i = 1
  for (row in 1:nrow(out_calcs)) {

    received <- if_else(balance == 0, in_calcs[i, "Received.Quantity"], balance)
    # balance is the difference between the received quantity and sent quantity
    balance = received - out_calcs[row, "Sent.Quantity"]
    
    #quantity disposed is the amount sent with maximum being received (or leftover received from negative balance)
    
    quantity_disposed = if_else(received - out_calcs[row, "Sent.Quantity"] >= 0, out_calcs[row, "Sent.Quantity"], received)
    
    output_row = nrow(output) + 1
    output[output_row, "ID_SELL_TRADE"] = out_calcs[row, "ID"]
    output[output_row, "ID_COST"] = in_calcs[i, "ID"]
    output[output_row, "QUANTITY_DISPOSED"] = quantity_disposed
    output[output_row, "CRYPTO"] = asset
    output[output_row, "ACQUISITION_DATE"] = in_calcs[i, "Date.and.Time"]
    output[output_row, "DISPOSAL_DATE"] = out_calcs[row, "Date.and.Time"]
    output[output_row, "GROSS_PROCEEDS"] = quantity_disposed * out_calcs[row, "Spot"]
    output[output_row, "COST_BASIS"] = quantity_disposed * in_calcs[i, "Spot"]
    
    i = if_else(balance <= 0, i + 1, i)
    # i = 2
    while (balance < 0) {
      output_row = nrow(output) + 1
      
      quantity_disposed = out_calcs[row, "Sent.Quantity"] - quantity_disposed
      quantity_disposed = if_else(in_calcs[i, "Received.Quantity"] - quantity_disposed >= 0, quantity_disposed, in_calcs[i, "Received.Quantity"])
      
      output[output_row, "ID_SELL_TRADE"] = out_calcs[row, "ID"]
      output[output_row, "ID_COST"] = in_calcs[i, "ID"]
      output[output_row, "QUANTITY_DISPOSED"] = quantity_disposed
      output[output_row, "CRYPTO"] = asset
      output[output_row, "ACQUISITION_DATE"] = in_calcs[i, "Date.and.Time"]
      output[output_row, "DISPOSAL_DATE"] = out_calcs[row, "Date.and.Time"]
      output[output_row, "GROSS_PROCEEDS"] = quantity_disposed * out_calcs[row, "Spot"]
      output[output_row, "COST_BASIS"] = quantity_disposed * in_calcs[i, "Spot"]
      
      balance <- in_calcs[i, "Received.Quantity"] + balance
      
      print(balance)
      if (is.na(balance)) {break}
      i = if_else(balance <= 0, i + 1, i)
    }
    
  }

}

output <- output %>% mutate("GAIN_LOSS" = GROSS_PROCEEDS - COST_BASIS)


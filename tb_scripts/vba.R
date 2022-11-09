library(dplyr)
library(stringi)
options(scipen=999)

# 1: READ DATA
df = read.csv("hifo.csv")

# 2: CREATE ID
df <- df %>%
  mutate("ID" = 1:nrow(df)) %>%
  mutate("ID" = paste0("T", ID, sep=""))

# 3. CREATE IN & OUT DATAFRAMES (NEED TO ADD TRANSACTION TYPES)
in_df = subset(df, select = c(ID, Date.and.Time, Received.Currency, Received.Quantity, Market.Value))
out_df = subset(df, select = c(ID, Date.and.Time, Sent.Currency, Sent.Quantity, Market.Value))

# 4. CREATE A LIST OF DISPOSED ASSETS
disposed_assets <- stri_remove_empty(unique(out_df$Sent.Currency), na_empty = TRUE)
disposed_assets <- disposed_assets[disposed_assets != "USD"]
disposed_assets <- c("SOL")

# 4. INITIATE AN OUTPUT DATAFRAME
output <- data.frame(ID_SELL_TRADE = character(),
                     ID_COST = character(),
                     QUANTITY_DISPOSED = integer(),
                     CRYPTO = character(),
                     ACQUISITION_DATE = character(),
                     DISPOSAL_DATE = character(),
                     GROSS_PROCEEDS = integer(),
                     COST_BASIS = integer())

for (asset in disposed_assets) {
  ins <- in_df[which(in_df$Received.Currency==asset),]
  ins <- ins %>%
    mutate("Spot" =  Market.Value / Received.Quantity) %>%
    mutate("Balance" = Received.Quantity) %>%
    arrange(desc(Spot), Date.and.Time)
  
  outs <- out_df[which(out_df$Sent.Currency==asset),]
  outs <- outs %>%
    mutate("Spot" =  Market.Value / Sent.Quantity) %>%
    arrange(Date.and.Time)
  
  acq_row = 1
  disp_row = 1
  
  for (row in 1:nrow(outs)) {
    print(ins[acq_row, 'Balance'])

    balance = ins[acq_row, 'Balance'] - outs[disp_row, 'Sent.Quantity']
    print(outs[disp_row, 'Sent.Quantity'])
    print(balance)
    if (balance < 0) {
      #print(ins[disp_row, 'Received.Quantity'])
      output_row = nrow(output) + 1
      output[output_row, "ID_SELL_TRADE"] = outs[disp_row, "ID"]
      output[output_row, "ID_COST"] = ins[acq_row, "ID"]
      output[output_row, "QUANTITY_DISPOSED"] = ins[acq_row, 'Balance']
      output[output_row, "CRYPTO"] = asset
      output[output_row, "ACQUISITION_DATE"] = ins[acq_row, "Date.and.Time"]
      output[output_row, "DISPOSAL_DATE"] = outs[disp_row, "Date.and.Time"]
      output[output_row, "GROSS_PROCEEDS"] = ins[acq_row, 'Balance'] * outs[disp_row, "Spot"]
      output[output_row, "COST_BASIS"] = ins[acq_row, 'Balance'] * ins[acq_row, "Spot"]
    }
    ins[acq_row, 'Balance'] <- balance
    
    if (balance > 0) {
      output_row = nrow(output) + 1
      output[output_row, "ID_SELL_TRADE"] = outs[disp_row, "ID"]
      output[output_row, "ID_COST"] = ins[acq_row, "ID"]
      output[output_row, "QUANTITY_DISPOSED"] = outs[disp_row, 'Sent.Quantity']
      output[output_row, "CRYPTO"] = asset
      output[output_row, "ACQUISITION_DATE"] = ins[acq_row, "Date.and.Time"]
      output[output_row, "DISPOSAL_DATE"] = outs[disp_row, "Date.and.Time"]
      output[output_row, "GROSS_PROCEEDS"] = outs[disp_row, 'Sent.Quantity'] * outs[disp_row, "Spot"]
      output[output_row, "COST_BASIS"] = outs[disp_row, 'Sent.Quantity'] * ins[acq_row, "Spot"]
      disp_row = disp_row + 1
    }
    
    if (balance == 0) {
      output_row = nrow(output) + 1
      output[output_row, "ID_SELL_TRADE"] = outs[disp_row, "ID"]
      output[output_row, "ID_COST"] = ins[acq_row, "ID"]
      output[output_row, "QUANTITY_DISPOSED"] = outs[disp_row, 'Sent.Quantity']
      output[output_row, "CRYPTO"] = asset
      output[output_row, "ACQUISITION_DATE"] = ins[acq_row, "Date.and.Time"]
      output[output_row, "DISPOSAL_DATE"] = outs[disp_row, "Date.and.Time"]
      output[output_row, "GROSS_PROCEEDS"] = outs[disp_row, 'Sent.Quantity'] * outs[disp_row, "Spot"]
      output[output_row, "COST_BASIS"] = outs[disp_row, 'Sent.Quantity'] * ins[acq_row, "Spot"]
      disp_row = disp_row + 1
      acq_row = acq_row + 1
    }
    
    while (balance < 0) {
      acq_row = acq_row + 1
      balance = ins[acq_row, 'Balance'] + ins[acq_row - 1, 'Balance']
      ins[acq_row, 'Balance'] <- balance
      # print(balance)
      
      ###
      
      if (balance > 0) {
        output_row = nrow(output) + 1
        output[output_row, "ID_SELL_TRADE"] = outs[disp_row, "ID"]
        output[output_row, "ID_COST"] = ins[acq_row, "ID"]
        output[output_row, "QUANTITY_DISPOSED"] = abs(balance)
        output[output_row, "CRYPTO"] = asset
        output[output_row, "ACQUISITION_DATE"] = ins[acq_row, "Date.and.Time"]
        output[output_row, "DISPOSAL_DATE"] = outs[disp_row, "Date.and.Time"]
        output[output_row, "GROSS_PROCEEDS"] = abs(balance) * outs[disp_row, "Spot"]
        output[output_row, "COST_BASIS"] = abs(balance) * ins[acq_row, "Spot"]
        disp_row = disp_row + 1
      }
      
      if (balance == 0) {
        output_row = nrow(output) + 1
        output[output_row, "ID_SELL_TRADE"] = outs[disp_row, "ID"]
        output[output_row, "ID_COST"] = ins[acq_row, "ID"]
        output[output_row, "QUANTITY_DISPOSED"] = abs(ins[acq_row - 1, 'Balance'])
        output[output_row, "CRYPTO"] = asset
        output[output_row, "ACQUISITION_DATE"] = ins[acq_row, "Date.and.Time"]
        output[output_row, "DISPOSAL_DATE"] = outs[disp_row, "Date.and.Time"]
        output[output_row, "GROSS_PROCEEDS"] = abs(ins[acq_row - 1, 'Balance']) * outs[disp_row, "Spot"]
        output[output_row, "COST_BASIS"] = abs(ins[acq_row - 1, 'Balance']) * ins[acq_row, "Spot"]
        disp_row = disp_row + 1
        acq_row = acq_row + 1
      }
      
      if (balance < 0) {
        output_row = nrow(output) + 1
        output[output_row, "ID_SELL_TRADE"] = outs[disp_row, "ID"]
        output[output_row, "ID_COST"] = ins[acq_row, "ID"]
        output[output_row, "QUANTITY_DISPOSED"] = ins[acq_row, 'Received.Quantity']
        output[output_row, "CRYPTO"] = asset
        output[output_row, "ACQUISITION_DATE"] = ins[acq_row, "Date.and.Time"]
        output[output_row, "DISPOSAL_DATE"] = outs[disp_row, "Date.and.Time"]
        output[output_row, "GROSS_PROCEEDS"] = ins[acq_row, 'Received.Quantity'] * outs[disp_row, "Spot"]
        output[output_row, "COST_BASIS"] = ins[acq_row, 'Received.Quantity'] * ins[acq_row, "Spot"]
      }
      
      ###
      
      if (is.na(balance)) {break}
    }
    
  }
  
}

output <- output %>% mutate("GAIN_LOSS" = GROSS_PROCEEDS - COST_BASIS)
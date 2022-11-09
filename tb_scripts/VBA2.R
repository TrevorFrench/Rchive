library(dplyr)
library(stringi)
options(scipen=999)

# 1: READ DATA
df = read.csv("hifo2.csv")

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
#disposed_assets <- c("SOL")

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
  
  while (disp_row <= nrow(outs)) {
    
    balance = ins[acq_row, 'Received.Quantity'] - outs[disp_row, 'Sent.Quantity']
    print(ins[acq_row, 'Received.Quantity'])
    print(outs[disp_row, 'Sent.Quantity'])
    ins[acq_row, 'Balance'] = balance
    #ins[acq_row, 'Received.Quantity'] = balance
    if (is.na(balance)) {break}
    
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
      
      acq_row = acq_row + 1
      disp_row = disp_row + 1
      
    }
    
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
      
      ins[acq_row, 'Received.Quantity'] = balance
      disp_row = disp_row + 1
      
    }
    
    if (balance < 0) {
      outs[disp_row, 'Sent.Quantity'] = abs(balance)
      
      output_row = nrow(output) + 1
      output[output_row, "ID_SELL_TRADE"] = outs[disp_row, "ID"]
      output[output_row, "ID_COST"] = ins[acq_row, "ID"]
      output[output_row, "QUANTITY_DISPOSED"] = ins[acq_row, 'Received.Quantity']
      output[output_row, "CRYPTO"] = asset
      output[output_row, "ACQUISITION_DATE"] = ins[acq_row, "Date.and.Time"]
      output[output_row, "DISPOSAL_DATE"] = outs[disp_row, "Date.and.Time"]
      output[output_row, "GROSS_PROCEEDS"] = ins[acq_row, 'Received.Quantity'] * outs[disp_row, "Spot"]
      output[output_row, "COST_BASIS"] = ins[acq_row, 'Received.Quantity'] * ins[acq_row, "Spot"]
      acq_row = acq_row + 1
    }
    
  }
  
}

output <- output %>% mutate("GAIN_LOSS" = GROSS_PROCEEDS - COST_BASIS)
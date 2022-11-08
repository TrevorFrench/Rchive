#Reads in raw data file
wd <- "C:/Users/Trevor French/Desktop/"
dataSource <- paste(wd, "freq.csv", sep='')
df <- read.csv(dataSource)

df <- subset(df, df$Integrations <= 15)

# Filled Density Plot
d <- density(df$Integrations)
plot(d, main="Connections per PP User")
polygon(d, col="#24cc99", border="#24cc99")
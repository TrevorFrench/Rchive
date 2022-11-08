library(tidyr)
df <- data.frame(person = c("John_Doe", "Jane_Doe"))
print(df)
# person
# 1 John_Doe
# 2 Jane_Doe

df <- df %>% separate(person, c("first_name", "last_name"), "_")
print(df)
# first_name last_name
# 1       John       Doe
# 2       Jane       Doe
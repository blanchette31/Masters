# Packages 
library(ggplot2)
library(gridExtra)
library(dplyr)


# Load dataframe

df2003 = read_delim("Data//Raw//debit//debit_journalier_2003.csv", " ")

# ne fonctionne pas en raison de la separation " " 
df = read.csv("Data//Raw//debit", pattern = "*.csv", full.names = TRUE, sep = " ", quote = " ") %>% 
  lapply(read_csv) %>%
  bind_rows

as.data.frame(df)

write.csv(df, "Data//Processed//debit//debit_merged.csv", row.names = TRUE)
# Packages 
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(tidyverse)

# Load dataframe

df2003 = read_delim("Data//Raw//debit//debit_journalier_2003.csv", " ")

# ne fonctionne pas en raison de la separation " " 
df = read.csv("Data//Raw//debit", pattern = "*.csv", full.names = TRUE, sep = " ", quote = " ") %>% 
  lapply(read_csv) %>%
  bind_rows

as.data.frame(df2003)

write.csv(df, "Data//Processed//debit//debit_merged.csv", row.names = TRUE)


#list discharge files
debit_files = list.files("Data//Raw//debit//", pattern = ".csv", full.names = FALSE)
 
#loop to read csv files
lapply(debit_files, read_delim

for (i in 1:length(debit_files)){
  assign(debit_files[i], read.csv(debit_files[i]))
  }

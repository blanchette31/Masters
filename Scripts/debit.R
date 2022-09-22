# Packages 
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(tidyverse)

# Load dataframe

df2011 = read_delim("Data//Raw//debit//debit_journalier_2011.csv")
as.data.frame(df2011)

df2012 = read_delim("Data//Raw//debit//debit_journalier_2012.csv")
as.data.frame(df2012)

df2014 = read_delim("Data//Raw//debit//debit_journalier_2014.csv")
as.data.frame(df2014)

df2015 = read_delim("Data//Raw//debit//debit_journalier_2015.csv")
as.data.frame(df2015)

df2016 = read_delim("Data//Raw//debit//debit_journalier_2016.csv")
as.data.frame(df2016)

df2017 = read_delim("Data//Raw//debit//debit_journalier_2017.csv")
as.data.frame(df2017)

df2018 = read_delim("Data//Raw//debit//debit_journalier_2018.csv")
as.data.frame(df2018)

df2019 = read_delim("Data//Raw//debit//debit_journalier_2019.csv")
as.data.frame(df2019)

df2020 = read_delim("Data//Raw//debit//debit_journalier_2020.csv")
as.data.frame(df2020)

df2021 = read_delim("Data//Raw//debit//debit_journalier_2021.csv")
as.data.frame(df2021)

# ne fonctionne pas en raison de la separation " " 
df = read.csv("Data//Raw//debit", pattern = "*.csv", full.names = TRUE, sep = " ", quote = " ") %>% 
  lapply(read_csv) %>%
  bind_rows

as.data.frame(df2012)

write.csv(df, "Data//Processed//debit//debit_merged.csv", row.names = TRUE)


#list discharge files
debit_files = list.files("Data//Raw//debit//", pattern = ".csv", full.names = T)
 
#loop to read csv files
files = lapply(debit_files, fread) %>%
  bind_rows


for (i in 1:length(debit_files)){
  assign(debit_files[i], read.csv(debit_files[i]))
  }

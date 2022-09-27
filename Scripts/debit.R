# Packages 
library(ggplot2)
library(gridExtra)
library(dplyr)
library(data.table)
library(tidyverse)

# Load dataframe

df2011 = read_delim("Data//Raw//debit//formatted//debit_2011.csv")
df2011 =as.data.frame(df2011)

df2012 = read_delim("Data//Raw//debit//formatted//debit_2012.csv")
df2012 = as.data.frame(df2012)

df2014 = read_delim("Data//Raw//debit//formatted//debit_journalier_2014.csv")
df2014 = as.data.frame(df2014)

df2015 = read_delim("Data//Raw//debit//debit_journalier_2015.csv")
df2015 = as.data.frame(df2015)

df2016 = read_delim("Data//Raw//debit//debit_journalier_2016.csv")
df2016 = as.data.frame(df2016)

df2017 = read_delim("Data//Raw//debit//debit_journalier_2017.csv")
df2017 = as.data.frame(df2017)

df2018 = read_delim("Data//Raw//debit//debit_journalier_2018.csv")
df2018 = as.data.frame(df2018)

df2019 = read_delim("Data//Raw//debit//debit_journalier_2019.csv")
df2019 = as.data.frame(df2019)

df2020 = read_delim("Data//Raw//debit//debit_journalier_2020.csv")
df2020 = as.data.frame(df2020)

df2021 = read_delim("Data//Raw//debit//debit_journalier_2021.csv")
df2021 = as.data.frame(df2021)

df_debit = rbind(df2012, df2014, df2015, df2016, df2017, df2018, df2019,df2020, df2021)

df_debit$date = as.Date(df_debit$date)

# ne fonctionne pas en raison de la separation " " 
df_debit = read.csv("Data//Raw//debit//formatted", sep = '\t') 
 

#as.data.frame(df2012)




#list discharge files
df_debit = list.files("Data//Raw//debit//formatted//", pattern = ".csv", full.names = T) %>%
lapply(read_csv) %>%
  bind_rows

#convert tibble to data frame
df_debit = as.data.frame(df_debit)

#add year and month columns to discharge data
df_debit = df_debit %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"))
 
# add DOY
#set columns as dates 

df_debit$doy = as.numeric(strftime(df_debit$date, "%j"))
df_debit$doy


df_debit$year = as.factor(df_debit$year)
df_debit$month = as.factor(df_debit$month)
#export dataframe

write.csv(df_debit, "Data//Processed//debit//debit_merged.csv", row.names = TRUE)

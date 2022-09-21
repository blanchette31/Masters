# Packages
library(dplyr)
library(readr)


#load dataframes
df = list.files(path = "Data//Raw//boue//Croche2016-2021", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows

as.data.frame(df)

write.csv(df, "Data//Processed//boue//boue_merged.csv", row.names = TRUE)
?as.POSIXct # 
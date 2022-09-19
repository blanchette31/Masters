# Packages
library(dplyr)
library(readr)


#load dataframe
df = list.files(path = "Data//Raw//precip//Saint-Hippolyte", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows

as.data.frame(df)

write.csv(df, "Data//Processed//precip//precip_sth_merged.csv", row.names = TRUE)

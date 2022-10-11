#Load packages 
library(readxl)
library(dplyr)

#Load data 
df = list.files(path = "Data//Raw//precip//SBL//", pattern = "*.xls", full.names = TRUE) %>% 
lapply(read_excel) %>%
  bind_rows

write.csv(df, "Data//Processed//precip//precip_sbl_pre_treat.csv")

df = read.csv("Data//Processed//precip//precip_sbl_pre_treat.csv", header = F, stringsAsFactors = F)

new_names = paste0(as.character(df[1,], as.character(df[2,])))

names(df) = new_names
df = df[3:nrow(df),]

precip_daily = df %>% group_by(jour) %>%
  summarise(precipitations = sum(Pluietmp))

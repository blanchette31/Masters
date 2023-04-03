# Packages
library(dplyr)
library(readr)


#load dataframe
df = list.files(path = "Data//Raw//precip//Saint-Hippolyte", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows

as.data.frame(df)
df$Year = as.factor(df$Year)
split(df, f = df$Year)

# DOY
#set columns as dates 
df$date = as.Date(with(df, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df$doy = as.numeric(strftime(df$date, "%j"))
df$doy

df_clean = df %>% select(date, Year, Month, Day, `Total Rain (mm)`,doy)
write.csv(df_clean, "Data//Processed//precip//precip_brief.csv", row.names = TRUE)

write.csv(df, "Data//Processed//precip//precip_sth_merged.csv", row.names = TRUE)
my_splits = split(df, df$Year)
split_names = c("p2012", "p2013", "p2014", "p2015", "p2016", "p2017", "p2018", "p2019", "p2020", "p2021", "p2022")

for(i in 1:length(my_splits)){
  assign(split_names[i],my_splits[i])
}

b.plot = barplot(height = df$`Total Rain (mm)`, names.arg = )

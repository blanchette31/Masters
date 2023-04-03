# write r code for a scatterplot using ggplot and make points red


library(ggplot2)

ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "red")
#write a for loop in r code to subset data into different groups based on multiple conditions of one column for each dataframe in a list of dataframes


# Create a list of dataframes
df_list <- list(df1, df2, df3)

# Loop through the list of dataframes
for (i in 1:length(df_list)) {
  
  # Subset data into different groups based on multiple conditions of one column
  df_list[[i]] <- df_list[[i]][df_list[[i]]$column == "condition1" | df_list[[i]]$column == "condition2" | df_list[[i]]$column == "condition3",]
  
}


# Create a list of dataframes
df_list <- list(df1, df2, df3)

# Loop through the list of dataframes
for (i in 1:length(df_list)) {
  
  # Subset the dataframe based on multiple conditions of one column
  df_list[[i]] <- df_list[[i]][df_list[[i]]$column_name == "condition1" | df_list[[i]]$column_name == "condition2" | df_list[[i]]$column_name == "condition3",]
  
} 


# Create a list of dataframes
df_list <- list(df1, df2, df3)

# Loop through each dataframe in the list
for (i in 1:length(df_list)) {
  # Subset rows based on conditions
  df_list[[i]] <- df_list[[i]][df_list[[i]]$column1 == "value1" & df_list[[i]]$column2 == "value2",]
}


# Create a list of dataframes
df_list <- list(df1, df2, df3)

# Loop through the list of dataframes
for (i in 1:length(df_list)) {
  df_list[[i]] <- df_list[[i]][10:20,]
}


for (i in 1:length(list_of_dataframes)) {
  list_of_dataframes[[i]] <- list_of_dataframes[[i]][10:20,]
}

library(ggplot2)

# Create a sample data set
data = data.frame(team = rep(c("A", "B", "C"), each = 10), 
                  time = rep(1:10, 3), 
                  performance = rnorm(30))

# Create the heatmap
ggplot(data, aes(x = time, y = team, fill = performance)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")




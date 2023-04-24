# prepare workspace 
rm(list = ls())

# Load required package
library(tidyverse)
library(MetBrewer)

# Define folder path
folder_path <- "Data/Processed/comments/"

# List all CSV files that match the pattern 'metab_*.csv'
files <- list.files(path = folder_path, pattern = "^metab_.*\\.csv$", full.names = TRUE)

# Read and combine all files
df <- files %>%
  lapply(read.csv) %>%
  bind_rows()

# View the combined dataframe
head(df)

pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

df = merge(df, pheno[, c("doy_start", "doy_end", "year")], by = "year")

df_filtered <- df %>%
  filter(doy >= 152 & doy < doy_start)



# Filter first
df_filtered <- df %>%
  filter(doy >= 152 & doy < doy_start)

# Define DOY for month labels (non-leap year)
month_breaks <- c(
  "June" = 152,      # June 1
  "July" = 182,      # July 1
  "August" = 213,    # Aug 1
  "September" = 244, # Sept 1
  "October" = 274,   # Oct 1
  "November" = 305   # Nov 1
)

# Define custom color palette
col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")


# -----------------
# NEP plot
p_nep <- ggplot(df_filtered, aes(x = doy, y = NEP, color = as.factor(year))) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(name = "Month", breaks = month_breaks, labels = names(month_breaks)) +
  scale_color_manual(values = col_ind) +
  labs(title = "NEP by Year", y = "NEP", color = "Year") +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Data/Figures/comments/NEP_by_year.png", plot = p_nep, width = 8, height = 5, dpi = 300)

# -----------------
# R plot
p_r <- ggplot(df_filtered, aes(x = doy, y = R, color = as.factor(year))) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(name = "Month", breaks = month_breaks, labels = names(month_breaks)) +
  scale_color_manual(values = col_ind) +
  labs(title = "R by Year", y = "R", color = "Year") +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Data/Figures/comments/R_by_year.png", plot = p_r, width = 8, height = 5, dpi = 300)

# -----------------
# GPP plot
p_gpp <- ggplot(df_filtered, aes(x = doy, y = GPP, color = as.factor(year))) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(name = "Month", breaks = month_breaks, labels = names(month_breaks)) +
  scale_color_manual(values = col_ind) +
  labs(title = "GPP by Year", y = "GPP", color = "Year") +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Data/Figures/comments/GPP_by_year.png", plot = p_gpp, width = 8, height = 5, dpi = 300)


# Create summary table
summary_table <- df_filtered %>%
  group_by(year) %>%
  summarise(
    total_NEP = sum(NEP, na.rm = TRUE),
    total_GPP = sum(GPP, na.rm = TRUE),
    total_R = sum(R, na.rm = TRUE),
    n_obs = n()
  ) %>%
  ungroup()

# View summary
print(summary_table)

# Save summary table as CSV
write.csv(summary_table, "Data/Processed/comments/summary_NEP_GPP_R_per_year.csv", row.names = FALSE)

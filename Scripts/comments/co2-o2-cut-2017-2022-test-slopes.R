#prepare workspace 
rm(list =ls())

# Load libraries
library(tidyverse)
library(ggpubr)
library(cowplot)
library(tidyverse)
library(scales)
library(MetBrewer)
library(patchwork)
library(nlme)

# Load data
df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

##=====================================##
##PREPARE DATA FRAME FOR TEMPORAL PLOTS##
##=====================================##


#Add start and end of leaf loss to doy
df <- merge(df, pheno[, c("doy_start", "doy_end", "year")], by = "year")

#Add factor with time periods
df <- df %>%
  rowwise() %>%
  mutate(period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"
  ))


#Calculate daily mean dCO2 and dO2
df_date <- df %>%
  group_by(date, year, doy, period_rel_leaf_fall) %>%
  summarise(
    co2 = mean(delta.CO2),
    o2 = mean(do_deviation),
    o2_meta = mean(do_deviation_meta),
    o2_hypo = mean(do_deviation_hypo)
  )

date_filt <- df_date %>%
  filter(!is.na(co2) | !is.na(o2))

#Keep data from june onward
df_june <- df_date %>%
  filter(doy >= 152)

#Put in a list
df.list <- split(df_june, df_june$year)

#Color palette
#Tam MetBrewer
paste(met.brewer("Hiroshige"))
c("#e76254","#ef8a47", "#f7aa58", "#ffd06f" ,"#72bcd5" ,"#528fad" ,"#1e466e")

#Create the recurrent theme for the annual temporal plots
theme_annual <- list(geom_hline(yintercept = 0),
                     scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305),
                                        labels = c("June", "July", "Aug", "Sept", "Oct", "Nov"),
                                        limits = c(min(df_june$doy),max(df_june$doy))),
                     scale_y_continuous(breaks = c(-120,-100,-80,-60,-40,-20,0,20,40,60,80), 
                                        limits = c(min(df_june$o2, na.rm = T), max(df_june$co2, na.rm = T))),
                     labs(x = '', y = expression(Delta*Gas *~'('*'µmol L'^-1*')')),
                     theme_classic())


#Superpose all years
df_june$year <- as.factor(df_june$year)
df_june$year <- factor(df_june$year, levels(df_june$year)[c(3,6,7,5,2,1,4)])
p.superposed <- ggplot(df_june) +
  geom_rect(aes(xmin = min(pheno$doy_start), xmax = max(pheno$doy_start),
                ymin = min(df_june$o2, na.rm = T), ymax = max(df_june$co2, na.rm = T)), fill = 'grey95') +
  # geom_rect(aes(xmin = min(pheno$doy_end), xmax = max(pheno$doy_end),
  #               ymin = min(df_june$o2, na.rm = T), ymax = max(df_june$co2, na.rm = T)), fill = 'grey95') +
  geom_rect(aes(xmin = mean(pheno$doy_start)-0.5, xmax = mean(pheno$doy_start)+0.5,
                ymin = min(df_june$o2, na.rm = T), ymax = max(df_june$co2, na.rm = T)), fill = 'grey70') +
  # geom_rect(aes(xmin = mean(pheno$doy_end)-0.5, xmax = mean(pheno$doy_end)+0.5,
  #               ymin = min(df_june$o2, na.rm = T), ymax = max(df_june$co2, na.rm = T)), fill = 'grey70') +
  theme_annual +
  geom_line(aes(x = doy, y = co2, color = year), linewidth = 1.5) +
  geom_line(aes(x = doy, y = o2, color = year), linewidth = 1.5) +
  scale_color_manual(values = met.brewer("Hiroshige",7), name = '') +
  geom_text(aes(x = 175, y = 50), label = expression(paste(Delta*CO[2])), size = 4) +
  geom_text(aes(x = 175, y = -100), label = expression(paste(Delta*O[2])), size = 4) +
  labs(title = "a) Summer and fall gas temporal variation" ) +
  theme(legend.position = 'none') 


##======================================##
##PLOT CO2 AND O2 ACCUMULATION/DEPLETION##
##======================================##

# Load data 

lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

# Apply filtering for 2017 and 2022 separately
lf <- lf %>%
  filter(
    !(year == 2017 & doy < 273),  # Exclude rows before DOY 273 in 2017
    !(year == 2022 & doy < 257)   # Exclude rows before DOY 257 in 2022
  )

labels_df <- data.frame(
  doy = c(244, 274, 305),
  label = c("Sept", "Oct", "Nov")
)
lf$year <- factor(lf$year, levels = rev(levels(lf$year)))
lf$year <- factor(lf$year, levels(lf$year)[c(5, 2, 1, 3, 6, 7, 4)])

# Custom color palette
# custom_palette <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

# Initialize an empty data frame to store slopes
slopes_df <- data.frame(year = character(), slope_co2 = numeric(), slope_o2 = numeric(), stringsAsFactors = FALSE)

# Fit GLS models with AR1 correlation structure, predict values for plotting, and store slopes
lf <- lf %>%
  group_by(year) %>%
  filter(!is.na(co2) & !is.na(o2)) %>%  # Remove rows with missing values
  do({
    co2_model <- gls(co2 ~ doy, data = ., correlation = corAR1(form = ~ doy))
    o2_model <- gls(o2 ~ doy, data = ., correlation = corAR1(form = ~ doy))
    data.frame(
      .,
      gls_co2 = predict(co2_model),
      gls_o2 = predict(o2_model)
    ) %>%
      bind_rows(
        data.frame(
          year = unique(.$year),
          slope_co2 = coef(co2_model)["doy"],
          slope_o2 = coef(o2_model)["doy"]
        )
      )
  })

# Extract slopes to a separate data frame
slopes <- lf %>%
  select(year, slope_co2, slope_o2) %>%
  distinct()


# Plot GLS regression for CO2
p.co2 <- ggplot(lf, aes(x = doy, y = co2, color = year)) +
  geom_line(aes(y = gls_co2), linetype = "solid", linewidth = 1.1) +  # GLS regression line
  geom_point(alpha = 0.8, size =2) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8), name = "") +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  scale_y_continuous(breaks = c(-120,-100,-80,-60,-40,-20,0,20,40,60,80)) +
  labs(y = expression(Delta*CO[2]*~'('*'µmol L'^-1*')'), x = "", 
       title = expression("b) CO"[2]*" accumulation during leaf loss")) +
  theme(legend.position = "none")



# Plot GLS regression for O2
p.o2 <- ggplot(lf, aes(x = doy, y = o2, color = year)) +
  geom_line(aes(y = gls_o2), linetype = "solid", linewidth = 1.1) +  # GLS regression line
  geom_point(alpha = 0.8, size = 2) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8), name = '') +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  scale_y_continuous(breaks = c(-120,-100,-80,-60,-40,-20,0,20,40,60,80)) +
  labs(y = expression(Delta*O[2]*~'('*'µmol L'^-1*')'), x = "", 
       title = expression("c) O"[2]*" depletion during leaf loss")) 



##ASSEMBLE GRAPHICS##

#Align bottom row with panel 1 (p.superpose)
plots <- cowplot::align_plots(p.superposed, p.co2, align = 'v', axis = 'l')
bottom_row <- plot_grid(plots[[2]], p.o2 + theme(legend.position = "none"))
#Legend as right panel
legend <- get_legend(
  # create some space to the left and right of the legend
  p.o2 + theme(legend.box.margin = margin(0, 5, 0, 5),
               legend.key.size = unit(1, 'cm'),
               legend.text = element_text(size = 12))
)

left_side <- plot_grid(p.superposed, bottom_row, nrow = 2, rel_heights = c(1,0.8))
grid <- plot_grid(left_side, legend, ncol = 2, rel_widths = c(1,0.1))


# Save the grid of plots to a file 
ggsave("Data/Figures/presentations/co2_o2_all_years_1722_cut.jpg",grid, dpi = 600, width = 10, height = 6)

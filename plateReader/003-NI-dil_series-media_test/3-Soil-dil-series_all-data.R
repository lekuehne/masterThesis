# Preface ----
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(data.table)
library(ggnested)
setwd("~/Documents/MSc_MMEI/MA/data/003-Soil_dil_series")

# Import Data ----
P1 <- read.csv("all_data_plate1.csv")
P2 <- read.csv("all_data_plate2.csv")
P3 <- read.csv("all_data_plate3.csv")
P4 <- read.csv("all_data_plate4.csv")

df <- rbind(P1, P2, P3, P4)
# write.csv(df, "all_data.csv", row.names = FALSE)

df$sample <- as.character(df$sample) # Convert values to numeric

# df <- df %>% mutate(elapsed_hours = round(elapsed_hours, 0)) # round elapsed time to one decimal

df <- df %>%
  mutate(elapsed_hours = round(elapsed_hours * 2) / 2)  # bins to 0.5 h

# df$datetime <- as.POSIXct(df$datetime) # convert to proper datetime
# 
# t0 <- min(df$datetime) # define a reference start time
# 
# df <- df %>%
#   mutate(elapsed_hours_calc = as.numeric(difftime(datetime, t0, units = "hours"))) # compute elapsed time




# Calculate summary statistics (mean and standard deviation)
df_stat <- df %>%
  group_by(sample, dilution, wavelength, elapsed_hours, media, datetime, file) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Mean and sd by replicates AND media replicates \
# (different soil samples) for LB/M9 media effect contribution
media_stat <- df %>%
  group_by(dilution, wavelength, elapsed_hours, media) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )


#df$dilution_expr <- parse(text = df$dilution) # Sub-/superscript expressions for labels in plots 

# Plot ----
## OD600 10^-3 all samples ----
df_stat %>% 
  filter(wavelength %in% c("OD600") &
           dilution %in% c("10^-3")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = sample)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  # scale_color_discrete(labels = parse(text = unique(df_stat$dilution))) +
  theme_minimal() +
  labs(y= expression("OD"[600]), x="Time", title = expression("OD"[600]*" of all 10"^"-3"*" Dilutions")) 



## OD600 10^-2 by media ----
media_stat %>% 
  filter(wavelength %in% c("OD600")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = media)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  # scale_color_discrete(labels = parse(text = unique(df_stat$dilution))) +
  theme_minimal() +
  labs(y= expression("OD"[600]), x="Time", title = expression("OD"[600]*" of all Dilutions grouped by media type")) +
  theme(plot.caption = element_markdown()) +
  facet_wrap(~dilution)


## OD600 all samples, facet dilutions ----
df_stat %>% 
  filter(wavelength %in% c("OD600")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = sample)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  theme_minimal() +
  labs(y= expression("OD"[600]), x="Time", title = expression("OD"[600]*" Of All Soil Samples At Different Starting Dilutions")) +
  facet_wrap(~ dilution)

## Plotting for LB and M9
media_stat %>% 
  filter(wavelength %in% c("OD600")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = media)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  theme_minimal() +
  labs(y= expression("OD"[600]), x="Time", title = expression("OD"[600]*" Of All Soil Samples At Different Starting Dilutions")) +
  facet_wrap(~ dilution)

## OD all grouped by media and dilution ----
media_stat %>% 
  filter(wavelength %in% c("OD600")) %>%
  ggnested(aes(x = elapsed_hours,
               y = mean_value, 
               main_group = media, 
               sub_group = dilution),
           legend_title = "Starting Density",
           main_keys = T,
           base_clr = "darkgreen",
           gradient_type = "tints"
           ) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, 
        ymax = mean_value + se),
        width = 0.2 ) +
  theme_minimal() +
  labs(y= expression("OD"[600]*" absorbance"), 
       x="Time [h]", 
       title = "OD<sub>600</sub> Growth Curves by **Media Type** and Serial Dilution of **Starting Density**") +
  theme(plot.title = element_markdown())

ggnested(summary_df, aes(
  x = time, 
  y = mean_OD, 
  main_group = species,
  sub_group = dilution ), 
  #legend_labeling = "join",
  #join_str = ": ",
  legend_title = "Species: Starting Frequencies",
  main_keys = F,
  base_clr = "green",
  gradient_type = "shades"
) +
  geom_line() +
  geom_point() +

## Everything else ----

df_stat %>% 
  filter(wavelength %in% c("OD600")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = dilution, sample)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  # scale_color_discrete(labels = parse(text = unique(df_stat$dilution))) +
  theme_classic() +
  labs(y= expression("OD"[600]), x="Time", title = expression("OD"[600]*" of all 10"^"-3"*" Dilutions"))


df_stat %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = sample)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  theme_classic() +
  labs(y= expression("OD"[600]), x="Time", title = expression("OD"[600]*" of all 10"^"-3"*" Dilutions"))

  
ggplot(df_stat, aes(x = elapsed_hours, y = mean_value, color = sample)) +
  geom_line() +
  geom_point()
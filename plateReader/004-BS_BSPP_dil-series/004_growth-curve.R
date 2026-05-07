library(tidyverse)
library(readxl)
library(stringr)
library(patchwork)
library(RColorBrewer)
library(ggnested)
library(growthcurver)
library(ggtext)
library(plotly)

setwd("~/Documents/MSc_MMEI/MA/data/004-BS_BSPP_dil_series")

# Load, sort and arrange ---- 
## Load Excel file into datasets ----
## First timeset 
OD <- read_excel("4-BS_BSPP_dil-series_20260408_151627.xlsx", range = "B122:CU146", col_names = TRUE)  
G <- read_excel("4-BS_BSPP_dil-series_20260408_151627.xlsx", range = "B176:CU200", col_names = TRUE) # GFP70
R <- read_excel("4-BS_BSPP_dil-series_20260408_151627.xlsx", range = "B203:CU227", col_names = TRUE) # RFP90

## Second timeset
OD2 <- read_excel("4-BS_BSPP_dil-series_20260409_130939.xlsx", range = "B122:CU144", col_names = TRUE)  
G2 <- read_excel("4-BS_BSPP_dil-series_20260409_130939.xlsx", range = "B172:CU194", col_names = TRUE) # GFP70
R2 <- read_excel("4-BS_BSPP_dil-series_20260409_130939.xlsx", range = "B197:CU219", col_names = TRUE) # RFP90

# Add time difference from starting point of first dataset
OD2$`Time [s]` <- OD2$`Time [s]` + 85278
G2$`Time [s]` <- G2$`Time [s]` + 85278
R2$`Time [s]` <- R2$`Time [s]` + 85278

## Third timeset
OD3 <- read_excel("4-BS_BSPP_dil-series_20260410_120536.xlsx", range = "B122:CU141", col_names = TRUE)
G3 <- read_excel("4-BS_BSPP_dil-series_20260410_120536.xlsx", range = "B166:CU185", col_names = TRUE) # GFP70
R3 <- read_excel("4-BS_BSPP_dil-series_20260410_120536.xlsx", range = "B188:CU207", col_names = TRUE) # RFP90

# Add time difference from starting point of first dataset
OD3$`Time [s]` <- OD3$`Time [s]` + 177074
G3$`Time [s]` <- G3$`Time [s]` + 177074
R3$`Time [s]` <- R3$`Time [s]` + 177074

# Combine all three parts of dataset
OD <- bind_rows(OD, OD2, OD3)
G <- bind_rows(G, G2, G3)
R <- bind_rows(R, R2, R3)

## Reshape from wide to long format ----
OD <- OD[-2]                 # Remove Temp column
OD_long <- pivot_longer(
  data = OD,
  cols = -1,                 # All columns except the first (time)
  names_to = "well",
  values_to = "OD600"
)

G <- G[-2]                 # Remove Temp column
G_long <- pivot_longer(
  data = G,
  cols = -1,                 # All columns except the first (time)
  names_to = "well",
  values_to = "G"
)

R <- R[-2]                 # Remove Temp column
R_long <- pivot_longer(
  data = R,
  cols = -1,                 # All columns except the first (time)
  names_to = "well",
  values_to = "R"
)

# Rename first column to "time" 
colnames(OD_long)[1] <- "time"
colnames(G_long)[1] <- "time"
colnames(R_long)[1] <- "time"

# Convert time to numeric and to hours
OD_long$time <- as.numeric(OD_long$time)
G_long$time <- as.numeric(G_long$time)
R_long$time <- as.numeric(R_long$time)

OD_long$time <- OD_long$time/3600
G_long$time <- G_long$time/3600
R_long$time <- R_long$time/3600

# Merge different wavelength into one df
df <- OD_long
df$G70 <- G_long$G
df$R90 <- R_long$R

# Scale GFP70 and RFP90 to 100%
# df$G <- df$G70 * (100/70)
# df$R <- df$R90 * (100/90)

# Rename G70 to G and R90 to R
df <- df %>% rename_at('G70', ~'G')
df <- df %>% rename_at('R90', ~'R')

# Convert 'well' column to numeric 
df <- df %>%
  mutate(
    column = as.numeric(str_extract(well, "\\d+")) 
  ) 

## Specify conditions ----
df <- df %>%
  mutate(
    dilution = case_when(
      column %in% 1  ~ "10^-2",
      column %in% 2  ~ "10^-3",
      column %in% 3  ~ "10^-4",
      column %in% 4  ~ "10^-5",
      column %in% 5  ~ "10^-6",
      column %in% 6  ~ "10^-7",
      column %in% 7  ~ "10^-2",
      column %in% 8  ~ "10^-3",
      column %in% 9  ~ "10^-4",
      column %in% 10  ~ "10^-5",
      column %in% 11  ~ "10^-6",
      column %in% 12  ~ "10^-7"
    )
  )

df <- df %>%
  mutate(
    species = case_when(
      column %in% 1:6  ~ "B.subtilis",
      column %in% 7:12  ~ "B.s. + P. putida"
    )
  )

## Summary stats ----
summary_df <- df %>%
  group_by(time, species, dilution) %>%
  summarise(
    mean_OD = mean(OD600, na.rm = TRUE),
    sd_OD   = sd(OD600, na.rm = TRUE),
    n       = n(),
    se_OD   = sd_OD / sqrt(n),
    mean_G = mean(G, na.rm = TRUE),
    sd_G   = sd(G, na.rm = TRUE),
    se_G   = sd_G / sqrt(n),
    mean_R = mean(R, na.rm = TRUE),
    sd_R   = sd(R, na.rm = TRUE),
    se_R   = sd_R / sqrt(n),
    .groups = "drop"
  )

summary_df <- summary_df %>%
  mutate(condition = paste(species, dilution, sep = " "))

# Make dilution a properly ordered factor
summary_df$dilution <- factor(summary_df$dilution,
                              levels = sort(unique(summary_df$dilution)))

# PLOTS ----
## OD all, facet ---- 
#(004-BS_BSPP_dil-series.facet)
OD.all <- ggplot(summary_df, aes(
  x = time, 
  y = mean_OD, 
  color = dilution,
  #linetype = species
  )) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_OD - se_OD, ymax = mean_OD + se_OD), width = 0.2 ) +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  theme_minimal() +
  labs(y= expression("OD"[600]), x="Time [h]", title = "OD<sub>600</sub> of *P. putida* and *B. subtilis* for a serial dilution of *B.s.* starting densities") + 
  facet_wrap(~ species) +
  theme(plot.title = element_markdown())

ggplotly(OD.all)

## OD all, with ggnested ----
#(004-BS_BSPP_dil-series.ggnested)
ggnested(summary_df, aes(
  x = time, 
  y = mean_OD, 
  main_group = species,
  sub_group = dilution ), 
  legend_labeling = "join",
  join_str = ": ",
  legend_title = "Species: Starting Frequencies",
  main_keys = F,
  base_clr = "green",
  gradient_type = "shades"
  ) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_OD - se_OD, ymax = mean_OD + se_OD), width = 0.2 ) +
  #scale_colour_discrete(labels = scales::label_parse()) +
  theme_minimal() +
  labs(y= expression("OD"[600]), x="Time [h]", title = "OD<sub>600</sub> of *B. subtilis* and *P. putida* growing together for different starting frequencies of *B.s*.") +
  # scale_color_discrete(labels = function(x) parse(text = x)) +
  theme(plot.title = element_markdown())

## OD BS ---- 
#(004-BS_OD_dil-series)
OD.BS <- summary_df %>% filter(species=="B.subtilis") %>%
  ggplot(aes(
  x = time, 
  y = mean_OD, 
  color = dilution,
  #linetype = species
)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_OD - se_OD, ymax = mean_OD + se_OD), width = 0.2 ) +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  theme_minimal() +
  labs(y= expression("OD"[600]), x="Time [h]", title = "OD<sub>600</sub> of a <i>B. subtilis</i> serial dilution of starting densities") + 
  theme(plot.title = element_markdown())

ggplotly(OD.BS)

## GFP vs RFP BS+PP, facet ---- 
#(004-BS_BSPP_dil-series.facet)
Co.GFP <- summary_df %>% 
  filter(species == "B.s. + P. putida") %>%
ggplot(aes(
  x = time, 
  y = mean_G, 
  color = dilution
)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_G - se_G, ymax = mean_G + se_G), width = 0.2 ) +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  theme_minimal() +
  labs(y= "GFP", x="Time [h]") + 
  coord_cartesian(ylim = c(6000, 50000))

Co.RFP <- summary_df %>% 
  filter(species == "B.s. + P. putida") %>%
  ggplot(aes(
    x = time, 
    y = mean_R, 
    color = dilution
  )) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_R - se_R, ymax = mean_R + se_R), width = 0.2 ) +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  theme_minimal() +
  labs(y= "RFP", x="Time [h]") + 
  coord_cartesian(ylim = c(6000, 50000))

Co.GFP + Co.RFP + plot_annotation(
  title = "GFP and RFP of *P. putida* and *B. subtilis* co-culture for a serial dilution of starting densities of *B. subtilis*", 
  tag_levels = "A") +
  plot_layout(guides = 'collect') &
  theme(
    plot.title = element_markdown(),
    legend.position = 'bottom') &
  labs(color = expression("OD"[600]*" of B. subtilis at inoculation"))


## GFP & RFP combined ----
fluorescence <- summary_df %>% 
  filter(species == "B.s. + P. putida") %>%
  ggplot(aes(x = time, color = dilution)) +
  
  # GFP
  geom_line(aes(y = mean_G)) +
  geom_point(aes(y = mean_G)) +
  geom_errorbar(
    aes(ymin = mean_G - se_G, ymax = mean_G + se_G),
    width = 0.2
  ) +
  # scale_color_manual(values=c("red4","red3","red2","orange3","orange2","orange")) +
  
  # RFP
  geom_line(aes(y = mean_R), linetype = "dashed") +
  geom_point(aes(y = mean_R), shape = 17) +
  geom_errorbar(
    aes(ymin = mean_R - se_R, ymax = mean_R + se_R),
    width = 0.2
  ) +
  
  scale_color_discrete(labels = function(x) parse(text = x)) +
  theme_minimal() +
  labs(y = "GFP / RFP", x = "Time [h]") +
  coord_cartesian(ylim = c(6000, 50000))

ggplotly(fluorescence) 
fluorescence.ly <- ggplotly(fluorescence) 

## OD and Fluorescence facet ----
OD.BSPP <- summary_df %>% filter(species=="B.s. + P. putida") %>%
  ggplot(aes(
    x = time, 
    y = mean_OD, 
    color = dilution
  )) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_OD - se_OD, ymax = mean_OD + se_OD), width = 0.2 ) +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  theme_minimal() +
  # theme(legend.position = "none") +
  labs(y= expression("OD"[600]), x="Time [h]")

OD.BSPP + fluorescence + plot_annotation(
  title = "Co-culture of *B. subtilis* and *P. putida* by OD<sub>600</sub> turbidity (A) and GFP/RFP fluorsecence intensity (B) for different inoculation frequencies of *B. s.*", 
  tag_levels = "A")  +
  plot_layout(guides = 'collect') &
  theme(
    plot.title = element_markdown(),
    legend.position = 'bottom'
  )

ggplotly(OD.BSPP)


# Fluorescence per OD ----
summary_df$`GFP/OD` <- summary_df$mean_G / summary_df$mean_OD 
summary_df$`RFP/OD` <- summary_df$mean_R / summary_df$mean_OD 

BS.GFPperOD <- summary_df %>% filter(species == "B.subtilis") %>%
ggplot(aes(x=time, y=`GFP/OD`, colour = dilution)) + 
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  labs(
    x = "Time [h]",
    y = expression("OD"[600]/"GFP"),
  ) 

BS.RFPperOD <- summary_df %>% filter(species == "B.subtilis") %>%
  ggplot(aes(x=time, y=`RFP/OD`, colour = dilution)) + 
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  labs(
    x = "Time [h]",
    y = expression("OD"[600]/"RFP"),
  ) 

BS.GFPperOD + BS.RFPperOD + plot_annotation(
  title = "GFP (A) and RFP (B) fluorescence per cell density (OD<sub>600</sub>) of a *B. subtilis* serial dilution", 
  tag_levels = "A")  +
  plot_layout(guides = 'collect') &
  theme(
    plot.title = element_markdown(),
    legend.position = 'bottom'
  ) &
  labs(color = expression("Initial density (OD"[600]*")")) &
  guides(
    color = guide_legend(
      nrow = 1,
      label.position = "top"
      ))


## Growth phases shaded regions ----
summary_df %>% filter(species == "B.subtilis") %>%
  ggplot(aes(x=time, y=`GFP/OD`, colour = dilution)) + 
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  labs(
    x = "Time [h]",
    y = expression("OD"[600]/"GFP"),
  ) +
  # Add shaded regions
  annotate("rect", xmin = 0, xmax = 25, ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "blue") +
  annotate("rect", xmin = 25, xmax = 50, ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "green") +
  annotate("rect", xmin = 50, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red")


## BS GFPperOD ----
summary_df %>% filter(species == "B.subtilis") %>%
  ggplot(aes(x=time, y=`GFP/OD`, colour = dilution)) + 
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(plot.title = element_markdown()) +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  labs(
    title = "GFP fluorescence per cell density (OD<sub>600</sub>) of a *B. subtilis* serial dilution",
    x = "Time [h]",
    y = expression("GFP"/"OD"[600]),
    color = expression("Initial OD"[600]),
    subtitle = "Growth phase indicators according to the 10^-3 dilution"
  ) +
  # Growth phase indicators
  geom_vline(xintercept = c(8, 28, 50), linetype = "dashed", color = "#8C8C8C") +
  annotate("text", x = 4, y = min(summary_df$`GFP/OD`) * 1.1, label = expression(lag), color = "#8C8C8C") +
  annotate("text", x = 18, y = min(summary_df$`GFP/OD`) * 1.1, label = expression(log), color = "#8C8C8C") +
  annotate("text", x = 39, y = min(summary_df$`GFP/OD`) * 1.1, label = "stationary", color = "#8C8C8C") +
  annotate("text", x = 59, y = min(summary_df$`GFP/OD`) * 1.1, label = "death", color = "#8C8C8C")

## BS RFPperOD ----
summary_df %>% filter(species == "B.subtilis") %>%
  ggplot(aes(x=time, y=`RFP/OD`, colour = dilution)) + 
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(plot.title = element_markdown()) +
  scale_color_discrete(labels = function(x) parse(text = x)) +
  labs(
    title = "RFP fluorescence per cell density (OD<sub>600</sub>) of a *B. subtilis* serial dilution",
    x = "Time [h]",
    y = expression("RFP"/"OD"[600]),
    color = expression("Initial OD"[600]),
    subtitle = "Growth phase indicators according to the 10^-3 dilution"
  ) +
  # Growth phase indicators
  geom_vline(xintercept = c(8, 28, 50), linetype = "dashed", color = "#8C8C8C") +
  annotate("text", x = 4, y = min(summary_df$`GFP/OD`) * 1.1, label = expression(lag), color = "#8C8C8C") +
  annotate("text", x = 18, y = min(summary_df$`GFP/OD`) * 1.1, label = expression(log), color = "#8C8C8C") +
  annotate("text", x = 39, y = min(summary_df$`GFP/OD`) * 1.1, label = "stationary", color = "#8C8C8C") +
  annotate("text", x = 59, y = min(summary_df$`GFP/OD`) * 1.1, label = "death", color = "#8C8C8C")




# GFP + RFP Second Axis ----


ggnested(summary_df, aes(
  x = time, 
  y = mean_OD, 
  main_group = species,
  sub_group = dilution ), 
  legend_labeling = "join",
  join_str = ": ",
  legend_title = "Species: Starting Frequencies",
  main_keys = F,
  base_clr = "green",
  gradient_type = "shades"
)


coeff <- 2
fluo.sec.axis <- summary_df %>% 
  filter(species == "B.s. + P. putida") %>%
  ggplot(aes(
    x = time, 
    color = dilution
    )) +
  
  # GFP
  geom_line(aes(y = mean_G,)) +
  geom_point(aes(y = mean_G)) +
  geom_errorbar(
    aes(ymin = mean_G - se_G, ymax = mean_G + se_G),
    width = 0.2
  ) +
  # scale_color_manual(values=c("red4","red3","red2","orange3","orange2","orange")) +
  
  # RFP
  geom_line(aes(y = mean_R / coeff), linetype = "dashed") +
  geom_point(aes(y = mean_R / coeff), shape = 17) +
  geom_errorbar(
    aes(ymin = (mean_R - se_R) / coeff, ymax = (mean_R + se_R) / coeff,
    width = 0.2
  )) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "GFP",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="RFP")
  ) + 
  scale_color_discrete(labels = function(x) parse(text = x)) +
  theme_minimal() +
  # theme(
    # axis.title.y = element_text(color = "green", size=13),
    # axis.title.y.right = element_text(color = "red", size=13)
  # ) +
  labs(x = "Time [h]") 

ggplotly(fluo.sec.axis)



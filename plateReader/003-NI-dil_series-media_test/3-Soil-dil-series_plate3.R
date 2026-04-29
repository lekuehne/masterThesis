# Preface ----
library(tidyr)
library(dplyr)
library(readxl)
library(tibble)
library(ggplot2)
library(data.table)
setwd("~/Documents/MSc_MMEI/MA/data/003-Soil_dil_series")

# Get files ----
files <- list.files(
  path = "~/Documents/MSc_MMEI/MA/data/003-Soil_dil_series",
  pattern = "*.xlsx",
  full.names = TRUE,
)

# Plate 1 ----
data <- lapply(files, function(file_path) {
  readxl::read_excel(file_path, sheet = "Plate3")
})

## Plate Processing Pipeline (w/o sheets) ----
process_plate <- function(file_path) {
  
  df <- read_excel(file_path, sheet = "Plate3", col_names = FALSE)
  
  # Load measurement values from excel
  OD600 <- df[25:32,2:13]
  GFP100 <- df[57:64,2:13]
  GFP80 <- df[89:96,2:13]
  RFP100 <- df[121:128,2:13]
  RFP80 <- df[153:160,2:13]
  
  # Convert to long format
  OD600 <- as.data.frame(as.table(as.matrix(OD600)))
  GFP100 <- as.data.frame(as.table(as.matrix(GFP100)))
  GFP80 <- as.data.frame(as.table(as.matrix(GFP80)))
  RFP100 <- as.data.frame(as.table(as.matrix(RFP100)))
  RFP80 <- as.data.frame(as.table(as.matrix(RFP80)))
  
  colnames(OD600) <- c("row", "col", "value") # Rename columns
  colnames(GFP100) <- c("row", "col", "value") # Rename columns
  colnames(GFP80) <- c("row", "col", "value") # Rename columns
  colnames(RFP100) <- c("row", "col", "value") # Rename columns
  colnames(RFP80) <- c("row", "col", "value") # Rename columns
  
  # Fix columns
  OD600$col <- as.numeric(OD600$col) 
  GFP100$col <- as.numeric(GFP100$col) 
  GFP80$col  <- as.numeric(GFP80$col) 
  RFP100$col <- as.numeric(RFP100$col) 
  RFP80$col  <- as.numeric(RFP80$col) 
  
  # Add wavelength
  OD600$wavelength <- "OD600"
  GFP100$wavelength <- "GFP100"
  GFP80$wavelength <- "GFP80"
  RFP100$wavelength <- "RFP100"
  RFP80$wavelength <- "RFP80"
  
  df_clean <- rbindlist(list(OD600, GFP100, GFP80, RFP100, RFP80)) # Rbind (concatenate) data.tables
  
  df_clean$value <- as.numeric(as.character(df_clean$value)) # Convert values to numeric
  
  df_clean$well <- paste0(df_clean$row, df_clean$col) # Add well ID
  
  df_clean$media <- "M9"
  
  # Add soil sample
  df_clean$sample <- dplyr::recode(
    df_clean$col,
    "1" = "5",
    "2" = "5",
    "3" = "5",
    "4" = "5",
    "5" = "5",
    "6" = "5",
    "7" = "6",
    "8" = "6",
    "9" = "6",
    "10" = "6",
    "11" = "6",
    "12" = "6"
  )
  
  # Add dilution
  df_clean$dilution <- dplyr::recode(
    df_clean$col,
    "1" = "10^-2",
    "2" = "10^-3",
    "3" = "10^-4",
    "4" = "10^-5",
    "5" = "10^-6",
    "6" = "ctrl",
    "7" = "10^-2",
    "8" = "10^-3",
    "9" = "10^-4",
    "10" = "10^-5",
    "11" = "10^-6",
    "12" = "ctrl",
  )
  
  # Extract date and time from excel 
  # Extract metadata (date + time)
  date <- read_excel(file_path, sheet = "Plate3", range = "B5", col_names = FALSE)[[1]]
  time <- read_excel(file_path, sheet = "Plate3", range = "B6", col_names = FALSE)[[1]]
  
  # Combine into datetime
  datetime <- as.POSIXct(
    paste(date, time),
    format = "%Y-%m-%d %I:%M:%S %p"
  )
  
  # Add datetime and filename to dataframe
  df_clean <- df_clean %>%
    dplyr::mutate(
      datetime = datetime,
      file = basename(file_path)
    )

  return(df_clean)
}

## Execute function and combine dataset ----
combined_data<- lapply(files, process_plate) %>%
  data.table::rbindlist()

# Add elapsed time in hours (first value = 0)
combined_data$elapsed_hours <- as.numeric(
  difftime(combined_data$datetime, combined_data$datetime[1], units = "hours")
)

# Tidy dataframe
combined_data <- subset(combined_data, select = -c(row, col)) # Remove date columns
combined_data <- combined_data[, c("file", "well", "datetime", "elapsed_hours", "sample", "media", "dilution", "wavelength", "value")] # Reorder

# write to CSV
write.csv(combined_data, "all_data_plate3.csv", row.names = FALSE)

# Summary statistics
summary_stat <- combined_data %>%
  group_by(sample, dilution, wavelength, elapsed_hours, media, datetime, file) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# PLOTTING----
## OD600 all dilutions ----
summary_stat %>% 
  filter(sample %in% c("5") &
           wavelength %in% c("OD600")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = dilution, wavelength)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  theme_classic() +
  labs(y="OD 600", x="Time", title = "OD 600 of Soil Sample 5 (M9)")


## OD600 sample 6 all dilutions ----
summary_stat %>% 
  filter(sample %in% c("6") &
           wavelength %in% c("OD600")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = dilution, wavelength)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  theme_classic() +
  labs(y="OD 600", x="Time", title = "OD 600 of Soil Sample 6 (M9)")


## Facet wavelength ----
summary_stat %>% 
  filter(sample %in% c("6")) %>%
  ggplot(aes(x = elapsed_hours, y = mean_value, color = dilution)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    width = 0.5 ) +
  theme_classic() +
  labs(y="OD 600", x="Time", title = "OD 600 of all conditions") +
  facet_wrap(~ wavelength)




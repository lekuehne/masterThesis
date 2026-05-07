library(tidyverse)
library(growthcurver)
library(purrr)
library(patchwork)
library(plotly)
library(broom)

summary_BS <- summary_df %>% filter(species=="B.subtilis")


fit_gc <- function(df) {
  tryCatch({
    growthcurver::SummarizeGrowth(
      data_t = as.numeric(df$time),
      data_n = as.numeric(df$mean_OD),
      bg_correct = "min"
    )
  }, error = function(e) {
    message("Failed fit")
    return(NULL)
  })
}

gc_fits <- summary_BS %>%
  group_by(species, dilution) %>%
  nest() %>%
  mutate(gc = map(data, fit_gc))

## Apply per group 
fits <- summary_BS %>%
  group_by(dilution) %>%
  nest() %>%
  mutate(
    model = map(data, fit_gc),
    params = map(model, ~ if (!is.null(.x)) tidy(.x) else NULL)
  )

# Extract parameters (including t_mid) ----
gc_params <- gc_fits %>%
  mutate(
    t_mid = map_dbl(gc, ~ ifelse(is.null(.x), NA, .x$vals$t_mid)),
    r     = map_dbl(gc, ~ ifelse(is.null(.x), NA, .x$vals$r)),
    k     = map_dbl(gc, ~ ifelse(is.null(.x), NA, .x$vals$k))
  )

## Background per group
bg_df <- summary_BS %>%
  group_by(dilution) %>%
  summarise(bg = min(mean_OD, na.rm = TRUE), .groups = "drop")

### Join into gc_params
gc_params <- gc_params %>%
  left_join(bg_df, by = c("dilution"))

# Generate smooth fitted curves ----
predict_growth <- function(species_name) {
  gc_params %>%
    filter(species == species_name) %>%
    filter(!is.na(k), !is.na(r), !is.na(t_mid)) %>%
    rowwise() %>%
    mutate(
      pred = list({
        time_seq <- seq(0, max(summary_BS$time), length.out = 100)
        pred_vals <- k / (1 + exp(-r * (time_seq - t_mid)))
        tibble(time = time_seq, pred_OD = pred_vals + bg)
      })
    ) %>%
    ungroup() %>%
    select(dilution, pred) %>%
    unnest(pred)
}

## BS ----
BS_pred <- gc_params %>%
  filter(!is.na(k), !is.na(r), !is.na(t_mid)) %>%
  rowwise() %>%
  mutate(
    pred = list({
      time_seq <- seq(0, max(summary_BS$time), length.out = 100)

      pred_vals <- k / (1 + exp(-r * (time_seq - t_mid)))

      tibble(
        time = time_seq,
        pred_OD = pred_vals + bg
      )
    })
  ) %>%
  ungroup() %>%
  select(dilution, pred) %>%
  unnest(pred)

# BS_pred <- predict_growth("Bs")

# Plot: raw data + fitted curves ----

## BS ----
BSfit <- summary_BS %>%
  ggplot(aes(x = time, y = mean_OD, color = dilution)) +
  geom_line(alpha = 0.4) +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_OD - se_OD, ymax = mean_OD + se_OD),
    width = 0.2
  ) +
  geom_line(
    data = BS_pred,
    aes(x = time, y = pred_OD, color = dilution),
    # linetype = "dashed",
    linewidth = 0.9
  ) +
  scale_color_discrete(labels = parse(text = unique(summary_BS$dilution))) +
  theme_minimal() +
  labs(y = expression("OD"[600]), 
       x = "Time [h]",
       title = "Logistic growth curve fit"
  ) +
  theme(plot.title = element_markdown()) +
  scale_color_discrete(labels = function(x) parse(text = x)) 

ggplotly(BSfit)

##############################
# Extract time of max growth rate (t₀) ----
analysis_df <- gc_params %>%
  select(dilution, t_mid) %>%
  rename(t0 = t_mid)   # optional: keep old naming if you like

t0_points <- gc_params %>%
  filter(!is.na(k), !is.na(r), !is.na(t_mid)) %>%
  rowwise() %>%
  mutate(
    OD_t0 = list({
      # Predicted OD at t_mid
      pred_OD <- k / (1 + exp(-r * (t_mid - t_mid))) + bg
      pred_OD
    }),
    start_OD_theoretical = 10^as.numeric(gsub("10\\^", "", dilution))
  ) %>%
  ungroup() %>%
  select(dilution, t_mid, OD_t0, start_OD_theoretical)

# Get starting OD per group ----
start_od <- summary_BS %>%
  filter(time == 0) %>%
  select(species, dilution, start_OD = mean_OD)

# 5. Combine ----
analysis_df <- analysis_df %>%
  left_join(start_od, by = c("dilution"))

## Add theoretical calculated starting OD
analysis_df <- analysis_df %>%
  mutate(
    start_OD_theoretical = 10^as.numeric(gsub("10\\^", "", dilution))
  )

# Plot + linear regression ----
## BS ----
BStime <- analysis_df %>%
  ggplot(aes(x = t0, y = start_OD_theoretical, color = dilution)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_log10() +  # optional, since dilutions span orders of magnitude
  labs(
    x = "Time [h] until max. growth rate",
    y = expression("Theoretical Starting OD"[600]),
    # title = "*B. subtilis*"
    title = "Growth timing for different starting densities of *B. subtilis*"
  ) +
  theme_minimal() +
  theme(#legend.position = "none",
        plot.title = element_markdown()
        ) +
  scale_color_discrete(labels = function(x) parse(text = x))


# 7. Fit regression explicitly ----
lm_model <- lm(t0 ~ start_OD_theoretical, data = analysis_df)
summary(lm_model)

# Plot with patchwork
BSfit + BStime + plot_annotation(
  title = "***B. subtilis*: Time until max. growth rate for serial dilution of starting densities**", 
  tag_levels = "A") &
  theme(
    plot.title = element_markdown()
  )

ggplotly(BStime)  






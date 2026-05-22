library(tidyverse)
library(here)
library(ggplot2)


master <- readRDS(here::here("data_clean", "master_dataset.rds"))

dir.create(
  here::here("outputs", "figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

# histogram for the realized disaster impacts

mean_impacts <- mean(master$affected_per_100k_15_24_mean_owid, na.rm = TRUE)
median_impacts <- median(master$affected_per_100k_15_24_mean_owid, na.rm = TRUE)


binwidth_val <- 2000
x_max <- ceiling(
  max(master$affected_per_100k_15_24_mean_owid, na.rm = TRUE) / binwidth_val
) * binwidth_val

y_max <- max(
  hist(
    master$affected_per_100k_15_24_mean_owid,
    breaks = seq(0, x_max, by = binwidth_val),
    plot = FALSE
  )$counts
)

p_hist_impacts <- ggplot(
  master,
  aes(x = affected_per_100k_15_24_mean_owid)
) +
  geom_histogram(
    binwidth = binwidth_val,
    boundary = 0,
    fill = "grey70",
    color = "grey30",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = mean_impacts,
    color = "black",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = median_impacts,
    color = "black",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = mean_impacts,
    y = y_max,
    label = "Mean",
    vjust = -0.4,
    hjust = -0.1,
    family = "Times New Roman",
    size = 3
  ) +
  annotate(
    "text",
    x = median_impacts,
    y = y_max,
    label = "Median",
    vjust = -0.4,
    hjust = -0.1,
    family = "Times New Roman",
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, x_max),
    breaks = seq(0, x_max, by = 2000),
    labels = scales::comma
  ) +
  scale_y_continuous(
    limits = c(0, y_max + 5),
    breaks = seq(0, y_max + 5, by = 20),
    minor_breaks = seq(0, y_max + 5, by = 5),
    expand = expansion(mult = c(0, 0))
  )+
  labs(
    x = "Realized Disaster Impacts (per 100k)",
    y = "Number of countries"
  ) +
  theme_bw(base_size = 11) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "fig_A2_hist_realized_disaster_impacts.png"
  ),
  plot = p_hist_impacts,
  width = 6.5,
  height = 4.5,
  dpi = 300
)


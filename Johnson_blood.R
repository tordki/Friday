#Johnson and Johnson is a pharmaceutical company
#They have a  data set JohnsonJohnson about quarterly earnings 1960-1980
#They released a blood screening test in 1971.
#Can we see this release in this data set?

library(tidyverse)

# 1. Tidy the built-in JohnsonJohnson time series
jj_df <- tibble(
  year_qtr = as.numeric(time(JohnsonJohnson)),
  year     = floor(as.numeric(time(JohnsonJohnson))),
  quarter  = paste0("Q", cycle(JohnsonJohnson)),
  eps      = as.numeric(JohnsonJohnson)
)

# 2. Build panel-specific datasets so one ggplot can show two views cleanly
panel_trend <- jj_df |>
  mutate(
    panel  = "1. Quarterly Trajectory & Product Milestones (1960–1980)",
    x_val  = year_qtr,
    y_val  = eps,
    series = "Quarterly EPS"
  )

panel_season <- jj_df |>
  group_by(quarter) |>
  summarise(
    y_val = mean(eps),
    .groups = "drop"
  ) |>
  mutate(
    panel  = "2. Average EPS by Fiscal Quarter (Seasonal Pattern)",
    x_val  = as.numeric(str_remove(quarter, "Q")),
    series = if_else(quarter %in% c("Q1", "Q3"), "Peak Quarter", "Lower Quarter")
  )

plot_data <- bind_rows(panel_trend, panel_season)

# Milestone annotations (placed only in Panel 1)
milestones <- tibble(
  panel = "1. Quarterly Trajectory & Product Milestones (1960–1980)",
  x_val = c(1960.0, 1967.0, 1970.0, 1978.0),
  y_val = c(0.71, 1.53, 2.79, 11.88),
  label = c(
    "1960: Tylenol OTC",
    "1967: Haldol",
    "1970: VICRYL",
    "1978: PROXIMATE"
  )
)

# KPI summary banner text placed inside Panel 1's upper-left whitespace
kpi_banner <- tibble(
  panel = "1. Quarterly Trajectory & Product Milestones (1960–1980)",
  x_val = 1960.3,
  y_val = 14.3,
  label = paste(
    "KEY HIGHLIGHTS (1960–1980)",
    "• Annual EPS: $2.63 (1960)  →  $58.50 (1980)",
    "• Total Growth: 22.2× over 21 years",
    "• Peak Quarter: $16.20 in 1980 Q1",
    sep = "\n"
  )
)

# 3. Build the unified infographic in pure ggplot2
infographic <- ggplot(plot_data, aes(x = x_val, y = y_val)) +
  # Panel 1 layers: area + line + milestone markers
  geom_area(
    data = filter(plot_data, str_starts(panel, "1\\.")),
    fill = "#FCA5A5",
    alpha = 0.35
  ) +
  geom_line(
    data = filter(plot_data, str_starts(panel, "1\\.")),
    color = "#DC2626",
    linewidth = 1.1
  ) +
  geom_point(
    data = milestones,
    color = "#0F172A",
    size = 2.6
  ) +
  geom_label(
    data = milestones,
    aes(label = label),
    nudge_y = 1.6,
    size = 3.1,
    fill = "#FFFFFF",
    color = "#0F172A",
    label.size = 0.25
  ) +
  geom_label(
    data = kpi_banner,
    aes(label = label),
    hjust = 0,
    vjust = 1,
    size = 3.4,
    lineheight = 1.25,
    fill = "#FFFFFF",
    color = "#0F172A",
    label.size = 0.35
  ) +
  # Panel 2 layers: quarterly seasonal bar comparison
  geom_col(
    data = filter(plot_data, str_starts(panel, "2\\.")),
    aes(fill = series),
    width = 0.55
  ) +
  geom_text(
    data = filter(plot_data, str_starts(panel, "2\\.")),
    aes(label = paste0("$", round(y_val, 2))),
    vjust = -0.5,
    fontface = "bold",
    size = 3.6,
    color = "#0F172A"
  ) +
  scale_fill_manual(
    values = c("Peak Quarter" = "#DC2626", "Lower Quarter" = "#64748B"),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = \(x) paste0("$", x),
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  facet_wrap(~ panel, ncol = 1, scales = "free") +
  labs(
    title = "JOHNSON & JOHNSON: TWO DECADES OF EARNINGS GROWTH",
    subtitle = "Quarterly Earnings Per Share (1960–1980) | Built-in R dataset: JohnsonJohnson",
    x = "Year (Top Panel)  /  Quarter Number 1–4 (Bottom Panel)",
    y = "Earnings Per Share (USD)",
    caption = "Created in R using only the tidyverse (dplyr, tidyr, stringr, tibble, ggplot2)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "#F8FAFC", color = NA),
    panel.background  = element_rect(fill = "#FFFFFF", color = "#E2E8F0", linewidth = 0.5),
    strip.background  = element_rect(fill = "#1E293B", color = NA),
    strip.text        = element_text(color = "#FFFFFF", face = "bold", size = 11, hjust = 0),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "#F1F5F9"),
    plot.title        = element_text(face = "bold", size = 16, color = "#991B1B", margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 10.5, color = "#334155", margin = margin(b = 12)),
    plot.caption      = element_text(size = 9, color = "#64748B", margin = margin(t = 10)),
    plot.margin       = margin(16, 16, 16, 16)
  )

# 4. Save high-resolution infographic PNG
ggsave(
  filename = "johnson_johnson_infographic.png",
  plot     = infographic,
  width    = 2200,
  height   = 2600,
  units    = "px",
  dpi      = 220
)

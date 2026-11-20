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

# 2. Milestone annotations (including 1971)
milestones <- tibble(
  year_qtr = c(1960.0, 1967.0, 1970.0, 1971.0, 1978.0),
  eps      = c(0.71,   1.53,   2.79,   3.60,   11.88),
  label    = c(
    "1960: Tylenol OTC",
    "1967: Haldol",
    "1970: VICRYL",
    "1971: Hapindex",
    "1978: PROXIMATE"
  )
)

# 3. KPI summary banner placed in the upper-left whitespace
kpi_banner <- tibble(
  year_qtr = 1960.3,
  eps      = 14.8,
  label    = paste(
    "KEY HIGHLIGHTS (1960–1980)",
    "• Annual EPS: $2.63 (1960)  →  $58.50 (1980)",
    "• Total Growth: 22.2× over 21 years",
    "• Peak Quarter: $16.20 in 1980 Q1",
    sep = "\n"
  )
)

# 4. Build the single-panel infographic in pure ggplot2
infographic <- ggplot(jj_df, aes(x = year_qtr, y = eps)) +
  geom_area(fill = "#FCA5A5", alpha = 0.35) +
  geom_line(color = "#DC2626", linewidth = 1.1) +
  geom_point(
    data = milestones,
    aes(x = year_qtr, y = eps),
    color = "#0F172A",
    size = 2.6
  ) +
  geom_label(
    data = milestones,
    aes(x = year_qtr, y = eps, label = label),
    nudge_y = 1.6,
    size = 3.1,
    fill = "#FFFFFF",
    color = "#0F172A",
    label.size = 0.25
  ) +
  geom_label(
    data = kpi_banner,
    aes(x = year_qtr, y = eps, label = label),
    hjust = 0,
    vjust = 1,
    size = 3.5,
    lineheight = 1.25,
    fill = "#FFFFFF",
    color = "#0F172A",
    label.size = 0.35
  ) +
  scale_x_continuous(breaks = seq(1960, 1980, by = 4)) +
  scale_y_continuous(
    labels = \(x) paste0("$", x),
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  labs(
    title = "JOHNSON & JOHNSON: TWO DECADES OF EARNINGS GROWTH",
    subtitle = "Quarterly Earnings Per Share (1960–1980) | Built-in R dataset: JohnsonJohnson",
    x = "Year",
    y = "Earnings Per Share (USD)",
    caption = "Created in R using only the tidyverse"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#F8FAFC", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = "#E2E8F0", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#F1F5F9"),
    plot.title       = element_text(face = "bold", size = 16, color = "#991B1B", margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 10.5, color = "#334155", margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = "#64748B", margin = margin(t = 10)),
    plot.margin      = margin(16, 16, 16, 16)
  )

# 5. Save infographic PNG
ggsave(
  filename = "johnson_johnson_infographic.png",
  plot     = infographic,
  width    = 2200,
  height   = 1500,
  units    = "px",
  dpi      = 220
)
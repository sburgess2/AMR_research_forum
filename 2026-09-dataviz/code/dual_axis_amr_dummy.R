library(tidyverse)
library(camcorder)
library(ragg)

gg_record(
  device = "png",
  width = 10,
  height = 6,
  unit = "in",
  dpi = 300
)

font <- "Lato"
text_col <- "#3b3b3b"
bg_col <- "white"

axis_scale <- 250 / 60

amr_data <- read_csv("2026/data/dual_axis_amr_dummy.csv") |>
  mutate(quarter = factor(quarter, levels = quarter))

resistance_data <- amr_data |>
  select(
    quarter,
    e_cloacae_carbapenem_resistance,
    a_baumannii_carbapenem_resistance,
    p_aeruginosa_ceftazidime_resistance
  ) |>
  pivot_longer(-quarter, names_to = "organism", values_to = "resistance_rate") |>
  mutate(
    organism = case_match(
      organism,
      "e_cloacae_carbapenem_resistance" ~ "E. cloacae to carbapenems",
      "a_baumannii_carbapenem_resistance" ~ "A. baumannii to carbapenems",
      "p_aeruginosa_ceftazidime_resistance" ~ "P. aeruginosa to ceftazidime"
    ),
    organism = factor(
      organism,
      levels = c(
        "E. cloacae to carbapenems",
        "A. baumannii to carbapenems",
        "P. aeruginosa to ceftazidime"
      )
    ),
    scaled_rate = resistance_rate * axis_scale
  )

organism_colours <- c(
  "E. cloacae to carbapenems" = "#C00000",
  "A. baumannii to carbapenems" = "#548235",
  "P. aeruginosa to ceftazidime" = "#7030A0"
)

organism_shapes <- c(
  "E. cloacae to carbapenems" = 15,
  "A. baumannii to carbapenems" = 17,
  "P. aeruginosa to ceftazidime" = 4
)

p <- ggplot() +
  geom_col(
    data = amr_data,
    aes(x = quarter, y = cbli_ddd_1000_patient_days, fill = "C/BLI combinations"),
    width = 0.65
  ) +
  geom_line(
    data = resistance_data,
    aes(x = quarter, y = scaled_rate, colour = organism, group = organism),
    linewidth = 0.6
  ) +
  geom_point(
    data = resistance_data,
    aes(x = quarter, y = scaled_rate, colour = organism, shape = organism),
    size = 2
  ) +
  scale_y_continuous(
    name = "DDDs/1000 patient-days",
    breaks = seq(0, 250, 50),
    limits = c(0, 250),
    expand = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(
      ~ . / axis_scale,
      name = "Resistance rates (%)",
      breaks = seq(0, 60, 10)
    )
  ) +
  scale_fill_manual(values = c("C/BLI combinations" = "#4472C4"), name = NULL) +
  scale_colour_manual(values = organism_colours, name = NULL) +
  scale_shape_manual(values = organism_shapes, name = NULL) +
  labs(x = NULL) +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, color = text_col),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 7,
      color = text_col
    ),
    axis.text.y = element_text(color = text_col),
    axis.title.y = element_text(color = text_col),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )

ggsave(
  plot = p,
  filename = "2026/output/dual_axis_amr_dummy.png",
  width = 10,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

#Code from Cara Thompson https://github.com/cararthompson/rmedicine2023-workshop/blob/main/level-up-workshop.qmd

library(tidyverse)
library(ggtext)
library(janitor)
library(ragg)

vit_c_palette <- c(
  "Orange Juice" = "#fab909",
  "Vitamin C" = "#E93603",
  light_text = "#323A30",
  dark_text = "#0C1509"
)

basic_plot <- ToothGrowth |>
  mutate(
    supplement = case_when(
      supp == "OJ" ~ "Orange Juice",
      supp == "VC" ~ "Vitamin C",
      TRUE ~ as.character(supp)
    )
  ) |>
  group_by(supplement, dose) |>
  summarise(mean_length = mean(len)) |>
  mutate(categorical_dose = factor(dose)) |>
  ggplot(aes(x = categorical_dose, y = mean_length, fill = supplement)) +
  geom_bar(aes(alpha = dose), stat = "identity", colour = "#FFFFFF", size = 2) +
  labs(
    x = "Dose",
    y = "Mean length (mm)",
    title = "In smaller doses, Orange Juice was associated with greater mean tooth growth,
compared to equivalent doses of Vitamin C",
    subtitle = "With the highest dose, the mean recorded length was almost identical."
  ) +
  scale_fill_manual(values = vit_c_palette, limits = force) +
  scale_alpha(range = c(0.4, 1)) +
  scale_x_discrete(breaks = c("0.5", "1", "2"), labels = function(x) {
    paste0(x, " mg/day")
  }) +
  coord_flip() +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal(base_size = 15)

themed_plot <- basic_plot +
  labs(
    title = paste0(
      "In smaller doses, **<span style='color:",
      vit_c_palette["Orange Juice"],
      "'>Orange Juice</span>**
                      was associated with greater mean tooth growth,
                      compared to equivalent doses of **<span style='color:",
      vit_c_palette["Vitamin C"],
      "'>Vitamin C</span>**"
    )
  ) +
  theme(
    legend.position = "none",
    text = element_text(colour = vit_c_palette["light_text"], family = "Cabin"),
    axis.title.y = element_blank(),
    plot.title = ggtext::element_textbox_simple(
      colour = vit_c_palette["dark_text"],
      size = rel(1.5),
      face = "bold",
      family = "Enriqueta",
      lineheight = 1.3,
      margin = margin(0.5, 0, 1, 0, "lines")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = "Cabin",
      size = rel(1.1),
      lineheight = 1.3,
      margin = margin(0, 0, 1, 0, "lines")
    ),
    strip.text = element_text(
      family = "Enriqueta",
      colour = vit_c_palette["light_text"],
      size = rel(1.1),
      face = "bold",
      margin = margin(2, 0, 0.5, 0, "lines")
    ),
    axis.text = element_text(colour = vit_c_palette["light_text"])
  )

vitc_plot <- themed_plot +
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(
    strip.text = element_text(hjust = 0.03),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  scale_colour_identity()

vitc_plot

ggsave(
  plot = vitc_plot,
  filename = "2026/output/vitc_tooth_growth.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = "white",
  dpi = 300,
  device = agg_png
)

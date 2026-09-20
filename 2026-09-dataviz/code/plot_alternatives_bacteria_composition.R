library(tidyverse)
library(camcorder)
library(ragg)
library(ggbeeswarm)
library(ggtintshade)

gg_record(
  device = "png",
  width = 8,
  height = 6,
  unit = "in",
  dpi = 300
)

font <- "Lato"
text_col <- "#3b3b3b"
bg_col <- "white"

species_levels <- c(
  "E. coli",
  "K. pneumoniae",
  "P. aeruginosa",
  "A. baumannii",
  "S. aureus",
  "Other"
)

species_colours <- c(
  "E. coli" = "#66C2A5",
  "K. pneumoniae" = "#FC8D62",
  "P. aeruginosa" = "#8DA0CB",
  "A. baumannii" = "#E78AC3",
  "S. aureus" = "#A6D854",
  "Other" = "#E5C494"
)

composition_data <- read_csv(
  "2026-09-dataviz/data/bacteria_composition_dummy.csv"
) |>
  mutate(species = factor(species, levels = species_levels))

stacked_bar <- ggplot(
  composition_data,
  aes(x = factor(year), y = percentage, fill = species)
) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = species_colours, name = NULL) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12, color = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
stacked_bar

record_polaroid()
ggsave(
  plot = stacked_bar,
  filename = "2026-09-dataviz/output/ex_stacked_bar.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

small_multiples_order <- composition_data |>
  filter(year == max(year)) |>
  arrange(desc(percentage)) |>
  pull(species) |>
  as.character()

composition_data_ordered <- composition_data |>
  mutate(
    species = factor(as.character(species), levels = small_multiples_order)
  )

background_lines <- composition_data_ordered |>
  select(year, percentage, bg_species = species)

small_multiples <- ggplot(
  composition_data_ordered,
  aes(x = year, y = percentage)
) +
  geom_line(
    data = background_lines,
    aes(x = year, y = percentage, group = bg_species),
    colour = "grey85",
    linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  geom_line(aes(colour = species), linewidth = 1.8) +
  #geom_point(aes(colour = species), size = 1.8) +
  facet_wrap(~species, nrow = 2) +
  scale_x_continuous(breaks = seq(2016, 2025, 3)) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    breaks = seq(0, 40, 10),
    limits = c(0, 40),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual(values = species_colours, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    axis.text = element_text(size = 12, color = text_col),
    axis.title.y = element_text(size = 12, color = text_col),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
small_multiples

record_polaroid()
ggsave(
  plot = small_multiples,
  filename = "2026-09-dataviz/output/alt_line.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

set.seed(42)

resistance_levels <- c(
  "E. coli",
  "Klebsiella pneumoniae",
  "Pseudomonas aeruginosa",
  "Acinetobacter baumannii",
  "Staphylococcus aureus",
  "Enterococcus faecium"
)

resistance_colours <- c(
  "E. coli" = "#66C2A5",
  "Klebsiella pneumoniae" = "#FC8D62",
  "Pseudomonas aeruginosa" = "#8DA0CB",
  "Acinetobacter baumannii" = "#E78AC3",
  "Staphylococcus aureus" = "#A6D854",
  "Enterococcus faecium" = "#E5C494"
)

n_hospitals <- 20

resistance_samples <- tibble(
  species = resistance_levels,
  mean_resistance = c(45, 60, 35, 70, 25, 50),
  sd_resistance = c(12, 15, 10, 18, 8, 20)
) |>
  reframe(
    resistance_pct = pmin(
      pmax(rnorm(n_hospitals, mean_resistance, sd_resistance), 0),
      100
    ),
    .by = species
  ) |>
  mutate(
    species = factor(species, levels = resistance_levels),
    x_pos = as.numeric(species)
  )

resistance_summary <- resistance_samples |>
  summarise(
    mean_resistance = mean(resistance_pct),
    sd_resistance = sd(resistance_pct),
    x_pos = first(x_pos),
    .by = species
  )

bar_with_errorbars <- ggplot(
  resistance_summary,
  aes(x = species, y = mean_resistance, fill = species)
) +
  geom_col(width = 0.6) +
  geom_errorbar(
    aes(
      ymin = mean_resistance - sd_resistance,
      ymax = mean_resistance + sd_resistance
    ),
    width = 0.2,
    colour = text_col
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = resistance_colours, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    axis.text.x = element_text(
      size = 12,
      angle = 45,
      hjust = 1,
      color = text_col
    ),
    axis.text.y = element_text(size = 12, color = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
bar_with_errorbars

record_polaroid()
ggsave(
  plot = bar_with_errorbars,
  filename = "2026-09-dataviz/output/ex_bar_errorbars.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

beeswarm_order <- resistance_summary |>
  arrange(desc(mean_resistance)) |>
  pull(species) |>
  as.character()

resistance_samples_ordered <- resistance_samples |>
  mutate(species = factor(as.character(species), levels = beeswarm_order))

resistance_summary_ordered <- resistance_summary |>
  mutate(
    species = factor(as.character(species), levels = beeswarm_order),
    x_pos = as.numeric(species)
  )

beeswarm_plot <- ggplot(
  resistance_samples_ordered,
  aes(x = species, y = resistance_pct)
) +
  geom_beeswarm(
    shape = 21,
    colour = "black",
    fill = "#FFE9A8",
    alpha = 0.7,
    size = 2,
    cex = 3
  ) +
  geom_segment(
    data = resistance_summary_ordered,
    aes(
      x = x_pos - 0.3,
      xend = x_pos + 0.3,
      y = mean_resistance,
      yend = mean_resistance
    ),
    colour = "black",
    linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(
    #name = "Resistance rate (%)",
    labels = scales::label_percent(scale = 1),
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    axis.text.x = element_text(size = 12, color = text_col),
    axis.text.y = element_text(size = 12, color = text_col),
    axis.title.y = element_text(size = 12, color = text_col),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
beeswarm_plot
record_polaroid()
ggsave(
  plot = beeswarm_plot,
  filename = "2026-09-dataviz/output/species_resistance_beeswarm.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

antibiotic_levels <- c("Antibiotic 1", "Antibiotic 2")

consumption_data <- read_csv(
  "2026-09-dataviz/data/antibiotic_consumption_dummy.csv"
) |>
  mutate(antibiotic = factor(antibiotic, levels = antibiotic_levels))

bad_line_errorbars <- ggplot(
  consumption_data,
  aes(x = year, y = mean_ddd, colour = antibiotic)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = mean_ddd - sd_ddd, ymax = mean_ddd + sd_ddd),
    width = 0.3
  ) +
  scale_x_continuous(breaks = seq(2016, 2025, 3)) +
  labs(x = NULL, y = NULL) +
  theme_grey(base_size = 12) +
  theme(axis.text = element_text(size = 12))

bad_line_errorbars
record_polaroid()
ggsave(
  plot = bad_line_errorbars,
  filename = "2026-09-dataviz/output/antibiotic_consumption_bad_errorbars.png",
  width = 8,
  height = 6,
  unit = "in",
  dpi = 300,
  device = agg_png
)

makeover_colours <- c(
  "Antibiotic 1" = "grey60",
  "Antibiotic 2" = "#0074D9"
)

line_end_labels <- consumption_data |>
  filter(year == max(year))

makeover_ribbon <- ggplot(
  consumption_data,
  aes(x = year, y = mean_ddd, colour = antibiotic, fill = antibiotic)
) +
  geom_ribbon(
    aes(ymin = mean_ddd - sd_ddd, ymax = mean_ddd + sd_ddd),
    alpha = 0.2,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  geom_text(
    data = line_end_labels,
    aes(label = antibiotic),
    hjust = -0.1,
    family = font,
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(2016, 2025, 3),
    limits = c(2016, 2028),
    expand = expansion(mult = c(0.02, 0))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(values = makeover_colours, guide = "none") +
  scale_fill_manual(values = makeover_colours, guide = "none") +
  labs(x = NULL, y = "NULL") +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    axis.text = element_text(size = 12, color = text_col),
    axis.title.y = element_text(size = 12, color = text_col),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
makeover_ribbon

record_polaroid()
ggsave(
  plot = makeover_ribbon,
  filename = "2026-09-dataviz/output/alt_ribbon.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

stewardship_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Neutral",
  "Agree",
  "Strongly agree"
)

stewardship_data <- read_csv(
  "2026-09-dataviz/data/stewardship_survey_dummy.csv"
) |>
  mutate(response = factor(response, levels = stewardship_levels))

bad_clustered_bar <- ggplot(
  stewardship_data,
  aes(x = statement, y = percentage, fill = response)
) +
  geom_col(position = position_dodge(), width = 0.8) +
  labs(x = NULL, y = NULL) +
  theme_grey(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 12, angle = 40, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
bad_clustered_bar

record_polaroid()
ggsave(
  plot = bad_clustered_bar,
  filename = "2026-09-dataviz/output/stewardship_bad_clustered_bar.png",
  width = 8,
  height = 6,
  unit = "in",
  dpi = 300,
  device = agg_png
)


record_polaroid()
stewardship_wide <- stewardship_data |>
  pivot_wider(names_from = response, values_from = percentage) |>
  mutate(
    net_score = (Agree + `Strongly agree`) - (Disagree + `Strongly disagree`)
  ) |>
  arrange(net_score) |>
  mutate(
    statement = factor(statement, levels = statement),
    y_num = row_number(),
    neutral_half = Neutral / 2
  )

diverging_segments <- bind_rows(
  stewardship_wide |>
    transmute(
      statement,
      y_num,
      response = "Strongly disagree",
      xmax = -neutral_half - Disagree,
      xmin = xmax - `Strongly disagree`
    ),
  stewardship_wide |>
    transmute(
      statement,
      y_num,
      response = "Disagree",
      xmax = -neutral_half,
      xmin = xmax - Disagree
    ),
  stewardship_wide |>
    transmute(
      statement,
      y_num,
      response = "Neutral",
      xmin = -neutral_half,
      xmax = neutral_half
    ),
  stewardship_wide |>
    transmute(
      statement,
      y_num,
      response = "Agree",
      xmin = neutral_half,
      xmax = xmin + Agree
    ),
  stewardship_wide |>
    transmute(
      statement,
      y_num,
      response = "Strongly agree",
      xmin = neutral_half + Agree,
      xmax = xmin + `Strongly agree`
    )
) |>
  mutate(response = factor(response, levels = stewardship_levels))

diverging_colours <- c(
  "Strongly disagree" = "#A65858",
  "Disagree" = "#E3BFBB",
  "Neutral" = "#B3B3B3",
  "Agree" = "#B8D0DA",
  "Strongly agree" = "#4A7C96"
)

diverging_stacked_bar <- ggplot(diverging_segments) +
  geom_rect(
    aes(
      ymin = y_num - 0.4,
      ymax = y_num + 0.4,
      xmin = xmin,
      xmax = xmax,
      fill = response
    ),
    colour = "white",
    linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, colour = text_col, linewidth = 0.4) +
  scale_y_continuous(
    breaks = stewardship_wide$y_num,
    labels = stewardship_wide$statement
  ) +
  scale_x_continuous(
    labels = function(x) scales::label_percent(scale = 1)(abs(x))
  ) +
  scale_fill_manual(values = diverging_colours, name = NULL) +
  labs(x = "% of respondents", y = NULL) +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8, color = text_col),
    axis.text = element_text(size = 12, color = text_col),
    axis.title.x = element_text(size = 12, color = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
diverging_stacked_bar
record_polaroid()
ggsave(
  plot = diverging_stacked_bar,
  filename = "2026-09-dataviz/output/alt_diverging_stacked_bar.png",
  width = 9,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

tintshade_data <- bind_rows(
  stewardship_wide |>
    transmute(
      statement,
      value = -`Strongly disagree`,
      sentiment_group = "Disagree",
      intensity = "Strong"
    ),
  stewardship_wide |>
    transmute(
      statement,
      value = -Disagree,
      sentiment_group = "Disagree",
      intensity = "Moderate"
    ),
  stewardship_wide |>
    transmute(
      statement,
      value = -Neutral / 2,
      sentiment_group = "Neutral",
      intensity = "Neutral"
    ),
  stewardship_wide |>
    transmute(
      statement,
      value = Neutral / 2,
      sentiment_group = "Neutral",
      intensity = "Neutral"
    ),
  stewardship_wide |>
    transmute(
      statement,
      value = Agree,
      sentiment_group = "Agree",
      intensity = "Moderate"
    ),
  stewardship_wide |>
    transmute(
      statement,
      value = `Strongly agree`,
      sentiment_group = "Agree",
      intensity = "Strong"
    )
) |>
  mutate(
    sentiment_group = factor(
      sentiment_group,
      levels = c("Disagree", "Neutral", "Agree")
    ),
    intensity = factor(intensity, levels = c("Strong", "Moderate", "Neutral"))
  )

tintshade_colours <- c(
  "Disagree" = "#A65858",
  "Neutral" = "#B3B3B3",
  "Agree" = "#4A7C96"
)

diverging_tintshade <- ggplot(
  tintshade_data,
  aes(x = statement, y = value, fill = sentiment_group, tintshade = intensity)
) +
  geom_col_tintshade(width = 0.7) +
  geom_hline(yintercept = 0, colour = text_col, linewidth = 0.4) +
  coord_flip() +
  scale_fill_manual(values = tintshade_colours, name = NULL) +
  scale_tintshade_discrete(name = "Strength") +
  scale_y_continuous(
    labels = function(x) scales::label_percent(scale = 1)(abs(x))
  ) +
  labs(x = NULL, y = "% of respondents") +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8, color = text_col),
    axis.text = element_text(size = 12, color = text_col),
    axis.title.x = element_text(size = 12, color = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
diverging_tintshade
ggsave(
  plot = diverging_tintshade,
  filename = "2026-09-dataviz/output/stewardship_diverging_tintshade.png",
  width = 9,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

class_data <- read_csv(
  "2026-09-dataviz/data/antibiotic_class_prescribing_dummy.csv"
)

bad_pie <- ggplot(
  class_data,
  aes(x = "", y = percentage, fill = antibiotic_class)
) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 12) +
  theme(legend.title = element_blank())
bad_pie
ggsave(
  plot = bad_pie,
  filename = "2026-09-dataviz/output/antibiotic_class_bad_pie.png",
  width = 9,
  height = 6,
  unit = "in",
  dpi = 300,
  device = agg_png
)

class_colours <- class_data |>
  mutate(
    colour = if_else(antibiotic_class == "Cephalosporins", "#0072B2", "grey70")
  ) |>
  select(antibiotic_class, colour) |>
  deframe()

makeover_bar <- ggplot(
  class_data,
  aes(
    x = fct_reorder(antibiotic_class, percentage),
    y = percentage,
    fill = antibiotic_class
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = class_colours, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 12) +
  theme(
    axis.text = element_text(size = 12, color = text_col),
    axis.title = element_text(size = 12, color = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
makeover_bar
ggsave(
  plot = makeover_bar,
  filename = "2026-09-dataviz/output/antibiotic_class_makeover_bar.png",
  width = 9,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

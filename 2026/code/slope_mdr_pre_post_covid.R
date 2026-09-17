library(tidyverse)
library(ggtext)
library(ggrepel)
library(camcorder)
library(ragg)

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


highlight_colour <- "#D55E00"
grey_colour <- "grey75"
grey_text_colour <- "grey50"

plot_title <- "MDR decreased across ICU pathogens post-COVID, but Acinetobacter spp. had the greatest proportion of MDR isolates"

mdr_data <- read_csv("2026/data/amr_mdr_pdr_pre_post_covid.csv") |>
  filter(category %in% c("MDR", "PDR"), species != "Other") |>
  summarise(percentage = sum(percentage), .by = c(period, species)) |>
  mutate(
    period = factor(period, levels = c("Pre-COVID-19", "Post-COVID-19")),
    x = if_else(period == "Pre-COVID-19", 1, 2)
  )

species_order <- mdr_data |>
  filter(period == "Pre-COVID-19") |>
  arrange(desc(percentage)) |>
  pull(species)

mdr_data <- mdr_data |>
  mutate(
    species = factor(species, levels = species_order),
    is_acinetobacter = species == "Acinetobacter spp.",
    line_colour = if_else(is_acinetobacter, highlight_colour, grey_colour),
    label_colour = if_else(
      is_acinetobacter,
      highlight_colour,
      grey_text_colour
    ),
    label_face = if_else(is_acinetobacter, "bold", "plain"),
    label_size = if_else(is_acinetobacter, 3.2, 2.6)
  )

pre_labels <- mdr_data |>
  filter(period == "Pre-COVID-19") |>
  mutate(label = glue::glue("{species} ({round(percentage, 1)}%)"))

post_labels <- mdr_data |>
  filter(period == "Post-COVID-19") |>
  mutate(label = glue::glue("{round(percentage, 1)}%"))

stack_data <- read_csv("2026/data/amr_mdr_pdr_pre_post_covid.csv") |>
  mutate(
    period = factor(period, levels = c("Pre-COVID-19", "Post-COVID-19")),
    species = factor(species, levels = unique(species)),
    category = factor(category, levels = c("MDR", "PDR", "NON-MDR/PDR"))
  ) |>
  arrange(period, species, category) |>
  group_by(period, species) |>
  mutate(
    ymax = cumsum(percentage),
    ymin = ymax - percentage,
    ycenter = (ymin + ymax) / 2,
    is_zero = percentage == 0
  ) |>
  ungroup() |>
  mutate(
    label = as.character(round(percentage, 2)),
    label_colour = if_else(percentage < 6, "grey20", "white"),
    x_num = as.numeric(species),
    label_x = if_else(is_zero, x_num + 0.45, x_num)
  )

stack_colours <- c(
  "MDR" = "#4472C4",
  "PDR" = "#FFC000",
  "NON-MDR/PDR" = "#C00000"
)

p <- ggplot(stack_data, aes(x = species, y = percentage, fill = category)) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  geom_text(
    aes(x = label_x, y = ycenter, label = label, color = label_colour),
    angle = 90,
    family = font,
    size = 2.6,
    show.legend = FALSE
  ) +
  facet_wrap(~period, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = stack_colours, name = NULL) +
  scale_color_identity() +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL, y = "%") +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 8, color = text_col),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = text_col
    ),
    axis.text.y = element_text(color = text_col),
    axis.title.y = element_text(angle = 0, vjust = 1.05, color = text_col),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey55", linewidth = 0.3),
    panel.spacing = unit(2, "pt"),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
p
ggsave(
  plot = p,
  filename = "2026/output/stacked_original.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

declutter_data <- stack_data |>
  filter(species != "Other") |>
  mutate(species = fct_drop(species))

p_declutter <- ggplot(
  declutter_data,
  aes(x = species, y = percentage, fill = category)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  facet_wrap(~period, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = stack_colours, name = NULL) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 8, color = text_col),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = text_col
    ),
    axis.text.y = element_text(color = text_col),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = text_col),
    panel.grid = element_blank(),
    panel.spacing = unit(2, "pt"),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
p_declutter

ggsave(
  plot = p_declutter,
  filename = "2026/output/stacked_declutter.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)


purposeful_colours <- c(
  "MDR" = "#fdae6b",
  "PDR" = "#f16913",
  "NON-MDR/PDR" = "#D9D9D9"
)


#727272
#f1595f
#79c36a
#599ad3
#f9a65a
#9e66ab
#cd7058
#d77fb3

p_colour <- ggplot(
  declutter_data,
  aes(x = species, y = percentage, fill = category)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  facet_wrap(~period, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = purposeful_colours, name = NULL) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 8, color = text_col),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = text_col
    ),
    axis.text.y = element_text(color = text_col),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = text_col),
    panel.grid = element_blank(),
    panel.spacing = unit(2, "pt"),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
p_colour
ggsave(
  plot = p_colour,
  filename = "2026/output/stacked_colour.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

reordered_species <- declutter_data |>
  filter(period == "Pre-COVID-19", category %in% c("MDR", "PDR")) |>
  summarise(combined = sum(percentage), .by = species) |>
  arrange(desc(combined)) |>
  pull(species) |>
  as.character()

reordered_data <- declutter_data |>
  mutate(species = factor(as.character(species), levels = reordered_species))

p_reorderd <- ggplot(
  reordered_data,
  aes(x = species, y = percentage, fill = category)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  facet_wrap(~period, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = purposeful_colours, name = NULL) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 8, color = text_col),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = text_col
    ),
    axis.text.y = element_text(color = text_col),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = text_col),
    panel.grid = element_blank(),
    panel.spacing = unit(2, "pt"),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
p_reorderd
ggsave(
  plot = p_colour,
  filename = "2026/output/stacked_reordered.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

p_horizontal <- ggplot(
  reordered_data,
  aes(x = species, y = percentage, fill = category)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  facet_wrap(~period, ncol = 1, strip.position = "top") +
  scale_fill_manual(values = purposeful_colours, name = NULL) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 8, color = text_col),
    axis.text.x = element_text(color = text_col),
    axis.text.y = element_text(color = text_col),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = text_col, hjust = 0),
    panel.grid = element_blank(),
    panel.spacing = unit(10, "pt"),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )
p_horizontal

ggsave(
  plot = p_horizontal,
  filename = "2026/output/stacked_horizontal.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

gutter <- 15

mirror_data <- reordered_data |>
  filter(category %in% c("MDR", "PDR")) |>
  mutate(
    y_num = as.numeric(fct_rev(species)),
    x_min = if_else(period == "Pre-COVID-19", -(ymax + gutter), ymin + gutter),
    x_max = if_else(period == "Pre-COVID-19", -(ymin + gutter), ymax + gutter)
  )

species_labels <- mirror_data |>
  distinct(y_num, species)

n_species <- max(mirror_data$y_num)

period_labels <- mirror_data |>
  summarise(max_extent = max(abs(c(x_min, x_max))), .by = period) |>
  mutate(
    label = as.character(period),
    x = (gutter + max_extent) / 2,
    x = if_else(period == "Pre-COVID-19", -x, x)
  )

ggplot(mirror_data) +
  geom_rect(
    aes(
      ymin = y_num - 0.4,
      ymax = y_num + 0.4,
      xmin = x_min,
      xmax = x_max,
      fill = category
    ),
    color = "white",
    linewidth = 0.3
  ) +
  geom_text(
    data = species_labels,
    aes(x = 0, y = y_num, label = species),
    family = font,
    color = text_col,
    size = 3,
    hjust = 0.5
  ) +
  geom_text(
    data = period_labels,
    aes(x = x, y = n_species + 1.2, label = label),
    family = font,
    fontface = "bold",
    color = text_col,
    size = 4.23
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  scale_x_continuous(
    breaks = seq(-100, 100, 25),
    labels = function(x) {
      scales::label_percent(scale = 1)(pmax(abs(x) - gutter, 0))
    },
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_fill_manual(values = purposeful_colours, name = NULL) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "none",
    #legend.justification = "right",
    legend.title = element_blank(),
    #legend.key.size = unit(0.8, "lines"),
    #legend.text = element_text(size = 8, color = text_col),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = text_col),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    plot.margin = margin(25, 10, 10, 10)
  )
record_polaroid()

ggsave(
  plot = p_mirrored,
  filename = "2026/output/stacked_mirrored.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

p_slope <- ggplot(mdr_data, aes(x = x, y = percentage, group = species)) +
  geom_line(aes(color = line_colour, linewidth = is_acinetobacter)) +
  geom_point(aes(color = line_colour, size = is_acinetobacter)) +
  labs(
    title = plot_title,
    caption = "Data: Golli et al. (2024), Pharmaceuticals 17(4):407",
    x = NULL,
    y = NULL
  ) +
  scale_color_identity() +
  scale_linewidth_manual(
    values = c(`TRUE` = 1.6, `FALSE` = 0.6),
    guide = "none"
  ) +
  scale_size_manual(values = c(`TRUE` = 3, `FALSE` = 2), guide = "none") +
  scale_x_continuous(limits = c(-2.4, 3.7)) +
  scale_y_continuous(limits = c(0, 112)) +
  geom_text_repel(
    data = pre_labels,
    aes(label = label, color = label_colour, fontface = label_face, size = label_size),
    hjust = 1,
    direction = "y",
    nudge_x = -0.1,
    segment.color = NA,
    family = font,
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = post_labels,
    aes(label = label, color = label_colour, fontface = label_face, size = label_size),
    hjust = 0,
    direction = "y",
    nudge_x = 0.1,
    segment.color = NA,
    family = font,
    show.legend = FALSE
  ) +
  scale_size_identity() +
  annotate(
    "text",
    x = c(1, 2),
    y = 108,
    label = c("Pre-COVID-19", "Post-COVID-19"),
    family = font,
    fontface = "plain",
    color = grey_text_colour,
    size = 2.8
  ) +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      color = text_col,
      face = "bold",
      size = 13,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      color = grey_text_colour,
      size = 8,
      hjust = 0,
      margin = margin(t = 10)
    ),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )

ggsave(
  plot = p_horizontal,
  filename = "2026/output/stacked_horizontal.png",
  width = 8,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

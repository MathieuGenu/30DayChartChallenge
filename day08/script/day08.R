library(pacman)
p_load(tidyverse, readxl, cowplot, ggtext, showtext)

showtext_auto()
showtext_opts(dpi = 300)

font_add_google(name = "Roboto", family = "roboto")


data <- read_xls("day08/data/Table Ciqual 2020_ENG_2020 07 07.xls")

data <- data |>
  mutate(across(everything(), ~ ifelse(str_detect(.x, "-"), NA, .x))) |>
  mutate(across(everything(), ~ ifelse(str_detect(.x, "<"), 0, .x))) |>
  mutate(across(
    `Energy, Regulation EU No 1169/2011 (kJ/100g)`:`Vitamin B12 (µg/100g)`,
    ~ str_replace(.x, ",", ".")
  )) |>
  mutate(across(
    `Energy, Regulation EU No 1169/2011 (kJ/100g)`:`Vitamin B12 (µg/100g)`,
    as.numeric
  )) |>
  rename(
    E_kcal_EU = `Energy, Regulation EU No 1169/2011 (kcal/100g)`,
    protein = `Protein (g/100g)`,
    carbs = `Carbohydrate (g/100g)`,
    fat = `Fat (g/100g)`,
    sugars = `Sugars (g/100g)`,
    fibres = `Fibres (g/100g)`
  )

# data |>
#   count(alim_grp_nom_eng, alim_ssgrp_nom_eng, alim_ssssgrp_nom_eng) |>
#   view()

data_macro <- data |>
  drop_na(alim_grp_nom_eng) |>
  group_by(alim_grp_nom_eng) |>
  summarize(
    across(
      c(E_kcal_EU, protein, carbs, fat),
      ~ mean(.x, na.rm = T),
      .names = "mean_{.col}"
    )
  ) |>
  mutate(alim_grp_nom_eng = fct_reorder(alim_grp_nom_eng, desc(mean_E_kcal_EU)))


col_cat <- c(
  "baby food" = "#00cfb3ff",
  "beverages" = "#90CAEE",
  "cereal products" = "#A96033",
  "fats and oils" = "#FFD951",
  "fruits, vegetables, legumes and nuts" = "#009B74",
  "ice cream and sorbet" = "#FFC1B4",
  "meat, egg and fish" = "#FF5F5F",
  "milk and milk products" = "#0090CF",
  "miscellaneous" = "#8a0bc5ff",
  "starters and dishes" = "#c010b1ff",
  "sugar and confectionery" = "#f74129ff"
)

col_bg <- "#eed5a7ff"

bars <- data_macro |>
  ggplot(aes(
    x = alim_grp_nom_eng,
    y = mean_E_kcal_EU,
    fill = alim_grp_nom_eng
  )) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(
    aes(
      label = paste0("  ", round(mean_E_kcal_EU), " kcal/100g   "),
      hjust = mean_E_kcal_EU > 200
    ),
    size = 4,
    fontface = "bold",
    family = "Spline Sans"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  scale_fill_manual(values = col_cat) +
  labs(
    title = "Energy content and macronutrients in the<br>most commonly consumed foods in France",
    caption = "By MathieuGenu\nData: Ciqual 2020 Food Composition Table"
  ) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(alim_grp_nom_eng), ncol = 1, scales = "free_y") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(
      hjust = 0,
      margin = margin(1, 0, 1, 0),
      size = rel(1.1),
      face = "bold"
    ),
    panel.grid = element_blank(),
    plot.margin = margin(10, 170, 30, 20),
    plot.title = element_markdown(
      size = 20,
      family = "roboto",
      # hjust = .3,
      margin = margin(b = 50)
    ),
    plot.caption = element_text(vjust = -0.5),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background = element_rect(fill = col_bg, colour = NA)
  )

cols_macro <- c(
  "Proteins" = "#0995f1",
  "Carbs" = "#d31717ff",
  "Fat" = "#dbce10ff"
)

donut <- data_macro |>
  pivot_longer(
    cols = c(mean_protein, mean_carbs, mean_fat),
    names_to = "macro",
    values_to = "macro_val"
  ) |>
  mutate(
    macro = case_when(
      macro == "mean_protein" ~ "Proteins",
      macro == "mean_carbs" ~ "Carbs",
      macro == "mean_fat" ~ "Fat"
    )
  ) |>
  ggplot(aes(x = alim_grp_nom_eng, y = macro_val, fill = macro)) +
  geom_col() +
  scale_fill_manual(values = cols_macro) +
  # ylim(-20,NA) +
  facet_wrap(vars(alim_grp_nom_eng), scales = "free", ncol = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    strip.text = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background = element_rect(fill = col_bg, colour = NA)
  )

set_null_device("png")
g <- ggdraw() +
  draw_plot(bars) +
  draw_plot(donut, x = 0.32, y = 0.05, height = 0.85)


ggsave(
  plot = g,
  filename = "day08/graph/composition_most_eaten_food_in_FR.png",
  device = ragg::agg_png,
  width = 7,
  height = 12,
  dpi = 300
)

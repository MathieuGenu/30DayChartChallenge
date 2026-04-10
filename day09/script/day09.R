library(pacman)
p_load(tidyverse, readxl, cowplot, ggimage, ggtext, showtext)

showtext_auto()
showtext_opts(dpi = 300)

font_add_google(name = "Roboto", family = "roboto")

data <- read_xlsx("day09/data/indice_niveau_de_vie_europe_2023.xlsx")

bg_col <- "#0f0a24ff"
text_col <- "#f0efebff"
col_country <- c(
  "EU" = "#db2929ff",
  "other" = "#1970e2ff"
)

g <- data |>
  mutate(
    code = case_when(
      Pays == "Allemagne" ~ "DE",
      Pays == "Autriche" ~ "AT",
      Pays == "Belgique" ~ "BE",
      Pays == "Bulgarie" ~ "BG",
      Pays == "Chypre" ~ "CY",
      Pays == "Croatie" ~ "HR",
      Pays == "Danemark" ~ "DK",
      Pays == "Espagne" ~ "ES",
      Pays == "Estonie" ~ "EE",
      Pays == "Finlande" ~ "FI",
      Pays == "France" ~ "FR",
      Pays == "Grèce" ~ "GR",
      Pays == "Irlande" ~ "IE",
      Pays == "Hongrie" ~ "HU",
      Pays == "Italie" ~ "IT",
      Pays == "Lettonie" ~ "LV",
      Pays == "Lituanie" ~ "LT",
      Pays == "Luxembourg" ~ "LU",
      Pays == "Malte" ~ "MT",
      Pays == "Pays-Bas" ~ "NL",
      Pays == "Pologne" ~ "PL",
      Pays == "Portugal" ~ "PT",
      Pays == "République tchèque" ~ "CZ",
      Pays == "Roumanie" ~ "RO",
      Pays == "Slovaquie" ~ "SK",
      Pays == "Slovénie" ~ "SI",
      Pays == "Suède" ~ "SE",
      Pays == "Union européenne" ~ "EU",
    )
  ) |>
  mutate(eng_country = fct_reorder(eng_country, indice)) |>
  mutate(
    type = case_when(
      code == "EU" ~ "EU",
      TRUE ~ "other"
    )
  ) |>
  ggplot(aes(x = eng_country, y = indice)) +
  geom_bar(
    stat = "identity",
    aes(fill = type),
    show.legend = F
  ) +
  geom_text(
    aes(
      label = paste0(scales::number(round(indice)), " €"),
      hjust = 1.1,
    ),
    size = 4.5,
    fontface = "bold",
    family = "Spline Sans",
    colour = text_col
  ) +
  scale_y_continuous(label = function(x) {
    paste0(scales::number(x), " €")
  }) +
  geom_flag(y = -1000, aes(image = code), by = "height", size = 0.03) +
  scale_fill_manual(values = col_country) +
  coord_flip() +
  labs(
    title = "Median standard of living in 2023 in EU",
    subtitle = "in purchasing power standards (PPS)",
    caption = "By MathieuGenu\nData: Eurostat, EU-Silc 2024"
  ) +
  theme_minimal() +
  theme(
    text = element_text(colour = text_col),
    panel.background = element_rect(fill = bg_col, colour = NA),
    plot.background = element_rect(fill = bg_col, colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed"),
    axis.text = element_text(size = 15, colour = text_col),
    axis.title = element_blank(),
    plot.title = element_text(
      family = "roboto",
      size = 20,
      margin = margin(t = 20)
    ),
    plot.subtitle = element_text(
      family = "roboto",
      size = 12,
      margin = margin(b = 20)
    )
  )


ggsave(
  plot = g,
  filename = "day09/graph/eu_wealth_index.png",
  dpi = 300,
  height = 9,
  width = 7
)

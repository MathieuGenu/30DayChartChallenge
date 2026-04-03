# Largely inspired by the treemap built by Yobanny Samano
# https://r-graph-gallery.com/web-treemap-with-annotations-labels-and-colors.html

library(pacman)

p_load(
  tidyverse,
  janitor,
  glue,
  ggtext,
  treemap,
  ggfittext,
  showtext
)

showtext_auto()
showtext_opts(dpi = 300)

font_add_google(name = "Roboto", family = "roboto")


data <- read_csv2(
  "day03/data/elecdom_courbes_horaires_detail_appareils.csv",
  locale = locale(encoding = "ISO-8859-1")
) |>
  clean_names() |>
  mutate(
    appareil_simplified = case_when(
      str_detect(appareil, "Chauffage") ~ "Chauffage",
      str_detect(appareil, "Eau chaude") ~ "Eau chaude",
      TRUE ~ appareil
    )
  )


# Correct data to extrapolate on a year
# heating in winter represent ~60% of annual heating electricity consumption
data <- data |>
  mutate(
    consommation_wh_h = case_when(
      str_detect(appareil, "Chauffage") ~ (consommation_wh_h / 0.6) * 1 / 4, # correction of heating
      TRUE ~ consommation_wh_h
    )
  ) |>
  filter(periode == "Toutes saisons" | str_detect(appareil, "Chauffage"))


conso_by_apparel <- data |>
  group_by(appareil, appareil_simplified) |>
  summarize(mean_conso = mean(consommation_wh_h)) |>
  group_by(appareil_simplified) |>
  summarize(mean_conso = sum(mean_conso)) |>
  arrange(desc(mean_conso))


# In english please !
dictionnary <- data.frame(
  appareil_simplified = c(
    "Chauffage",
    "Eau chaude",
    "Réfricongélateur",
    "Sèche-linge",
    "Congélateur",
    "Ventilation",
    "Téléviseurs",
    "Réfrigérateur",
    "Cuisinière",
    "Lave-vaisselle",
    "Fours encastrés",
    "Plaques de cuisson",
    "Eclairage",
    "Lave-linge",
    "Autres audiovisuel",
    "Autres usages informatique/bureautique",
    "Box informatique",
    "Fours micro-ondes",
    "Box TV"
  ),
  equipment = c(
    "Heating",
    "Hot water",
    "Fridge-freezer",
    "Tumble dryer",
    "Freezer",
    "Ventilation",
    "TV",
    "Fridge",
    "Cooker",
    "Dishwasher",
    "Built-in ovens",
    "Hobs",
    "Lighting",
    "Washing machine",
    "Other audiovisual equipment",
    "Other IT/office equipment",
    "IT box",
    "Microwave ovens",
    "TV box"
  )
)


conso_by_apparel <- conso_by_apparel |>
  left_join(dictionnary, by = "appareil_simplified") |>
  mutate(
    category = case_when(
      appareil_simplified == "Chauffage" ~ "heating",
      appareil_simplified == "Eau chaude" ~ "Water heater",
      appareil_simplified %in%
        c("Réfricongélateur", "Congélateur", "Réfrigérateur") ~ "Cold",
      appareil_simplified %in%
        c("Téléviseurs", "Autres audiovisuel", "Box TV") ~ "Audiovisual",
      appareil_simplified %in%
        c("Sèche-linge", "Lave-linge", "Lave-vaisselle") ~ "Wash and dry",
      appareil_simplified %in%
        c(
          "Cuisinière",
          "Fours encastrés",
          "Plaques de cuisson",
          "Fours micro-ondes"
        ) ~ "Cuisine",
      appareil_simplified %in%
        c("Autres usages informatique/bureautique", "Box informatique") ~ "IT",
      appareil_simplified == "Eclairage" ~ "Lighting",
      appareil_simplified == "Ventilation" ~ "Ventilation"
    )
  )

tree_conso <- treemap(
  conso_by_apparel,
  index = c("equipment"),
  vSize = "mean_conso",
  algorithm = "pivotSize",
  type = "categorical",
  vColor = "category",
  aspRatio = 5 / 3
)


data_ggplot <- tree_conso$tm |>
  as_tibble() |>
  arrange(desc(vSize)) |>
  mutate(
    rank = row_number(),
    xmax = x0 + w,
    ymax = y0 + h
  )


g_tree <- ggplot(data_ggplot) +
  geom_rect(
    aes(xmin = x0, ymin = y0, xmax = xmax, ymax = ymax, fill = vColor),
    linewidth = 0.1,
    colour = "#1E1D23",
    alpha = 0.9
  ) +
  geom_fit_text(
    data = data_ggplot,
    aes(xmin = x0, xmax = xmax, ymin = y0, ymax = ymax, label = equipment),
    size = 50,
    colour = "#1E1D23",
    family = "roboto",
    min.size = 4,
    reflow = TRUE
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    fill = "Equipment\ncategory",
    title = "Breakdown of electricity consumption\nin French households",
    caption = "By MathieuGenu\nData: ADEME"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = "#E8EADC", family = "roboto"),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "#1E1D23", colour = "#1E1D23"),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )


ggsave(
  g_tree,
  file = "day03/graph/mosaic_electricity_use.png",
  width = 11,
  height = 8,
  dpi = 300
)

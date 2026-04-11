library(pacman)

p_load(tidyverse, arrow, scales, ggtext, showtext)

showtext_auto()
showtext_opts(dpi = 300)

font_add_google(name = "Playfair Display", family = "playflair")

font <- "playflair"


data_link <- "https://data.culture.gouv.fr/api/explore/v2.1/catalog/datasets/frequentation-dans-les-salles-de-cinema/exports/parquet?lang=fr&timezone=America%2FSt_Johns"

data <- read_parquet(data_link)

data <- data |>
  mutate(
    annee = as.numeric(annee),
    recette_guichet_meur_courants = recette_guichet_meur_courants * 1e6,
    seances = seances_milliers * 1000
  )

coef <- mean(data$seances) / mean(data$recette_guichet_meur_courants)
col_entree <- "#2c97d4ff"
col_recette <- "#118f3bff"


g <- data |>
  # pivot_longer(
  #   cols = c(seances_milliers:recette_moyenne_par_entree_eur),
  #   names_to = "what",
  #   values_to = "values"
  # ) |>
  # filter(what %in% c("entrees_millions","recette_guichet_meur_courants")) |>
  ggplot(aes(x = annee)) +
  geom_point(aes(y = seances), colour = col_entree, size = 3) +
  geom_line(aes(y = seances), colour = col_entree, linewidth = 1.5) +
  geom_point(
    aes(y = recette_guichet_meur_courants * coef),
    colour = col_recette,
    size = 3
  ) +
  geom_line(
    aes(y = recette_guichet_meur_courants * coef),
    colour = col_recette,
    linewidth = 1.5
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Cinema admissions",
    sec.axis = sec_axis(
      name = "box office takings",
      transform = ~ . / coef,
      labels = scales::label_currency(
        prefix = "",
        suffix = "€",
        scale_cut = cut_short_scale()
      )
    )
  ) +
  theme_minimal() +
  labs(
    title = "Cinema attendance from 1980 to 2022 in France",
    subtitle = "Number of <span style='color: #2c97d4ff'>cinema admissions</span> and <span style='color : #118f3bff'>box office takings</span> per year",
    caption = "By MathieuGenu\nData: National Centre for Cinema and the Moving Image (CNC)"
  ) +
  theme(
    text = element_text(family = font, size = 20),
    plot.title = element_markdown(size = 30),
    plot.subtitle = element_markdown(
      size = 20
    ),
    plot.caption = element_text(size = 13),
    axis.line.y.left = element_line(colour = col_entree),
    axis.text.y.left = element_text(colour = col_entree),
    axis.line.y.right = element_line(colour = col_recette),
    axis.text.y.right = element_text(colour = col_recette),
    axis.line.x.bottom = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.x.bottom = element_line(),
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    axis.title.y.left = element_text(colour = col_entree),
    axis.title.y.right = element_text(colour = col_recette),
    panel.background = element_rect(colour = NA, fill = "#f1f1eaff"),
    plot.background = element_rect(colour = NA, fill = "#f1f1eaff")
  )

ggsave(
  plot = g,
  filename = "day10/graph/cinema_frequentation_and_earnings.png",
  dpi = 300,
  height = 6,
  width = 11
)

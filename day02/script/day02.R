library(pacman)
if (!("waffle" %in% installed.packages())) {
  remotes::install_github("hrbrmstr/waffle")
}
p_load(
  tidyverse,
  janitor,
  waffle,
  glue,
  ggtext,
  emojifont
)


# Unzip to a temporary directory and read
temp_dir <- tempdir()
unzip("day02/data/incendies.zip", exdir = temp_dir)
df <- read_csv2(file.path(temp_dir, "Incendies.csv"), skip = 3)
df <- df |>
  clean_names() |>
  mutate(annee = as.factor(annee))

fire_by_year <- df |>
  group_by(annee) |>
  summarize(N_fires = n())

g <- ggplot(
  fire_by_year,
  aes(label = annee, values = N_fires / 100, color = annee)
) +
  geom_pictogram(
    n_rows = 10,
    family = "fontawesome-webfont",
    size = 15,
    colour = "#F39C12",
    show.legend = F
  ) +
  # Association de la flamme à chaque catégorie
  scale_label_pictogram(
    name = NULL,
    values = rep("fire", 5)
  ) +
  coord_equal() +
  theme_void() +
  labs(
    title = "Forest fires in France from 2020 to 2024",
    subtitle = "Every flame logo represent 100 fires",
    caption = "By MathieuGenu\nData: bdiff.agriculture.gouv.fr"
  ) +
  facet_wrap(vars(annee), nrow = 1) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 60),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(b = 30),
      size = 50
    ),
    strip.text = element_text(size = 50),
    text = element_text(size = 30),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  )

ggsave(
  g,
  filename = "day02/graph/Nb_fires_2020_2024.png",
  device = "png",
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

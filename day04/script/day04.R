library(pacman)

p_load(
  tidyverse,
  janitor,
  glue,
  ggtext,
  showtext,
  cowplot,
  sf
)

showtext_auto()
showtext_opts(dpi = 300)

font_add_google(name = "Tenor Sans", family = "tenor")


# data water quality
# data here : https://www.data.gouv.fr/datasets/qualite-des-cours-deau-vis-a-vis-de-lindice-biologique-macro-invertebre-en-bretagne
data <- read_csv2("day04/data/oeb_eau_qualite_macroinvertebres_bretagne.csv")

# French dpt
temp_dir <- tempdir()
unzip("day04/data/GEO_Contours_Departements.zip", exdir = temp_dir)
dpt <- st_read(file.path(temp_dir, "Departements.shp"))
dpt_bzh <- dpt |>
  filter(DDEP_C_COD %in% c("29", "22", "56", "35"))

data |>
  glimpse()

data_sf <- data |>
  rename(
    lon = coordX_WGS84,
    lat = coordY_WGS84
  ) |>
  mutate(
    resultat = as.numeric(resultat),
    code_station = as.factor(code_station),
    code_departement = as.factor(code_departement)
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 2154)


sub_data <- data_sf |>
  filter(
    str_detect(serie, "I2M2"),
    code_departement %in% c("29", "22", "56", "35")
  )

sub_data |>
  ggplot(aes(y = resultat)) +
  # geom_boxplot(aes(x = as.factor(annee))) +
  geom_smooth(aes(x = annee), method = "glm") +
  facet_wrap(vars(code_departement))


mod <- lm(
  resultat ~ annee * code_departement,
  data = sub_data
)


pred_grid <- expand.grid(
  annee = seq(min(sub_data$annee), max(sub_data$annee), 1),
  code_departement = c("29", "22", "56", "35")
)
pred <- predict(
  mod,
  newdata = pred_grid,
  interval = "confidence"
)

pred_grid <- pred_grid |>
  bind_cols(pred)


dpt_chart <- function(dpt) {
  g <- pred_grid |>
    filter(code_departement == dpt) |>
    ggplot(aes(x = annee, y = fit, ymin = lwr, ymax = upr)) +
    geom_boxplot(
      data = sub_data |> filter(code_departement == dpt),
      aes(x = annee, y = resultat, group = annee),
      inherit.aes = F
    ) +
    ylim(0, 1) +
    scale_x_continuous(breaks = seq(2013, 2023, 2)) +
    geom_ribbon(fill = 'light grey', alpha = 0.4) +
    geom_line(color = 'red', size = 1.2) +
    theme_minimal() +
    theme(
      axis.line = element_line(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background = element_rect(fill = NA, colour = NA)
    )
  return(g)
}

line_ca <- dpt_chart("22")
line_fi <- dpt_chart("29")
line_mo <- dpt_chart("56")
line_il <- dpt_chart("35")


map_bzh <- ggplot() +
  geom_sf(data = dpt_bzh, fill = "#FFF2AF") +
  labs(
    title = "Evolution of river water quality in Brittany, France",
    subtitle = "Using the I2M2, a biological index based on the macroinvertebrates present in watershed",
    caption = "By MathieuGenu\nData: LYXEA - Dreal Bretagne, Naïades - OFB, 2024. Traitement : Observatoire de l'environnement en Bretagne, 2025."
  ) +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 27, family = "tenor", hjust = 0.5),
    plot.subtitle = element_text(size = 15, family = "tenor", hjust = 0.5)
    # plot.background = element_rect(fill = "#091413", colour = "#091413"),
    # panel.background = element_rect(fill = "#091413", colour = "#091413"),
  )

map_fr <- ggplot() +
  geom_sf(
    data = dpt |>
      filter(
        !(DREG_L_LIB %in%
          c("Guadeloupe", "Martinique", "Guyane", "La Réunion", NA, "Mayotte"))
      )
  ) +
  geom_sf(data = dpt_bzh, fill = "red") +
  theme_void()

plot <- ggdraw() +
  draw_plot(map_bzh, 0, 0, 1, 1) +
  draw_plot(line_ca, 0.4, 0.55, 0.3, 0.25) +
  draw_plot(line_fi, 0.1, 0.4, 0.3, 0.25) +
  draw_plot(line_mo, 0.4, 0.2, 0.3, 0.25) +
  draw_plot(line_il, 0.7, 0.4, 0.3, 0.25) +
  draw_plot(map_fr, 0, 0, 0.2, 0.25)


ggsave(
  plot,
  filename = "day04/graph/river_quality_bzh.png",
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

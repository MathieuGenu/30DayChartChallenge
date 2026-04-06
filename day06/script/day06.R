library(pacman)

p_load(
  tidyverse,
  magick,
  cropcircles,
  cowplot,
  ggtext,
  showtext,
  sysfonts
)


showtext_auto()
showtext_opts(dpi = 300)

font_add_google(name = "Libre Caslon Text", family = "libre")
font_add_google(name = "Roboto", family = "roboto")

# get RSF data
list_files <- list.files(
  "day06/data/",
  pattern = "*.csv",
  full.names = T
)

get_data <- function(file) {
  tmp <- file |>
    read_csv2() |>
    suppressMessages()
  colnames(tmp)[
    colnames(tmp) %in% c("Score", "Score N", "Score 2025")
  ] <- "Score"
  colnames(tmp)[colnames(tmp) %in% c("Rank", "Rank N")] <- "Rank"
  tmp <- tmp |>
    rename(year = `Year (N)`) |>
    mutate(
      year = ifelse(year == "2011-12", "2011", year),
      year = as.numeric(year)
    ) |>
    filter(ISO == "FRA") |>
    select("year", "Score", "Rank")
  return(tmp)
}

data <- list_files |>
  map_dfr(get_data)


# build quinquennat data
quinquennat <- data.frame(
  year = c((2005:2026), 2017.5)
) |>
  mutate(
    president = case_when(
      year <= 2017.5 ~ "François Hollande",
      year > 2017.5 ~ "Emmanuel Macron"
    )
  )

data <- data |>
  left_join(quinquennat, by = "year")


# img
hollande_img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Fran%C3%A7ois_Hollande_-_2017_%2827869823159%29_%28cropped_2%29.jpg/960px-Fran%C3%A7ois_Hollande_-_2017_%2827869823159%29_%28cropped_2%29.jpg"
macron_img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Emmanuel_Macron_2025_%28cropped%29.jpg/960px-Emmanuel_Macron_2025_%28cropped%29.jpg"

crop_hollande <- crop_circle(
  hollande_img,
  border_size = 20,
  border_colour = "#ff8080",
  just = 'top'
)
crop_macron <- crop_circle(
  macron_img,
  border_size = 20,
  border_colour = "#03215A",
  just = 'top'
)


# graph part

cols <- c(
  "François Hollande" = "#ff8080",
  "Emmanuel Macron" = "#03215A"
)
p <- data |>
  ggplot(aes(x = year, y = Score, colour = president)) +
  geom_line(aes(group = 1), linewidth = 1, show.legend = F) +
  geom_point(show.legend = F, size = 4) +
  ylim(75, 80) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = 2013:2025) +
  labs(
    title = "Evolution of freedom press index in France",
    subtitle = "during the presidencies of <span style='color:#ff8080'>François Hollande</span> and <span style='color:#03215A'>Emmanuel Macron</span>",
    caption = "By MathieuGenu\nData: Reporter Sans Frontières (Reporters Without Borders)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    axis.text = element_markdown(family = "roboto", size = 15),
    axis.title = element_blank(),
    # margins = margin(10,10,70,10),
    axis.line = element_line(),
    axis.ticks = element_line(),
    plot.subtitle = element_markdown(
      size = 16,
      margin = margin(t = 5, b = -150)
    ),
    title = element_text(
      family = "libre",
      size = 26,
      margin = margin(t = 5, b = -150)
    ),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  )


plot <- ggdraw() +
  draw_plot(p) +
  draw_image(crop_hollande, x = 0.2, y = 0.55, width = .2, height = .2) +
  draw_image(crop_macron, x = 0.6, y = 0.55, width = .2, height = .2)


ggsave(
  plot,
  filename = "day06/graph/RSF_score_president.png",
  width = 10,
  height = 6,
  dpi = 300
)

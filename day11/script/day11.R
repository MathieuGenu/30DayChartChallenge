library(pacman)

p_load(tidyverse, terra, tidyterra, sf, marmap, scales, patchwork, showtext)

showtext_auto()
showtext_opts(dpi = 300)

font_add_google(name = "Libre Caslon Text", family = "libre")
font_add_google(name = "Roboto", family = "roboto")

font <- "roboto"

rast_data <- rast("day11/data/sst_subset.nc")


pts <- data.frame(
  loc = c("Mediterranean", "Atlantic", "Eastern english channel"),
  lon = c(4.728175, -3.263444, -0.254552),
  lat = c(43.221461, 46.456251, 50.183678)
)

pts_sf <- pts |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

extracted_values <- terra::extract(rast_data, pts_sf)

final_data <- cbind(pts_sf, extracted_values) |>
  pivot_longer(
    cols = starts_with("analysed"),
    values_to = "SST_Kelvin",
    names_to = "day"
  )

final_data <- final_data |>
  mutate(
    SST_degre = SST_Kelvin - 273,
    15
  ) |>
  group_by(loc) |>
  mutate(
    date = seq(
      from = as.Date("2010-01-10"),
      to = as.Date("2025-11-04"),
      by = "1 day"
    )
  ) |>
  ungroup() |>
  mutate(week = isoweek(date), year = year(date), month = month(date))


month_data <- final_data |>
  group_by(year, month, loc) |>
  summarize(SST_month = mean(SST_degre, na.rm = T))


p <- month_data |>
  mutate(
    loc = factor(
      loc,
      levels = c("Eastern english channel", "Atlantic", "Mediterranean")
    )
  ) |>
  ggplot(aes(x = month, y = SST_month, group = year, colour = year)) +
  geom_line() +
  scale_colour_viridis_c() +
  theme_minimal() +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_y_continuous(labels = function(x) {
    paste0(x, "°C")
  }) +
  facet_wrap(vars(loc), ncol = 1) +
  theme(
    text = element_text(size = 15),
    axis.title = element_blank(),
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.text.x = element_text(size = 9)
  )


# map part
bat <- getNOAA.bathy(lon1 = -12, lon2 = 12, lat1 = 38, lat2 = 57, res = 1)

bat_xyz <- as.xyz(bat) %>%
  rename(Longitude = V1, Latitude = V2, Depth = V3) %>%
  filter(Depth < 1)

Bathy_sf <- st_as_sf(bat_xyz, coords = c("Longitude", "Latitude"), crs = 4326)
Bathy_terra <- as_spatraster(bat_xyz, xycols = c(1:2), crs = 4326)


map <- ggplot() +
  geom_spatraster(data = Bathy_terra, aes(fill = Depth), show.legend = F) +
  geom_sf(data = pts_sf, size = 2, colour = "black") +
  geom_sf_label(
    data = pts_sf,
    aes(label = loc),
    size = 5,
    nudge_y = 0.9,
    fill = "white"
  ) +
  theme_void()

set_null_device("png")

bg_col <- "#f1efe5ff"
plot <- (map | p) +
  plot_annotation(
    title = 'SST (Sea Surface Temperature) evolution from 2010 to 2025 in France',
    subtitle = '3 locations sampled : one in the Eastern english channel, one in the Atlantic and one in Mediterranean Sea\nSST has been averaged at month scale for each year',
    caption = 'By MathieuGenu\nData: Copernicus, data id : cmems-IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE'
  ) &
  theme(
    text = element_text(family = font),
    plot.title = element_text(size = 19, margin = margin(l = 10, r = 10)),
    plot.subtitle = element_text(margin = margin(l = 10, r = 10)),
    plot.background = element_rect(fill = bg_col, colour = NA),
    panel.background = element_rect(fill = bg_col, colour = NA)
  )


ggsave(
  plot,
  filename = "day11/graph/SST_evolution.png",
  device = ragg::agg_png,
  width = 9,
  height = 7
)

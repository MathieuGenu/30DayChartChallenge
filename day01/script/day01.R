library(pacman)

p_load(tidyverse, arrow, glue, ggtext, scales)


# Import parquet data from data.gouv.fr
flat_rent <- read_parquet(
  "https://object.files.data.gouv.fr/hydra-parquet/hydra-parquet/55b34088-0964-415f-9df7-d87dd98a09be.parquet"
)

pop_muni <- read_parquet(
  "https://object.files.data.gouv.fr/hydra-parquet/hydra-parquet/be303501-5c46-48a1-87b4-3d198423ff49.parquet"
)


# Add latest pop estimation in rent data
flat_rent <- flat_rent |>
  left_join(pop_muni |> select(codgeo, p23_pop), by = c("INSEE_C" = "codgeo"))


# Group data by zone, maille or EPCI
flat_maille <- flat_rent |>
  filter(TYPPRED == "maille") |>
  group_by(id_zone, TYPPRED) |>
  summarize(
    rent_pred = unique(loypredm2),
    lower_pred = unique(`lwr.IPm2`),
    upper_pred = unique(`upr.IPm2`),
    pop = mean(p23_pop)
  )

flat_commune <- flat_rent |>
  filter(TYPPRED == "commune") |>
  group_by(id_zone, TYPPRED, LIBGEO) |>
  summarize(
    rent_pred = unique(loypredm2),
    lower_pred = unique(`lwr.IPm2`),
    upper_pred = unique(`upr.IPm2`),
    pop = p23_pop
  )


flat <- bind_rows(
  flat_commune,
  flat_maille
)


# Insepct data
flat_rent |>
  glimpse()


# First visualisation
mean_rent <- mean(flat$rent_pred)

prop_over_under <- flat |>
  mutate(
    bin_pop = cut(
      pop,
      breaks = c(0, 2000, 20000, 1e5, Inf),
      labels = c(
        "< 2000\ninhabitants",
        "2000 to 20 000\ninhabitants",
        "20 000 to 100 000\ninhabitants",
        ">100 000\ninhabitants"
      )
    )
  ) |>
  drop_na(bin_pop) |>
  ungroup() |>
  mutate(over_under = ifelse(rent_pred > mean_rent, "over", "under")) |>
  group_by(bin_pop, over_under) |>
  summarize(N = n()) |>
  group_by(bin_pop) |>
  mutate(total = sum(N)) |>
  ungroup()

cols <- c("over" = "#FF6F68", "under" = "#4DD091")

g_pie <- prop_over_under |>
  ggplot(aes(x = bin_pop, y = N, fill = over_under)) +
  geom_col(show.legend = F) +
  geom_label(
    aes(label = percent(N / total)),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE,
    size = 5
  ) +
  facet_wrap(vars(bin_pop), scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = cols) +
  coord_polar(theta = "y") +
  labs(
    caption = "By MathieuGenu\nData: Estimates by ANIL, based on data from the SeLoger Group and leboncoin",
    title = glue(
      "Proportion of flat rents in France that are<br> <b><span style='color:#FF6F68'>higher</span></b> or <b><span style='color:#4DD091'>lower</span></b> than the national average rent ({round(mean_rent,1)} €/m²),<br> by size of the town where the rental advertisement is published"
    )
  ) +
  theme(
    text = element_text(family = "karst", size = 11),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_markdown(hjust = 0.5),
    strip.text = element_text(size = 12)
  )


ggsave(
  g_pie,
  width = 8,
  height = 9,
  dpi = 300,
  filename = "day01/graph/under_over_rental_price.png",
  bg = "white"
)

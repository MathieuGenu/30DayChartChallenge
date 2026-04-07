library(pacman)
p_load(tidyverse, arrow, sf, cowplot, pals, sysfonts, ragg, ggtext)

# showtext_auto()
# showtext_opts(dpi = 300)

font_add_google(name = "Roboto", family = "roboto")

# data
link_data <- "https://object.files.data.gouv.fr/hydra-parquet/hydra-parquet/75753852-aca0-4c53-bfd4-a29513358d2f.parquet"
data <- read_parquet(link_data)

# Departement
temp_dir <- tempdir()
unzip("day07/data/GEO_Contours_Regions.zip", exdir = temp_dir)
reg <- st_read(file.path(temp_dir, "Regions.shp"))
reg_ord_lat <- reg |>
    st_make_valid() |>
    st_centroid() %>%
    mutate(lat = st_coordinates(.)[, "Y"]) |>
    st_drop_geometry()


metropole <- data |>
    filter(indicateur == "Homicides") |>
    left_join(reg, by = c("Code_region" = "DREG_C_COD")) |>
    filter(!(Code_region %in% c("01", "02", "03", "04", "06"))) |>
    st_as_sf() |>
    mutate(taux_pour_mille = taux_pour_mille * 100)

maps <- metropole |>
    ggplot() +
    geom_sf(aes(fill = taux_pour_mille)) +
    facet_wrap(vars(annee), nrow = 1) +
    scale_fill_viridis_c() +
    theme_void() +
    # labs(fill = "Homicides per\n10 000 inhabitants") +
    theme(
        plot.margin = margin(0, 0, 0, 0),
        margins = margin(0, 0, 0, 0),
        legend.position = "top",
        strip.text = element_blank(),
        legend.title = element_blank()
    )


# Visualiser

g <- metropole |>
    ggplot(aes(x = annee, y = taux_pour_mille, colour = DREG_L_LIB)) +
    geom_line(aes(group = DREG_L_LIB), linewidth = 1) +
    geom_point(size = 3) +
    labs(
        title = "Number of homicides per 10,000 inhabitants in the regions of mainland France",
        subtitle = "Two regions stand out from the rest: <span style='color:#191919'>Corse</span> and <span style='color:#C20088'>Provence-Alpes-Côte d'Azur</span>",
        caption = "By MathieuGenu\nData: French Ministerial Statistical Department for Internal Security (SSMSI)"
    ) +
    scale_colour_discrete(palette = alphabet(13)) +
    theme_minimal() +
    theme(
        text = element_text(family = "roboto"),
        legend.position = "bottom",
        plot.margin = margin(r = 25, l = 25, b = 10),
        axis.title = element_blank(),
        axis.text = element_text(family = "roboto", size = 15),
        legend.title = element_blank(),
        plot.title = element_text(
            family = "roboto",
            size = 23,
            margin = margin(t = 20, b = 0),
            hjust = 0.5
        ),
        plot.subtitle = element_markdown(
            family = "roboto",
            margin = margin(t = 20, b = 170),
            size = 15,
            hjust = .5,
            colour = "grey60"
        )
        # plot.subtitle = element_markdown(
        #     family = "sans",
        #     size = 15,
        #     colour = "grey50",
        #     margin = margin(t = 20, b = 170)
        # )
    )

set_null_device("png")
plot <- ggdraw() +
    draw_plot(g) +
    draw_plot(maps, height = 0.227, y = 0.58, width = 0.89, x = 0.06)


ggsave(
    plot = plot,
    filename = "day07/graph/homicides_france.png",
    device = ragg::agg_png,
    width = 12,
    height = 7,
    dpi = 300,
    bg = "#F0F0F2"
)

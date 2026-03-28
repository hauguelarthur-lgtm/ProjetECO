#!/usr/bin/env Rscript

# Objectif:
# Tracer la carte de la zone Grenoble + Nord et dessiner la delimitation
# de la zone complete en rouge.
#
# Inputs:
# - Cours/data/raw/georef-france-iris-millesime.shp
#
# Output:
# - Cours/data/processed/carte_zone_grenoble_nord.png
#
# Execution:
# Rscript Cours/scripts/plot_zone_grenoble_nord.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

normalize_commune <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z0-9]", "")
  str_squish(x)
}

find_first_existing <- function(paths) {
  ok <- paths[file.exists(paths)]
  if (length(ok) == 0) return(NA_character_)
  ok[[1]]
}

# Detection robuste du dossier Cours
cours_candidates <- c("Cours", ".", "..", "../..")
cours_dir <- find_first_existing(file.path(cours_candidates, "data", "raw", "georef-france-iris-millesime.shp"))
if (is.na(cours_dir)) {
  stop("Impossible de trouver Cours/data/raw/georef-france-iris-millesime.shp depuis: ", getwd())
}
cours_dir <- dirname(dirname(dirname(cours_dir)))

raw_dir <- file.path(cours_dir, "data", "raw")
processed_dir <- file.path(cours_dir, "data", "processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

iris_path <- file.path(raw_dir, "georef-france-iris-millesime.shp")
iris <- st_read(iris_path, quiet = TRUE)

names(iris) <- tolower(names(iris))
if (!"com_name" %in% names(iris)) {
  stop("La colonne com_name est absente du shapefile IRIS.")
}

communes_nord <- c(
  "GRENOBLE", "MEYLAN", "LATRONCHE", "CORENC",
  "SAINTMARTINLEVINOUX", "SAINTEGREVE"
)

iris <- iris %>%
  mutate(commune_norm = normalize_commune(com_name))

zone_nord <- iris %>%
  filter(commune_norm %in% communes_nord)

if (nrow(zone_nord) == 0) {
  stop("Aucun IRIS trouve pour la zone Grenoble + Nord.")
}

zone_outline <- zone_nord %>%
  st_make_valid() %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(zone = "Grenoble + Nord")

bbox <- st_bbox(zone_outline)
p <- ggplot() +
  geom_sf(data = iris, fill = "grey96", color = "grey85", linewidth = 0.15) +
  geom_sf(data = zone_nord, fill = "#FDECEC", color = "#F8CACA", linewidth = 0.2) +
  geom_sf(data = zone_outline, fill = NA, color = "#D40000", linewidth = 1.15) +
  coord_sf(
    xlim = c(bbox["xmin"] - 0.01, bbox["xmax"] + 0.01),
    ylim = c(bbox["ymin"] - 0.01, bbox["ymax"] + 0.01),
    expand = FALSE
  ) +
  labs(
    title = "Zone d'étude Grenoble + Nord",
    subtitle = "Délimitation complète de la zone en rouge"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold")
  )

out_png <- file.path(processed_dir, "carte_zone_grenoble_nord.png")
ggsave(out_png, p, width = 10, height = 8, dpi = 180)

message("Carte exportee: ", out_png)

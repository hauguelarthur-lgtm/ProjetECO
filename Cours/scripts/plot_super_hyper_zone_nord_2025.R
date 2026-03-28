#!/usr/bin/env Rscript

# Objectif
# Produire une carte lisible des supermarches et hypermarches (2025)
# pour la zone Grenoble + Nord.
#
# Inputs
# - Cours/data/actif|raw|data/dataseed-sirene-1.shp
# - Cours/data/raw/georef-france-iris-millesime.shp
#
# Output
# - Cours/data/processed/plots/carte_super_hyper_zone_nord_2025.png
#
# Execution
# Rscript Cours/scripts/plot_super_hyper_zone_nord_2025.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

find_first_existing <- function(paths) {
  ok <- paths[file.exists(paths)]
  if (length(ok) == 0) return(NA_character_)
  ok[[1]]
}

normalize_commune <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z0-9]", "")
  str_squish(x)
}

normalize_naf <- function(x) {
  str_to_upper(str_replace_all(as.character(x), "[^A-Z0-9]", ""))
}

coalesce_chr <- function(df, candidates) {
  out <- rep(NA_character_, nrow(df))
  for (col in candidates) {
    if (!col %in% names(df)) next
    val <- as.character(df[[col]])
    idx <- (is.na(out) | str_trim(out) == "") & !is.na(val) & str_trim(val) != ""
    out[idx] <- val[idx]
  }
  out
}

# Detection du dossier Cours
cours_candidates <- c("Cours", ".", "..", "../..")
cours_marker <- find_first_existing(file.path(cours_candidates, "data", "raw", "georef-france-iris-millesime.shp"))
if (is.na(cours_marker)) {
  stop("Impossible de trouver Cours/data/raw/georef-france-iris-millesime.shp depuis: ", getwd())
}
cours_dir <- dirname(dirname(dirname(cours_marker)))

raw_dir <- file.path(cours_dir, "data", "raw")
processed_plots_dir <- file.path(cours_dir, "data", "processed", "plots")
if (!dir.exists(processed_plots_dir)) dir.create(processed_plots_dir, recursive = TRUE)

# Lecture IRIS
iris_path <- file.path(raw_dir, "georef-france-iris-millesime.shp")
iris <- st_read(iris_path, quiet = TRUE)
names(iris) <- tolower(names(iris))
if (!"com_name" %in% names(iris)) {
  stop("Colonne com_name absente dans georef-france-iris-millesime.shp")
}

# Lecture SIRENE 2025 avec priorite actif > raw > data
sirene_layer <- "dataseed-sirene-1"
sirene_dsn_candidates <- c(
  file.path(cours_dir, "data", "actif"),
  file.path(cours_dir, "data", "raw"),
  file.path(cours_dir, "data")
)

sirene_dsn <- sirene_dsn_candidates[
  file.exists(file.path(sirene_dsn_candidates, paste0(sirene_layer, ".shp")))
][1]

if (is.na(sirene_dsn)) {
  stop("Shapefile dataseed-sirene-1.shp introuvable dans data/actif, data/raw ou data/")
}

sirene <- st_read(dsn = sirene_dsn, layer = sirene_layer, quiet = TRUE)
names(sirene) <- tolower(names(sirene))

required <- c("commune_de_", "activite_pr.1")
missing_cols <- required[!required %in% names(sirene)]
if (length(missing_cols) > 0) {
  stop("Colonnes manquantes dans SIRENE: ", paste(missing_cols, collapse = ", "))
}

# Zone Grenoble + Nord
communes_nord <- c(
  "GRENOBLE", "MEYLAN", "LATRONCHE", "CORENC",
  "SAINTMARTINLEVINOUX", "SAINTEGREVE"
)

iris <- iris %>% mutate(commune_norm = normalize_commune(com_name))
sirene <- sirene %>% mutate(
  commune_norm = normalize_commune(commune_de_),
  code_naf = normalize_naf(activite_pr.1)
)

# Filtre super/hyper uniquement
sirene_zone <- sirene %>%
  filter(
    commune_norm %in% communes_nord,
    code_naf %in% c("4711D", "4711F")
  ) %>%
  mutate(
    Type_PV = ifelse(code_naf == "4711F", "Hypermarche", "Supermarche"),
    Enseigne = coalesce_chr(., c("enseigne_de", "denominatio", "enseigne_de.1", "denominatio.1")),
    Enseigne = ifelse(is.na(Enseigne) | str_squish(Enseigne) == "", "(Sans enseigne)", Enseigne)
  )

if (nrow(sirene_zone) == 0) {
  stop("Aucun point super/hyper trouve pour Grenoble + Nord.")
}

# Emprise zone
zone_nord <- iris %>% filter(commune_norm %in% communes_nord)
if (nrow(zone_nord) == 0) {
  stop("Aucun IRIS pour Grenoble + Nord.")
}

zone_outline <- zone_nord %>%
  st_make_valid() %>%
  st_union() %>%
  st_as_sf()

# Projection metrique pour un rendu cartographique propre
target_crs <- 2154
iris_plot <- st_transform(iris, target_crs)
zone_nord_plot <- st_transform(zone_nord, target_crs)
zone_outline_plot <- st_transform(zone_outline, target_crs)
sirene_zone_plot <- st_transform(sirene_zone, target_crs)

# Comptages
nb_super <- sum(sirene_zone$Type_PV == "Supermarche", na.rm = TRUE)
nb_hyper <- sum(sirene_zone$Type_PV == "Hypermarche", na.rm = TRUE)

# Labels hyper uniquement (2 points, reste lisible)
hypers <- sirene_zone_plot %>% filter(Type_PV == "Hypermarche")

# Carte
p <- ggplot() +
  geom_sf(data = iris_plot, fill = "#f6f6f6", color = "#d9d9d9", linewidth = 0.15) +
  geom_sf(data = zone_nord_plot, fill = "#fff1f0", color = "#f4c7c3", linewidth = 0.2) +
  geom_sf(data = zone_outline_plot, fill = NA, color = "#d7191c", linewidth = 1.1) +
  geom_sf(
    data = sirene_zone_plot %>% filter(Type_PV == "Supermarche"),
    aes(shape = Type_PV, fill = Type_PV),
    color = "white", size = 2.8, stroke = 0.35
  ) +
  geom_sf(
    data = sirene_zone_plot %>% filter(Type_PV == "Hypermarche"),
    aes(shape = Type_PV, fill = Type_PV),
    color = "white", size = 3.8, stroke = 0.45
  ) +
  geom_sf_text(
    data = hypers,
    aes(label = Enseigne),
    size = 2.9,
    nudge_y = 0.003,
    check_overlap = TRUE,
    color = "#3a3a3a"
  ) +
  scale_shape_manual(values = c("Supermarche" = 21, "Hypermarche" = 24)) +
  scale_fill_manual(
    values = c("Supermarche" = "#2c7fb8", "Hypermarche" = "#f03b20"),
    name = "Type"
  ) +
  guides(shape = "none") +
  labs(
    title = "Supermarches et hypermarches - Zone Grenoble + Nord (2025)",
    subtitle = paste0(nb_super, " supermarches | ", nb_hyper, " hypermarches"),
    caption = "Source: SIRENE (dataseed-sirene-1), filtre NAF 4711D/4711F"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

out_png <- file.path(processed_plots_dir, "carte_super_hyper_zone_nord_2025.png")
ggsave(out_png, p, width = 11, height = 8.5, dpi = 220)

message("Carte exportee: ", out_png)
message("Comptage detecte: ", nb_super, " supermarches | ", nb_hyper, " hypermarches")

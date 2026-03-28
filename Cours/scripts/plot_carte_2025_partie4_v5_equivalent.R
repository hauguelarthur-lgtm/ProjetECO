#!/usr/bin/env Rscript

# Objectif:
# Reproduire l'équivalent de la carte 2025 de la Partie 4
# de supermarket_implantation_v5.Rmd.
#
# Output:
# - Cours/data/processed/plots/carte_concurrence_2025_partie4_v5_equivalent.png
#
# Execution:
# Rscript Cours/scripts/plot_carte_2025_partie4_v5_equivalent.R

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

classify_type_pv <- function(df, naf_cols, classe_cols, enseigne_cols, denom_cols = character()) {
  naf_raw <- coalesce_chr(df, naf_cols)
  naf <- normalize_naf(naf_raw)
  out <- dplyr::case_when(
    naf == "4711F" ~ "hyper",
    naf == "4711D" ~ "super",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("super", "hyper"))
}

map_group_reseau <- function(enseigne, denomination = NULL) {
  txt <- str_to_upper(str_squish(paste(enseigne, denomination, sep = " | ")))
  dplyr::case_when(
    str_detect(txt, "CARREFOUR") ~ "Groupe Carrefour",
    str_detect(txt, "AUCHAN") ~ "Groupe Auchan",
    str_detect(txt, "CASINO|GEANT|MONOPRIX|SPAR|VIVAL") ~ "Groupe Casino",
    str_detect(txt, "INTERMARCHE|NETTO") ~ "Groupement Les Mousquetaires",
    str_detect(txt, "LECLERC|E\\.?\\s*LECLERC") ~ "E.Leclerc",
    str_detect(txt, "SUPER U|HYPER U|U EXPRESS|SYSTEME U|UTILE") ~ "Systeme U",
    str_detect(txt, "LIDL") ~ "Lidl",
    str_detect(txt, "ALDI") ~ "Aldi",
    TRUE ~ "Independant/Autre"
  )
}

separate_overlapping_points <- function(sf_points, offset_m = 22) {
  if (!inherits(sf_points, "sf") || nrow(sf_points) == 0) return(sf_points)
  geom_type <- unique(as.character(sf::st_geometry_type(sf_points, by_geometry = TRUE)))
  if (!all(geom_type %in% c("POINT", "MULTIPOINT"))) return(sf_points)

  crs_in <- sf::st_crs(sf_points)
  pts_3857 <- sf::st_transform(sf_points, 3857)
  coords <- sf::st_coordinates(pts_3857)

  key <- paste(round(coords[, 1], 3), round(coords[, 2], 3), sep = "_")
  grp <- split(seq_len(nrow(pts_3857)), key)

  new_coords <- coords[, 1:2, drop = FALSE]
  for (idx in grp) {
    if (length(idx) <= 1) next
    n <- length(idx)
    angles <- seq(0, 2 * pi, length.out = n + 1)[1:n]
    new_coords[idx, 1] <- coords[idx, 1] + offset_m * cos(angles)
    new_coords[idx, 2] <- coords[idx, 2] + offset_m * sin(angles)
  }

  sf::st_geometry(pts_3857) <- sf::st_sfc(
    lapply(seq_len(nrow(new_coords)), function(i) sf::st_point(new_coords[i, ])),
    crs = sf::st_crs(pts_3857)
  )

  sf::st_transform(pts_3857, crs_in)
}

plot_concurrence_map <- function(sirene_sf, iris_zone_etude, title_txt) {
  sirene_sf_plot <- separate_overlapping_points(sirene_sf, offset_m = 22)
  ggplot() +
    geom_sf(data = iris_zone_etude, fill = "grey97", color = "grey80", linewidth = 0.2) +
    geom_sf(
      data = sirene_sf_plot,
      aes(color = groupe_reseau_nom, shape = Type_PV),
      size = 2.2,
      alpha = 0.95
    ) +
    scale_shape_manual(
      values = c(super = 16, hyper = 17),
      drop = FALSE,
      name = "Type de point de vente"
    ) +
    labs(
      title = title_txt,
      subtitle = "Zone d'étude : Grenoble + communes Nord",
      color = "Groupe / réseau"
    ) +
    theme_minimal()
}

# Dossier Cours
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
iris_shp <- st_read(dsn = raw_dir, layer = "georef-france-iris-millesime", quiet = TRUE)
names(iris_shp) <- tolower(names(iris_shp))
iris_shp <- iris_shp %>% mutate(commune_norm = normalize_commune(com_name))

# Lecture SIRENE 2025 (priorite actif > raw > data)
sirene_layer <- "dataseed-sirene-1"
sirene_dsn_candidates <- c(
  file.path(cours_dir, "data", "actif"),
  file.path(cours_dir, "data", "raw"),
  file.path(cours_dir, "data")
)
sirene_dsn <- sirene_dsn_candidates[file.exists(file.path(sirene_dsn_candidates, paste0(sirene_layer, ".shp")))][1]
if (is.na(sirene_dsn)) {
  stop("Shapefile dataseed-sirene-1.shp introuvable dans actif/raw/data")
}

sirene_shp <- st_read(dsn = sirene_dsn, layer = sirene_layer, quiet = TRUE)
names(sirene_shp) <- tolower(names(sirene_shp))
sirene_shp <- sirene_shp %>% mutate(commune_norm = normalize_commune(commune_de_))

# Perimetre zone Nord (comme v5)
communes_nord_norm <- c(
  "GRENOBLE", "MEYLAN", "LATRONCHE", "CORENC",
  "SAINTMARTINLEVINOUX", "SAINTEGREVE"
)

# Filtre NAF (super/hyper uniquement)
target_naf_codes <- c("4711D", "4711F")

sirene_zone_etude <- sirene_shp %>%
  filter(commune_norm %in% communes_nord_norm) %>%
  mutate(
    code_naf = normalize_naf(coalesce_chr(
      ., c("activite_pr.1", "activitepri.1", "activiteprincipaleetablissement", "activite_pr", "activitepri")
    )),
    Type_PV = classify_type_pv(
      df = ., 
      naf_cols = c("activite_pr.1", "activitepri.1", "activiteprincipaleetablissement", "activite_pr", "activitepri"),
      classe_cols = c("classe_de_l", "classeetabl", "classe"),
      enseigne_cols = c("enseigne_de", "enseigne1et"),
      denom_cols = c("denominatio")
    )
  ) %>%
  filter(code_naf %in% target_naf_codes, !is.na(Type_PV)) %>%
  mutate(
    Enseigne = as.character(enseigne_de),
    Enseigne = ifelse(is.na(Enseigne) | Enseigne == "", "ENSEIGNE_INCONNUE", Enseigne),
    Classe = classe_de_l,
    groupe_reseau_nom = map_group_reseau(Enseigne, denominatio)
  )

if (nrow(sirene_zone_etude) == 0) {
  stop("Aucun point de vente trouve dans la zone Nord avec le filtre NAF cible.")
}

iris_zone_etude <- iris_shp %>% filter(commune_norm %in% communes_nord_norm)
if (nrow(iris_zone_etude) == 0) {
  stop("Aucun IRIS dans la zone Nord.")
}

# CRS commun
sirene_zone_etude <- st_transform(sirene_zone_etude, st_crs(iris_zone_etude))

# Carte équivalente à la partie 4 (2025)
map_2025 <- plot_concurrence_map(
  sirene_zone_etude,
  iris_zone_etude,
  "Concurrence super/hyper - 2025"
)

# Export PNG
out_png <- file.path(processed_plots_dir, "carte_concurrence_2025_partie4_v5_equivalent.png")
ggsave(out_png, map_2025, width = 11, height = 7.5, dpi = 220)

# Log
count_by_type <- table(sirene_zone_etude$Type_PV)
message("Carte exportée: ", out_png)
message("Comptage par type: ", paste(names(count_by_type), as.integer(count_by_type), sep = "=", collapse = ", "))

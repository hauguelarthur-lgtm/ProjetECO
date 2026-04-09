#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(readr)
})

find_first_existing <- function(paths) {
  ok <- paths[file.exists(paths)]
  if (length(ok) == 0) return(NA_character_)
  ok[[1]]
}

save_plot <- function(plot_obj, out_path, width = 11, height = 7.5, dpi = 220) {
  ggsave(filename = out_path, plot = plot_obj, width = width, height = height, dpi = dpi)
  message("Graphique exporte: ", out_path)
}

# -----------------------------------------------------------------------------
# Localisation dossier Cours
# -----------------------------------------------------------------------------
cours_candidates <- c("Cours", ".", "..", "../..")
marker <- find_first_existing(file.path(cours_candidates, "data", "raw", "georef-france-iris-millesime.shp"))
if (is.na(marker)) stop("Impossible de trouver Cours/data/raw/georef-france-iris-millesime.shp depuis: ", getwd())
cours_dir <- dirname(dirname(dirname(marker)))

raw_dir <- file.path(cours_dir, "data", "raw")
processed_dir <- file.path(cours_dir, "data", "processed")
plots_dir <- file.path(processed_dir, "plots")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# Lecture table de classement (priorite v5)
# -----------------------------------------------------------------------------
table_candidates <- c(
  file.path(processed_dir, "table_iris_indicateurs_dashboard_v5.csv"),
  file.path(processed_dir, "table_iris_indicateurs_dashboard_v4.csv")
)
table_path <- find_first_existing(table_candidates)
if (is.na(table_path)) {
  stop("Aucune table indicateurs trouvee (v5/v4) dans: ", processed_dir)
}

ind <- readr::read_csv(table_path, show_col_types = FALSE)

pick_col <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

code_col <- pick_col(names(ind), c("Code_IRIS", "CODE_IRIS", "code_iris", "iris_code"))
name_col <- pick_col(names(ind), c("Nom_IRIS", "iris_name", "nom_iris"))
com_col <- pick_col(names(ind), c("Commune", "com_name", "commune"))
rank_b_col <- pick_col(names(ind), c("Rang_classement_B", "rank_B", "rang_B"))
score_b_col <- pick_col(names(ind), c("Score_classement_B", "score_total_B_100", "score_total_B"))

if (is.na(code_col) || is.na(name_col)) {
  stop("Colonnes IRIS introuvables dans la table: ", table_path)
}
if (is.na(rank_b_col) && is.na(score_b_col)) {
  stop("Ni rang B ni score B trouves dans la table: ", table_path)
}

ind_std <- ind %>%
  transmute(
    Code_IRIS = as.character(.data[[code_col]]),
    Nom_IRIS = as.character(.data[[name_col]]),
    Commune = if (!is.na(com_col)) as.character(.data[[com_col]]) else NA_character_,
    Rank_B_raw = if (!is.na(rank_b_col)) suppressWarnings(as.numeric(.data[[rank_b_col]])) else NA_real_,
    Score_B_raw = if (!is.na(score_b_col)) suppressWarnings(as.numeric(.data[[score_b_col]])) else NA_real_
  )

# Si le rang B n'est pas fourni, on le reconstruit depuis le score B (descendant)
if (all(!is.finite(ind_std$Rank_B_raw)) && any(is.finite(ind_std$Score_B_raw))) {
  ind_std <- ind_std %>% mutate(Rank_B_raw = rank(-Score_B_raw, ties.method = "first"))
}

if (!any(is.finite(ind_std$Rank_B_raw))) {
  stop("Impossible de determiner le classement B (rang).")
}

# -----------------------------------------------------------------------------
# Geometrie IRIS
# -----------------------------------------------------------------------------
iris_shp <- st_read(dsn = raw_dir, layer = "georef-france-iris-millesime", quiet = TRUE)
names(iris_shp) <- tolower(names(iris_shp))

iris_code_col <- if ("code_iris" %in% names(iris_shp)) "code_iris" else if ("iris_code" %in% names(iris_shp)) "iris_code" else NA_character_
if (is.na(iris_code_col)) stop("Colonne code IRIS introuvable dans georef-france-iris-millesime.")

# Zone utilisee dans v5: Grenoble + Nord
normalize_commune <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z0-9]", "")
  str_squish(x)
}

iris_zone <- iris_shp %>%
  mutate(
    commune_raw = dplyr::coalesce(
      if ("com_name" %in% names(.)) as.character(com_name) else NA_character_,
      if ("nom_com" %in% names(.)) as.character(nom_com) else NA_character_
    )
  ) %>%
  mutate(
    Code_IRIS = as.character(.data[[iris_code_col]]),
    commune_norm = normalize_commune(commune_raw)
  ) %>%
  select(-commune_raw) %>%
  filter(commune_norm %in% c("GRENOBLE", "MEYLAN", "LATRONCHE", "CORENC", "SAINTMARTINLEVINOUX", "SAINTEGREVE"))

if (nrow(iris_zone) == 0) stop("Aucun IRIS detecte dans la zone cible.")

# -----------------------------------------------------------------------------
# Construction Top 4
# -----------------------------------------------------------------------------
rank_tbl <- ind_std %>%
  filter(!is.na(Code_IRIS), str_squish(Code_IRIS) != "") %>%
  group_by(Code_IRIS, Nom_IRIS, Commune) %>%
  summarise(
    Rank_B = suppressWarnings(min(Rank_B_raw[is.finite(Rank_B_raw)], na.rm = TRUE)),
    Score_B = suppressWarnings(max(Score_B_raw[is.finite(Score_B_raw)], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    Rank_B = ifelse(is.infinite(Rank_B), NA_real_, Rank_B),
    Score_B = ifelse(is.infinite(Score_B), NA_real_, Score_B)
  )

rank_tbl <- rank_tbl %>% arrange(Rank_B, desc(Score_B), Nom_IRIS)

top4 <- rank_tbl %>%
  filter(is.finite(Rank_B)) %>%
  slice_head(n = 4) %>%
  mutate(Rank_B = as.integer(Rank_B))

if (nrow(top4) == 0) stop("Top 4 vide apres traitement.")

message("Top 4 retenu (", basename(table_path), "):")
print(top4 %>% select(Rank_B, Nom_IRIS, Commune, Score_B))

iris_top4 <- iris_zone %>%
  left_join(top4 %>% select(Code_IRIS, Nom_IRIS, Rank_B, Score_B), by = "Code_IRIS") %>%
  mutate(
    zone_top4 = ifelse(is.na(Rank_B), "Autres zones", paste0("Top ", Rank_B)),
    zone_top4 = factor(zone_top4, levels = c("Top 1", "Top 2", "Top 3", "Top 4", "Autres zones")),
    label_top4 = ifelse(is.na(Rank_B), NA_character_, paste0("Top ", Rank_B, " - ", coalesce(iris_name, Nom_IRIS)))
  )

# -----------------------------------------------------------------------------
# Carte Top 4 (style proche v5)
# -----------------------------------------------------------------------------
p_top4_map <- ggplot() +
  geom_sf(
    data = iris_top4 %>% filter(zone_top4 == "Autres zones"),
    fill = "grey85",
    color = "white",
    linewidth = 0.25,
    alpha = 0.9
  ) +
  geom_sf(
    data = iris_top4 %>% filter(zone_top4 != "Autres zones"),
    aes(fill = zone_top4),
    color = "white",
    linewidth = 0.6,
    alpha = 0.95
  ) +
  geom_sf_text(
    data = iris_top4 %>% filter(zone_top4 != "Autres zones"),
    aes(label = label_top4),
    size = 2.9,
    color = "black",
    check_overlap = TRUE
  ) +
  scale_fill_manual(
    values = c("Top 1" = "#005F73", "Top 2" = "#0A9396", "Top 3" = "#94D2BD", "Top 4" = "#E9D8A6"),
    drop = FALSE,
    name = "Classement B"
  ) +
  labs(
    title = "Top 4 des zones d'implantation preferables (classement B)",
    subtitle = paste0("Source classement: ", basename(table_path))
  ) +
  theme_minimal()

save_plot(
  p_top4_map,
  file.path(plots_dir, "carte_top4_implantation_partie9_v5_equivalent.png")
)

# -----------------------------------------------------------------------------
# Graphe scores (Top 10 du classement B)
# -----------------------------------------------------------------------------
top10 <- rank_tbl %>%
  filter(is.finite(Rank_B)) %>%
  slice_head(n = 10) %>%
  mutate(
    label = paste0("Top ", as.integer(Rank_B), " - ", Nom_IRIS),
    Score_B = suppressWarnings(as.numeric(Score_B))
  )

if (nrow(top10) > 0 && any(is.finite(top10$Score_B))) {
  p_top10 <- ggplot(top10, aes(x = reorder(label, Score_B), y = Score_B, fill = Rank_B)) +
    geom_col(width = 0.75) +
    coord_flip() +
    scale_fill_gradient(low = "#94D2BD", high = "#005F73", name = "Rang B") +
    labs(
      title = "Top 10 des emplacements - Score classement B",
      x = NULL,
      y = "Score classement B"
    ) +
    theme_minimal()

  save_plot(
    p_top10,
    file.path(plots_dir, "graphe_scores_top10_classementB_partie9_v5_equivalent.png"),
    width = 11,
    height = 6.8
  )
}

# Export table Top 4
readr::write_csv(
  top4 %>% arrange(Rank_B),
  file.path(processed_dir, "top4_emplacements_partie9_v5_equivalent.csv")
)
message("Table exportee: ", file.path(processed_dir, "top4_emplacements_partie9_v5_equivalent.csv"))

message("Termine. Sorties dans: ", plots_dir)

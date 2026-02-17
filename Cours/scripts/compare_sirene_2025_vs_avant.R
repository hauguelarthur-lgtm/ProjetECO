library(dplyr)
library(sf)
library(readr)

cours_dir_candidates <- c("Cours", ".", "..")
cours_dir <- cours_dir_candidates[
  file.exists(file.path(cours_dir_candidates, "data", "raw", "dataseed-sirene-1.shp")) &
    file.exists(file.path(cours_dir_candidates, "data", "raw", "dataseed-sirene-2.shp"))
][1]
if (is.na(cours_dir)) {
  stop("Dossier Cours/data/raw introuvable depuis: ", getwd())
}

raw_dir <- file.path(cours_dir, "data", "raw")
processed_dir <- file.path(cours_dir, "data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

normalize_sirene <- function(x) {
  if (!"siret" %in% names(x) && "SIRET" %in% names(x)) x <- rename(x, siret = SIRET)
  if (!"libellecomm" %in% names(x) && "Commune_de_" %in% names(x)) x <- rename(x, libellecomm = Commune_de_)
  if (!"etatadminis" %in% names(x) && "Etat_admini" %in% names(x)) x <- rename(x, etatadminis = Etat_admini)
  if (!"enseigne1et" %in% names(x) && "Enseigne_de" %in% names(x)) x <- rename(x, enseigne1et = Enseigne_de)
  if (!"activitepri.1" %in% names(x) && "Activite_pr.1" %in% names(x)) x <- rename(x, activitepri.1 = Activite_pr.1)
  if (!"activitepri.2" %in% names(x) && "Activite_pr.2" %in% names(x)) x <- rename(x, activitepri.2 = Activite_pr.2)
  if (!"activitepri" %in% names(x) && "Activite_pr" %in% names(x)) x <- rename(x, activitepri = Activite_pr)
  x
}

is_4711 <- function(df) {
  cols <- c("activitepri.1", "activitepri.2", "activitepri")
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(FALSE, nrow(df)))
  Reduce(
    `|`,
    lapply(
      cols,
      function(col) {
        v <- as.character(df[[col]])
        !is.na(v) & startsWith(v, "47.11")
      }
    )
  )
}

d_2025 <- st_read(file.path(raw_dir, "dataseed-sirene-1.shp"), quiet = TRUE) %>%
  normalize_sirene() %>%
  mutate(source = "2025")

d_old <- st_read(file.path(raw_dir, "dataseed-sirene-2.shp"), quiet = TRUE) %>%
  normalize_sirene() %>%
  mutate(source = "avant")

required <- c("siret", "libellecomm", "etatadminis")
for (nm in required) {
  if (!nm %in% names(d_2025)) stop("Colonne absente dans dataseed-sirene-1: ", nm)
  if (!nm %in% names(d_old)) stop("Colonne absente dans dataseed-sirene-2: ", nm)
}

summary_global <- bind_rows(
  tibble(
    source = "2025",
    n_rows = nrow(d_2025),
    n_siret_unique = n_distinct(d_2025$siret),
    n_active = sum(d_2025$etatadminis == "Actif", na.rm = TRUE),
    n_4711 = sum(is_4711(d_2025), na.rm = TRUE)
  ),
  tibble(
    source = "avant",
    n_rows = nrow(d_old),
    n_siret_unique = n_distinct(d_old$siret),
    n_active = sum(d_old$etatadminis == "Actif", na.rm = TRUE),
    n_4711 = sum(is_4711(d_old), na.rm = TRUE)
  )
)

zone_coms <- c(
  "GRENOBLE", "MEYLAN", "LA TRONCHE",
  "CORENC", "SAINT-MARTIN-LE-VINOUX", "SAINT-EGREVE"
)

zone_counts <- bind_rows(
  d_2025 %>%
    mutate(is_4711 = is_4711(d_2025)) %>%
    filter(libellecomm %in% zone_coms) %>%
    st_drop_geometry() %>%
    group_by(source) %>%
    summarise(
      n_zone = n(),
      n_zone_active = sum(etatadminis == "Actif", na.rm = TRUE),
      n_zone_4711 = sum(is_4711, na.rm = TRUE),
      .groups = "drop"
    ),
  d_old %>%
    mutate(is_4711 = is_4711(d_old)) %>%
    filter(libellecomm %in% zone_coms) %>%
    st_drop_geometry() %>%
    group_by(source) %>%
    summarise(
      n_zone = n(),
      n_zone_active = sum(etatadminis == "Actif", na.rm = TRUE),
      n_zone_4711 = sum(is_4711, na.rm = TRUE),
      .groups = "drop"
    )
)

s_2025 <- unique(as.character(d_2025$siret))
s_old <- unique(as.character(d_old$siret))

diff_siret <- bind_rows(
  tibble(type = "present_2025_not_old", siret = setdiff(s_2025, s_old)),
  tibble(type = "present_old_not_2025", siret = setdiff(s_old, s_2025)),
  tibble(type = "present_both", siret = intersect(s_2025, s_old))
)

write_csv(summary_global, file.path(processed_dir, "sirene_compare_global_2025_vs_old.csv"))
write_csv(zone_counts, file.path(processed_dir, "sirene_compare_zone_2025_vs_old.csv"))
write_csv(diff_siret, file.path(processed_dir, "sirene_compare_siret_2025_vs_old.csv"))

cat("Resume global:\n")
print(summary_global)
cat("\nResume zone Grenoble+Nord:\n")
print(zone_counts)
cat("\nFichiers produits dans:", processed_dir, "\n")

library(dplyr)
library(sf)
library(readxl)
library(readr)

# ----------------------------
# Parameters
# ----------------------------
sirene_layer <- "dataseed-sirene-2" # change to dataseed-sirene-1 if needed
min_distance_to_existing_m <- 700
competition_radius_m <- 1000
top_n <- 10

# Weights (must sum to 1)
w_pop <- 0.35
w_revenue <- 0.20
w_residence <- 0.20
w_active <- 0.10
w_dist <- 0.10
w_comp <- 0.05

if (abs((w_pop + w_revenue + w_residence + w_active + w_dist + w_comp) - 1) > 1e-9) {
  stop("Les poids doivent sommer a 1.")
}

# ----------------------------
# Paths
# ----------------------------
cours_dir_candidates <- c("Cours", ".", "..")
cours_dir <- cours_dir_candidates[
  file.exists(file.path(cours_dir_candidates, "data", "raw", paste0(sirene_layer, ".shp")))
][1]
if (is.na(cours_dir)) {
  stop("Dossier Cours/data/raw introuvable depuis: ", getwd())
}

raw_dir <- file.path(cours_dir, "data", "raw")
processed_dir <- file.path(cours_dir, "data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# Helpers
# ----------------------------
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

rescale01 <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ----------------------------
# Load data
# ----------------------------
iris_shp <- st_read(dsn = raw_dir, layer = "georef-france-iris-millesime", quiet = TRUE)
sirene_shp <- st_read(dsn = raw_dir, layer = sirene_layer, quiet = TRUE) %>% normalize_sirene()
iris_data <- read_excel(file.path(raw_dir, "iris_isere.xlsx"))

# Study area
sirene_coms <- c(
  "GRENOBLE", "MEYLAN", "LA TRONCHE",
  "CORENC", "SAINT-MARTIN-LE-VINOUX", "SAINT-EGREVE"
)
iris_coms <- c(
  "Grenoble", "Meylan", "La Tronche",
  "Corenc", "Saint-Martin-le-Vinoux", "Saint-Égrève"
)

# ----------------------------
# Prepare IRIS features
# ----------------------------
iris_grenoble <- iris_shp %>%
  filter(com_name %in% iris_coms)
colnames(iris_grenoble)[20] <- "CODE_IRIS"

iris_data$nb_residence_princ <- iris_data$Res_princ_30_moins_40_m2 +
  iris_data$Res_princ_40_moins_60_m2 +
  iris_data$Res_princ_60_moins_80_m2 +
  iris_data$Res_princ_80_moins_100_m2 +
  iris_data$Res_princ_100_moins_120_m2

vars_to_keep <- c(
  "CODE_IRIS",
  "Mediane_euro",
  "Actifs_15-64_ans2",
  "Pop_15-64_ans",
  "nb_residence_princ"
)

iris_grenoble <- merge(
  iris_grenoble,
  iris_data[, vars_to_keep],
  by = "CODE_IRIS",
  all.x = TRUE
) %>%
  rename(
    Mediane_revenu = Mediane_euro,
    Actif_total = `Actifs_15-64_ans2`,
    Pop_15_64 = `Pop_15-64_ans`,
    Nb_residence_principale = nb_residence_princ
  )

# Replace zeros with NA on key fields
iris_grenoble <- iris_grenoble %>%
  mutate(
    across(
      c(Mediane_revenu, Actif_total, Pop_15_64, Nb_residence_principale),
      ~ na_if(., 0)
    )
  )

# ----------------------------
# Prepare existing supermarkets/hypermarkets
# ----------------------------
required_cols <- c("siret", "libellecomm", "etatadminis", "enseigne1et")
missing_cols <- setdiff(required_cols, names(sirene_shp))
if (length(missing_cols) > 0) {
  stop("Colonnes SIRENE manquantes: ", paste(missing_cols, collapse = ", "))
}

sirene_existing <- sirene_shp %>%
  filter(
    libellecomm %in% sirene_coms,
    etatadminis == "Actif",
    is_4711(.)
  )

# Remove empty geometries
if (nrow(sirene_existing) > 0) {
  sirene_existing <- sirene_existing[!st_is_empty(sirene_existing), ]
}

if (nrow(sirene_existing) == 0) {
  stop(
    "Aucun supermarche/hypermarche (47.11*) actif dans la zone pour ", sirene_layer, "."
  )
}

# ----------------------------
# Build candidate points (IRIS centroids)
# ----------------------------
candidates <- st_centroid(iris_grenoble)

# Project for metric distances
cand_l93 <- st_transform(candidates, 2154)
shops_l93 <- st_transform(sirene_existing, 2154)

# Distance to nearest existing store
dist_mat <- st_distance(cand_l93, shops_l93)
candidates$dist_min_m <- apply(dist_mat, 1, function(v) as.numeric(min(v)))

# Number of competitors within radius
within_list <- st_is_within_distance(cand_l93, shops_l93, dist = competition_radius_m)
candidates$n_competitors <- lengths(within_list)

# ----------------------------
# Score
# ----------------------------
# Fill NA with median for scoring only
for (col in c("Pop_15_64", "Mediane_revenu", "Nb_residence_principale", "Actif_total")) {
  med <- median(candidates[[col]], na.rm = TRUE)
  candidates[[col]][is.na(candidates[[col]])] <- med
}

candidates$score_pop <- rescale01(candidates$Pop_15_64)
candidates$score_revenue <- rescale01(candidates$Mediane_revenu)
candidates$score_residence <- rescale01(candidates$Nb_residence_principale)
candidates$score_active <- rescale01(candidates$Actif_total)
candidates$score_dist <- rescale01(candidates$dist_min_m)
candidates$score_comp <- 1 - rescale01(candidates$n_competitors)

candidates$score_final <- (
  w_pop * candidates$score_pop +
    w_revenue * candidates$score_revenue +
    w_residence * candidates$score_residence +
    w_active * candidates$score_active +
    w_dist * candidates$score_dist +
    w_comp * candidates$score_comp
)

# Exclude too-close candidates
candidates <- candidates %>%
  mutate(valid_candidate = dist_min_m >= min_distance_to_existing_m)

ranked <- candidates %>%
  filter(valid_candidate) %>%
  arrange(desc(score_final)) %>%
  mutate(rank = row_number())

top_candidates <- ranked %>% slice_head(n = top_n)

# ----------------------------
# Outputs
# ----------------------------
out_gpkg_all <- file.path(processed_dir, "candidate_locations_scored.gpkg")
out_gpkg_top <- file.path(processed_dir, "candidate_locations_top.gpkg")
out_csv_top <- file.path(processed_dir, "candidate_locations_top.csv")

st_write(candidates, out_gpkg_all, delete_dsn = TRUE, quiet = TRUE)
st_write(top_candidates, out_gpkg_top, delete_dsn = TRUE, quiet = TRUE)
write_csv(
  top_candidates %>%
    st_drop_geometry() %>%
    select(rank, iris_name, CODE_IRIS, score_final, dist_min_m, n_competitors,
           Pop_15_64, Mediane_revenu, Nb_residence_principale, Actif_total),
  out_csv_top
)

cat("Top emplacements proposes (", nrow(top_candidates), "):\n", sep = "")
print(
  top_candidates %>%
    st_drop_geometry() %>%
    select(rank, iris_name, score_final, dist_min_m, n_competitors) %>%
    head(top_n)
)

cat("\nSorties:\n")
cat(" -", out_gpkg_all, "\n")
cat(" -", out_gpkg_top, "\n")
cat(" -", out_csv_top, "\n")

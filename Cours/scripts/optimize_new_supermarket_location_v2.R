library(dplyr)
library(sf)
library(readxl)
library(readr)

# ----------------------------
# Parameters
# ----------------------------
sirene_layer <- "dataseed-sirene-2" # change to dataseed-sirene-1 if needed
min_distance_to_existing_m <- 700
min_distance_to_hyper_m <- 1000
competition_radius_m <- 1000
competition_decay_m <- 900
competition_cutoff_m <- 2500
top_n <- 10

# Weights for score (must sum to 1)
w_pop <- 0.33
w_revenue <- 0.20
w_residence <- 0.20
w_active <- 0.10
w_dist <- 0.07
w_comp <- 0.10

# Competition penalty by store type
penalty_super <- 1.0
penalty_hyper <- 2.4
penalty_other_4711 <- 1.2

if (abs((w_pop + w_revenue + w_residence + w_active + w_dist + w_comp) - 1) > 1e-9) {
  stop("Les poids du score final doivent sommer a 1.")
}
if (penalty_hyper <= penalty_super) {
  stop("Reglage invalide: penalty_hyper doit etre strictement > penalty_super.")
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

normalize_activity_code <- function(x) {
  out <- toupper(trimws(as.character(x)))
  out <- gsub("[^A-Z0-9]", "", out)
  out
}

extract_activity_code <- function(df) {
  cols <- c("activitepri.1", "activitepri.2", "activitepri")
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(NA_character_, nrow(df)))

  out <- rep(NA_character_, nrow(df))
  for (col in cols) {
    v <- as.character(df[[col]])
    fill <- is.na(out) | out == ""
    out[fill] <- v[fill]
  }
  out
}

classify_store_type <- function(code_norm) {
  # In this project, 47.11F = hyper, 47.11D = super.
  # Other 47.11* values are kept as "other_4711".
  out <- rep(NA_character_, length(code_norm))
  is_4711 <- grepl("^4711", code_norm)
  out[is_4711] <- "other_4711"
  out[grepl("^4711D", code_norm)] <- "super"
  out[grepl("^4711F", code_norm)] <- "hyper"
  out
}

rescale01 <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

safe_min_by_type <- function(dist_m, selector) {
  if (!any(selector)) return(rep(NA_real_, nrow(dist_m)))
  apply(dist_m[, selector, drop = FALSE], 1, min)
}

safe_count_within <- function(dist_m, selector, radius_m) {
  if (!any(selector)) return(rep(0L, nrow(dist_m)))
  rowSums(dist_m[, selector, drop = FALSE] <= radius_m)
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

if (!"CODE_IRIS" %in% names(iris_grenoble)) {
  if ("iris_code" %in% names(iris_grenoble)) {
    iris_grenoble <- rename(iris_grenoble, CODE_IRIS = iris_code)
  } else {
    stop("Colonne CODE_IRIS/iris_code absente dans la couche IRIS.")
  }
}

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
missing_vars <- setdiff(vars_to_keep, names(iris_data))
if (length(missing_vars) > 0) {
  stop("Colonnes manquantes dans iris_isere.xlsx: ", paste(missing_vars, collapse = ", "))
}

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
  ) %>%
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
  mutate(
    activity_code = extract_activity_code(.),
    activity_code_norm = normalize_activity_code(activity_code),
    store_type = classify_store_type(activity_code_norm),
    libellecomm_u = toupper(trimws(as.character(libellecomm))),
    etatadminis_u = toupper(trimws(as.character(etatadminis)))
  ) %>%
  filter(
    libellecomm_u %in% sirene_coms,
    etatadminis_u %in% c("ACTIF", "A"),
    !is.na(store_type)
  )

if (nrow(sirene_existing) > 0) {
  sirene_existing <- sirene_existing[!st_is_empty(sirene_existing), ]
}

if (nrow(sirene_existing) == 0) {
  stop(
    "Aucun etablissement 47.11* actif dans la zone pour ", sirene_layer, "."
  )
}

# ----------------------------
# Build candidate points (IRIS centroids)
# ----------------------------
candidates <- st_centroid(iris_grenoble)

# Project for metric distances
cand_l93 <- st_transform(candidates, 2154)
shops_l93 <- st_transform(sirene_existing, 2154)

dist_m <- matrix(
  as.numeric(st_distance(cand_l93, shops_l93)),
  nrow = nrow(cand_l93),
  ncol = nrow(shops_l93)
)

shop_penalty <- case_when(
  shops_l93$store_type == "hyper" ~ penalty_hyper,
  shops_l93$store_type == "super" ~ penalty_super,
  TRUE ~ penalty_other_4711
)

# Weighted competition influence:
# influence = penalty_type * exp(-distance / competition_decay_m)
# only up to competition_cutoff_m
weight_matrix <- matrix(
  shop_penalty,
  nrow = nrow(dist_m),
  ncol = ncol(dist_m),
  byrow = TRUE
)
dist_decay <- exp(-dist_m / competition_decay_m)
in_cutoff <- dist_m <= competition_cutoff_m
competition_weighted <- rowSums(weight_matrix * dist_decay * in_cutoff, na.rm = TRUE)

is_hyper <- shops_l93$store_type == "hyper"
is_super <- shops_l93$store_type == "super"

candidates$dist_min_m <- apply(dist_m, 1, min)
candidates$dist_min_hyper_m <- safe_min_by_type(dist_m, is_hyper)
candidates$dist_min_super_m <- safe_min_by_type(dist_m, is_super)
candidates$n_hyper_1km <- safe_count_within(dist_m, is_hyper, competition_radius_m)
candidates$n_super_1km <- safe_count_within(dist_m, is_super, competition_radius_m)
candidates$competition_weighted <- competition_weighted

# ----------------------------
# Score
# ----------------------------
for (col in c("Pop_15_64", "Mediane_revenu", "Nb_residence_principale", "Actif_total")) {
  med <- median(candidates[[col]], na.rm = TRUE)
  candidates[[col]][is.na(candidates[[col]])] <- med
}

candidates$score_pop <- rescale01(candidates$Pop_15_64)
candidates$score_revenue <- rescale01(candidates$Mediane_revenu)
candidates$score_residence <- rescale01(candidates$Nb_residence_principale)
candidates$score_active <- rescale01(candidates$Actif_total)
candidates$score_dist <- rescale01(candidates$dist_min_m)
candidates$score_comp <- 1 - rescale01(candidates$competition_weighted)

candidates$score_final <- (
  w_pop * candidates$score_pop +
    w_revenue * candidates$score_revenue +
    w_residence * candidates$score_residence +
    w_active * candidates$score_active +
    w_dist * candidates$score_dist +
    w_comp * candidates$score_comp
)

# Additional validity constraint: farther from hypermarkets
candidates <- candidates %>%
  mutate(
    valid_candidate = dist_min_m >= min_distance_to_existing_m &
      (is.na(dist_min_hyper_m) | dist_min_hyper_m >= min_distance_to_hyper_m)
  )

ranked <- candidates %>%
  filter(valid_candidate) %>%
  arrange(desc(score_final)) %>%
  mutate(rank = row_number())

top_candidates <- ranked %>% slice_head(n = top_n)

# ----------------------------
# Outputs
# ----------------------------
out_gpkg_all <- file.path(processed_dir, "candidate_locations_scored_v2.gpkg")
out_gpkg_top <- file.path(processed_dir, "candidate_locations_top_v2.gpkg")
out_csv_top <- file.path(processed_dir, "candidate_locations_top_v2.csv")

st_write(candidates, out_gpkg_all, delete_dsn = TRUE, quiet = TRUE)
st_write(top_candidates, out_gpkg_top, delete_dsn = TRUE, quiet = TRUE)
write_csv(
  top_candidates %>%
    st_drop_geometry() %>%
    select(
      rank, iris_name, CODE_IRIS, score_final,
      dist_min_m, dist_min_hyper_m, dist_min_super_m,
      n_hyper_1km, n_super_1km, competition_weighted,
      Pop_15_64, Mediane_revenu, Nb_residence_principale, Actif_total
    ),
  out_csv_top
)

# Explicit check : hyper penalty must be stronger than super
demo_dist <- 500
demo_super <- penalty_super * exp(-demo_dist / competition_decay_m)
demo_hyper <- penalty_hyper * exp(-demo_dist / competition_decay_m)

cat(
  sprintf(
    "Controle penalite concurrence a %dm: super=%.4f ; hyper=%.4f (hyper > super = %s)\n",
    demo_dist, demo_super, demo_hyper, ifelse(demo_hyper > demo_super, "OK", "NON")
  )
)

cat("Top emplacements proposes (", nrow(top_candidates), "):\n", sep = "")
print(
  top_candidates %>%
    st_drop_geometry() %>%
    select(rank, iris_name, score_final, dist_min_m, n_hyper_1km, n_super_1km, competition_weighted) %>%
    head(top_n)
)

cat("\nSorties:\n")
cat(" -", out_gpkg_all, "\n")
cat(" -", out_gpkg_top, "\n")
cat(" -", out_csv_top, "\n")

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(readr)
  library(readxl)
  library(tibble)
})

message("Debut export_table_iris_dashboard_v4.R")

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
cours_dir_candidates <- c("Cours", ".", "..", "../..")
cours_dir <- cours_dir_candidates[
  file.exists(file.path(cours_dir_candidates, "data", "raw", "dataseed-sirene-1.shp"))
][1]
if (is.na(cours_dir)) stop("Dossier Cours introuvable depuis: ", getwd())

raw_dir <- file.path(cours_dir, "data", "raw")
processed_dir <- file.path(cours_dir, "data", "processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helpers (aligned with supermarket_implantation_v4.Rmd)
# -----------------------------------------------------------------------------
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

normalize_naf <- function(x) {
  str_to_upper(str_replace_all(as.character(x), "[^A-Z0-9]", ""))
}

normalize_commune <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z0-9]", "")
  str_squish(x)
}

classify_type_pv <- function(df, naf_cols) {
  naf_raw <- coalesce_chr(df, naf_cols)
  naf <- normalize_naf(naf_raw)
  out <- dplyr::case_when(
    naf == "4711F" ~ "hyper",
    naf == "4711D" ~ "super",
    naf %in% c("4711B", "4711C") ~ "superette",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("super", "hyper", "superette"))
}

compute_coverage_pct <- function(iris_sf, iso_sf, iris_id_col) {
  iris_base <- iris_sf %>%
    dplyr::select(dplyr::all_of(c(iris_id_col, "iris_name")), geometry) %>%
    mutate(iris_area = as.numeric(st_area(geometry)))

  if (is.null(iso_sf) || nrow(iso_sf) == 0) {
    return(
      iris_base %>%
        st_drop_geometry() %>%
        dplyr::transmute(
          !!iris_id_col := .data[[iris_id_col]],
          intersect_area = 0,
          couverture_pct = 0
        )
    )
  }

  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)

  iso_valid <- st_make_valid(iso_sf)
  iso_union <- st_union(st_geometry(iso_valid))
  iso_union <- st_sf(id = 1, geometry = st_sfc(iso_union, crs = st_crs(iso_valid)))

  inter <- st_intersection(iris_base, iso_union)
  if (nrow(inter) == 0) {
    return(
      iris_base %>%
        st_drop_geometry() %>%
        dplyr::transmute(
          !!iris_id_col := .data[[iris_id_col]],
          intersect_area = 0,
          couverture_pct = 0
        )
    )
  }

  inter %>%
    mutate(intersect_area = as.numeric(st_area(geometry))) %>%
    st_drop_geometry() %>%
    group_by(dplyr::across(dplyr::all_of(iris_id_col))) %>%
    summarise(intersect_area = sum(intersect_area), .groups = "drop") %>%
    right_join(
      iris_base %>% st_drop_geometry() %>% dplyr::select(dplyr::all_of(c(iris_id_col, "iris_area"))),
      by = iris_id_col
    ) %>%
    mutate(
      intersect_area = ifelse(is.na(intersect_area), 0, intersect_area),
      couverture_pct = pmin(100, (intersect_area / iris_area) * 100)
    ) %>%
    dplyr::select(dplyr::all_of(iris_id_col), intersect_area, couverture_pct)
}

get_env_int <- function(var_name, default = NA_integer_) {
  raw <- Sys.getenv(var_name, "")
  if (!nzchar(raw)) return(default)
  val <- suppressWarnings(as.integer(raw))
  if (is.na(val)) return(default)
  val
}

connect_otp_any <- function(port_candidates = c(8081L, 8080L, 8082L, 8901L:8910L, 8801L:8810L)) {
  if (!requireNamespace("opentripplanner", quietly = TRUE)) return(NULL)
  for (p in port_candidates) {
    con <- tryCatch(
      opentripplanner::otp_connect(hostname = "localhost", port = p),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      attr(con, "otp_port") <- p
      message("Connexion OTP detectee sur le port ", p)
      return(con)
    }
  }
  NULL
}

prepare_points_for_otp <- function(points_sf, id_col = "siret") {
  if (!inherits(points_sf, "sf")) stop("prepare_points_for_otp: objet sf attendu.")
  if (!id_col %in% names(points_sf)) stop("prepare_points_for_otp: colonne ID absente: ", id_col)
  out <- points_sf %>%
    mutate(!!id_col := as.character(.data[[id_col]])) %>%
    filter(!is.na(.data[[id_col]]) & str_squish(.data[[id_col]]) != "")
  out <- out[!sf::st_is_empty(out), ]
  coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(out))))
  if (nrow(coords) > 0) {
    keep_xy <- stats::complete.cases(coords[, c("X", "Y"), drop = FALSE])
    out <- out[keep_xy, ]
  }
  out <- out %>% distinct(.data[[id_col]], .keep_all = TRUE)
  if (nrow(out) == 0) stop("OTP: aucun point valide apres nettoyage.")
  out
}

normalize_iso_id_col <- function(iso_sf, id_col = "siret", known_ids = NULL) {
  if (is.null(iso_sf) || nrow(iso_sf) == 0) return(iso_sf)
  id_candidates <- c(id_col, "from_id", "fromId", "id", "otp_id", "siret")
  id_found <- id_candidates[id_candidates %in% names(iso_sf)]
  key_col <- if (length(id_found) > 0) id_found[[1]] else names(iso_sf)[min(3, ncol(iso_sf))]
  raw_id <- iso_sf[[key_col]]
  out_id <- as.character(raw_id)

  if (!is.null(known_ids)) {
    known_ids <- as.character(known_ids)
    known_ids <- known_ids[!is.na(known_ids) & str_squish(known_ids) != ""]
    if (length(known_ids) > 0 && !all(out_id %in% known_ids)) {
      id_num <- suppressWarnings(as.integer(raw_id))
      mapped <- rep(NA_character_, length(id_num))
      ok_1based <- !is.na(id_num) & id_num >= 1L & id_num <= length(known_ids)
      mapped[ok_1based] <- known_ids[id_num[ok_1based]]
      need_map <- is.na(mapped)
      ok_0based <- need_map & !is.na(id_num) & id_num >= 0L & id_num < length(known_ids)
      mapped[ok_0based] <- known_ids[id_num[ok_0based] + 1L]
      need_map <- is.na(mapped)
      if (any(need_map)) {
        parsed_num <- suppressWarnings(as.integer(str_extract(as.character(raw_id[need_map]), "\\d+$")))
        ok_parsed <- !is.na(parsed_num) & parsed_num >= 1L & parsed_num <= length(known_ids)
        if (any(ok_parsed)) {
          idx_need <- which(need_map)
          mapped[idx_need[ok_parsed]] <- known_ids[parsed_num[ok_parsed]]
        }
      }
      direct_ok <- out_id %in% known_ids
      out_id <- ifelse(direct_ok, out_id, mapped)
    }
  }

  iso_sf[[id_col]] <- as.character(out_id)
  iso_sf
}

otp_isochrone_safe <- function(
    otpcon, points_sf, id_col = "siret", mode = c("CAR"),
    date_time, cutoffSec, maxWalkDistance = NULL, batch_size = 8L
) {
  if (nrow(points_sf) == 0 || is.null(otpcon)) return(NULL)
  pts <- sf::st_transform(points_sf, 4326)
  request_timeout_sec <- max(20L, get_env_int("OTP_REQUEST_TIMEOUT_SEC", 90L))
  idx_batches <- split(seq_len(nrow(pts)), ceiling(seq_len(nrow(pts)) / as.integer(batch_size)))
  out_parts <- list()

  for (b in seq_along(idx_batches)) {
    pts_chunk <- pts[idx_batches[[b]], ]
    args <- list(
      otpcon = otpcon,
      fromPlace = pts_chunk,
      fromID = as.character(pts_chunk[[id_col]]),
      mode = mode,
      date_time = date_time,
      cutoffSec = cutoffSec,
      ncores = 1
    )
    if (!is.null(maxWalkDistance)) args$maxWalkDistance <- maxWalkDistance

    res <- tryCatch({
      setTimeLimit(elapsed = request_timeout_sec, transient = TRUE)
      on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
      do.call(opentripplanner::otp_isochrone, args)
    }, error = function(e) NULL)

    if (!is.null(res) && nrow(res) > 0) {
      res <- normalize_iso_id_col(
        iso_sf = res, id_col = id_col, known_ids = as.character(pts_chunk[[id_col]])
      )
      out_parts[[length(out_parts) + 1]] <- res
    }
  }

  if (length(out_parts) == 0) return(NULL)
  out <- do.call(rbind, out_parts)
  normalize_iso_id_col(iso_sf = out, id_col = id_col, known_ids = as.character(pts[[id_col]]))
}

make_buffer_iso_fallback <- function(points_sf, id_col = "siret", cutoffSec, speed_m_per_min = 80) {
  if (nrow(points_sf) == 0) return(points_sf[0, ])
  pts <- st_transform(points_sf, 2154)
  ids <- as.character(pts[[id_col]])
  out_list <- vector("list", length = nrow(pts) * length(cutoffSec))
  k <- 0L
  for (i in seq_len(nrow(pts))) {
    for (tt in cutoffSec) {
      k <- k + 1L
      d_m <- (as.numeric(tt) / 60) * speed_m_per_min
      out_list[[k]] <- st_buffer(pts[i, ], dist = d_m) %>%
        mutate(!!id_col := ids[i], time = as.integer(tt))
    }
  }
  iso <- do.call(rbind, out_list)
  st_transform(iso, st_crs(points_sf))
}

get_iso_specs <- function(profile = c("walk", "car")) {
  profile <- match.arg(profile)
  if (profile == "walk") {
    return(list(
      hyper = c(8, 10) * 60,
      super = c(5, 10) * 60,
      superette = c(5, 10) * 60
    ))
  }
  list(
    hyper = c(20, 30) * 60,
    super = c(10, 15) * 60,
    superette = c(5, 10) * 60
  )
}

build_multiformat_isochrones <- function(
    points_sf, otpcon = NULL, mode = c("CAR"), profile = c("car", "walk"),
    strict = TRUE, id_col = "siret", maxWalkDistance = NULL
) {
  profile <- match.arg(profile)
  specs <- get_iso_specs(profile)
  parts <- list()
  for (tp in names(specs)) {
    pts_tp <- points_sf %>% filter(as.character(Type_PV) == tp)
    if (nrow(pts_tp) == 0) next
    cut_tp <- specs[[tp]]
    iso_tp <- otp_isochrone_safe(
      otpcon = otpcon, points_sf = pts_tp, id_col = id_col, mode = mode,
      date_time = as.POSIXct(strptime("2026-02-18 08:35", "%Y-%m-%d %H:%M")),
      cutoffSec = cut_tp, maxWalkDistance = maxWalkDistance, batch_size = 8L
    )
    if (is.null(iso_tp) || nrow(iso_tp) == 0) {
      if (strict) {
        stop("Aucune isochrone OTP pour type=", tp, " mode=", paste(mode, collapse = "+"), " profile=", profile)
      }
      iso_tp <- make_buffer_iso_fallback(
        points_sf = pts_tp, id_col = id_col, cutoffSec = cut_tp,
        speed_m_per_min = if (profile == "car") 500 else 80
      )
    }
    iso_tp <- normalize_iso_id_col(
      iso_sf = iso_tp, id_col = id_col, known_ids = as.character(pts_tp[[id_col]])
    )
    iso_tp$Type_PV <- tp
    parts[[length(parts) + 1]] <- iso_tp
  }
  if (length(parts) == 0) return(points_sf[0, ])
  do.call(rbind, parts)
}

minmax01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  r <- range(x, na.rm = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) return(rep(0.5, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

row_weighted_mean <- function(df, cols, w) {
  m <- as.matrix(df[, cols, drop = FALSE])
  apply(m, 1, function(v) {
    ok <- is.finite(v)
    if (!any(ok)) return(NA_real_)
    sum(v[ok] * w[ok]) / sum(w[ok])
  })
}

# -----------------------------------------------------------------------------
# Data loading
# -----------------------------------------------------------------------------
target_naf_codes <- c("4711B", "4711C", "4711D", "4711F")

iris_shp <- st_read(dsn = raw_dir, layer = "georef-france-iris-millesime", quiet = TRUE)

sirene_layer <- "dataseed-sirene-1"
sirene_dsn_candidates <- c(
  file.path(cours_dir, "data", "actif"),
  file.path(cours_dir, "data", "raw"),
  file.path(cours_dir, "data")
)
sirene_dsn <- sirene_dsn_candidates[file.exists(file.path(sirene_dsn_candidates, paste0(sirene_layer, ".shp")))][1]
if (is.na(sirene_dsn)) stop("dataseed-sirene-1.shp introuvable.")
sirene_shp <- st_read(dsn = sirene_dsn, layer = sirene_layer, quiet = TRUE)

sirene_layer_avant <- "dataseed-sirene-2"
sirene_dsn_avant_candidates <- c(
  file.path(cours_dir, "data", "raw"),
  file.path(cours_dir, "data", "actif"),
  file.path(cours_dir, "data")
)
sirene_dsn_avant <- sirene_dsn_avant_candidates[file.exists(file.path(sirene_dsn_avant_candidates, paste0(sirene_layer_avant, ".shp")))][1]
if (is.na(sirene_dsn_avant)) stop("dataseed-sirene-2.shp introuvable.")
sirene_shp_avant <- st_read(dsn = sirene_dsn_avant, layer = sirene_layer_avant, quiet = TRUE)

# -----------------------------------------------------------------------------
# Scope
# -----------------------------------------------------------------------------
communes_nord_norm <- c(
  "GRENOBLE", "MEYLAN", "LATRONCHE", "CORENC",
  "SAINTMARTINLEVINOUX", "SAINTEGREVE"
)
communes_ouest_norm <- c("SASSENAGE", "FONTAINE", "SEYSSINETPARISET", "SEYSSINS")
communes_sudest_norm <- c("ECHIROLLES", "EYBENS", "SAINTMARTINDHERES", "GIERES")
communes_agglo_norm <- unique(c(communes_nord_norm, communes_ouest_norm, communes_sudest_norm))
communes_zone_cible_norm <- communes_nord_norm

names(sirene_shp) <- tolower(names(sirene_shp))
iris_shp <- iris_shp %>% mutate(commune_norm = normalize_commune(com_name))
sirene_shp <- sirene_shp %>% mutate(commune_norm = normalize_commune(commune_de_))

prepare_sirene_scope <- function(sirene_sf) {
  out <- sirene_sf
  out$code_naf <- normalize_naf(coalesce_chr(
    out, c("activite_pr.1", "activitepri.1", "activiteprincipaleetablissement", "activite_pr", "activitepri")
  ))
  out$Type_PV <- classify_type_pv(
    df = out,
    naf_cols = c("activite_pr.1", "activitepri.1", "activiteprincipaleetablissement", "activite_pr", "activitepri")
  )
  out %>% filter(code_naf %in% target_naf_codes, !is.na(Type_PV))
}

sirene_agglo_full_raw <- sirene_shp %>% filter(commune_norm %in% communes_agglo_norm)
sirene_agglo_full <- prepare_sirene_scope(sirene_agglo_full_raw)
sirene_zone_etude <- sirene_agglo_full %>% filter(commune_norm %in% communes_zone_cible_norm)

iris_agglo <- iris_shp %>% filter(commune_norm %in% communes_agglo_norm)
iris_zone_etude <- iris_agglo %>% filter(commune_norm %in% communes_zone_cible_norm)

if (!"Code_IRIS" %in% names(iris_zone_etude)) {
  if ("iris_code" %in% names(iris_zone_etude)) iris_zone_etude$Code_IRIS <- as.character(iris_zone_etude$iris_code)
  if ("code_iris" %in% names(iris_zone_etude)) iris_zone_etude$Code_IRIS <- as.character(iris_zone_etude$code_iris)
}
if (!"Code_IRIS" %in% names(iris_zone_etude)) stop("Identifiant IRIS introuvable dans iris_zone_etude.")
iris_zone_etude$Code_IRIS <- as.character(iris_zone_etude$Code_IRIS)

# -----------------------------------------------------------------------------
# Socio variables for ranking
# -----------------------------------------------------------------------------
iris_data <- read_excel(file.path(raw_dir, "iris_isere.xlsx"))
colnames(iris_data)[colnames(iris_data) == "CODE_IRIS"] <- "Code_IRIS"

iris_data$Pct_actifs <- iris_data$`Actifs_15-64_ans2` / iris_data$`Pop_15-64_ans`
iris_data$Pct_Menages_enfants <- (
  iris_data$Men_fam_princ_Couple_enfant_s + iris_data$Men_fam_princ_Famille_mono
) / iris_data$Menages
iris_data$Pct_Menages_voiture <- iris_data$Menages_une_voiture / iris_data$Menages
iris_data$Pct_Cadres_sup <- iris_data$Pop_15_ansplus_Cadres_Prof_intel_sup / iris_data$Pop_15_ansplus
iris_data$Densite_pop <- iris_data$Popution / iris_data$Superficie

vars_a_garder <- c(
  "Code_IRIS", "1er_quartile_euro", "Mediane_euro", "3e_quartile_euro",
  "Pct_actifs", "Pct_Menages_enfants", "Pct_Menages_voiture", "Pct_Cadres_sup", "Densite_pop"
)
iris_zone_etude <- merge(
  iris_zone_etude,
  iris_data[, vars_a_garder],
  by = "Code_IRIS",
  all.x = TRUE
)

# -----------------------------------------------------------------------------
# 2020 scope
# -----------------------------------------------------------------------------
sirene_zone_avant <- sirene_shp_avant %>%
  mutate(commune_norm = normalize_commune(libellecomm)) %>%
  filter(commune_norm %in% communes_zone_cible_norm & etatadminis == "Actif") %>%
  mutate(
    Enseigne = ifelse(is.na(enseigne1et) | enseigne1et == "", "ENSEIGNE_INCONNUE", enseigne1et),
    Classe = classeetabl
  )

sirene_zone_avant$code_naf <- normalize_naf(coalesce_chr(
  sirene_zone_avant,
  c("activitepri.1", "activite_pr.1", "activiteprincipaleetablissement", "activitepri", "activite_pr")
))
sirene_zone_avant$Type_PV <- classify_type_pv(
  df = sirene_zone_avant,
  naf_cols = c("activitepri.1", "activite_pr.1", "activitepri_1", "activiteprincipaleetablissement", "activitepri")
)
sirene_zone_avant <- sirene_zone_avant %>%
  filter(code_naf %in% target_naf_codes, !is.na(Type_PV))

# -----------------------------------------------------------------------------
# Point-in-IRIS counts
# -----------------------------------------------------------------------------
iris_id_col <- if ("Code_IRIS" %in% names(iris_zone_etude)) "Code_IRIS" else "iris_code"

sirene_zone_avant <- st_transform(sirene_zone_avant, st_crs(iris_zone_etude))
sirene_zone_etude <- st_transform(sirene_zone_etude, st_crs(iris_zone_etude))

joined_2020 <- st_join(
  sirene_zone_avant,
  iris_zone_etude %>% dplyr::select(dplyr::all_of(iris_id_col)),
  left = TRUE
)
joined_2025 <- st_join(
  sirene_zone_etude,
  iris_zone_etude %>% dplyr::select(dplyr::all_of(iris_id_col)),
  left = TRUE
)
sirene_zone_avant$Code_IRIS <- as.character(st_drop_geometry(joined_2020)[[iris_id_col]])
sirene_zone_etude$Code_IRIS <- as.character(st_drop_geometry(joined_2025)[[iris_id_col]])

compte_avant <- sirene_zone_avant %>%
  st_drop_geometry() %>%
  group_by(Code_IRIS) %>%
  summarise(Nb_pdv_2020 = n(), .groups = "drop")
compte_2025 <- sirene_zone_etude %>%
  st_drop_geometry() %>%
  group_by(Code_IRIS) %>%
  summarise(Nb_pdv_2025 = n(), .groups = "drop")

iris_zone_etude <- iris_zone_etude %>%
  left_join(compte_avant, by = "Code_IRIS") %>%
  left_join(compte_2025, by = "Code_IRIS") %>%
  mutate(
    Nb_pdv_2020 = coalesce(Nb_pdv_2020, 0L),
    Nb_pdv_2025 = coalesce(Nb_pdv_2025, 0L)
  )

# -----------------------------------------------------------------------------
# Drives count 2025
# -----------------------------------------------------------------------------
super_hyper_2025 <- sirene_zone_etude %>%
  filter(as.character(Type_PV) %in% c("super", "hyper")) %>%
  mutate(siret_chr = as.character(siret), siret = as.character(siret))

crossref_candidates <- c(
  file.path(cours_dir, "data", "processed", "crossref_supermarches_drives_zone_v3.csv"),
  file.path(cours_dir, "data", "processed", "crossref_supermarches_drives_zone.csv"),
  file.path(cours_dir, "data", "processed", "crossref_supermarches_drives.csv")
)
crossref_path <- crossref_candidates[file.exists(crossref_candidates)][1]

if (is.na(crossref_path)) {
  drives_2025 <- super_hyper_2025[0, ] %>% mutate(drives_types = NA_character_, match_method = NA_character_)
} else {
  crossref_drives <- readr::read_csv(crossref_path, show_col_types = FALSE) %>%
    mutate(
      siret_chr = as.character(siret),
      drive_trouve = as.logical(drive_trouve),
      drives_types = ifelse(
        is.na(drives_types) | str_squish(as.character(drives_types)) == "",
        "non_precise",
        as.character(drives_types)
      ),
      match_method = as.character(match_method)
    ) %>%
    filter(drive_trouve %in% TRUE) %>%
    distinct(siret_chr, .keep_all = TRUE) %>%
    select(siret_chr, drives_types, match_method)

  drives_2025 <- super_hyper_2025 %>%
    inner_join(crossref_drives, by = "siret_chr") %>%
    distinct(siret_chr, .keep_all = TRUE)
}

if (nrow(drives_2025) > 0) {
  iris_join_key <- "iris_join_code"
  drives_join <- st_join(
    drives_2025,
    iris_zone_etude %>%
      mutate(!!iris_join_key := as.character(.data[[iris_id_col]])) %>%
      dplyr::select(dplyr::all_of(iris_join_key)),
    left = TRUE
  )
  drives_2025$Code_IRIS <- as.character(st_drop_geometry(drives_join)[[iris_join_key]])
} else {
  drives_2025$Code_IRIS <- character(0)
}

compte_drives_2025 <- drives_2025 %>%
  st_drop_geometry() %>%
  filter(!is.na(Code_IRIS) & str_squish(Code_IRIS) != "") %>%
  group_by(Code_IRIS) %>%
  summarise(Nb_drives_2025 = n(), .groups = "drop")

iris_zone_etude <- iris_zone_etude %>%
  dplyr::select(-any_of("Nb_drives_2025")) %>%
  left_join(compte_drives_2025, by = "Code_IRIS") %>%
  mutate(Nb_drives_2025 = coalesce(as.integer(Nb_drives_2025), 0L))

# -----------------------------------------------------------------------------
# Isochrones / accessibility
# -----------------------------------------------------------------------------
# IMPORTANT:
# - By default, this script now REQUIRES real OTP isochrones.
# - Fallback buffers are disabled unless STRICT_ISOCHRONES=0 is explicitly set.
strict_isochrones <- Sys.getenv("STRICT_ISOCHRONES", "1") != "0"
otpcon <- connect_otp_any()
iso_mode <- if (is.null(otpcon)) "fallback_buffer" else "otp_reseau"
if (is.null(otpcon) && strict_isochrones) {
  stop(
    "OTP indisponible: impossible de produire des resultats fiables sans isochrones reseau. ",
    "Demarre OTP puis relance le script (ou force STRICT_ISOCHRONES=0 pour un mode approximation)."
  )
}

safe_prepare <- function(x) {
  tryCatch(prepare_points_for_otp(x, id_col = "siret"), error = function(e) x[0, ])
}

sirene_zone_etude_otp <- safe_prepare(sirene_zone_etude)
sirene_zone_avant_otp <- safe_prepare(sirene_zone_avant)

iso_walk_2025 <- build_multiformat_isochrones(
  points_sf = sirene_zone_etude_otp,
  otpcon = otpcon,
  mode = c("WALK"),
  profile = "walk",
  strict = strict_isochrones,
  id_col = "siret",
  maxWalkDistance = 1500
)
iso600_2025 <- iso_walk_2025 %>% filter(time == 600)
iris_zone_etude$Nb_supermarches_2025 <- if (nrow(iso600_2025) == 0) 0L else {
  rowSums(ifelse(st_intersects(x = iris_zone_etude, y = iso600_2025, sparse = FALSE), 1, 0))
}

iso_car_2025 <- build_multiformat_isochrones(
  points_sf = sirene_zone_etude_otp,
  otpcon = otpcon,
  mode = c("CAR"),
  profile = "car",
  strict = strict_isochrones,
  id_col = "siret"
)
iso_primary_car_2025 <- iso_car_2025 %>%
  mutate(Type_PV = as.character(Type_PV)) %>%
  filter(
    (Type_PV == "hyper" & time == 20 * 60) |
      (Type_PV == "super" & time == 10 * 60) |
      (Type_PV == "superette" & time == 5 * 60)
  )
iris_zone_etude$Nb_pdv_access_primaire_voiture_2025 <- if (nrow(iso_primary_car_2025) == 0) 0L else {
  rowSums(ifelse(st_intersects(x = iris_zone_etude, y = iso_primary_car_2025, sparse = FALSE), 1, 0))
}

iso_walk_2020 <- build_multiformat_isochrones(
  points_sf = sirene_zone_avant_otp,
  otpcon = otpcon,
  mode = c("WALK"),
  profile = "walk",
  strict = strict_isochrones,
  id_col = "siret",
  maxWalkDistance = 1500
)
iso600_2020 <- iso_walk_2020 %>% filter(time == 600)
iris_zone_etude$Nb_supermarches_2020 <- if (nrow(iso600_2020) == 0) 0L else {
  rowSums(ifelse(st_intersects(x = iris_zone_etude, y = iso600_2020, sparse = FALSE), 1, 0))
}

iso_car_2020 <- build_multiformat_isochrones(
  points_sf = sirene_zone_avant_otp,
  otpcon = otpcon,
  mode = c("CAR"),
  profile = "car",
  strict = strict_isochrones,
  id_col = "siret"
)
iso_primary_car_2020 <- iso_car_2020 %>%
  mutate(Type_PV = as.character(Type_PV)) %>%
  filter(
    (Type_PV == "hyper" & time == 20 * 60) |
      (Type_PV == "super" & time == 10 * 60) |
      (Type_PV == "superette" & time == 5 * 60)
  )
iris_zone_etude$Nb_pdv_access_primaire_voiture_2020 <- if (nrow(iso_primary_car_2020) == 0) 0L else {
  rowSums(ifelse(st_intersects(x = iris_zone_etude, y = iso_primary_car_2020, sparse = FALSE), 1, 0))
}

cov_2025 <- compute_coverage_pct(
  iris_sf = iris_zone_etude, iso_sf = iso_primary_car_2025, iris_id_col = iris_id_col
) %>% rename(couverture_pct_2025 = couverture_pct)
cov_2020 <- compute_coverage_pct(
  iris_sf = iris_zone_etude, iso_sf = iso_primary_car_2020, iris_id_col = iris_id_col
) %>% rename(couverture_pct_2020 = couverture_pct)

iris_zone_etude <- iris_zone_etude %>%
  select(-any_of(c("couverture_pct_2025", "couverture_pct_2020"))) %>%
  left_join(cov_2025 %>% select(all_of(c(iris_id_col, "couverture_pct_2025"))), by = iris_id_col) %>%
  left_join(cov_2020 %>% select(all_of(c(iris_id_col, "couverture_pct_2020"))), by = iris_id_col) %>%
  mutate(
    couverture_pct_2025 = coalesce(couverture_pct_2025, 0),
    couverture_pct_2020 = coalesce(couverture_pct_2020, 0)
  )

# -----------------------------------------------------------------------------
# Rankings A / B (same formulas as dashboard)
# -----------------------------------------------------------------------------
cols_needed <- c(
  "1er_quartile_euro", "Mediane_euro", "3e_quartile_euro",
  "Pct_actifs", "Pct_Cadres_sup", "Pct_Menages_enfants", "Pct_Menages_voiture",
  "Densite_pop", "Nb_pdv_access_primaire_voiture_2025",
  "couverture_pct_2025", "Nb_supermarches_2025", "Nb_pdv_2025", "Nb_drives_2025"
)
cols_ok <- intersect(cols_needed, names(iris_zone_etude))

candidats <- iris_zone_etude %>%
  st_drop_geometry() %>%
  select(Code_IRIS, iris_name, com_name, all_of(cols_ok)) %>%
  group_by(Code_IRIS, iris_name, com_name) %>%
  summarise(
    across(
      everything(),
      ~ {
        x <- suppressWarnings(as.numeric(.x))
        if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
      }
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Nb_pdv_access_primaire_voiture_2025 = coalesce(Nb_pdv_access_primaire_voiture_2025, 0),
    couverture_pct_2025 = coalesce(couverture_pct_2025, 0),
    Nb_supermarches_2025 = coalesce(Nb_supermarches_2025, 0),
    Nb_pdv_2025 = coalesce(Nb_pdv_2025, 0),
    Nb_drives_2025 = coalesce(Nb_drives_2025, 0)
  ) %>%
  mutate(
    s_q1 = minmax01(`1er_quartile_euro`),
    s_mediane = minmax01(Mediane_euro),
    s_q3 = minmax01(`3e_quartile_euro`),
    s_actifs = minmax01(Pct_actifs),
    s_cadres = minmax01(Pct_Cadres_sup),
    s_familles = minmax01(Pct_Menages_enfants),
    s_voiture = minmax01(Pct_Menages_voiture),
    s_densite = minmax01(log1p(Densite_pop))
  ) %>%
  mutate(
    score_total_A = row_weighted_mean(
      cur_data(),
      c("s_q1", "s_mediane", "s_q3", "s_actifs", "s_cadres", "s_familles", "s_voiture", "s_densite"),
      c(0.12, 0.20, 0.10, 0.14, 0.10, 0.11, 0.10, 0.13)
    ),
    s_faible_concurrence_car = 1 - minmax01(Nb_pdv_access_primaire_voiture_2025),
    s_sous_couverture = 1 - minmax01(couverture_pct_2025),
    s_faible_presence_pdv = 1 - minmax01(Nb_pdv_2025),
    s_faible_drive = 1 - minmax01(Nb_drives_2025),
    score_offre_gap_B = row_weighted_mean(
      cur_data(),
      c("s_faible_concurrence_car", "s_sous_couverture", "s_faible_presence_pdv", "s_faible_drive"),
      c(0.38, 0.32, 0.22, 0.08)
    ),
    score_total_B = 0.62 * score_total_A + 0.38 * score_offre_gap_B,
    rank_A = rank(-score_total_A, ties.method = "first"),
    rank_B = rank(-score_total_B, ties.method = "first"),
    score_total_A_100 = round(score_total_A * 100, 1),
    score_total_B_100 = round(score_total_B * 100, 1)
  ) %>%
  select(Code_IRIS, score_total_A_100, rank_A, score_total_B_100, rank_B)

# -----------------------------------------------------------------------------
# Final table
# -----------------------------------------------------------------------------
final_tbl <- iris_zone_etude %>%
  st_drop_geometry() %>%
  mutate(Code_IRIS = as.character(Code_IRIS)) %>%
  select(
    Code_IRIS, iris_name, com_name,
    Nb_pdv_access_primaire_voiture_2025,
    Nb_supermarches_2025,
    couverture_pct_2025,
    Nb_pdv_2025,
    Nb_drives_2025,
    Nb_pdv_access_primaire_voiture_2020,
    Nb_supermarches_2020,
    couverture_pct_2020
  ) %>%
  left_join(candidats, by = "Code_IRIS") %>%
  distinct(Code_IRIS, .keep_all = TRUE) %>%
  transmute(
    Code_IRIS = Code_IRIS,
    Nom_IRIS = iris_name,
    Commune = com_name,
    Nb_PDV_access_voiture_2025 = as.integer(round(Nb_pdv_access_primaire_voiture_2025)),
    Nb_supermarches_access_pied_2025 = as.integer(round(Nb_supermarches_2025)),
    Taux_couverture_IRIS_2025_pct = round(as.numeric(couverture_pct_2025), 3),
    Nb_PDV_dans_IRIS_2025 = as.integer(round(Nb_pdv_2025)),
    Nb_drives_dans_IRIS_2025 = as.integer(round(Nb_drives_2025)),
    Nb_PDV_access_voiture_2020 = as.integer(round(Nb_pdv_access_primaire_voiture_2020)),
    Nb_supermarches_access_pied_2020 = as.integer(round(Nb_supermarches_2020)),
    Taux_couverture_IRIS_2020_pct = round(as.numeric(couverture_pct_2020), 3),
    Score_classement_A = as.numeric(score_total_A_100),
    Rang_classement_A = as.integer(rank_A),
    Score_classement_B = as.numeric(score_total_B_100),
    Rang_classement_B = as.integer(rank_B)
  ) %>%
  arrange(Commune, Nom_IRIS)

global_info <- tibble::tribble(
  ~Parametre, ~Valeur, ~Details,
  "Seuil_accessibilite_voiture_dashboard",
  "Zone primaire hierarchisee: hyper=20 min, super=10 min, superette=5 min",
  "Utilise pour Nb_PDV_access_voiture_2025/2020 et couverture_2025/2020.",
  "Seuil_accessibilite_pied_dashboard",
  "Comptage sur isochrones WALK a 10 min (time==600)",
  "Profil walk: hyper=8/10 min, super=5/10 min, superette=5/10 min; le comptage retient la couronne 10 min.",
  "Regle_hierarchisation_points_de_vente",
  "Hierarchie par format pour la zone primaire voiture",
  "Filtre: (Type_PV==hyper & time==20*60) OR (Type_PV==super & time==10*60) OR (Type_PV==superette & time==5*60)."
) %>%
  mutate(
    Mode_isochrones_execute = iso_mode,
    Strict_isochrones = strict_isochrones
  )

out_table <- file.path(processed_dir, "table_iris_indicateurs_dashboard_v4.csv")
out_global <- file.path(processed_dir, "parametres_accessibilite_dashboard_v4.csv")

readr::write_csv(final_tbl, out_table, na = "")
readr::write_csv(global_info, out_global, na = "")

message("Table IRIS exportee: ", out_table)
message("Parametres globaux exportes: ", out_global)
message("Mode isochrones utilise: ", iso_mode)
message("Nb IRIS: ", nrow(final_tbl))

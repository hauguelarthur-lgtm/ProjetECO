#!/usr/bin/env Rscript

# Download open datasets for report 2 (BIO) without account/API key.
# Data are stored under Cours/rapport_2/data.
#
# Scope controls (env vars):
#   DOWNLOAD_SCOPE=zone_nord (default) | full
#   INCLUDE_NATIONAL=0 (default) | 1
#   INCLUDE_OVERPASS=1 (default) | 0
#
# Example:
#   DOWNLOAD_SCOPE=zone_nord INCLUDE_NATIONAL=0 Rscript download_open_data_bio.R

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install with install.packages('jsonlite').")
  }
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

sanitize_filename <- function(x) {
  x <- gsub("\\?.*$", "", x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "download.bin" else x
}

script_dir <- local({
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- args_full[grepl("^--file=", args_full)]
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
  } else {
    getwd()
  }
})

as_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "yes", "y", "on")
}

download_scope <- tolower(trimws(Sys.getenv("DOWNLOAD_SCOPE", "zone_nord")))
include_national <- as_flag(Sys.getenv("INCLUDE_NATIONAL", "0"))
include_overpass <- as_flag(Sys.getenv("INCLUDE_OVERPASS", "1"))

# Zone cible Nord (communes du notebook)
communes_nord_names <- c(
  "GRENOBLE", "MEYLAN", "LA TRONCHE", "CORENC", "SAINT-MARTIN-LE-VINOUX", "SAINT-EGREVE"
)
communes_nord_codes <- c("38185", "38229", "38516", "38187", "38423", "38382")

local_name_patterns <- c(
  "grenoble", "isere", "d38", "_38_", " 38 ", "aura", "auvergne-rhone-alpes",
  "lametro", "la metro", "saint-egreve", "saint-martin-le-vinoux", "meylan", "corenc", "tronche"
)

is_local_resource <- function(text) {
  txt <- tolower(paste(text, collapse = " "))
  any(vapply(local_name_patterns, function(p) grepl(p, txt, fixed = TRUE), logical(1)))
}

data_dir <- file.path(script_dir, "data")
raw_dir <- file.path(data_dir, "raw")
meta_dir <- file.path(data_dir, "meta")
logs_dir <- file.path(data_dir, "logs")
filtered_dir <- file.path(data_dir, "filtered", "zone_nord")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(filtered_dir, recursive = TRUE, showWarnings = FALSE)

download_log <- list()

add_log <- function(source, name, url, dest, status, note = "") {
  download_log[[length(download_log) + 1]] <<- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    source = source,
    name = name,
    url = url,
    destination = dest,
    status = status,
    note = note,
    stringsAsFactors = FALSE
  )
}

download_one <- function(source, name, url, dest_file, overwrite = FALSE, timeout_sec = 900) {
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest_file) && !overwrite && file.info(dest_file)$size > 0) {
    add_log(source, name, url, dest_file, "skip_exists", "File already present")
    return(invisible(TRUE))
  }

  ok <- TRUE
  note <- ""
  tryCatch(
    {
      utils::download.file(
        url = url,
        destfile = dest_file,
        mode = "wb",
        method = "libcurl",
        quiet = FALSE,
        cacheOK = TRUE
      )
    },
    error = function(e) {
      ok <<- FALSE
      note <<- conditionMessage(e)
    }
  )

  if (ok && file.exists(dest_file) && file.info(dest_file)$size > 0) {
    add_log(source, name, url, dest_file, "ok", "")
  } else {
    if (file.exists(dest_file) && file.info(dest_file)$size == 0) {
      unlink(dest_file)
    }
    if (!nzchar(note)) note <- "Download failed or empty file"
    add_log(source, name, url, dest_file, "error", note)
  }

  invisible(ok)
}

extract_resources_df <- function(meta_obj) {
  res <- meta_obj$resources
  if (is.null(res)) return(data.frame())

  if (is.data.frame(res)) {
    out <- res
  } else if (is.list(res)) {
    out <- do.call(
      rbind,
      lapply(res, function(r) {
        data.frame(
          id = as.character(r$id %||% NA_character_),
          title = as.character(r$title %||% NA_character_),
          format = as.character(r$format %||% NA_character_),
          filetype = as.character(r$filetype %||% NA_character_),
          url = as.character(r$url %||% NA_character_),
          stringsAsFactors = FALSE
        )
      })
    )
  } else {
    return(data.frame())
  }

  need_cols <- c("id", "title", "format", "filetype", "url")
  for (cc in need_cols) {
    if (!cc %in% names(out)) out[[cc]] <- NA_character_
  }
  out <- out[, need_cols, drop = FALSE]
  out <- out[!is.na(out$url) & nzchar(trimws(out$url)), , drop = FALSE]
  out
}

download_dataset_resources <- function(dataset_api_url, dataset_code, local_only = TRUE, accept_if_no_local_match = FALSE) {
  dataset_dir <- file.path(raw_dir, dataset_code)
  dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)

  meta_file <- file.path(meta_dir, paste0(dataset_code, "_dataset_meta.json"))
  download_one(
    source = "data.gouv.dataset.api",
    name = paste0(dataset_code, "_meta"),
    url = dataset_api_url,
    dest_file = meta_file,
    overwrite = TRUE
  )

  if (!file.exists(meta_file) || file.info(meta_file)$size == 0) {
    return(invisible(NULL))
  }

  meta <- tryCatch(jsonlite::fromJSON(meta_file, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(meta)) {
    add_log("data.gouv.dataset.api", dataset_code, dataset_api_url, meta_file, "error", "Cannot parse dataset metadata JSON")
    return(invisible(NULL))
  }

  res_df <- extract_resources_df(meta)
  if (nrow(res_df) == 0) {
    add_log("data.gouv.dataset.api", dataset_code, dataset_api_url, dataset_dir, "warn_no_resources", "No downloadable resources found")
    return(invisible(NULL))
  }

  if (local_only) {
    keep_idx <- vapply(seq_len(nrow(res_df)), function(i) {
      is_local_resource(c(res_df$title[i], res_df$url[i], res_df$format[i], res_df$filetype[i]))
    }, logical(1))

    if (!any(keep_idx) && !accept_if_no_local_match) {
      add_log(
        "data.gouv.dataset.api",
        dataset_code,
        dataset_api_url,
        dataset_dir,
        "skip_scope",
        "No local (Grenoble/38/Nord) resource detected in dataset metadata"
      )
      return(invisible(NULL))
    }
    if (any(keep_idx)) {
      res_df <- res_df[keep_idx, , drop = FALSE]
    }
  }

  for (i in seq_len(nrow(res_df))) {
    u <- as.character(res_df$url[i])
    rid <- as.character(res_df$id[i])
    title <- as.character(res_df$title[i])
    fmt <- tolower(as.character(res_df$format[i]))

    base_name <- sanitize_filename(basename(gsub("\\?.*$", "", u)))
    if (!nzchar(base_name) || identical(base_name, "download.bin")) {
      ext <- if (!is.na(fmt) && nzchar(fmt)) paste0(".", fmt) else ".bin"
      base_name <- paste0("resource_", rid %||% sprintf("%03d", i), ext)
    }

    out_file <- file.path(
      dataset_dir,
      sprintf("%02d_%s_%s", i, sanitize_filename(title %||% dataset_code), base_name)
    )
    download_one(
      source = paste0("data.gouv.resource.", dataset_code),
      name = title %||% paste0(dataset_code, "_", i),
      url = u,
      dest_file = out_file
    )
  }
}

download_overpass_default <- function() {
  query <- paste(
    "[out:json][timeout:180];",
    "area[name=\"Grenoble-Alpes Metropole\"]->.a;",
    "(node[\"shop\"=\"organic\"](area.a); way[\"shop\"=\"organic\"](area.a););",
    "out center;",
    sep = "\n"
  )
  query_file <- file.path(meta_dir, "overpass_default_query.txt")
  writeLines(query, query_file, useBytes = TRUE)

  overpass_url <- paste0(
    "https://overpass-api.de/api/interpreter?data=",
    utils::URLencode(query, reserved = TRUE)
  )
  out_file <- file.path(raw_dir, "overpass", "osm_organic_grenoble_metropole.json")
  download_one(
    source = "overpass",
    name = "osm_organic_grenoble_metropole",
    url = overpass_url,
    dest_file = out_file,
    overwrite = TRUE
  )
}

cat("Data root:", data_dir, "\n")
cat("Download scope:", download_scope, "\n")
cat("Include national dumps:", include_national, "\n")
cat("Starting downloads...\n")

# -------------------------------------------------------------------
# Direct links (no account/API key required)
# -------------------------------------------------------------------
direct_links <- list(
  list(
    source = "geopf",
    name = "contours_iris_2023_7z",
    scope_class = "national",
    url = "https://data.geopf.fr/telechargement/download/CONTOURS-IRIS/CONTOURS-IRIS_3-0__SHP__FRA_2023-01-01/CONTOURS-IRIS_3-0__SHP__FRA_2023-01-01.7z",
    dest = file.path(raw_dir, "iris", "CONTOURS-IRIS_3-0__SHP__FRA_2023-01-01.7z")
  ),
  list(
    source = "geopf",
    name = "geopf_download_capabilities",
    scope_class = "meta",
    url = "https://data.geopf.fr/telechargement/capabilities",
    dest = file.path(meta_dir, "geopf_capabilities.xml")
  ),
  list(
    source = "insee",
    name = "reference_IRIS_geo2024_zip",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/7708995/reference_IRIS_geo2024.zip",
    dest = file.path(raw_dir, "insee", "reference_IRIS_geo2024.zip")
  ),
  list(
    source = "insee",
    name = "rp_2022_pop_zip",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/8647014/base-ic-evol-struct-pop-2022_csv.zip",
    dest = file.path(raw_dir, "insee", "rp_2022", "base-ic-evol-struct-pop-2022_csv.zip")
  ),
  list(
    source = "insee",
    name = "rp_2022_logement_zip",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/8647012/base-ic-logement-2022_csv.zip",
    dest = file.path(raw_dir, "insee", "rp_2022", "base-ic-logement-2022_csv.zip")
  ),
  list(
    source = "insee",
    name = "rp_2022_diplomes_zip",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/8647010/base-ic-diplomes-formation-2022_csv.zip",
    dest = file.path(raw_dir, "insee", "rp_2022", "base-ic-diplomes-formation-2022_csv.zip")
  ),
  list(
    source = "insee",
    name = "rp_2022_familles_zip",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/8647008/base-ic-couples-familles-menages-2022_csv.zip",
    dest = file.path(raw_dir, "insee", "rp_2022", "base-ic-couples-familles-menages-2022_csv.zip")
  ),
  list(
    source = "insee",
    name = "rp_2022_activite_zip",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/8647006/base-ic-activite-residents-2022_csv.zip",
    dest = file.path(raw_dir, "insee", "rp_2022", "base-ic-activite-residents-2022_csv.zip")
  ),
  list(
    source = "insee",
    name = "bpe_iris_2024_zip",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/8217527/ds_bpe_iris_2024_geo_2024.zip",
    dest = file.path(raw_dir, "insee", "bpe_2024", "ds_bpe_iris_2024_geo_2024.zip")
  ),
  list(
    source = "insee",
    name = "bpe24_parquet",
    scope_class = "national",
    url = "https://www.insee.fr/fr/statistiques/fichier/8217525/BPE24.parquet",
    dest = file.path(raw_dir, "insee", "bpe_2024", "BPE24.parquet")
  ),
  list(
    source = "data.gouv",
    name = "sirene_geo_etudes_parquet",
    scope_class = "national",
    url = "https://www.data.gouv.fr/api/1/datasets/r/672007af-0146-491f-835c-8314d63fa44e",
    dest = file.path(raw_dir, "sirene", "sirene_geo_etudes.parquet")
  ),
  list(
    source = "data.gouv",
    name = "sirene_geo_etudes_zip",
    scope_class = "national",
    url = "https://www.data.gouv.fr/api/1/datasets/r/ba6a4e4c-aac6-4764-bbd2-f80ae345afc5",
    dest = file.path(raw_dir, "sirene", "sirene_geo_etudes.zip")
  ),
  list(
    source = "mobilites_m",
    name = "stationnement_normes_csv",
    scope_class = "local",
    url = "https://data.mobilites-m.fr/api/normes/lieuxstationnements?epci=LaMetro",
    dest = file.path(raw_dir, "mobilite", "stationnement", "lieuxstationnements_lametro.csv")
  ),
  list(
    source = "mobilites_m",
    name = "stationnement_points_json",
    scope_class = "local",
    url = "https://data.mobilites-m.fr/api/points/json?types=parking",
    dest = file.path(raw_dir, "mobilite", "stationnement", "parkings_points.json")
  ),
  list(
    source = "mobilites_m",
    name = "stationnement_temps_reel_json",
    scope_class = "local",
    url = "https://data.mobilites-m.fr/api/dyn/parking/json",
    dest = file.path(raw_dir, "mobilite", "stationnement", "parkings_temps_reel.json")
  ),
  list(
    source = "mobilites_m",
    name = "gtfs_sem_zip",
    scope_class = "local",
    url = "https://data.mobilites-m.fr/api/gtfs/SEM",
    dest = file.path(raw_dir, "mobilite", "gtfs", "gtfs_SEM.zip")
  ),
  list(
    source = "mobilites_m",
    name = "gtfs_c38_zip",
    scope_class = "local",
    url = "https://data.mobilites-m.fr/api/gtfs/C38",
    dest = file.path(raw_dir, "mobilite", "gtfs", "gtfs_C38.zip")
  ),
  list(
    source = "metropole_grenoble",
    name = "pistes_49_communes_json",
    scope_class = "local",
    url = "https://data.metropolegrenoble.fr/sites/default/files/dataset/2023/02/22/894dc599-33c6-4f62-86a1-b2250d34fdc0/json",
    dest = file.path(raw_dir, "mobilite", "velo", "pistes_49_communes.json")
  ),
  list(
    source = "mobilites_m",
    name = "pistes_api_lametro_json",
    scope_class = "local",
    url = "https://data.mobilites-m.fr/api/lines/json?types=chronovelo,tempovelo,voieverte,veloamenage,velononamenage,velodifficile&epci=LaMetro",
    dest = file.path(raw_dir, "mobilite", "velo", "pistes_api_lametro.json")
  ),
  list(
    source = "geopf",
    name = "ocsge_38_shp_7z",
    scope_class = "local",
    url = "https://data.geopf.fr/telechargement/download/OCSGE/OCSGE_1-1__SHP_LAMB93_D038_2021-10-07/OCSGE_1-1__SHP_LAMB93_D038_2021-10-07.7z",
    dest = file.path(raw_dir, "environnement", "ocsge", "OCSGE_1-1__SHP_LAMB93_D038_2021-10-07.7z")
  ),
  list(
    source = "geopf",
    name = "ocsge_38_gpkg",
    scope_class = "local",
    url = "https://data.geopf.fr/telechargement/download/OCSGE/OCSGE_1-1__GPKG_LAMB93_D038_2021-10-07/OCSGE_1-1__GPKG_LAMB93_D038_2021-10-07.gpkg",
    dest = file.path(raw_dir, "environnement", "ocsge", "OCSGE_1-1__GPKG_LAMB93_D038_2021-10-07.gpkg")
  )
)

for (it in direct_links) {
  scope_class <- it$scope_class %||% "national"
  is_scope_skip <- identical(download_scope, "zone_nord") && !include_national && identical(scope_class, "national")
  if (is_scope_skip) {
    add_log(it$source, it$name, it$url, it$dest, "skip_scope", "Skipped in zone_nord mode (national dump)")
    next
  }
  # Keep one compact format in zone_nord mode to reduce volume.
  if (identical(download_scope, "zone_nord") && !include_national) {
    if (it$name %in% c("sirene_geo_etudes_zip", "ocsge_38_shp_7z")) {
      add_log(it$source, it$name, it$url, it$dest, "skip_scope", "Skipped duplicate/heavier format in zone_nord mode")
      next
    }
  }
  download_one(
    source = it$source,
    name = it$name,
    url = it$url,
    dest_file = it$dest
  )
}

# Overpass endpoint does not need API key; include a default query.
if (include_overpass) {
  download_overpass_default()
}

# -------------------------------------------------------------------
# Dataset APIs (data.gouv) that expose downloadable resources
# -------------------------------------------------------------------
download_dataset_resources(
  dataset_api_url = "https://www.data.gouv.fr/api/1/datasets/demandes-de-valeurs-foncieres/",
  dataset_code = "dvf",
  local_only = identical(download_scope, "zone_nord") && !include_national,
  accept_if_no_local_match = FALSE
)
download_dataset_resources(
  dataset_api_url = "https://www.data.gouv.fr/api/1/datasets/indices-atmo/",
  dataset_code = "indices_atmo",
  local_only = identical(download_scope, "zone_nord") && !include_national,
  accept_if_no_local_match = FALSE
)
download_dataset_resources(
  dataset_api_url = "https://www.data.gouv.fr/api/1/datasets/professionnels-engages-en-bio/",
  dataset_code = "professionnels_bio",
  local_only = identical(download_scope, "zone_nord") && !include_national,
  accept_if_no_local_match = FALSE
)

# -------------------------------------------------------------------
# Write log
# -------------------------------------------------------------------
if (length(download_log) == 0) {
  download_df <- data.frame()
} else {
  download_df <- do.call(rbind, download_log)
}

log_file <- file.path(logs_dir, "download_log.csv")
utils::write.csv(download_df, log_file, row.names = FALSE, fileEncoding = "UTF-8", na = "")

ok_n <- if (nrow(download_df) == 0) 0 else sum(download_df$status == "ok")
skip_n <- if (nrow(download_df) == 0) 0 else sum(download_df$status == "skip_exists")
err_n <- if (nrow(download_df) == 0) 0 else sum(download_df$status == "error")
warn_n <- if (nrow(download_df) == 0) 0 else sum(grepl("^warn", download_df$status))

cat("\nDownload summary:\n")
cat("  OK          :", ok_n, "\n")
cat("  SKIP EXISTS :", skip_n, "\n")
cat("  WARN        :", warn_n, "\n")
cat("  ERROR       :", err_n, "\n")
cat("Log file      :", log_file, "\n")
cat("Data folder   :", raw_dir, "\n")
cat("Filtered dir  :", filtered_dir, "\n")

# -------------------------------------------------------------------
# End-of-script notes for links requiring special API key/account
# -------------------------------------------------------------------
# DATAtourisme API:
#   Base URL: https://api.datatourisme.fr/v1
#   Requires authentication token/account according to API docs.
#   Not downloaded automatically by this script.

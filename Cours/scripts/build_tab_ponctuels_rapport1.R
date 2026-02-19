#!/usr/bin/env Rscript

# =============================================================================
# build_tab_ponctuels_rapport1.R
#
# Objectif:
#   Construire la table ponctuelle des points de vente (super/hyper) pour la
#   zone d'etude grenobloise (Tache 1.1) et son dictionnaire de donnees.
#
# Inputs principaux (priorites geres dans le script):
#   - SIRENE geolocalisee: dataseed-sirene-1 / dataseed-sirene-2 (actif/raw/data)
#   - Zone d'etude: zone_etude.gpkg/shp/geojson (processed/raw/data)
#   - IRIS: georef-france-iris-millesime.* (raw puis data)
#   - Drives (optionnel): drives_grenoble.csv (actif/raw/data)
#
# Outputs:
#   - Cours/data/processed/GROUPEXX_TabPonctuels_rapport1.csv
#   - Cours/data/processed/GROUPEXX_DicoDonnées_rapport1.csv
#
# Lancement (depuis la racine du repo):
#   Rscript Cours/scripts/build_tab_ponctuels_rapport1.R
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(readr)
  library(lubridate)
})

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

find_first_existing <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (length(paths) == 0) return(NA_character_)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

normalize_colnames <- function(nm) {
  nm |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

normalize_enseigne <- function(x) {
  y <- x |>
    as.character() |>
    stringr::str_replace_all("[\\r\\n\\t]+", " ") |>
    stringr::str_squish()
  y[y %in% c("", "NA", "N/A", "NULL", "[ND]", "ND", "Missing", "MISSING", "SANS ENSEIGNE")] <- NA_character_
  y <- stringr::str_to_upper(y)
  y
}

format_date_fr <- function(x) {
  if (inherits(x, "Date")) {
    out <- format(x, "%d/%m/%Y")
    out[out == "NA/NA/NA"] <- NA_character_
    return(out)
  }

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    out <- format(as.Date(x), "%d/%m/%Y")
    out[out == "NA/NA/NA"] <- NA_character_
    return(out)
  }

  sx <- as.character(x)
  sx <- stringr::str_squish(sx)
  sx[sx %in% c("", "NA", "N/A", "NULL")] <- NA_character_

  parsed <- suppressWarnings(lubridate::parse_date_time(
    sx,
    orders = c(
      "Ymd HMS z", "Ymd HMS", "Ymd HM", "Ymd",
      "dmY HMS", "dmY HM", "dmY",
      "mdY HMS", "mdY HM", "mdY"
    ),
    tz = "Europe/Paris"
  ))
  out <- format(as.Date(parsed), "%d/%m/%Y")
  out[out == "NA/NA/NA"] <- NA_character_
  out
}

normalize_address_text <- function(x) {
  z <- x |> as.character()
  z <- iconv(z, from = "", to = "ASCII//TRANSLIT")
  z <- ifelse(is.na(z), as.character(x), z)
  z <- stringr::str_to_upper(z)
  z <- stringr::str_replace_all(z, "[,;]", " ")
  z <- stringr::str_replace_all(z, "[^A-Z0-9 ]", " ")
  z <- stringr::str_squish(z)
  z[z %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  z
}

normalize_address_core <- function(x) {
  z <- normalize_address_text(x)
  z <- stringr::str_replace(z, "\\b[0-9]{5}\\b.*$", "")
  z <- stringr::str_squish(z)
  z[z == ""] <- NA_character_
  z
}

normalize_address_signature <- function(x) {
  z <- normalize_address_core(x)
  z <- stringr::str_replace_all(z, "\\b(BIS|TER|QUATER)\\b", " ")
  z <- stringr::str_replace_all(
    z,
    "\\b(RUE|AVENUE|AV|BOULEVARD|BD|CHEMIN|ROUTE|RTE|COURS|PLACE|PL|VOIE|ALLEE|ALL|QUAI|IMPASSE|ZA|ZI|ZAC|LOTISSEMENT|LIEU|DIT|LIEUDIT)\\b",
    " "
  )
  z <- stringr::str_replace_all(z, "\\b(DE|DU|DES|LA|LE|LES|D|L)\\b", " ")
  z <- stringr::str_squish(z)
  z[z == ""] <- NA_character_
  z
}

detect_delim <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(first_line) == 0) return(",")
  n_sc <- stringr::str_count(first_line[[1]], ";")
  n_cm <- stringr::str_count(first_line[[1]], ",")
  if (n_sc > n_cm) ";" else ","
}

paste_unique_or_na <- function(x) {
  x <- sort(unique(na.omit(as.character(x))))
  if (length(x) == 0) NA_character_ else paste(x, collapse = "|")
}

append_source_tag <- function(x, tag) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "none"
  has_tag <- vapply(strsplit(x, "\\|", fixed = FALSE), function(parts) tag %in% parts, logical(1))
  out <- dplyr::if_else(
    x == "none",
    tag,
    dplyr::if_else(has_tag, x, paste0(x, "|", tag))
  )
  as.character(out)
}

normalize_naf_display <- function(code) {
  z <- code |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9]", "")
  z[z %in% c("", "NA")] <- NA_character_

  out <- z
  idx_4711 <- !is.na(out) & stringr::str_detect(out, "^4711[A-Z]$")
  out[idx_4711] <- paste0("47.11", stringr::str_sub(out[idx_4711], 5, 5))

  idx_4 <- !is.na(out) & stringr::str_detect(out, "^[0-9]{4}[A-Z]$")
  out[idx_4] <- paste0(
    stringr::str_sub(out[idx_4], 1, 2), ".",
    stringr::str_sub(out[idx_4], 3, 4),
    stringr::str_sub(out[idx_4], 5, 5)
  )

  out
}

extract_naf_code <- function(df) {
  cand <- c("activitepri_1", "activitepri_2", "activitepri", "activiteprincipaleetablissement")
  cand <- cand[cand %in% names(df)]
  if (length(cand) == 0) return(rep(NA_character_, nrow(df)))

  out <- rep(NA_character_, nrow(df))
  for (cc in cand) {
    v <- as.character(df[[cc]])
    fill <- is.na(out) | out == ""
    out[fill] <- v[fill]
  }
  normalize_naf_display(out)
}

map_naf_libelle <- function(code_naf) {
  z <- code_naf |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9]", "")
  out <- rep(NA_character_, length(z))

  out[stringr::str_detect(z, "^4711D$")] <- "Supermarches"
  out[stringr::str_detect(z, "^4711F$")] <- "Hypermarches"
  out[stringr::str_detect(z, "^4711[ABCDE]$")] <- "Commerce d'alimentation generale (format non specialise)"
  out[stringr::str_detect(z, "^4711")] <- "Commerce de detail a predominance alimentaire en magasin non specialise"

  out
}

normalize_bool <- function(x) {
  z <- x |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_squish()
  dplyr::case_when(
    z %in% c("OUI", "TRUE", "1", "Y", "YES") ~ TRUE,
    z %in% c("NON", "FALSE", "0", "N", "NO") ~ FALSE,
    TRUE ~ NA
  )
}

normalize_commune <- function(x) {
  z <- as.character(x)
  z <- iconv(z, from = "", to = "ASCII//TRANSLIT")
  z <- ifelse(is.na(z), as.character(x), z)
  z <- stringr::str_to_upper(z)
  z <- stringr::str_replace_all(z, "[^A-Z0-9]", "")
  z
}

normalize_sirene_schema <- function(x) {
  n <- names(x)

  rename_if_exists <- function(data, from, to) {
    if (!(to %in% names(data)) && (from %in% names(data))) {
      data <- dplyr::rename(data, !!to := !!rlang::sym(from))
    }
    data
  }

  # Colonnes coeur
  x <- rename_if_exists(x, "SIREN", "siren")
  x <- rename_if_exists(x, "SIRET", "siret")
  x <- rename_if_exists(x, "Commune_de_", "libellecomm")
  x <- rename_if_exists(x, "Etat_admini", "etatadminis")
  x <- rename_if_exists(x, "Etablisseme", "etablisseme")
  x <- rename_if_exists(x, "SIRET_du_si", "siretsiegeu")

  # Enseignes / denomination
  x <- rename_if_exists(x, "Enseigne_de", "enseigne1et")
  x <- rename_if_exists(x, "Enseigne_de_1", "enseigne2et")
  x <- rename_if_exists(x, "Enseigne_de_2", "enseigne3et")
  x <- rename_if_exists(x, "Enseigne_de.1", "enseigne2et")
  x <- rename_if_exists(x, "Enseigne_de.2", "enseigne3et")
  x <- rename_if_exists(x, "Denominatio", "denominatio")
  x <- rename_if_exists(x, "Denominatio.1", "denominatio_1")
  x <- rename_if_exists(x, "denominatio.1", "denominatio_1")
  x <- rename_if_exists(x, "Nom_de_la_p", "nomuniteleg")
  x <- rename_if_exists(x, "Nom_dusage_", "nomusageuni")

  # Activite / NAF
  x <- rename_if_exists(x, "Activite_pr.1", "activitepri_1")
  x <- rename_if_exists(x, "Activite_pr.2", "activitepri_2")
  x <- rename_if_exists(x, "Activite_pr", "activitepri")
  x <- rename_if_exists(x, "activitepri.1", "activitepri_1")
  x <- rename_if_exists(x, "activitepri.2", "activitepri_2")

  # Adresse
  x <- rename_if_exists(x, "Numero_de_v", "numerovoiee")
  x <- rename_if_exists(x, "Indice_de_r", "indicerepet")
  x <- rename_if_exists(x, "Type_de_voi", "typevoieeta")
  x <- rename_if_exists(x, "Libelle_de_", "libellevoie")
  x <- rename_if_exists(x, "Code_postal", "codepostale")
  x <- rename_if_exists(x, "Complement_", "complementa")
  x <- rename_if_exists(x, "Adresse_de_", "adresseetab")
  x <- rename_if_exists(x, "Premiere_li", "l1_adressag")

  # Dates
  x <- rename_if_exists(x, "Date_de_cre", "datecreatio")
  x <- rename_if_exists(x, "Date_du_deb", "datedebutet")
  x <- rename_if_exists(x, "Date_de_la_", "datedernier")

  # Effectifs
  x <- rename_if_exists(x, "Tranche_de_.2", "trancheeffe_2")
  x <- rename_if_exists(x, "trancheeffe.2", "trancheeffe_2")

  # Si nomuniteleg existe deja en minuscule dans d'autres jeux
  if (!("nomuniteleg" %in% names(x)) && ("nomuniteleg" %in% n)) {
    # rien a faire
    invisible(NULL)
  }

  # Colonnes minimales
  mandatory <- c(
    "siren", "siret", "libellecomm", "etatadminis", "etablisseme", "siretsiegeu",
    "enseigne1et", "enseigne2et", "enseigne3et",
    "denominatio", "denominatio_1", "nomuniteleg", "nomusageuni",
    "activitepri_1", "activitepri_2", "activitepri",
    "numerovoiee", "indicerepet", "typevoieeta", "libellevoie", "codepostale",
    "complementa", "adresseetab", "l1_adressag",
    "datecreatio", "datedebutet", "datedernier", "trancheeffe_2"
  )
  for (cc in mandatory) {
    if (!(cc %in% names(x))) x[[cc]] <- NA_character_
  }

  x
}

make_adresse_postale <- function(df) {
  # Si adresse complete existe, elle est prioritaire.
  full_addr <- df$adresseetab |>
    as.character() |>
    stringr::str_squish()
  full_addr[full_addr %in% c("", "NA", "N/A", "NULL")] <- NA_character_

  pieces <- cbind(
    as.character(df$numerovoiee),
    as.character(df$indicerepet),
    as.character(df$typevoieeta),
    as.character(df$libellevoie),
    as.character(df$complementa),
    as.character(df$codepostale),
    as.character(df$libellecomm)
  )

  build_addr <- apply(pieces, 1, function(v) {
    v <- v |>
      stringr::str_squish() |>
      (\(x) x[!(is.na(x) | x == "" | x %in% c("NA", "N/A", "NULL"))])()
    if (length(v) == 0) return(NA_character_)
    paste(v, collapse = " ")
  })

  out <- dplyr::coalesce(full_addr, build_addr)
  out <- stringr::str_squish(out)
  out[out == ""] <- NA_character_
  out
}

classify_type_point_vente <- function(code_naf, enseigne, denomination_usuelle, raison_sociale, trancheeffe_2 = NULL) {
  naf_norm <- code_naf |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9]", "")

  txt <- paste(enseigne, denomination_usuelle, raison_sociale, sep = " | ") |>
    as.character() |>
    stringr::str_to_upper()

  is_hyper_kw <- stringr::str_detect(
    txt,
    "HYPERMARCHE|HYPER U|GEANT|CORA|AUCHAN HYPER|CARREFOUR HYPER"
  )
  is_super_kw <- stringr::str_detect(
    txt,
    "SUPERMARCHE|CARREFOUR MARKET|CARREFOUR CITY|CARREFOUR EXPRESS|INTERMARCHE|LIDL|ALDI|SUPER U|U EXPRESS|MONOPRIX|BIOCOOP|NETTO|UTILE|COCCINELLE"
  )

  # Heuristique taille (si dispo) utile pour departager certains cas ambigus.
  size_txt <- if (is.null(trancheeffe_2)) rep(NA_character_, length(txt)) else as.character(trancheeffe_2)
  is_large <- stringr::str_detect(
    stringr::str_to_upper(size_txt),
    "250 A 499|500 A 999|1 000 A 1 999|2 000 A 4 999|5 000 A 9 999|10 000"
  )

  dplyr::case_when(
    stringr::str_detect(naf_norm, "^4711F$") ~ "hyper",
    is_hyper_kw & is_large ~ "hyper",
    is_hyper_kw ~ "hyper",
    stringr::str_detect(naf_norm, "^4711") ~ "super",
    is_super_kw ~ "super",
    TRUE ~ "autre"
  )
}

read_zone_sf <- function(cours_dir, raw_dir, processed_dir, data_dir) {
  zone_path <- NA_character_
  zone_source <- NA_character_

  p_processed <- file.path(processed_dir, "zone_etude.gpkg")
  if (file.exists(p_processed)) {
    zone_path <- p_processed
    zone_source <- "processed/zone_etude.gpkg"
  } else {
    raw_candidates <- c(
      file.path(raw_dir, "zone_etude.gpkg"),
      file.path(raw_dir, "zone_etude.shp"),
      file.path(raw_dir, "zone_etude.geojson"),
      file.path(raw_dir, "zone_etude.json")
    )
    p_raw <- find_first_existing(raw_candidates)
    if (!is.na(p_raw)) {
      zone_path <- p_raw
      zone_source <- paste0("raw/", basename(p_raw))
    } else {
      data_candidates <- c(
        file.path(data_dir, "zone_etude.gpkg"),
        file.path(data_dir, "zone_etude.shp"),
        file.path(data_dir, "zone_etude.geojson"),
        file.path(data_dir, "zone_etude.json")
      )
      p_data <- find_first_existing(data_candidates)
      if (!is.na(p_data)) {
        zone_path <- p_data
        zone_source <- paste0("data/", basename(p_data))
      }
    }
  }

  if (!is.na(zone_path)) {
    ext <- tolower(tools::file_ext(zone_path))
    if (ext == "gpkg") {
      lyr <- sf::st_layers(zone_path)$name
      if (length(lyr) == 1) {
        zone_sf <- sf::st_read(zone_path, layer = lyr[[1]], quiet = TRUE)
        message("Zone d'etude chargee: ", zone_source, " (layer: ", lyr[[1]], ")")
      } else if ("zone_etude" %in% lyr) {
        zone_sf <- sf::st_read(zone_path, layer = "zone_etude", quiet = TRUE)
        message("Zone d'etude chargee: ", zone_source, " (layer: zone_etude)")
      } else {
        warning(
          "zone_etude.gpkg contient plusieurs layers. Premier layer utilise: ",
          lyr[[1]]
        )
        zone_sf <- sf::st_read(zone_path, layer = lyr[[1]], quiet = TRUE)
      }
    } else {
      zone_sf <- sf::st_read(zone_path, quiet = TRUE)
      message("Zone d'etude chargee: ", zone_source)
    }
    zone_sf <- sf::st_make_valid(zone_sf)
    return(list(zone_sf = zone_sf, zone_source = zone_source, fallback_bbox = FALSE))
  }

  warning(
    "Aucun fichier zone_etude trouve. Fallback sur bbox Grenoble+Nord. ",
    "Risque: perimetre approximatif, verifier manuellement."
  )
  bb <- sf::st_bbox(c(xmin = 5.64, ymin = 45.12, xmax = 5.83, ymax = 45.30), crs = sf::st_crs(4326))
  zone_sf <- sf::st_as_sfc(bb) |> sf::st_sf(id = "bbox_fallback", geometry = _)
  list(zone_sf = zone_sf, zone_source = "fallback_bbox_grenoble_nord", fallback_bbox = TRUE)
}

count_4711_in_zone <- function(sirene_sf, zone_sf) {
  if (nrow(sirene_sf) == 0) return(0L)
  z <- zone_sf
  if (is.na(sf::st_crs(z))) sf::st_crs(z) <- sf::st_crs(sirene_sf)
  z <- sf::st_transform(z, sf::st_crs(sirene_sf))

  code_naf <- extract_naf_code(sirene_sf)
  naf_norm <- code_naf |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9]", "")
  active <- stringr::str_to_upper(stringr::str_squish(as.character(sirene_sf$etatadminis))) %in% c("ACTIF", "A")
  in_zone <- lengths(sf::st_intersects(sirene_sf, z)) > 0
  as.integer(sum(active & in_zone & stringr::str_detect(naf_norm, "^4711"), na.rm = TRUE))
}

read_sirene_sf <- function(zone_sf, actif_dir, raw_dir, data_dir, preferred_layer = "dataseed-sirene-1") {
  read_layer <- function(layer) {
    candidates <- c(
      file.path(actif_dir, paste0(layer, ".shp")),
      file.path(raw_dir, paste0(layer, ".shp")),
      file.path(data_dir, paste0(layer, ".shp"))
    )
    shp <- find_first_existing(candidates)
    if (is.na(shp)) return(NULL)
    dsn <- dirname(shp)
    out <- sf::st_read(dsn = dsn, layer = layer, quiet = TRUE) |>
      normalize_sirene_schema()
    attr(out, "source_layer") <- layer
    attr(out, "source_shp") <- shp
    out
  }

  sirene_sf <- read_layer(preferred_layer)
  if (is.null(sirene_sf)) {
    stop(
      "Impossible de lire la couche SIRENE ", preferred_layer,
      " dans data/actif, data/raw ou data/."
    )
  }

  if (preferred_layer == "dataseed-sirene-1") {
    n_4711 <- count_4711_in_zone(sirene_sf, zone_sf)
    message("Controle 47.11* en zone avec ", preferred_layer, ": ", n_4711)

    if (n_4711 == 0) {
      alt <- read_layer("dataseed-sirene-2")
      if (!is.null(alt)) {
        n_4711_alt <- count_4711_in_zone(alt, zone_sf)
        message("Controle 47.11* en zone avec dataseed-sirene-2: ", n_4711_alt)
        if (n_4711_alt > 0) {
          warning(
            "Fallback automatique: dataseed-sirene-1 ne contient aucun 47.11* en zone. ",
            "Utilisation de dataseed-sirene-2."
          )
          sirene_sf <- alt
        }
      }
    }
  }

  message(
    "Source SIRENE retenue: ",
    attr(sirene_sf, "source_shp"),
    " (layer: ", attr(sirene_sf, "source_layer"), ")"
  )
  sirene_sf
}

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

cours_dir_candidates <- c("Cours", ".", "..")
cours_dir <- cours_dir_candidates[dir.exists(file.path(cours_dir_candidates, "data"))][1]
if (is.na(cours_dir)) {
  stop("Dossier Cours/data introuvable depuis: ", getwd())
}

data_dir <- file.path(cours_dir, "data")
raw_dir <- file.path(data_dir, "raw")
actif_dir <- file.path(data_dir, "actif")
processed_dir <- file.path(data_dir, "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

out_tab <- file.path(processed_dir, "GROUPEXX_TabPonctuels_rapport1.csv")
out_dico <- file.path(processed_dir, "GROUPEXX_DicoDonnées_rapport1.csv")

message("Cours dir: ", cours_dir)
message("Output dir: ", processed_dir)

# -----------------------------------------------------------------------------
# Zone d'etude
# -----------------------------------------------------------------------------

zone_obj <- read_zone_sf(cours_dir = cours_dir, raw_dir = raw_dir, processed_dir = processed_dir, data_dir = data_dir)
zone_sf <- zone_obj$zone_sf

# Groupes geomarketing (reference: script carte groupes report.R)
communes_nord <- c(
  "GRENOBLE", "MEYLAN", "LATRONCHE", "CORENC",
  "SAINTMARTINLEVINOUX", "SAINTEGREVE"
)
communes_nord_only <- c("MEYLAN", "LATRONCHE", "CORENC", "SAINTMARTINLEVINOUX", "SAINTEGREVE")
communes_ouest <- c("SASSENAGE", "FONTAINE", "SEYSSINETPARISET", "SEYSSINS")
communes_sudest <- c("ECHIROLLES", "EYBENS", "SAINTMARTINDHERES", "GIERES")
communes_agglo_groupes <- unique(c(communes_nord, communes_ouest, communes_sudest))

# -----------------------------------------------------------------------------
# SIRENE
# -----------------------------------------------------------------------------

sirene_sf <- read_sirene_sf(
  zone_sf = zone_sf,
  actif_dir = actif_dir,
  raw_dir = raw_dir,
  data_dir = data_dir,
  preferred_layer = "dataseed-sirene-1"
)

if (all(sf::st_is_empty(sirene_sf))) {
  stop("Toutes les geometries SIRENE sont vides.")
}
sirene_sf <- sirene_sf[!sf::st_is_empty(sirene_sf), ]

# Harmonisation CRS
if (is.na(sf::st_crs(zone_sf))) {
  sf::st_crs(zone_sf) <- sf::st_crs(sirene_sf)
}
zone_sf <- sf::st_transform(zone_sf, sf::st_crs(sirene_sf))

# Indicateur spatial zone_etude (fichier zone/bbox fallback)
sirene_sf$dans_zone_fichier <- lengths(sf::st_intersects(sirene_sf, zone_sf)) > 0

# Statut actif
sirene_sf$etatadminis_u <- sirene_sf$etatadminis |>
  as.character() |>
  stringr::str_to_upper() |>
  stringr::str_squish()
sirene_sf <- sirene_sf |>
  filter(etatadminis_u %in% c("ACTIF", "A"))
if (nrow(sirene_sf) == 0) {
  stop("Aucun etablissement actif SIRENE dans la zone d'etude.")
}

# Groupes geomarketing + indicateurs de zone
sirene_sf$commune_norm <- normalize_commune(sirene_sf$libellecomm)
sirene_sf$groupe_geomarketing <- dplyr::case_when(
  sirene_sf$commune_norm %in% communes_nord_only ~ "Nord",
  sirene_sf$commune_norm %in% communes_ouest ~ "Ouest",
  sirene_sf$commune_norm %in% communes_sudest ~ "Sud-Est",
  sirene_sf$commune_norm == "GRENOBLE" ~ "Grenoble",
  TRUE ~ NA_character_
)
sirene_sf$dans_zone_etude <- sirene_sf$commune_norm %in% communes_nord
sirene_sf$dans_zone_agglo_groupes <- sirene_sf$commune_norm %in% communes_agglo_groupes

# Filtrage final: agglo complete (Grenoble + Nord/Ouest/Sud-Est)
sirene_sf <- sirene_sf |> filter(dans_zone_agglo_groupes)
if (nrow(sirene_sf) == 0) {
  stop("Aucun point SIRENE dans l'agglo complete (Grenoble + Nord/Ouest/Sud-Est).")
}

# Variables derivees SIRENE
sirene_sf$code_naf <- extract_naf_code(sirene_sf)
sirene_sf$libelle_naf <- map_naf_libelle(sirene_sf$code_naf)

sirene_sf$enseigne <- dplyr::coalesce(
  normalize_enseigne(sirene_sf$enseigne1et),
  normalize_enseigne(sirene_sf$enseigne2et),
  normalize_enseigne(sirene_sf$enseigne3et),
  normalize_enseigne(sirene_sf$denominatio)
)

sirene_sf$raison_sociale <- dplyr::coalesce(
  normalize_enseigne(sirene_sf$nomuniteleg),
  normalize_enseigne(sirene_sf$denominatio_1),
  normalize_enseigne(sirene_sf$denominatio),
  normalize_enseigne(sirene_sf$enseigne),
  as.character(sirene_sf$siret)
)

sirene_sf$denomination_usuelle <- dplyr::coalesce(
  normalize_enseigne(sirene_sf$denominatio),
  normalize_enseigne(sirene_sf$nomusageuni),
  normalize_enseigne(sirene_sf$enseigne),
  sirene_sf$raison_sociale
)

est_siege_raw <- normalize_bool(sirene_sf$etablisseme)
sirene_sf$est_siege <- dplyr::coalesce(
  est_siege_raw,
  as.character(sirene_sf$siret) == as.character(sirene_sf$siretsiegeu),
  FALSE
)

sirene_sf$date_creation_etablissement <- format_date_fr(sirene_sf$datecreatio)
sirene_sf$adresse_postale <- make_adresse_postale(sirene_sf)

# Typologie point de vente (super/hyper/autre)
sirene_sf$type_point_vente <- classify_type_point_vente(
  code_naf = sirene_sf$code_naf,
  enseigne = sirene_sf$enseigne,
  denomination_usuelle = sirene_sf$denomination_usuelle,
  raison_sociale = sirene_sf$raison_sociale,
  trancheeffe_2 = sirene_sf$trancheeffe_2
)

# Garde les commerces alimentaires cibles (inclut minima 47.11*)
naf_norm <- sirene_sf$code_naf |>
  as.character() |>
  stringr::str_to_upper() |>
  stringr::str_replace_all("[^A-Z0-9]", "")
txt_ref <- paste(sirene_sf$enseigne, sirene_sf$denomination_usuelle, sirene_sf$raison_sociale, sep = " | ")
is_food_keyword <- stringr::str_detect(
  stringr::str_to_upper(txt_ref),
  "SUPERMARCHE|HYPERMARCHE|CARREFOUR|INTERMARCHE|LECLERC|AUCHAN|LIDL|ALDI|U EXPRESS|SUPER U|MONOPRIX|NETTO|BIOCOOP"
)
keep_mask <- stringr::str_detect(naf_norm, "^4711") | is_food_keyword
sirene_sf <- sirene_sf[keep_mask, ]

if (nrow(sirene_sf) == 0) {
  stop("Filtrage alimentaire: 0 ligne retenue dans la zone d'etude.")
}

# -----------------------------------------------------------------------------
# Jointure IRIS (code_iris)
# -----------------------------------------------------------------------------

iris_candidates <- c(
  file.path(raw_dir, "georef-france-iris-millesime.shp"),
  file.path(data_dir, "georef-france-iris-millesime.shp")
)
iris_path <- find_first_existing(iris_candidates)
code_iris <- rep(NA_character_, nrow(sirene_sf))

if (!is.na(iris_path)) {
  iris_sf <- sf::st_read(iris_path, quiet = TRUE)
  if (!is.na(sf::st_crs(iris_sf)) && !is.na(sf::st_crs(sirene_sf)) &&
      sf::st_crs(iris_sf) != sf::st_crs(sirene_sf)) {
    iris_sf <- sf::st_transform(iris_sf, sf::st_crs(sirene_sf))
  }

  iris_code_col <- c("CODE_IRIS", "iris_code", "code_iris", "IRIS")
  iris_code_col <- iris_code_col[iris_code_col %in% names(iris_sf)]
  if (length(iris_code_col) > 0) {
    ic <- iris_code_col[[1]]
    joined <- suppressWarnings(sf::st_join(sirene_sf, iris_sf[, ic, drop = FALSE], join = sf::st_intersects, left = TRUE))
    code_iris <- as.character(joined[[ic]])
    message("Jointure IRIS realisee via: ", basename(iris_path), " (colonne ", ic, ")")
  } else {
    warning("Fichier IRIS trouve, mais aucune colonne code IRIS reconnue.")
  }
} else {
  warning("Fichier IRIS non trouve (georef-france-iris-millesime). code_iris sera NA.")
}

sirene_sf$code_iris <- code_iris

# -----------------------------------------------------------------------------
# Groupes / reseaux (mapping editable)
# -----------------------------------------------------------------------------

groupe_mapping <- tibble::tribble(
  ~pattern, ~groupe_reseau_nom,
  "CARREFOUR|SUPECO|PROMOCASH", "Groupe Carrefour",
  "INTERMARCHE|NETTO", "Groupement Les Mousquetaires (ITM)",
  "AUCHAN|SIMPLY", "Groupe Auchan",
  "E\\.? ?LECLERC|LECLERC", "E.Leclerc",
  "SUPER U|HYPER U|U EXPRESS|UTILE|MARCHE U", "Systeme U",
  "CASINO|MONOPRIX|FRANPRIX|SPAR|VIVAL", "Groupe Casino",
  "LIDL", "Lidl",
  "ALDI", "Aldi",
  "COCCINELLE|CODI", "Francap",
  "BIOCOOP", "Biocoop",
  "GRAND FRAIS", "Prosol (Grand Frais)"
)

text_for_group <- paste(
  sirene_sf$enseigne,
  sirene_sf$denomination_usuelle,
  sirene_sf$raison_sociale,
  sep = " | "
) |>
  stringr::str_to_upper()

groupe_reseau_nom <- rep(NA_character_, nrow(sirene_sf))
for (i in seq_len(nrow(groupe_mapping))) {
  idx <- is.na(groupe_reseau_nom) & stringr::str_detect(text_for_group, groupe_mapping$pattern[[i]])
  groupe_reseau_nom[idx] <- groupe_mapping$groupe_reseau_nom[[i]]
}

sirene_sf$groupe_reseau_nom <- groupe_reseau_nom
sirene_sf$appartient_groupe_reseau <- !is.na(sirene_sf$groupe_reseau_nom)

# -----------------------------------------------------------------------------
# Drives (optionnel)
# -----------------------------------------------------------------------------

sirene_sf$drive_accole <- FALSE
sirene_sf$date_creation_drive_accole <- NA_character_
sirene_sf$drive_deporte <- FALSE
sirene_sf$source_drive <- "none"

drives_candidates <- c(
  file.path(actif_dir, "drives_grenoble.csv"),
  file.path(raw_dir, "drives_grenoble.csv"),
  file.path(data_dir, "drives_grenoble.csv")
)
drives_path <- find_first_existing(drives_candidates)

if (is.na(drives_path)) {
  warning(
    "Fichier drives_grenoble.csv absent. Colonnes drive_* initialisees a FALSE/NA. ",
    "Ajouter un fichier externe pour enrichissement web."
  )
} else {
  message("Fichier drives detecte: ", drives_path)
  drives_delim <- detect_delim(drives_path)
  message("Lecture drives (delimiteur detecte: '", drives_delim, "')")
  drives_raw <- readr::read_delim(
    drives_path,
    delim = drives_delim,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  )
  names(drives_raw) <- normalize_colnames(names(drives_raw))

  siret_col <- names(drives_raw)[stringr::str_detect(names(drives_raw), "siret")]
  addr_col <- names(drives_raw)[stringr::str_detect(names(drives_raw), "adresse|address|addr")]
  type_col <- names(drives_raw)[stringr::str_detect(names(drives_raw), "^type$|type_drive|drive_type|format")]
  date_col <- names(drives_raw)[stringr::str_detect(names(drives_raw), "date")]
  src_col <- names(drives_raw)[stringr::str_detect(names(drives_raw), "source")]
  commune_col <- names(drives_raw)[stringr::str_detect(names(drives_raw), "^commune$|ville|city|libellecomm")]
  enseigne_col <- names(drives_raw)[stringr::str_detect(names(drives_raw), "^enseigne$|brand")]

  message(
    "Colonnes drives detectees - siret: ", ifelse(length(siret_col) > 0, siret_col[[1]], "absente"),
    " | adresse: ", ifelse(length(addr_col) > 0, addr_col[[1]], "absente"),
    " | type: ", ifelse(length(type_col) > 0, type_col[[1]], "absente")
  )

  if (length(type_col) == 0 || (length(siret_col) == 0 && length(addr_col) == 0)) {
    warning(
      "drives_grenoble.csv present mais structure insuffisante (type + siret/adresse requis). ",
      "Colonnes drive_* laissees a FALSE/NA."
    )
  } else {
    dc <- drives_raw
    dc$drive_id <- seq_len(nrow(dc))
    dc$siret <- if (length(siret_col) > 0) {
      as.character(dc[[siret_col[[1]]]]) |>
        stringr::str_replace_all("[^0-9]", "") |>
        (\(x) ifelse(nchar(x) == 14, x, NA_character_))()
    } else {
      NA_character_
    }
    dc$adresse_norm <- if (length(addr_col) > 0) normalize_address_text(dc[[addr_col[[1]]]]) else NA_character_
    dc$adresse_core <- if (length(addr_col) > 0) normalize_address_core(dc[[addr_col[[1]]]]) else NA_character_
    dc$adresse_sig <- if (length(addr_col) > 0) normalize_address_signature(dc[[addr_col[[1]]]]) else NA_character_
    dc$commune_norm <- if (length(commune_col) > 0) normalize_commune(dc[[commune_col[[1]]]]) else NA_character_
    dc$enseigne_norm <- if (length(enseigne_col) > 0) normalize_enseigne(dc[[enseigne_col[[1]]]]) else NA_character_

    type_raw <- as.character(dc[[type_col[[1]]]]) |> stringr::str_to_lower() |> stringr::str_squish()
    dc$drive_type <- dplyr::case_when(
      stringr::str_detect(type_raw, "accol") ~ "accole",
      stringr::str_detect(type_raw, "deport") ~ "deporte",
      TRUE ~ NA_character_
    )

    dc$date_drive <- if (length(date_col) > 0) format_date_fr(dc[[date_col[[1]]]]) else NA_character_
    dc$source_drive <- if (length(src_col) > 0) {
      as.character(dc[[src_col[[1]]]]) |> stringr::str_squish() |> (\(x) ifelse(x == "", NA_character_, x))()
    } else {
      "manual_web"
    }
    dc$source_drive[is.na(dc$source_drive)] <- "manual_web"

    message("Analyse drives:")
    message(" - Nb lignes brutes: ", nrow(dc))
    message(" - Nb lignes type drive reconnu: ", sum(!is.na(dc$drive_type), na.rm = TRUE))
    message(" - Nb lignes avec SIRET valide: ", sum(!is.na(dc$siret), na.rm = TRUE))
    message(" - Nb lignes avec adresse exploitable (core): ", sum(!is.na(dc$adresse_core), na.rm = TRUE))
    message(" - Repartition drive_type:")
    print(dc |> count(drive_type, sort = TRUE))

    drive_by_siret <- dc |>
      filter(!is.na(siret), !is.na(drive_type)) |>
      group_by(siret) |>
      summarise(
        drive_accole_s = any(drive_type == "accole"),
        drive_deporte_s = any(drive_type == "deporte"),
        date_creation_drive_accole_s = dplyr::first(na.omit(date_drive[drive_type == "accole"])),
        source_drive_s = paste_unique_or_na(source_drive),
        .groups = "drop"
      )

    drive_by_addr_commune <- dc |>
      filter(!is.na(commune_norm), !is.na(adresse_core), !is.na(drive_type)) |>
      group_by(commune_norm, adresse_core) |>
      summarise(
        drive_accole_ac = any(drive_type == "accole"),
        drive_deporte_ac = any(drive_type == "deporte"),
        date_creation_drive_accole_ac = dplyr::first(na.omit(date_drive[drive_type == "accole"])),
        source_drive_ac = paste_unique_or_na(source_drive),
        .groups = "drop"
      )

    drive_by_addr <- dc |>
      filter(!is.na(adresse_core), !is.na(drive_type)) |>
      group_by(adresse_core) |>
      summarise(
        drive_accole_a = any(drive_type == "accole"),
        drive_deporte_a = any(drive_type == "deporte"),
        date_creation_drive_accole_a = dplyr::first(na.omit(date_drive[drive_type == "accole"])),
        source_drive_a = paste_unique_or_na(source_drive),
        .groups = "drop"
      )

    drive_by_sig_commune <- dc |>
      filter(!is.na(commune_norm), !is.na(adresse_sig), !is.na(drive_type)) |>
      group_by(commune_norm, adresse_sig) |>
      summarise(
        drive_accole_sc = any(drive_type == "accole"),
        drive_deporte_sc = any(drive_type == "deporte"),
        date_creation_drive_accole_sc = dplyr::first(na.omit(date_drive[drive_type == "accole"])),
        source_drive_sc = paste_unique_or_na(source_drive),
        .groups = "drop"
      )

    drive_by_sig <- dc |>
      filter(!is.na(adresse_sig), !is.na(drive_type)) |>
      group_by(adresse_sig) |>
      summarise(
        drive_accole_sig = any(drive_type == "accole"),
        drive_deporte_sig = any(drive_type == "deporte"),
        date_creation_drive_accole_sig = dplyr::first(na.omit(date_drive[drive_type == "accole"])),
        source_drive_sig = paste_unique_or_na(source_drive),
        .groups = "drop"
      )

    sirene_sf$adresse_norm <- normalize_address_text(sirene_sf$adresse_postale)
    sirene_sf$adresse_core <- normalize_address_core(sirene_sf$adresse_postale)
    sirene_sf$adresse_sig <- normalize_address_signature(sirene_sf$adresse_postale)
    sirene_sf$siret <- as.character(sirene_sf$siret)

    tab_drive <- sirene_sf |>
      left_join(drive_by_siret, by = "siret") |>
      left_join(drive_by_addr_commune, by = c("commune_norm", "adresse_core")) |>
      left_join(drive_by_addr, by = "adresse_core") |>
      left_join(drive_by_sig_commune, by = c("commune_norm", "adresse_sig")) |>
      left_join(drive_by_sig, by = "adresse_sig")

    has_siret <- dplyr::coalesce(tab_drive$drive_accole_s, FALSE) | dplyr::coalesce(tab_drive$drive_deporte_s, FALSE)
    has_addr_commune <- dplyr::coalesce(tab_drive$drive_accole_ac, FALSE) | dplyr::coalesce(tab_drive$drive_deporte_ac, FALSE)
    has_addr <- dplyr::coalesce(tab_drive$drive_accole_a, FALSE) | dplyr::coalesce(tab_drive$drive_deporte_a, FALSE)
    has_sig_commune <- dplyr::coalesce(tab_drive$drive_accole_sc, FALSE) | dplyr::coalesce(tab_drive$drive_deporte_sc, FALSE)
    has_sig <- dplyr::coalesce(tab_drive$drive_accole_sig, FALSE) | dplyr::coalesce(tab_drive$drive_deporte_sig, FALSE)

    sirene_sf$drive_match_mode <- dplyr::case_when(
      has_siret ~ "siret",
      has_addr_commune ~ "adresse_commune",
      has_addr ~ "adresse",
      has_sig_commune ~ "signature_commune",
      has_sig ~ "signature",
      TRUE ~ "none"
    )

    sirene_sf$drive_accole <- dplyr::case_when(
      sirene_sf$drive_match_mode == "siret" ~ dplyr::coalesce(tab_drive$drive_accole_s, FALSE),
      sirene_sf$drive_match_mode == "adresse_commune" ~ dplyr::coalesce(tab_drive$drive_accole_ac, FALSE),
      sirene_sf$drive_match_mode == "adresse" ~ dplyr::coalesce(tab_drive$drive_accole_a, FALSE),
      sirene_sf$drive_match_mode == "signature_commune" ~ dplyr::coalesce(tab_drive$drive_accole_sc, FALSE),
      sirene_sf$drive_match_mode == "signature" ~ dplyr::coalesce(tab_drive$drive_accole_sig, FALSE),
      TRUE ~ FALSE
    )

    sirene_sf$drive_deporte <- dplyr::case_when(
      sirene_sf$drive_match_mode == "siret" ~ dplyr::coalesce(tab_drive$drive_deporte_s, FALSE),
      sirene_sf$drive_match_mode == "adresse_commune" ~ dplyr::coalesce(tab_drive$drive_deporte_ac, FALSE),
      sirene_sf$drive_match_mode == "adresse" ~ dplyr::coalesce(tab_drive$drive_deporte_a, FALSE),
      sirene_sf$drive_match_mode == "signature_commune" ~ dplyr::coalesce(tab_drive$drive_deporte_sc, FALSE),
      sirene_sf$drive_match_mode == "signature" ~ dplyr::coalesce(tab_drive$drive_deporte_sig, FALSE),
      TRUE ~ FALSE
    )

    sirene_sf$date_creation_drive_accole <- dplyr::case_when(
      sirene_sf$drive_match_mode == "siret" ~ tab_drive$date_creation_drive_accole_s,
      sirene_sf$drive_match_mode == "adresse_commune" ~ tab_drive$date_creation_drive_accole_ac,
      sirene_sf$drive_match_mode == "adresse" ~ tab_drive$date_creation_drive_accole_a,
      sirene_sf$drive_match_mode == "signature_commune" ~ tab_drive$date_creation_drive_accole_sc,
      sirene_sf$drive_match_mode == "signature" ~ tab_drive$date_creation_drive_accole_sig,
      TRUE ~ NA_character_
    )

    sirene_sf$source_drive <- dplyr::case_when(
      sirene_sf$drive_match_mode == "siret" ~ dplyr::coalesce(tab_drive$source_drive_s, "none"),
      sirene_sf$drive_match_mode == "adresse_commune" ~ dplyr::coalesce(tab_drive$source_drive_ac, "none"),
      sirene_sf$drive_match_mode == "adresse" ~ dplyr::coalesce(tab_drive$source_drive_a, "none"),
      sirene_sf$drive_match_mode == "signature_commune" ~ dplyr::coalesce(tab_drive$source_drive_sc, "none"),
      sirene_sf$drive_match_mode == "signature" ~ dplyr::coalesce(tab_drive$source_drive_sig, "none"),
      TRUE ~ "none"
    )

    message("Match drives -> SIRENE (mode de rapprochement):")
    print(as.data.frame(table(sirene_sf$drive_match_mode), stringsAsFactors = FALSE))
    message(" - Nb etablissements avec drive_accole = TRUE: ", sum(sirene_sf$drive_accole, na.rm = TRUE))
    message(" - Nb etablissements avec drive_deporte = TRUE: ", sum(sirene_sf$drive_deporte, na.rm = TRUE))

    dc_valid <- dc |> filter(!is.na(drive_type))
    sir_key_commune_addr <- unique(paste0(sirene_sf$commune_norm, "||", sirene_sf$adresse_core))
    sir_addr_only <- unique(na.omit(sirene_sf$adresse_core))
    drives_match_any <- dc_valid |>
      mutate(
        key_commune_addr = paste0(commune_norm, "||", adresse_core),
        matched_siret = !is.na(siret) & siret %in% sirene_sf$siret,
        matched_addr_commune = !is.na(adresse_core) & !is.na(commune_norm) & key_commune_addr %in% sir_key_commune_addr,
        matched_addr = !is.na(adresse_core) & adresse_core %in% sir_addr_only,
        matched_any = matched_siret | matched_addr_commune | matched_addr
      )
    message(
      " - Lignes drives reconnues match au moins un point SIRENE (SIRET/adresse): ",
      sum(drives_match_any$matched_any, na.rm = TRUE), "/", nrow(drives_match_any)
    )
  }
}

# Overrides manuels valides par l'equipe (drives accoles certains)
# Regle stricte: un seul etablissement retenu par entree manuelle (commune+adresse),
# avec priorite au match enseigne/pattern pour eviter les doubles marquages.
manual_drive_accole <- tibble::tribble(
  ~commune_raw, ~adresse_raw, ~pattern_regex,
  "FONTAINE", "120 Boulevard Paul Langevin", "AUCHAN|HYPER",
  "SAINT-MARTIN-D'HERES", "135 Avenue Gabriel Peri", "CARREFOUR",
  "MEYLAN", "1 Boulevard des Alpes", "CARREFOUR",
  "SAINT-EGREVE", "1 Rue des Abattoirs", "CARREFOUR",
  "GRENOBLE", "10 Rue Edouard Vaillant", "SUPER U|CAPUCHE",
  "GRENOBLE", "13 Boulevard Marechal Foch", "INTERMARCHE",
  "GRENOBLE", "15 Route de Lyon", "INTERMARCHE",
  "SEYSSINS", "112 Rue de la Liberte", "INTERMARCHE|JOCILE",
  "GRENOBLE", "22 Cours Jean Jaures", "CARREFOUR",
  "GRENOBLE", "7 Square des Fusilles", "CARREFOUR",
  "SASSENAGE", "Rue du 19 Mars 1962", "CARREFOUR",
  "SASSENAGE", "Rue du 19 Mars 1862", "CARREFOUR"
) |>
  mutate(
    manual_id = row_number(),
    commune_norm = normalize_commune(commune_raw),
    adresse_core = normalize_address_core(adresse_raw),
    pattern_regex = stringr::str_to_upper(pattern_regex)
  ) |>
  filter(!is.na(commune_norm), !is.na(adresse_core)) |>
  distinct(manual_id, .keep_all = TRUE)

if (!("adresse_core" %in% names(sirene_sf))) sirene_sf$adresse_core <- normalize_address_core(sirene_sf$adresse_postale)
if (!("drive_accole" %in% names(sirene_sf))) sirene_sf$drive_accole <- FALSE
if (!("source_drive" %in% names(sirene_sf))) sirene_sf$source_drive <- "none"

sirene_candidates <- sirene_sf |>
  mutate(
    row_id = dplyr::row_number(),
    manual_text = stringr::str_to_upper(paste(enseigne, denomination_usuelle, raison_sociale, groupe_reseau_nom, sep = " | "))
  )

manual_candidates <- sirene_candidates |>
  inner_join(manual_drive_accole, by = c("commune_norm", "adresse_core")) |>
  mutate(
    pattern_hit = dplyr::if_else(
      is.na(pattern_regex) | pattern_regex == "",
      TRUE,
      stringr::str_detect(manual_text, pattern_regex)
    ),
    score_pattern = dplyr::if_else(pattern_hit, 1L, 0L),
    score_known = dplyr::if_else(!is.na(enseigne) & enseigne != "ENSEIGNE_INCONNUE", 1L, 0L),
    score_group = dplyr::if_else(!is.na(groupe_reseau_nom) & groupe_reseau_nom != "", 1L, 0L),
    score_total = score_pattern * 10L + score_group * 2L + score_known
  )

manual_selected <- manual_candidates |>
  group_by(manual_id) |>
  mutate(any_pattern_hit = any(pattern_hit, na.rm = TRUE)) |>
  filter((!any_pattern_hit) | pattern_hit) |>
  arrange(desc(score_total), siret, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

idx_manual <- rep(FALSE, nrow(sirene_sf))
if (nrow(manual_selected) > 0) {
  idx_manual[manual_selected$row_id] <- TRUE
}

n_manual_match <- sum(idx_manual, na.rm = TRUE)
n_manual_new <- sum(idx_manual & !dplyr::coalesce(sirene_sf$drive_accole, FALSE), na.rm = TRUE)
n_manual_total <- nrow(manual_drive_accole)
n_manual_with_candidate <- n_distinct(manual_candidates$manual_id)
n_manual_without_candidate <- n_manual_total - n_manual_with_candidate
n_manual_fallback_no_pattern <- manual_selected |>
  filter(!any_pattern_hit) |>
  nrow()

sirene_sf$drive_accole[idx_manual] <- TRUE
sirene_sf$source_drive[idx_manual] <- append_source_tag(sirene_sf$source_drive[idx_manual], "manual_web")

if ("drive_match_mode" %in% names(sirene_sf)) {
  sirene_sf$drive_match_mode[idx_manual & sirene_sf$drive_match_mode == "none"] <- "manual_override"
}

# Verrou strict: sur les adresses manuelles, ne conserver qu'une seule ligne drive_accole=TRUE.
manual_key_commune_addr <- unique(paste0(manual_drive_accole$commune_norm, "||", manual_drive_accole$adresse_core))
sirene_key_commune_addr <- paste0(sirene_sf$commune_norm, "||", sirene_sf$adresse_core)
idx_manual_scope <- !is.na(sirene_sf$commune_norm) & !is.na(sirene_sf$adresse_core) &
  sirene_key_commune_addr %in% manual_key_commune_addr
idx_manual_non_selected <- idx_manual_scope & !idx_manual
n_manual_suppressed <- sum(idx_manual_non_selected & dplyr::coalesce(sirene_sf$drive_accole, FALSE), na.rm = TRUE)

sirene_sf$drive_accole[idx_manual_non_selected] <- FALSE
sirene_sf$date_creation_drive_accole[idx_manual_non_selected] <- NA_character_
sirene_sf$source_drive[idx_manual_non_selected] <- "none"

if ("drive_match_mode" %in% names(sirene_sf)) {
  sirene_sf$drive_match_mode[idx_manual_non_selected] <- "manual_strict_filtered"
}

message("Overrides manuels drive_accole (mode strict adresse+enseigne):")
message(" - Entrees manuelles totales: ", n_manual_total)
message(" - Entrees manuelles avec candidat adresse+commune: ", n_manual_with_candidate)
message(" - Entrees manuelles sans candidat adresse+commune: ", n_manual_without_candidate)
message(" - Entrees manuelles sans match pattern (fallback adresse seule): ", n_manual_fallback_no_pattern)
message(" - Nb etablissements retenus via override manuel: ", n_manual_match)
message(" - Nb etablissements nouvellement passes a drive_accole=TRUE: ", n_manual_new)
message(" - Nb lignes drive_accole auto retirees par verrou strict manuel: ", n_manual_suppressed)

# -----------------------------------------------------------------------------
# Construction table finale
# -----------------------------------------------------------------------------

tab <- sirene_sf |>
  st_drop_geometry() |>
  transmute(
    siret = as.character(siret),
    siren = as.character(siren),
    raison_sociale = as.character(raison_sociale),
    denomination_usuelle = as.character(denomination_usuelle),
    est_siege = as.logical(est_siege),
    appartient_groupe_reseau = as.logical(appartient_groupe_reseau),
    groupe_reseau_nom = as.character(groupe_reseau_nom),
    date_creation_etablissement = as.character(date_creation_etablissement),
    drive_accole = as.logical(drive_accole),
    date_creation_drive_accole = as.character(date_creation_drive_accole),
    drive_deporte = as.logical(drive_deporte),
    code_naf = as.character(code_naf),
    libelle_naf = as.character(libelle_naf),
    adresse_postale = as.character(adresse_postale),
    code_iris = as.character(code_iris),
    dans_zone_etude = as.logical(dans_zone_etude),
    dans_zone_agglo_groupes = as.logical(dans_zone_agglo_groupes),
    groupe_geomarketing = as.character(groupe_geomarketing),
    dans_zone_fichier = as.logical(dans_zone_fichier),
    type_point_vente = as.character(type_point_vente),
    enseigne = as.character(enseigne),
    source_drive = as.character(source_drive)
  ) |>
  mutate(
    denomination_usuelle = dplyr::coalesce(denomination_usuelle, raison_sociale),
    enseigne = dplyr::coalesce(enseigne, "ENSEIGNE_INCONNUE"),
    type_point_vente = dplyr::coalesce(type_point_vente, "autre"),
    source_drive = dplyr::coalesce(source_drive, "none")
  ) |>
  arrange(desc(dans_zone_etude), groupe_geomarketing, type_point_vente, enseigne, siret) |>
  distinct(siret, .keep_all = TRUE)

if (nrow(tab) == 0) {
  stop("Table finale vide: 0 ligne.")
}

# -----------------------------------------------------------------------------
# Sanity checks
# -----------------------------------------------------------------------------

n_total <- nrow(tab)
n_super <- sum(tab$type_point_vente == "super", na.rm = TRUE)
n_hyper <- sum(tab$type_point_vente == "hyper", na.rm = TRUE)
n_autre <- sum(tab$type_point_vente == "autre", na.rm = TRUE)
n_dup_siret <- sum(duplicated(tab$siret))

message("Sanity checks")
message(" - Nb lignes total: ", n_total)
message(" - Nb super: ", n_super)
message(" - Nb hyper: ", n_hyper)
message(" - Nb autre: ", n_autre)
message(" - Doublons siret: ", n_dup_siret)
message(" - Nb zone etude (Grenoble+Nord): ", sum(tab$dans_zone_etude, na.rm = TRUE))
message(" - Nb agglo groupes (Grenoble+Nord/Ouest/Sud-Est): ", sum(tab$dans_zone_agglo_groupes, na.rm = TRUE))
message(" - Repartition groupes geomarketing: ")
print(tab %>% count(groupe_geomarketing, sort = TRUE))

na_pct <- function(v) round(100 * mean(is.na(v) | v == "", na.rm = TRUE), 2)
message(" - %NA enseigne: ", na_pct(tab$enseigne), "%")
message(" - %NA code_naf: ", na_pct(tab$code_naf), "%")
message(" - %NA adresse_postale: ", na_pct(tab$adresse_postale), "%")
message(" - %NA code_iris: ", na_pct(tab$code_iris), "%")
message(" - %NA raison_sociale: ", na_pct(tab$raison_sociale), "%")

if (n_total == 0) {
  stop("Stop securite: 0 ligne dans la table ponctuelle.")
}

# -----------------------------------------------------------------------------
# Dictionnaire de donnees
# -----------------------------------------------------------------------------

dico <- tibble::tribble(
  ~variable, ~codes_modalites, ~description, ~type, ~source, ~regle_construction,
  "siret", "14 caracteres numeriques", "Identifiant SIRET etablissement", "character", "SIRENE", "reprend colonne siret normalisee",
  "siren", "9 caracteres numeriques", "Identifiant SIREN unite legale", "character", "SIRENE", "reprend colonne siren normalisee",
  "raison_sociale", "texte libre", "Nom officiel de la societe", "character", "SIRENE", "coalesce(nomuniteleg, denominatio_1, denominatio, enseigne, siret)",
  "denomination_usuelle", "texte libre", "Nom public/usuel du point de vente", "character", "SIRENE", "coalesce(denominatio, nomusageuni, enseigne, raison_sociale)",
  "est_siege", "true/false/NA", "Etablissement siege", "logical", "SIRENE", "etablisseme (oui/non) sinon comparaison siret == siret_siege",
  "appartient_groupe_reseau", "true/false", "Appartenance a un groupe/reseau", "logical", "Heuristique script", "TRUE si enseigne/denomination matche table de correspondance",
  "groupe_reseau_nom", "noms de groupes / NA", "Nom du groupe ou reseau rattache", "character", "Heuristique script", "mapping regex enseigne->groupe",
  "date_creation_etablissement", "jj/mm/aaaa / NA", "Date de creation de l'etablissement", "Date", "SIRENE", "format_date_fr(datecreatio)",
  "drive_accole", "true/false", "Presence d'un drive accole", "logical", "Fichier drives", "join drives par siret ou adresse",
  "date_creation_drive_accole", "jj/mm/aaaa / NA", "Date de creation du drive accole", "Date", "Fichier drives", "date drive type accole",
  "drive_deporte", "true/false", "Presence d'un drive deporte", "logical", "Fichier drives", "join drives par siret ou adresse",
  "code_naf", "code NAF rev2 / NA", "Code NAF du point de vente", "character", "SIRENE", "extract_naf_code sur colonnes activite",
  "libelle_naf", "texte / NA", "Libelle du code NAF", "character", "Heuristique script", "mapping simplifie des codes 47.11*",
  "adresse_postale", "texte libre", "Adresse postale complete", "character", "SIRENE", "make_adresse_postale (adresse complete sinon concat champs adresse)",
  "code_iris", "code IRIS / NA", "Code IRIS du point", "character", "INSEE georef IRIS", "jointure spatiale point -> polygons IRIS",
  "dans_zone_etude", "true/false", "Point dans la zone du groupe (Grenoble + Nord)", "logical", "Heuristique commune", "TRUE si commune in Grenoble/Meylan/La Tronche/Corenc/Saint-Martin-le-Vinoux/Saint-Egreve",
  "dans_zone_agglo_groupes", "true/false", "Point dans l'agglo complete des groupes geomarketing", "logical", "Heuristique commune", "TRUE si commune in Grenoble+Nord+Ouest+Sud-Est",
  "groupe_geomarketing", "Grenoble/Nord/Ouest/Sud-Est/NA", "Groupe territorial geomarketing", "character", "Heuristique commune", "mapping commune -> groupe selon script carte groupes report.R",
  "dans_zone_fichier", "true/false", "Point dans la zone spatiale chargee via fichier zone_etude (ou bbox fallback)", "logical", "Zone d'etude", "st_intersects avec zone_etude chargee",
  "type_point_vente", "super/hyper/autre", "Typologie pragmatique du point de vente", "character", "Heuristique script + SIRENE", "NAF 47.11* + mots-cles enseigne/denomination + taille etab",
  "enseigne", "texte normalise / ENSEIGNE_INCONNUE", "Enseigne normalisee", "character", "SIRENE", "normalize_enseigne + coalesce enseigne1/2/3/denomination",
  "source_drive", "none/manual_web/bonial_nielsen/lsa_europresse/autre", "Source de l'information drive", "character", "Fichier drives", "none si pas d'info drives sinon source du fichier"
)

# -----------------------------------------------------------------------------
# Ecriture outputs
# -----------------------------------------------------------------------------

readr::write_csv(tab, out_tab, na = "")
readr::write_csv(dico, out_dico, na = "")

message("Fichiers ecrits:")
message(" - ", out_tab)
message(" - ", out_dico)

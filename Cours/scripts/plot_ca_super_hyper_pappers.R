#!/usr/bin/env Rscript

# Objectif:
# - Lire CA_super_hyper_pappers_nombres.csv
# - Croiser avec les super/hyper (NAF 4711D / 4711F) de la zone Grenoble + Nord
# - Produire 2 cartes HTML:
#   1) CA_2021 non NA + capital social < 1 000 000
#   2) CA_2024 non NA + capital social < 1 000 000
# - Tenter un modele LM/GLM de prediction de CA a partir des features disponibles
#
# Lancement:
#   Rscript Cours/scripts/plot_ca_super_hyper_pappers.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(readxl)
  library(leaflet)
  library(htmlwidgets)
})

find_repo_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  root <- candidates[file.exists(file.path(candidates, ".git"))][1]
  if (is.na(root)) {
    "."
  } else {
    root
  }
}

find_first_existing <- function(paths) {
  out <- paths[file.exists(paths)][1]
  if (is.na(out)) NA_character_ else out
}

clean_names_ascii <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N A", "NULL")] <- NA_character_
  x
}

normalize_commune <- function(x) {
  x <- normalize_text(x)
  x <- gsub("[[:space:]-]", "", x)
  x
}

normalize_naf <- function(x) {
  x <- as.character(x)
  x <- toupper(gsub("[^A-Z0-9]", "", x))
  x[x == ""] <- NA_character_
  x
}

coalesce_chr <- function(df, candidates) {
  out <- rep(NA_character_, nrow(df))
  for (col in candidates) {
    if (!col %in% names(df)) next
    val <- as.character(df[[col]])
    idx <- (is.na(out) | trimws(out) == "") & !is.na(val) & trimws(val) != ""
    out[idx] <- val[idx]
  }
  out
}

as_siret14 <- function(x) {
  x_chr <- trimws(as.character(x))
  x_norm <- gsub(",", ".", x_chr, fixed = TRUE)
  x_num <- suppressWarnings(as.numeric(x_norm))
  x_from_num <- ifelse(!is.na(x_num), format(x_num, scientific = FALSE, trim = TRUE), NA_character_)
  x_from_num <- gsub("[^0-9]", "", x_from_num)
  x_from_raw <- gsub("[^0-9]", "", x_chr)

  x_digits <- ifelse(!is.na(x_from_num) & nzchar(x_from_num), x_from_num, x_from_raw)
  x_digits[!nzchar(x_digits)] <- NA_character_

  out <- ifelse(
    is.na(x_digits),
    NA_character_,
    ifelse(
      nchar(x_digits) >= 14,
      substr(x_digits, 1, 14),
      str_pad(x_digits, width = 14, side = "right", pad = "0")
    )
  )
  out
}

parse_fr_number <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("€", "", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

parse_date_flexible <- function(x) {
  x <- as.character(x)
  d <- suppressWarnings(as.Date(x))
  idx <- is.na(d) & !is.na(x)
  if (any(idx)) {
    d[idx] <- suppressWarnings(as.Date(x[idx], format = "%d/%m/%Y"))
  }
  d
}

safe_div <- function(num, den) {
  out <- suppressWarnings(as.numeric(num) / as.numeric(den))
  out[!is.finite(out)] <- NA_real_
  out
}

first_non_na <- function(x) {
  y <- x[!is.na(x)]
  if (length(y) == 0) {
    if (is.numeric(x)) NA_real_ else NA_character_
  } else {
    y[1]
  }
}

fmt_num <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "NA",
    format(round(x, digits = digits), big.mark = " ", scientific = FALSE, trim = TRUE)
  )
}

build_leaflet_ca_map <- function(sf_points, ca_col, title_txt) {
  if (nrow(sf_points) == 0) return(NULL)

  sf_points <- sf_points %>%
    st_transform(4326) %>%
    mutate(
      ca_value = .data[[ca_col]],
      type_color = ifelse(type_pv == "hyper", "#C1121F", "#005F73")
    )

  pal <- colorNumeric(palette = "YlOrRd", domain = sf_points$ca_value, na.color = "#CFCFCF")

  leaflet(sf_points) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      radius = ~ifelse(type_pv == "hyper", 8, 6),
      stroke = TRUE,
      weight = 1,
      color = ~type_color,
      fillColor = ~pal(ca_value),
      fillOpacity = 0.9,
      label = ~paste0("SIRET: ", siret14),
      popup = ~paste0(
        "<b>SIRET:</b> ", siret14,
        "<br><b>Type:</b> ", type_pv,
        "<br><b>Enseigne:</b> ", ifelse(is.na(enseigne), "NA", enseigne),
        "<br><b>CA:</b> ", fmt_num(ca_value), " EUR",
        "<br><b>Capital social:</b> ", fmt_num(capital_social_num), " EUR",
        "<br><b>Match Pappers/SIRENE:</b> ", ifelse(is.na(match_methods), "NA", match_methods),
        "<br><b>Adresse:</b> ", ifelse(is.na(adresse), "NA", adresse)
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("#005F73", "#C1121F"),
      labels = c("Super", "Hyper"),
      title = "Type point de vente",
      opacity = 1
    ) %>%
    addLegend(
      position = "bottomleft",
      pal = pal,
      values = sf_points$ca_value,
      title = title_txt,
      opacity = 0.9
    )
}

message("Initialisation des chemins...")
repo_root <- find_repo_root()
cours_dir <- file.path(repo_root, "Cours")
data_dir <- file.path(cours_dir, "data")
raw_dir <- file.path(data_dir, "raw")
actif_dir <- file.path(data_dir, "actif")
processed_dir <- file.path(data_dir, "processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# Zone Grenoble + Nord utilisee dans vos notebooks
zone_communes <- c(
  "GRENOBLE", "MEYLAN", "LA TRONCHE",
  "CORENC", "SAINT-MARTIN-LE-VINOUX", "SAINT-EGREVE"
)
zone_keys <- unique(normalize_commune(zone_communes))

message("Chargement SIRENE (dataseed-sirene-1)...")
sirene_layer <- "dataseed-sirene-1"
sirene_shp_candidates <- c(
  file.path(actif_dir, paste0(sirene_layer, ".shp")),
  file.path(raw_dir, paste0(sirene_layer, ".shp")),
  file.path(data_dir, paste0(sirene_layer, ".shp"))
)
sirene_shp_path <- find_first_existing(sirene_shp_candidates)
if (is.na(sirene_shp_path)) {
  stop("Shapefile dataseed-sirene-1 introuvable (actif/raw/data).")
}

sirene_dsn <- dirname(sirene_shp_path)
sirene <- st_read(dsn = sirene_dsn, layer = sirene_layer, quiet = TRUE)
names(sirene) <- tolower(names(sirene))

if (!all(c("siret", "commune_de_") %in% names(sirene))) {
  stop("Colonnes SIRENE minimales absentes (siret, commune_de_).")
}

sirene_zone <- sirene %>%
  mutate(
    commune_key = normalize_commune(commune_de_),
    etat_key = if ("etat_admini" %in% names(.)) normalize_text(etat_admini) else "ACTIF",
    code_naf = normalize_naf(coalesce_chr(
      .,
      c("activite_pr.1", "activitepri.1", "activiteprincipaleetablissement", "activite_pr", "activitepri", "activite_pr.2")
    )),
    type_pv = case_when(
      code_naf == "4711F" ~ "hyper",
      code_naf == "4711D" ~ "super",
      TRUE ~ NA_character_
    ),
    siret14 = as_siret14(siret),
    siren9 = substr(siret14, 1, 9),
    enseigne = coalesce_chr(., c("enseigne_de", "enseigne_de.1", "enseigne_de.2", "enseigne1et")),
    denomination = coalesce_chr(., c("denominatio", "denominatio.1", "denominatio.2", "denominatio.3", "denominatio.4")),
    adresse = str_squish(paste(numero_de_v, type_de_voi, libelle_de_, ",", code_postal, commune_de_)),
    date_creation_sirene = parse_date_flexible(date_de_cre)
  ) %>%
  filter(
    commune_key %in% zone_keys,
    etat_key == "ACTIF",
    !is.na(type_pv),
    !is.na(siret14)
  )

if (nrow(sirene_zone) == 0) {
  stop("Aucun super/hyper actif trouve dans la zone Grenoble + Nord.")
}

message(
  "SIRENE zone charge: ", nrow(sirene_zone), " points (",
  paste0(names(table(sirene_zone$type_pv)), "=", as.integer(table(sirene_zone$type_pv)), collapse = ", "),
  ")."
)

message("Chargement fichier Pappers CA...")
pappers_path <- file.path(raw_dir, "CA_super_hyper_pappers_nombres.csv")
if (!file.exists(pappers_path)) {
  stop("Fichier introuvable: ", pappers_path)
}

pappers_raw <- suppressMessages(read_delim(
  pappers_path,
  delim = ";",
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8"),
  trim_ws = TRUE
))
names(pappers_raw) <- clean_names_ascii(names(pappers_raw))

required_cols <- c("siret", "capital_social", "ca_2021", "ca_2024")
missing_cols <- setdiff(required_cols, names(pappers_raw))
if (length(missing_cols) > 0) {
  stop("Colonnes absentes dans le CSV Pappers: ", paste(missing_cols, collapse = ", "))
}

pappers <- pappers_raw %>%
  mutate(
    row_id = row_number(),
    siret_raw = as.character(siret),
    siret_approx14 = as_siret14(siret_raw),
    key6 = substr(siret_approx14, 1, 6),
    key6_num = suppressWarnings(as.integer(key6)),
    date_creation_pappers = parse_date_flexible(date_creation),
    capital_social_num = parse_fr_number(capital_social),
    ca_2021_num = parse_fr_number(ca_2021),
    ca_2024_num = parse_fr_number(ca_2024)
  )

message(
  "Pappers: ", nrow(pappers), " lignes | NA CA_2021=",
  sum(is.na(pappers$ca_2021_num)), " | NA CA_2024=", sum(is.na(pappers$ca_2024_num))
)

message("Matching Pappers -> SIRENE (SIRET tronque: heuristique prefixe6)...")
sirene_idx <- sirene_zone %>%
  st_drop_geometry() %>%
  transmute(
    siret14,
    key6 = substr(siret14, 1, 6),
    key6_num = suppressWarnings(as.integer(key6)),
    date_creation_sirene
  )

match_one <- function(r, idx) {
  k6 <- as.character(r$key6)
  k6n <- suppressWarnings(as.integer(r$key6_num))
  dref <- as.Date(r$date_creation_pappers)

  cands <- idx %>% filter(key6 == k6)
  method <- "prefix6_exact"

  if (nrow(cands) == 0 && !is.na(k6n)) {
    cands <- idx %>% filter(!is.na(key6_num), abs(key6_num - k6n) <= 1L)
    method <- "prefix6_pm1"
  }

  if (nrow(cands) == 0) {
    return(tibble(
      row_id = r$row_id,
      matched_siret = NA_character_,
      match_method = "no_match",
      n_candidates = 0L
    ))
  }

  if (nrow(cands) > 1 && !is.na(dref)) {
    by_date <- cands %>% filter(!is.na(date_creation_sirene), date_creation_sirene == dref)
    if (nrow(by_date) == 1) {
      return(tibble(
        row_id = r$row_id,
        matched_siret = by_date$siret14[1],
        match_method = paste0(method, "+date_exact"),
        n_candidates = 1L
      ))
    }
    if (nrow(by_date) > 1) {
      cands <- by_date
      method <- paste0(method, "+date_multi")
    }
  }

  if (nrow(cands) == 1) {
    return(tibble(
      row_id = r$row_id,
      matched_siret = cands$siret14[1],
      match_method = method,
      n_candidates = 1L
    ))
  }

  tibble(
    row_id = r$row_id,
    matched_siret = NA_character_,
    match_method = paste0(method, "_ambiguous"),
    n_candidates = nrow(cands)
  )
}

match_tbl <- bind_rows(lapply(seq_len(nrow(pappers)), function(i) match_one(pappers[i, , drop = FALSE], sirene_idx)))
pappers_match <- pappers %>% left_join(match_tbl, by = "row_id")

message("Resume matching:")
print(table(pappers_match$match_method, useNA = "ifany"))

pappers_matched <- pappers_match %>% filter(!is.na(matched_siret))
if (nrow(pappers_matched) == 0) {
  stop(
    "Aucune ligne Pappers matchee de facon fiable.\n",
    "Cause probable: SIRET tronques en notation scientifique dans le CSV source."
  )
}

pappers_by_point <- pappers_matched %>%
  group_by(matched_siret) %>%
  summarise(
    n_lignes_pappers = n(),
    capital_social_num = first_non_na(capital_social_num),
    ca_2021_num = first_non_na(ca_2021_num),
    ca_2024_num = first_non_na(ca_2024_num),
    date_creation_pappers = first_non_na(date_creation_pappers),
    match_methods = paste(sort(unique(match_method)), collapse = " | "),
    .groups = "drop"
  )

sirene_ca <- sirene_zone %>%
  left_join(pappers_by_point, by = c("siret14" = "matched_siret"))

message(
  "Points SIRENE enrichis CA: ", sum(!is.na(sirene_ca$ca_2024_num) | !is.na(sirene_ca$ca_2021_num)),
  " sur ", nrow(sirene_ca)
)

message("Generation des deux cartes demandees...")
map_2021_data <- sirene_ca %>%
  filter(!is.na(ca_2021_num), !is.na(capital_social_num), capital_social_num < 1000000)
map_2024_data <- sirene_ca %>%
  filter(!is.na(ca_2024_num), !is.na(capital_social_num), capital_social_num < 1000000)

map_2021 <- build_leaflet_ca_map(
  map_2021_data,
  ca_col = "ca_2021_num",
  title_txt = "CA 2021 (EUR)"
)
map_2024 <- build_leaflet_ca_map(
  map_2024_data,
  ca_col = "ca_2024_num",
  title_txt = "CA 2024 (EUR)"
)

map_2021_path <- file.path(processed_dir, "carte_ca_2021_capital_lt_1M.html")
map_2024_path <- file.path(processed_dir, "carte_ca_2024_capital_lt_1M.html")

if (!is.null(map_2021)) {
  saveWidget(map_2021, file = map_2021_path, selfcontained = FALSE)
  message("Carte 2021 ecrite: ", map_2021_path, " (n=", nrow(map_2021_data), ")")
} else {
  message("Carte 2021 non produite: aucun point apres filtre.")
}

if (!is.null(map_2024)) {
  saveWidget(map_2024, file = map_2024_path, selfcontained = FALSE)
  message("Carte 2024 ecrite: ", map_2024_path, " (n=", nrow(map_2024_data), ")")
} else {
  message("Carte 2024 non produite: aucun point apres filtre.")
}

write_csv(
  pappers_match %>% select(row_id, siret_raw, siret_approx14, key6, date_creation_pappers, capital_social_num, ca_2021_num, ca_2024_num, matched_siret, match_method, n_candidates),
  file.path(processed_dir, "pappers_ca_matching_detail.csv"),
  na = ""
)
write_csv(
  st_drop_geometry(sirene_ca) %>%
    select(siret14, siren9, enseigne, denomination, type_pv, code_naf, adresse, capital_social_num, ca_2021_num, ca_2024_num, match_methods),
  file.path(processed_dir, "super_hyper_zone_ca_enrichi.csv"),
  na = ""
)

message("Preparation features pour modele LM/GLM...")

# Drives (optionnel)
crossref_candidates <- c(
  file.path(processed_dir, "crossref_supermarches_drives_zone_v3.csv"),
  file.path(processed_dir, "crossref_supermarches_drives_zone.csv"),
  file.path(processed_dir, "crossref_supermarches_drives.csv")
)
crossref_path <- find_first_existing(crossref_candidates)

drive_info <- tibble(siret14 = character(), drive_trouve = logical())
if (!is.na(crossref_path)) {
  drive_info <- read_csv(crossref_path, show_col_types = FALSE) %>%
    mutate(
      siret14 = as_siret14(siret),
      drive_trouve = as.logical(drive_trouve)
    ) %>%
    filter(!is.na(siret14)) %>%
    distinct(siret14, .keep_all = TRUE) %>%
    select(siret14, drive_trouve)
}

sirene_ca <- sirene_ca %>%
  left_join(drive_info, by = "siret14") %>%
  mutate(drive_trouve = ifelse(is.na(drive_trouve), FALSE, drive_trouve))

# Jointure IRIS + features socio-demo
iris_shp_path <- file.path(raw_dir, "georef-france-iris-millesime.shp")
iris_xlsx_path <- file.path(raw_dir, "iris_isere.xlsx")

sirene_ca <- sirene_ca %>% mutate(code_iris = NA_character_)

if (file.exists(iris_shp_path)) {
  iris <- st_read(iris_shp_path, quiet = TRUE)
  names(iris) <- tolower(names(iris))
  if (all(c("com_name", "iris_code") %in% names(iris))) {
    iris_zone <- iris %>%
      mutate(
        commune_key = normalize_commune(com_name),
        code_iris = as.character(iris_code)
      ) %>%
      filter(commune_key %in% zone_keys)

    sirene_ca <- st_transform(sirene_ca, st_crs(iris_zone))
    j <- st_join(sirene_ca, iris_zone %>% select(code_iris), left = TRUE)
    j_tbl <- st_drop_geometry(j)
    iris_col <- intersect(c("code_iris.y", "code_iris"), names(j_tbl))
    if (length(iris_col) == 0) {
      message("Jointure IRIS effectuee mais colonne code_iris introuvable dans le resultat.")
    } else {
      sirene_ca$code_iris <- as.character(j_tbl[[iris_col[1]]])
    }
  } else {
    message("Colonnes com_name/iris_code absentes dans georef-france-iris-millesime.shp")
  }
} else {
  message("Shapefile IRIS absent: ", iris_shp_path)
}

iris_features <- tibble(
  code_iris = character(),
  Mediane_euro = numeric(),
  Densite_pop = numeric(),
  Pct_actifs = numeric(),
  Pct_Menages_enfants = numeric(),
  Pct_Menages_voiture = numeric(),
  Pct_Cadres_sup = numeric()
)

if (file.exists(iris_xlsx_path)) {
  iris_data <- read_excel(iris_xlsx_path)
  if ("CODE_IRIS" %in% names(iris_data) && !"Code_IRIS" %in% names(iris_data)) {
    names(iris_data)[names(iris_data) == "CODE_IRIS"] <- "Code_IRIS"
  }

  if ("Code_IRIS" %in% names(iris_data)) {
    get_num <- function(df, col) {
      if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else rep(NA_real_, nrow(df))
    }
    iris_features <- tibble(
      code_iris = as.character(iris_data$Code_IRIS),
      Mediane_euro = get_num(iris_data, "Mediane_euro"),
      Densite_pop = safe_div(get_num(iris_data, "Popution"), get_num(iris_data, "Superficie")),
      Pct_actifs = safe_div(get_num(iris_data, "Actifs_15-64_ans2"), get_num(iris_data, "Pop_15-64_ans")),
      Pct_Menages_enfants = safe_div(
        get_num(iris_data, "Men_fam_princ_Couple_enfant_s") + get_num(iris_data, "Men_fam_princ_Famille_mono"),
        get_num(iris_data, "Menages")
      ),
      Pct_Menages_voiture = safe_div(get_num(iris_data, "Menages_une_voiture"), get_num(iris_data, "Menages")),
      Pct_Cadres_sup = safe_div(
        get_num(iris_data, "Pop_15_ansplus_Cadres_Prof_intel_sup"),
        get_num(iris_data, "Pop_15_ansplus")
      )
    ) %>%
      filter(!is.na(code_iris), code_iris != "") %>%
      group_by(code_iris) %>%
      summarise(
        across(
          .cols = c(Mediane_euro, Densite_pop, Pct_actifs, Pct_Menages_enfants, Pct_Menages_voiture, Pct_Cadres_sup),
          .fns = ~ first_non_na(.x)
        ),
        .groups = "drop"
      )
  } else {
    message("Code_IRIS absent dans iris_isere.xlsx: features socio-demo non jointes.")
  }
} else {
  message("iris_isere.xlsx absent: ", iris_xlsx_path)
}

sirene_ca_model <- sirene_ca %>%
  left_join(iris_features, by = "code_iris")

model_input <- sirene_ca_model %>%
  st_drop_geometry() %>%
  transmute(
    siret14,
    enseigne,
    type_pv = factor(type_pv),
    drive_trouve = as.integer(drive_trouve),
    ca_2024_num = as.numeric(ca_2024_num),
    log_ca_2024 = log1p(pmax(as.numeric(ca_2024_num), 0)),
    Mediane_euro = as.numeric(Mediane_euro),
    Densite_pop = as.numeric(Densite_pop),
    Pct_actifs = as.numeric(Pct_actifs),
    Pct_Menages_enfants = as.numeric(Pct_Menages_enfants),
    Pct_Menages_voiture = as.numeric(Pct_Menages_voiture),
    Pct_Cadres_sup = as.numeric(Pct_Cadres_sup)
  ) %>%
  filter(!is.na(ca_2024_num))

predictors_candidates <- c(
  "type_pv", "drive_trouve",
  "Mediane_euro", "Densite_pop", "Pct_actifs",
  "Pct_Menages_enfants", "Pct_Menages_voiture", "Pct_Cadres_sup"
)

predictor_ok <- function(df, v) {
  x <- df[[v]]
  if (is.numeric(x)) {
    x <- x[is.finite(x)]
    return(length(x) >= 8 && stats::var(x) > 0)
  }
  if (is.factor(x) || is.character(x)) {
    lv <- unique(na.omit(as.character(x)))
    return(length(lv) >= 2 && sum(!is.na(x)) >= 8)
  }
  FALSE
}

predictors_ok <- predictors_candidates[predictors_candidates %in% names(model_input)]
predictors_ok <- predictors_ok[vapply(predictors_ok, function(v) predictor_ok(model_input, v), logical(1))]

model_summary_path <- file.path(processed_dir, "modele_ca_pappers_resume.txt")
predictions_path <- file.path(processed_dir, "prediction_ca_2024_super_hyper_zone.csv")

model_lines <- c(
  "Modele CA super/hyper (entrainement sur Pappers + features zone Grenoble+Nord)",
  paste0("Date execution: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  ""
)

if (length(predictors_ok) == 0) {
  model_lines <- c(
    model_lines,
    "Modele non faisable:",
    "- Aucun predicteur exploitable (variance insuffisante ou trop de NA).",
    "- Cause probable: peu de lignes CA_2024 matchees apres harmonisation du SIRET Pappers."
  )
  writeLines(model_lines, model_summary_path)
  message("Modele non faisable (aucun predicteur valide). Resume: ", model_summary_path)
} else {
  model_cc <- model_input %>%
    select(log_ca_2024, ca_2024_num, all_of(predictors_ok)) %>%
    filter(complete.cases(.))

  n_min <- max(10, length(predictors_ok) + 3)
  if (nrow(model_cc) < n_min) {
    model_lines <- c(
      model_lines,
      "Modele non faisable:",
      paste0("- Lignes completes disponibles: ", nrow(model_cc)),
      paste0("- Minimum requis: ", n_min),
      "- Conclusion: echantillon trop petit pour un LM/GLM defensable."
    )
    writeLines(model_lines, model_summary_path)
    message("Modele non faisable (echantillon trop petit). Resume: ", model_summary_path)
  } else {
    lm_formula <- as.formula(paste("log_ca_2024 ~", paste(predictors_ok, collapse = " + ")))
    lm_fit <- stats::lm(lm_formula, data = model_cc)

    model_lines <- c(
      model_lines,
      paste0("LM utilise: ", deparse(lm_formula)),
      paste0("n lignes (complete cases): ", nrow(model_cc)),
      paste0("R2 ajuste: ", round(summary(lm_fit)$adj.r.squared, 4)),
      ""
    )
    model_lines <- c(model_lines, "----- Resume LM -----", capture.output(summary(lm_fit)), "")

    glm_fit <- NULL
    if (all(model_cc$ca_2024_num > 0)) {
      glm_formula <- as.formula(paste("ca_2024_num ~", paste(predictors_ok, collapse = " + ")))
      glm_fit <- tryCatch(
        stats::glm(glm_formula, data = model_cc, family = Gamma(link = "log")),
        error = function(e) e
      )
      if (inherits(glm_fit, "glm")) {
        model_lines <- c(model_lines, "----- Resume GLM Gamma(log) -----", capture.output(summary(glm_fit)), "")
      } else {
        model_lines <- c(
          model_lines,
          "GLM non estime:",
          paste0("- ", as.character(glm_fit$message)),
          ""
        )
      }
    } else {
      model_lines <- c(
        model_lines,
        "GLM Gamma(log) non estime:",
        "- CA_2024 contient des valeurs <= 0 (incompatible avec Gamma).",
        ""
      )
    }

    # Prediction pour les points de la zone (si features completes)
    pred_base <- sirene_ca_model %>%
      st_drop_geometry() %>%
      transmute(
        siret14,
        enseigne,
        type_pv = factor(type_pv, levels = levels(model_cc$type_pv)),
        drive_trouve = as.integer(drive_trouve),
        adresse,
        Mediane_euro = as.numeric(Mediane_euro),
        Densite_pop = as.numeric(Densite_pop),
        Pct_actifs = as.numeric(Pct_actifs),
        Pct_Menages_enfants = as.numeric(Pct_Menages_enfants),
        Pct_Menages_voiture = as.numeric(Pct_Menages_voiture),
        Pct_Cadres_sup = as.numeric(Pct_Cadres_sup)
      )

    pred_ready <- pred_base %>%
      filter(complete.cases(select(., all_of(predictors_ok))))

    if (nrow(pred_ready) > 0) {
      pred_ready$pred_ca_2024_lm <- pmax(exp(stats::predict(lm_fit, newdata = pred_ready)) - 1, 0)
      if (inherits(glm_fit, "glm")) {
        pred_ready$pred_ca_2024_glm <- suppressWarnings(stats::predict(glm_fit, newdata = pred_ready, type = "response"))
      }
      write_csv(pred_ready, predictions_path, na = "")
      model_lines <- c(model_lines, paste0("Predictions ecrites: ", predictions_path))
    } else {
      model_lines <- c(model_lines, "Predictions non produites: aucune ligne avec features completes.")
    }

    writeLines(model_lines, model_summary_path)
    message("Modele estime. Resume: ", model_summary_path)
  }
}

message("Script termine.")

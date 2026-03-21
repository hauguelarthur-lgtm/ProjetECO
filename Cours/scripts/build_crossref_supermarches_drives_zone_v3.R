#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
})

repo_candidates <- c('.', '..', '../..')
repo_root <- repo_candidates[file.exists(file.path(repo_candidates, '.git'))][1]
if (is.na(repo_root)) repo_root <- '.'

cours_dir <- file.path(repo_root, 'Cours')
raw_dir <- file.path(cours_dir, 'data', 'raw')
actif_dir <- file.path(cours_dir, 'data', 'actif')
processed_dir <- file.path(cours_dir, 'data', 'processed')

if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = '', to = 'ASCII//TRANSLIT')
  x <- toupper(x)
  x <- str_replace_all(x, '[^A-Z0-9 ]', ' ')
  x <- str_squish(x)
  x[x %in% c('', 'NA', 'N A', 'NULL')] <- NA_character_
  x
}

normalize_naf <- function(x) {
  x <- as.character(x)
  x <- str_to_upper(str_replace_all(x, '[^A-Z0-9]', ''))
  x[x == ''] <- NA_character_
  x
}

normalize_num <- function(x) {
  x <- normalize_text(x)
  x <- str_replace_all(x, '\\s+', '')
  x[x == ''] <- NA_character_
  x
}

extract_num_first <- function(x) {
  as.integer(str_extract(as.character(x), '\\d+'))
}

num_match_with_range <- function(sirene_num, drive_num_raw) {
  if (is.na(sirene_num)) return(FALSE)
  dr <- normalize_text(drive_num_raw)
  if (is.na(dr)) return(FALSE)

  if (str_detect(dr, '^\\d+\\s*-\\s*\\d+$')) {
    a <- as.integer(str_extract(dr, '^\\d+'))
    b <- as.integer(str_extract(dr, '\\d+$'))
    if (is.na(a) || is.na(b)) return(FALSE)
    lo <- min(a, b)
    hi <- max(a, b)
    return(sirene_num >= lo && sirene_num <= hi)
  }

  dnum <- extract_num_first(dr)
  if (is.na(dnum)) return(FALSE)
  sirene_num == dnum
}

first_non_empty <- function(df, candidates) {
  out <- rep(NA_character_, nrow(df))
  for (col in candidates) {
    if (!col %in% names(df)) next
    val <- as.character(df[[col]])
    idx <- (is.na(out) | str_trim(out) == '') & !is.na(val) & str_trim(val) != ''
    out[idx] <- val[idx]
  }
  out
}

extract_cp5 <- function(x) {
  x <- as.character(x)
  y <- str_extract(x, '\\b\\d{5}\\b')
  y[is.na(y)] <- str_extract(str_replace_all(x, '[^0-9]', ''), '^\\d{5}$')
  y
}

parse_drive_adresse <- function(adresse) {
  adr <- as.character(adresse)
  m <- str_match(adr, '^\\s*([^,]+?),\\s*(\\d{5})\\s+(.+?)\\s*$')
  avant <- m[, 2]
  cp <- m[, 3]
  ville <- m[, 4]

  numero <- str_extract(avant, '^[0-9]+(?:\\s*[A-Z])?(?:\\s*-\\s*[0-9]+(?:\\s*[A-Z])?)?')
  voie <- str_squish(str_remove(avant, '^[0-9]+(?:\\s*[A-Z])?(?:\\s*-\\s*[0-9]+(?:\\s*[A-Z])?)?\\s*'))
  voie[is.na(voie) | voie == ''] <- str_squish(avant[is.na(voie) | voie == ''])

  tibble(
    numero_from_adresse = numero,
    voie_from_adresse = voie,
    cp_from_adresse = cp,
    ville_from_adresse = ville
  )
}

harmonize_drives <- function(df, source_tag) {
  names(df) <- tolower(names(df))

  if (!'adresse' %in% names(df)) {
    df$adresse <- NA_character_
  }
  parsed <- parse_drive_adresse(df$adresse)

  out <- tibble(
    source_dataset = source_tag,
    commune = if ('commune' %in% names(df)) as.character(df$commune) else NA_character_,
    groupe = if ('groupe' %in% names(df)) as.character(df$groupe) else NA_character_,
    enseigne = if ('enseigne' %in% names(df)) as.character(df$enseigne) else NA_character_,
    adresse = as.character(df$adresse),
    type_drive = if ('type_drive' %in% names(df)) as.character(df$type_drive) else NA_character_,
    source_url = if ('source_url' %in% names(df)) as.character(df$source_url) else NA_character_,
    numero = if ('numero' %in% names(df)) as.character(df$numero) else parsed$numero_from_adresse,
    voie = if ('voie' %in% names(df)) as.character(df$voie) else parsed$voie_from_adresse,
    cp = if ('cp' %in% names(df)) as.character(df$cp) else parsed$cp_from_adresse,
    ville = if ('ville' %in% names(df)) as.character(df$ville) else parsed$ville_from_adresse,
    voie_normalisee = if ('voie_normalisee' %in% names(df)) as.character(df$voie_normalisee) else NA_character_
  )

  out <- out %>%
    mutate(
      commune = coalesce(commune, ville),
      cp = coalesce(cp, extract_cp5(adresse)),
      voie = coalesce(voie, parsed$voie_from_adresse),
      voie_norm = normalize_text(coalesce(voie_normalisee, voie)),
      numero_norm = normalize_num(numero),
      cp_norm = extract_cp5(cp),
      commune_norm = normalize_text(commune),
      key_no_num = paste(cp_norm, commune_norm, voie_norm, sep = '|'),
      key_full = paste(cp_norm, commune_norm, numero_norm, voie_norm, sep = '|')
    ) %>%
    select(-matches('_from_adresse$'))

  out
}

message('Chargement SIRENE (zone notebook v3)...')

sirene_layer <- 'dataseed-sirene-1'
sirene_candidates <- c(actif_dir, raw_dir, file.path(cours_dir, 'data'))
sirene_dsn <- sirene_candidates[file.exists(file.path(sirene_candidates, paste0(sirene_layer, '.shp')))][1]
if (is.na(sirene_dsn)) stop('Shapefile dataseed-sirene-1 introuvable.')

sirene <- st_read(sirene_dsn, layer = sirene_layer, quiet = TRUE)
names(sirene) <- tolower(names(sirene))

zone_communes <- c(
  'GRENOBLE', 'MEYLAN', 'LA TRONCHE',
  'CORENC', 'SAINT-MARTIN-LE-VINOUX', 'SAINT-EGREVE'
)

sirene_zone <- sirene %>%
  filter(normalize_text(commune_de_) %in% normalize_text(zone_communes))

sirene_zone <- sirene_zone %>%
  mutate(
    code_naf = normalize_naf(first_non_empty(
      ., c('activite_pr.1', 'activitepri.1', 'activiteprincipaleetablissement', 'activite_pr', 'activitepri', 'activite_pr.2')
    )),
    type_pv = case_when(
      code_naf == '4711F' ~ 'hyper',
      code_naf == '4711D' ~ 'super',
      code_naf %in% c('4711B', '4711C') ~ 'superette',
      TRUE ~ NA_character_
    )
  )

sirene_zone <- sirene_zone %>%
  filter(code_naf %in% c('4711B', '4711C', '4711D', '4711F'), !is.na(type_pv))

if ('etat_admini' %in% names(sirene_zone)) {
  sirene_zone <- sirene_zone %>% filter(normalize_text(etat_admini) == 'ACTIF')
}

sirene_tbl <- sirene_zone %>%
  st_drop_geometry() %>%
  mutate(
    enseigne_sirene_tmp = first_non_empty(., c('enseigne_de', 'enseigne_de.1', 'enseigne_de.2')),
    denomination_sirene_tmp = first_non_empty(., c('denominatio', 'denominatio.1', 'denominatio.2', 'denominatio.3', 'denominatio.4'))
  ) %>%
  transmute(
    row_id = row_number(),
    siret = as.character(siret),
    siren = as.character(siren),
    commune = as.character(commune_de_),
    cp = as.character(code_postal),
    numero = as.character(numero_de_v),
    type_voie = as.character(type_de_voi),
    libelle_voie = as.character(libelle_de_),
    adresse_sirene = str_squish(paste(numero_de_v, type_de_voi, libelle_de_, ',', code_postal, commune_de_)),
    enseigne_sirene = enseigne_sirene_tmp,
    denomination_sirene = denomination_sirene_tmp,
    code_naf,
    type_pv
  ) %>%
  mutate(
    voie_norm = normalize_text(paste(type_voie, libelle_voie)),
    numero_norm = normalize_num(numero),
    cp_norm = extract_cp5(cp),
    commune_norm = normalize_text(commune),
    key_no_num = paste(cp_norm, commune_norm, voie_norm, sep = '|'),
    key_full = paste(cp_norm, commune_norm, numero_norm, voie_norm, sep = '|')
  )

message('Chargement drives...')

drives_main_path <- file.path(raw_dir, 'drives_grenoble.csv')
drives_extra_candidates <- c(
  file.path(raw_dir, 'drives_grenoble_complement.csv'),
  file.path(processed_dir, 'drives_grenoble_complement.csv')
)
drives_extra_path <- drives_extra_candidates[file.exists(drives_extra_candidates)][1]

if (!file.exists(drives_main_path)) stop('drives_grenoble.csv introuvable dans Cours/data/raw.')

drives_main <- read_delim(drives_main_path, delim = ';', show_col_types = FALSE, locale = locale(encoding = 'UTF-8'))
drives_all <- harmonize_drives(drives_main, 'drives_grenoble.csv')

if (!is.na(drives_extra_path)) {
  drives_extra <- read_csv(drives_extra_path, show_col_types = FALSE, locale = locale(encoding = 'UTF-8'))
  drives_all <- bind_rows(drives_all, harmonize_drives(drives_extra, basename(drives_extra_path)))
}

# Nettoyage des doublons stricts

drives_all <- drives_all %>%
  mutate(
    enseigne_norm = normalize_text(enseigne),
    adresse_norm = normalize_text(adresse)
  ) %>%
  distinct(cp_norm, commune_norm, voie_norm, numero_norm, enseigne_norm, .keep_all = TRUE)

message('Matching super/hyper/superette zone vs drives...')

match_rows <- vector('list', nrow(sirene_tbl))

for (i in seq_len(nrow(sirene_tbl))) {
  s <- sirene_tbl[i, , drop = FALSE]
  cands <- drives_all %>%
    filter(
      cp_norm == s$cp_norm,
      commune_norm == s$commune_norm,
      voie_norm == s$voie_norm
    )

  if (nrow(cands) == 0) next

  s_num <- extract_num_first(s$numero)

  if (!is.na(s_num)) {
    keep <- vapply(cands$numero, function(dn) num_match_with_range(s_num, dn), logical(1))
    cands_num <- cands[keep, , drop = FALSE]
    if (nrow(cands_num) > 0) {
      match_rows[[i]] <- cands_num %>%
        transmute(
          row_id = s$row_id,
          enseigne,
          adresse,
          type_drive,
          source_dataset,
          source_url,
          match_method = 'num_or_range'
        )
      next
    }
  }

  # Fallback uniquement si numero SIRENE absent
  if (is.na(s_num)) {
    match_rows[[i]] <- cands %>%
      transmute(
        row_id = s$row_id,
        enseigne,
        adresse,
        type_drive,
        source_dataset,
        source_url,
        match_method = 'no_num'
      )
  }
}

matches <- bind_rows(match_rows)

match_summary <- matches %>%
  group_by(row_id) %>%
  summarise(
    drive_trouve = TRUE,
    nb_drives_match = n(),
    drives_types = paste(sort(unique(na.omit(type_drive))), collapse = ' | '),
    drives_enseignes = paste(sort(unique(na.omit(enseigne))), collapse = ' | '),
    drives_adresses = paste(sort(unique(na.omit(adresse))), collapse = ' | '),
    drives_sources = paste(sort(unique(na.omit(source_dataset))), collapse = ' | '),
    drives_urls = paste(sort(unique(na.omit(source_url))), collapse = ' | '),
    match_method = paste(sort(unique(match_method)), collapse = ' | '),
    .groups = 'drop'
  )

manual_drive_overrides <- tibble(
  siret = c(
    '45132133501120',
    '45132133502185',
    '40098842400505',
    '84287351500014',
    '81422763300025'
  ),
  manual_drive_type = 'voiture',
  manual_drive_source = 'validation_manuelle_user_2026-03-21',
  manual_match_method = 'manual_override_voiture'
)

street_rows <- vector('list', nrow(sirene_tbl))
for (i in seq_len(nrow(sirene_tbl))) {
  s <- sirene_tbl[i, , drop = FALSE]
  cands <- drives_all %>%
    filter(
      cp_norm == s$cp_norm,
      commune_norm == s$commune_norm,
      voie_norm == s$voie_norm
    )

  street_rows[[i]] <- tibble(
    row_id = s$row_id,
    nb_candidats_meme_voie = nrow(cands),
    candidats_drives_meme_voie = if (nrow(cands) > 0) paste(sort(unique(na.omit(cands$adresse))), collapse = ' | ') else NA_character_
  )
}
street_summary <- bind_rows(street_rows)

crossref <- sirene_tbl %>%
  left_join(street_summary, by = 'row_id') %>%
  left_join(match_summary, by = 'row_id') %>%
  left_join(manual_drive_overrides, by = 'siret') %>%
  mutate(
    drive_trouve = if_else(!is.na(manual_drive_type), TRUE, if_else(is.na(drive_trouve), FALSE, drive_trouve)),
    nb_drives_match = if_else(
      !is.na(manual_drive_type) & coalesce(nb_drives_match, 0L) == 0L,
      1L,
      coalesce(nb_drives_match, 0L)
    ),
    drives_types = if_else(!is.na(manual_drive_type), manual_drive_type, drives_types),
    drives_enseignes = if_else(!is.na(manual_drive_type), coalesce(drives_enseignes, enseigne_sirene), drives_enseignes),
    drives_adresses = if_else(!is.na(manual_drive_type), coalesce(drives_adresses, adresse_sirene), drives_adresses),
    drives_sources = if_else(
      !is.na(manual_drive_type),
      if_else(is.na(drives_sources) | drives_sources == '', manual_drive_source, paste0(drives_sources, ' | ', manual_drive_source)),
      drives_sources
    ),
    match_method = if_else(
      !is.na(manual_drive_type),
      if_else(is.na(match_method) | match_method == '', manual_match_method, paste0(match_method, ' | ', manual_match_method)),
      match_method
    ),
    nb_candidats_meme_voie = coalesce(nb_candidats_meme_voie, 0L)
  ) %>%
  select(
    siret, siren, commune, cp, numero, type_voie, libelle_voie,
    adresse_sirene, enseigne_sirene, denomination_sirene,
    code_naf, type_pv,
    nb_candidats_meme_voie, candidats_drives_meme_voie,
    drive_trouve, nb_drives_match,
    drives_types, drives_enseignes, drives_adresses, drives_sources, drives_urls, match_method
  ) %>%
  arrange(desc(drive_trouve), commune, type_pv, enseigne_sirene, adresse_sirene)

out_path <- file.path(processed_dir, 'crossref_supermarches_drives_zone_v3.csv')
write_csv(crossref, out_path, na = '')

message('Fichier genere: ', out_path)
message('Lignes: ', nrow(crossref), ' | avec drive: ', sum(crossref$drive_trouve, na.rm = TRUE))

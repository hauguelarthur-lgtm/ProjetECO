#!/usr/bin/env Rscript

# Objectif:
# Exporter en PNG les principales cartes des parties 4, 4.1, 5 et 5.1
# de supermarket_implantation_v5.Rmd (zone Grenoble + Nord),
# avec uniquement supermarches/hypermarches (sans superettes).
#
# Sorties PNG:
# - Cours/data/processed/plots/carte_concurrence_2020_partie4_v5_equivalent.png
# - Cours/data/processed/plots/carte_concurrence_2025_partie4_v5_equivalent.png
# - Cours/data/processed/plots/carte_comptage_iris_2020_partie4_v5_equivalent.png
# - Cours/data/processed/plots/carte_comptage_iris_2025_partie4_v5_equivalent.png
# - Cours/data/processed/plots/carte_super_hyper_vs_drives_2025_partie4_1_v5_equivalent.png
# - Cours/data/processed/plots/carte_drives_par_iris_2025_partie4_1_v5_equivalent.png
# - Cours/data/processed/plots/carte_isochrones_pied_2025_partie5_v5_equivalent.png
# - Cours/data/processed/plots/carte_isochrones_voiture_2025_partie5_v5_equivalent.png
# - Cours/data/processed/plots/carte_recouvrement_intra_enseigne_2025_partie5_1_v5_equivalent.png
# - Cours/data/processed/plots/carte_pct_actifs_zone_nord_partie3_2_v5_equivalent.png
# - Cours/data/processed/plots/carte_pct_menages_enfants_zone_nord_partie3_3_v5_equivalent.png
# - Cours/data/processed/plots/carte_pct_menages_voiture_zone_nord_partie3_3_v5_equivalent.png
# - Cours/data/processed/plots/carte_pct_cadres_zone_nord_partie3_4_v5_equivalent.png
# - Cours/data/processed/plots/carte_densite_population_zone_nord_partie3_4_v5_equivalent.png
# - Cours/data/processed/plots/carte_residus_ols_accessibilite_partie8_v5_equivalent.png
#
# Note OTP:
# - Si un serveur OTP est joignable (localhost:OTP_PORT, 8081 par defaut),
#   les isochrones reseau sont utilisees.
# - Sinon, fallback geometrique (buffers) pour garantir les PNG.
#
# Execution:
# Rscript Cours/scripts/plot_carte_2025_partie4_v5_equivalent.R
# OTP_PORT=8081 Rscript Cours/scripts/plot_carte_2025_partie4_v5_equivalent.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(readr)
  library(readxl)
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

classify_type_pv <- function(df, naf_cols) {
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

save_map <- function(plot_obj, out_path, width = 11, height = 7.5, dpi = 220) {
  ggsave(filename = out_path, plot = plot_obj, width = width, height = height, dpi = dpi)
  message("Carte exportee: ", out_path)
}

to_num <- function(x) {
  xx <- as.character(x)
  xx <- str_trim(xx)
  xx <- str_replace_all(xx, "\u00A0", "")
  xx <- str_replace_all(xx, " ", "")

  # Cas milliers en virgule (ex: 23,410)
  is_thousands_comma <- str_detect(xx, "^-?[0-9]{1,3}(,[0-9]{3})+$")
  xx[is_thousands_comma] <- str_replace_all(xx[is_thousands_comma], ",", "")

  # Cas decimal en virgule (ex: 12,5)
  is_decimal_comma <- !is_thousands_comma & str_detect(xx, "^-?[0-9]+,[0-9]+$")
  xx[is_decimal_comma] <- str_replace_all(xx[is_decimal_comma], ",", ".")

  suppressWarnings(as.numeric(xx))
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

num_col_or_na <- function(df, col_name) {
  if (is.na(col_name) || !col_name %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  to_num(df[[col_name]])
}

safe_ratio <- function(num, den) {
  out <- ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)
  as.numeric(out)
}

read_sirene_layer <- function(cours_dir, layer_name) {
  dsn_candidates <- c(
    file.path(cours_dir, "data", "actif"),
    file.path(cours_dir, "data", "raw"),
    file.path(cours_dir, "data")
  )
  dsn <- dsn_candidates[file.exists(file.path(dsn_candidates, paste0(layer_name, ".shp")))][1]
  if (is.na(dsn)) return(NULL)

  shp <- st_read(dsn = dsn, layer = layer_name, quiet = TRUE)
  names(shp) <- tolower(names(shp))
  shp
}

prepare_sirene_zone <- function(sirene_shp, communes_nord_norm, iris_crs) {
  if (is.null(sirene_shp) || nrow(sirene_shp) == 0) {
    return(NULL)
  }

  naf_cols <- c(
    "activite_pr.1", "activitepri.1", "activiteprincipaleetablissement",
    "activite_pr", "activitepri"
  )

  out <- sirene_shp %>%
    mutate(
      commune_norm = normalize_commune(coalesce_chr(., c(
        "commune_de_", "commune", "libcom",
        "libellecomm.1", "libellecomm", "libellecomm.2", "libellecomm.3"
      ))),
      code_naf = normalize_naf(coalesce_chr(., naf_cols)),
      Type_PV = classify_type_pv(., naf_cols = naf_cols),
      siret = coalesce_chr(., c("siret")),
      Enseigne = coalesce_chr(., c("enseigne_de", "enseigne1et")),
      Classe = coalesce_chr(., c("classe_de_l", "classeetabl", "classe")),
      Denomination = coalesce_chr(., c("denominatio", "denominationusuelleetablissement"))
    ) %>%
    filter(
      commune_norm %in% communes_nord_norm,
      code_naf %in% c("4711D", "4711F"),
      !is.na(Type_PV),
      !is.na(siret),
      str_squish(siret) != ""
    ) %>%
    mutate(
      Enseigne = ifelse(is.na(Enseigne) | str_squish(Enseigne) == "", "ENSEIGNE_INCONNUE", Enseigne),
      groupe_reseau_nom = map_group_reseau(Enseigne, Denomination),
      siret = as.character(siret)
    ) %>%
    distinct(siret, .keep_all = TRUE)

  if (nrow(out) == 0) {
    return(out)
  }

  st_transform(out, iris_crs)
}

connect_otp_if_available <- function() {
  stop("connect_otp_if_available() obsolete: utiliser connect_otp_runtime().")
}

get_env_int <- function(var_name, default = NA_integer_) {
  raw <- Sys.getenv(var_name, "")
  if (!nzchar(raw)) return(default)
  val <- suppressWarnings(as.integer(raw))
  if (is.na(val)) return(default)
  val
}

connect_otp_any <- function(port_candidates = c(8081L, 8901L:8910L, 8801L:8810L, 8080L:8089L)) {
  for (p in unique(as.integer(port_candidates))) {
    con <- tryCatch(
      opentripplanner::otp_connect(hostname = "localhost", port = p),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      attr(con, "otp_port") <- p
      message("Serveur OTP detecte sur localhost:", p)
      return(con)
    }
  }
  NULL
}

start_otp_any <- function(path_otp, path_data, start_ports = c(8901L:8910L, 8801L:8810L, 8080L:8089L)) {
  startup_wait_sec <- max(20L, get_env_int("OTP_STARTUP_WAIT_SEC", 90L))
  for (p in unique(as.integer(start_ports))) {
    started <- tryCatch({
      opentripplanner::otp_setup(
        otp = path_otp,
        dir = path_data,
        port = p,
        securePort = as.integer(p + 100L),
        wait = FALSE,
        quiet = TRUE,
        open_browser = FALSE
      )
      TRUE
    }, error = function(e) {
      message("Echec demarrage OTP sur port ", p, ": ", conditionMessage(e))
      FALSE
    })

    if (!started) next

    con <- NULL
    for (i in seq_len(startup_wait_sec)) {
      Sys.sleep(1)
      con <- tryCatch(
        opentripplanner::otp_connect(hostname = "localhost", port = p),
        error = function(e) NULL
      )
      if (!is.null(con)) break
    }

    if (!is.null(con)) {
      attr(con, "otp_port") <- p
      message("OTP demarre et connecte sur localhost:", p)
      return(con)
    }

    message("OTP non joignable sur le port ", p, " apres ", startup_wait_sec, "s.")
  }
  NULL
}

connect_otp_runtime <- function(path_data, path_otp, strict_isochrones = FALSE, otp_autostart = FALSE) {
  if (!requireNamespace("opentripplanner", quietly = TRUE)) {
    if (strict_isochrones) {
      stop("Package opentripplanner indisponible et STRICT_ISOCHRONES=1.")
    }
    message("Package opentripplanner indisponible: fallback buffers pour isochrones.")
    return(NULL)
  }

  otp_port_env <- get_env_int("OTP_PORT", NA_integer_)
  otp_candidates <- c(
    if (!is.na(otp_port_env)) otp_port_env else integer(),
    8081L, 8901L:8910L, 8801L:8810L, 8080L:8089L
  )

  # 1) tentative OTP_PORT explicite + scan des ports usuels
  con <- connect_otp_any(otp_candidates)
  if (!is.null(con)) return(con)

  # 2) autostart optionnel
  if (otp_autostart) {
    if (is.na(path_otp) || !file.exists(path_otp)) {
      if (strict_isochrones) {
        stop("OTP_AUTOSTART=1 mais jar OTP introuvable: ", path_otp)
      }
      message("OTP_AUTOSTART=1 mais jar OTP introuvable: ", path_otp)
      return(NULL)
    }
    con <- start_otp_any(path_otp = path_otp, path_data = path_data, start_ports = otp_candidates)
    if (!is.null(con)) return(con)
  }

  if (strict_isochrones) {
    stop("Serveur OTP indisponible et STRICT_ISOCHRONES=1.")
  }
  message("Serveur OTP non joignable -> fallback buffers.")
  NULL
}

make_buffer_iso <- function(point_sf, siret_id, type_pv, cutoff_sec, speed_m_per_min) {
  pt_2154 <- st_transform(point_sf, 2154)
  parts <- lapply(cutoff_sec, function(tt) {
    dist_m <- (as.numeric(tt) / 60) * speed_m_per_min
    st_buffer(pt_2154, dist = dist_m) %>%
      mutate(
        siret = as.character(siret_id),
        Type_PV = as.character(type_pv),
        time = as.integer(tt),
        iso_source = "buffer"
      )
  })
  out <- do.call(rbind, parts)
  st_transform(out, st_crs(point_sf))
}

get_iso_specs <- function(profile = c("walk", "car")) {
  profile <- match.arg(profile)
  if (profile == "walk") {
    return(list(
      super = c(5, 10) * 60,
      hyper = c(8, 10) * 60
    ))
  }
  list(
    super = c(10, 15) * 60,
    hyper = c(20, 30) * 60
  )
}

build_multiformat_isochrones <- function(
    points_sf,
    profile = c("walk", "car"),
    otpcon = NULL,
    strict = FALSE,
    allow_partial = FALSE
) {
  profile <- match.arg(profile)
  specs <- get_iso_specs(profile)
  mode_vec <- if (profile == "walk") c("WALK") else c("CAR")
  speed_fallback <- if (profile == "walk") 80 else 500
  walk_try <- if (profile == "walk") c(1500, 2500, 4000) else c(1200, 2500, 5000, 8000)

  all_iso <- list()
  kk <- 0L
  failed_ids <- character()

  for (tp in names(specs)) {
    pts_tp <- points_sf %>% filter(as.character(Type_PV) == tp)
    if (nrow(pts_tp) == 0) next

    cut_tp <- as.integer(specs[[tp]])

    for (i in seq_len(nrow(pts_tp))) {
      pt_i <- pts_tp[i, ]
      siret_i <- as.character(pt_i$siret)
      iso_i <- NULL

      if (!is.null(otpcon) && requireNamespace("opentripplanner", quietly = TRUE)) {
        for (mw in walk_try) {
          iso_i <- tryCatch({
            args <- list(
              otpcon = otpcon,
              fromPlace = pt_i,
              fromID = siret_i,
              mode = mode_vec,
              date_time = as.POSIXct("2026-02-18 08:35:00", tz = "Europe/Paris"),
              cutoffSec = cut_tp,
              maxWalkDistance = as.integer(mw),
              ncores = 1
            )
            out <- do.call(opentripplanner::otp_isochrone, args)
            if (is.null(out) || nrow(out) == 0) return(NULL)
            out <- suppressWarnings(st_make_valid(out))
            out <- out[!st_is_empty(out), ]
            if (nrow(out) == 0) return(NULL)
            out
          }, error = function(e) NULL)

          if (!is.null(iso_i) && nrow(iso_i) > 0) {
            if (!"siret" %in% names(iso_i)) {
              iso_i$siret <- siret_i
            }
            iso_i$Type_PV <- as.character(tp)
            iso_i$iso_source <- "otp"
            iso_i <- st_transform(iso_i, st_crs(points_sf))
            break
          }
        }
      }

      if (is.null(iso_i) || nrow(iso_i) == 0) {
        if (strict) {
          if (allow_partial) {
            failed_ids <- c(failed_ids, siret_i)
            message(
              "Isochrone OTP vide pour siret=", siret_i,
              " (type=", tp, ", profile=", profile, ") -> point ignore (strict+partial)."
            )
            next
          }
          stop(
            "Isochrone OTP impossible (strict=TRUE) pour siret=", siret_i,
            " type=", tp, " profile=", profile,
            ". Essaye OTP_PORT=8901 et OTP_AUTOSTART=0 si un serveur OTP tourne deja."
          )
        }
        iso_i <- make_buffer_iso(
          point_sf = pt_i,
          siret_id = siret_i,
          type_pv = tp,
          cutoff_sec = cut_tp,
          speed_m_per_min = speed_fallback
        )
      }

      kk <- kk + 1L
      all_iso[[kk]] <- iso_i
    }
  }

  if (length(all_iso) == 0) {
    if (strict) {
      stop("Aucune isochrone OTP valide produite (strict=TRUE).")
    }
    return(points_sf[0, ])
  }

  if (length(failed_ids) > 0) {
    message(
      "Isochrones OTP non disponibles pour ", length(unique(failed_ids)),
      " point(s): ", paste(unique(failed_ids), collapse = ", ")
    )
  }

  out <- do.call(rbind, all_iso)
  out$minutes <- as.numeric(out$time) / 60
  out
}

plot_concurrence_map <- function(iris_zone_etude, sirene_sf, title_txt) {
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
      subtitle = "Zone d'etude : Grenoble + communes Nord",
      color = "Groupe / reseau"
    ) +
    theme_minimal()
}

plot_iris_count_map <- function(iris_zone_etude, value_col, max_count, title_txt) {
  ggplot() +
    geom_sf(
      data = iris_zone_etude,
      aes(fill = .data[[value_col]]),
      color = "grey80",
      linewidth = 0.15
    ) +
    scale_fill_gradient(
      low = "#deebf7",
      high = "#08519c",
      limits = c(0, max_count),
      name = "Nb points de vente"
    ) +
    labs(title = title_txt) +
    theme_minimal()
}

plot_super_hyper_vs_drives <- function(iris_zone_etude, super_hyper_2025, drives_2025) {
  base <- ggplot() +
    geom_sf(data = iris_zone_etude, fill = "white", color = "grey75", linewidth = 0.25) +
    geom_sf(
      data = separate_overlapping_points(super_hyper_2025, offset_m = 22),
      aes(shape = Type_PV),
      size = 2.3,
      color = "#334E68",
      alpha = 0.9
    ) +
    scale_shape_manual(
      values = c(super = 16, hyper = 17),
      name = "Type de point de vente"
    ) +
    labs(
      title = "Super/hyper (2025) vs points avec drive",
      subtitle = "Base grise = super/hyper ; bleu = points avec drive"
    ) +
    theme_minimal()

  if (nrow(drives_2025) == 0) return(base)

  drives_plot <- separate_overlapping_points(drives_2025, offset_m = 28)
  drives_super <- drives_plot %>% filter(as.character(Type_PV) == "super")
  drives_hyper <- drives_plot %>% filter(as.character(Type_PV) == "hyper")

  base +
    geom_sf(
      data = drives_super,
      shape = 21,
      size = 3,
      color = "#003049",
      fill = "#00A6FB",
      stroke = 1.1,
      alpha = 0.95
    ) +
    geom_sf(
      data = drives_hyper,
      shape = 24,
      size = 3.3,
      color = "#003049",
      fill = "#00A6FB",
      stroke = 1.1,
      alpha = 0.95
    )
}

plot_drives_by_iris <- function(iris_zone_etude) {
  ggplot() +
    geom_sf(
      data = iris_zone_etude,
      aes(fill = Nb_drives_2025),
      color = "grey78",
      linewidth = 0.15
    ) +
    scale_fill_gradient(
      low = "#edf8fb",
      high = "#2c7fb8",
      name = "Nb drives 2025"
    ) +
    labs(title = "Comptage drives par IRIS - 2025") +
    theme_minimal()
}

plot_iso_map <- function(iris_zone_etude, iso_sf, pdv_sf, title_txt, fill_title) {
  pdv_sf_plot <- separate_overlapping_points(pdv_sf, offset_m = 22)
  bb <- sf::st_bbox(iris_zone_etude)
  pad_x <- as.numeric((bb["xmax"] - bb["xmin"]) * 0.03)
  pad_y <- as.numeric((bb["ymax"] - bb["ymin"]) * 0.03)
  xlim <- c(as.numeric(bb["xmin"] - pad_x), as.numeric(bb["xmax"] + pad_x))
  ylim <- c(as.numeric(bb["ymin"] - pad_y), as.numeric(bb["ymax"] + pad_y))
  ggplot() +
    geom_sf(data = iris_zone_etude, fill = "grey95", color = "grey82", linewidth = 0.2) +
    geom_sf(data = iso_sf, aes(fill = factor(minutes)), color = NA, alpha = 0.28) +
    geom_sf(data = pdv_sf_plot, aes(shape = Type_PV), color = "#263238", size = 2.2) +
    scale_shape_manual(values = c(super = 16, hyper = 17), name = "Type de point de vente") +
    scale_fill_brewer(palette = "OrRd", name = fill_title) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal()
}

plot_overlap_map <- function(iris_zone_etude, overlap_map_sf, pdv_sf) {
  p <- ggplot() +
    geom_sf(data = iris_zone_etude, fill = "grey97", color = "grey82", linewidth = 0.2)

  if (!is.null(overlap_map_sf) && nrow(overlap_map_sf) > 0) {
    p <- p +
      geom_sf(
        data = overlap_map_sf,
        aes(fill = groupe_reseau_nom),
        color = "grey20",
        linewidth = 0.25,
        alpha = 0.45
      )
  }

  p +
    geom_sf(
      data = separate_overlapping_points(pdv_sf, offset_m = 22),
      aes(shape = Type_PV),
      color = "#263238",
      size = 2
    ) +
    scale_shape_manual(values = c(super = 16, hyper = 17), name = "Type de point de vente") +
    labs(title = "Synthese recouvrement intra-enseigne (2025)", fill = "Groupe") +
    theme_minimal()
}

plot_income_map <- function(iris_zone_etude, pdv_sf, value_col, title_txt, legend_txt, palette_opt = "C") {
  pdv_sf_plot <- separate_overlapping_points(pdv_sf, offset_m = 22)

  ggplot() +
    geom_sf(
      data = iris_zone_etude,
      aes(fill = .data[[value_col]]),
      color = "grey82",
      linewidth = 0.2
    ) +
    geom_sf(
      data = pdv_sf_plot,
      aes(shape = Type_PV),
      color = "#263238",
      size = 1.8,
      alpha = 0.9
    ) +
    scale_shape_manual(values = c(super = 16, hyper = 17), name = "Type de point de vente") +
    scale_fill_viridis_c(
      option = palette_opt,
      na.value = "grey92",
      name = legend_txt
    ) +
    labs(
      title = title_txt,
      subtitle = "Zone d'etude : Grenoble + communes Nord"
    ) +
    theme_minimal()
}

parse_income_from_v5_html <- function(html_path) {
  if (!file.exists(html_path)) return(tibble::tibble())

  txt <- readr::read_file(html_path)
  # Le htmlwidget encode des balises avec \/ ; on normalise d'abord.
  txt <- str_replace_all(txt, "\\\\/", "/")
  txt <- str_replace_all(txt, "\\\\\"", "\"")
  txt <- str_replace_all(txt, "\\\\n", " ")
  txt <- str_replace_all(txt, "\\\\t", " ")

  extract_var <- function(var_name, out_col) {
    # On recupere les popups du type:
    # <b>Nord-Est</b> ... <nobr>Mediane_euro</nobr> ... <nobr>31,290</nobr>
    pattern <- paste0(
      "<b>([^<]+)</b>.*?",
      "<nobr>", var_name, "</nobr>.*?<td align=\"right\"><nobr>([^<]+)</nobr>"
    )
    m <- str_match_all(txt, regex(pattern, dotall = TRUE))[[1]]
    if (nrow(m) == 0) return(tibble::tibble())
    tibble::tibble(
      iris_name = str_squish(m[, 2]),
      !!out_col := to_num(m[, 3])
    ) %>% distinct(iris_name, .keep_all = TRUE)
  }

  q1 <- extract_var("1er_quartile_euro", "q1_html")
  med <- extract_var("Mediane_euro", "med_html")
  q3 <- extract_var("3e_quartile_euro", "q3_html")

  if (nrow(q1) == 0 && nrow(med) == 0 && nrow(q3) == 0) {
    return(tibble::tibble())
  }

  full_join(full_join(q1, med, by = "iris_name"), q3, by = "iris_name")
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

# IRIS
iris_shp <- st_read(dsn = raw_dir, layer = "georef-france-iris-millesime", quiet = TRUE)
names(iris_shp) <- tolower(names(iris_shp))
iris_shp <- iris_shp %>% mutate(commune_norm = normalize_commune(coalesce_chr(., c("com_name", "nom_com"))))

communes_nord_norm <- c(
  "GRENOBLE", "MEYLAN", "LATRONCHE", "CORENC",
  "SAINTMARTINLEVINOUX", "SAINTEGREVE"
)
iris_zone_etude <- iris_shp %>% filter(commune_norm %in% communes_nord_norm)
if (nrow(iris_zone_etude) == 0) {
  stop("Aucun IRIS detecte dans la zone Nord.")
}

iris_code_col <- if ("code_iris" %in% names(iris_zone_etude)) "code_iris" else if ("iris_code" %in% names(iris_zone_etude)) "iris_code" else NA_character_
if (is.na(iris_code_col)) stop("Colonne code IRIS introuvable dans georef-france-iris-millesime.")
iris_zone_etude <- iris_zone_etude %>% mutate(Code_IRIS = as.character(.data[[iris_code_col]]))

# SIRENE
sirene_2025_raw <- read_sirene_layer(cours_dir, "dataseed-sirene-1")
sirene_2020_raw <- read_sirene_layer(cours_dir, "dataseed-sirene-2")

sirene_zone_2025 <- prepare_sirene_zone(sirene_2025_raw, communes_nord_norm, st_crs(iris_zone_etude))
if (is.null(sirene_zone_2025) || nrow(sirene_zone_2025) == 0) {
  stop("Aucun super/hyper 2025 detecte dans la zone Nord.")
}

if (is.null(sirene_2020_raw)) {
  message("dataseed-sirene-2.shp introuvable: cartes 2020 non exportees.")
  sirene_zone_2020 <- sirene_zone_2025[0, ]
} else {
  sirene_zone_2020 <- prepare_sirene_zone(sirene_2020_raw, communes_nord_norm, st_crs(iris_zone_etude))
}

message(
  "Comptage zone Nord | 2025: super=", sum(sirene_zone_2025$Type_PV == "super", na.rm = TRUE),
  ", hyper=", sum(sirene_zone_2025$Type_PV == "hyper", na.rm = TRUE)
)
if (nrow(sirene_zone_2020) > 0) {
  message(
    "Comptage zone Nord | 2020: super=", sum(sirene_zone_2020$Type_PV == "super", na.rm = TRUE),
    ", hyper=", sum(sirene_zone_2020$Type_PV == "hyper", na.rm = TRUE)
  )
}

# Copie enrichie avec variables socio-demographiques (alimentee plus bas)
iris_zone_socio <- iris_zone_etude

# ------------------------------
# Partie 3.1 - Cartes revenus (Q1, mediane, Q3)
# ------------------------------
iris_income_candidates <- c(
  file.path(cours_dir, "data", "raw", "iris_isere.xlsx"),
  file.path(cours_dir, "data", "actif", "iris_isere.xlsx"),
  file.path(cours_dir, "data", "iris_isere.xlsx")
)
iris_income_path <- iris_income_candidates[file.exists(iris_income_candidates)][1]

if (is.na(iris_income_path)) {
  message("iris_isere.xlsx introuvable: cartes revenus non exportees.")
} else {
  iris_income <- readxl::read_excel(iris_income_path)
  income_names <- names(iris_income)
  code_col <- income_names[income_names %in% c("CODE_IRIS", "Code_IRIS", "iris_code")][1]

  if (is.na(code_col)) {
    message("Colonne code IRIS absente dans iris_isere.xlsx: cartes revenus non exportees.")
  } else {
    req_cols <- c(code_col, "1er_quartile_euro", "Mediane_euro", "3e_quartile_euro")
    miss <- setdiff(req_cols, income_names)

    if (length(miss) > 0) {
      message(
        "Colonnes revenus manquantes dans iris_isere.xlsx: ",
        paste(miss, collapse = ", "),
        ". Cartes revenus non exportees."
      )
    } else {
      # Variables socio-demo (meme logique que v5)
      col_actifs <- first_existing_col(iris_income, c("Actifs_15-64_ans2", "Actifs_15-64_ans"))
      col_pop_15_64 <- first_existing_col(iris_income, c("Pop_15-64_ans"))
      col_menages <- first_existing_col(iris_income, c("Menages", "Menages2"))
      col_fam_couple_enfants <- first_existing_col(iris_income, c("Men_fam_princ_Couple_enfant_s", "Fam_Couple_enfant_s"))
      col_fam_mono <- first_existing_col(iris_income, c("Men_fam_princ_Famille_mono", "Fam_Monoparentales"))
      col_menages_une_voiture <- first_existing_col(iris_income, c("Menages_une_voiture"))
      col_cadres <- first_existing_col(iris_income, c("Pop_15_ansplus_Cadres_Prof_intel_sup", "Actifs_15-64_ans_Cadres_Prof_intel_sup"))
      col_pop_15_plus <- first_existing_col(iris_income, c("Pop_15_ansplus", "Pop_15_ansplus2"))
      col_population <- first_existing_col(iris_income, c("Popution", "Population"))
      col_superficie <- first_existing_col(iris_income, c("Superficie"))

      iris_income_sub <- tibble::tibble(
        Code_IRIS = as.character(iris_income[[code_col]]),
        `1er_quartile_euro` = to_num(iris_income$`1er_quartile_euro`),
        Mediane_euro = to_num(iris_income$Mediane_euro),
        `3e_quartile_euro` = to_num(iris_income$`3e_quartile_euro`),
        Pct_actifs = safe_ratio(
          num_col_or_na(iris_income, col_actifs),
          num_col_or_na(iris_income, col_pop_15_64)
        ),
        Pct_Menages_enfants = safe_ratio(
          num_col_or_na(iris_income, col_fam_couple_enfants) + num_col_or_na(iris_income, col_fam_mono),
          num_col_or_na(iris_income, col_menages)
        ),
        Pct_Menages_voiture = safe_ratio(
          num_col_or_na(iris_income, col_menages_une_voiture),
          num_col_or_na(iris_income, col_menages)
        ),
        Pct_Cadres_sup = safe_ratio(
          num_col_or_na(iris_income, col_cadres),
          num_col_or_na(iris_income, col_pop_15_plus)
        ),
        Densite_pop = safe_ratio(
          num_col_or_na(iris_income, col_population),
          num_col_or_na(iris_income, col_superficie)
        )
      )

      # Reprise stricte de la logique v5:
      # completer les revenus NA/0 via BASE_TD_FILO_IRIS_2021_DISP.csv.
      filo_candidates <- c(
        file.path(cours_dir, "data", "raw", "BASE_TD_FILO_IRIS_2021_DISP.csv"),
        file.path(cours_dir, "data", "actif", "BASE_TD_FILO_IRIS_2021_DISP.csv"),
        file.path(cours_dir, "data", "BASE_TD_FILO_IRIS_2021_DISP.csv")
      )
      filo_path <- filo_candidates[file.exists(filo_candidates)][1]

      if (!is.na(filo_path)) {
        filo <- read.csv(filo_path, sep = ";", encoding = "Latin1")
        names(filo) <- str_trim(names(filo))
        if ("IRIS" %in% names(filo) && !"Code_IRIS" %in% names(filo)) {
          names(filo)[names(filo) == "IRIS"] <- "Code_IRIS"
        }

        cols_filo <- c("Code_IRIS", "DISP_Q121", "DISP_MED21", "DISP_Q321")
        if (all(cols_filo %in% names(filo))) {
          filo_sub <- filo %>%
            transmute(
              Code_IRIS = as.character(Code_IRIS),
              DISP_Q121 = to_num(DISP_Q121),
              DISP_MED21 = to_num(DISP_MED21),
              DISP_Q321 = to_num(DISP_Q321)
            )

          iris_income_sub <- iris_income_sub %>%
            left_join(filo_sub, by = "Code_IRIS") %>%
            mutate(
              `1er_quartile_euro` = ifelse(
                is.na(`1er_quartile_euro`) | `1er_quartile_euro` == 0,
                DISP_Q121,
                `1er_quartile_euro`
              ),
              Mediane_euro = ifelse(
                is.na(Mediane_euro) | Mediane_euro == 0,
                DISP_MED21,
                Mediane_euro
              ),
              `3e_quartile_euro` = ifelse(
                is.na(`3e_quartile_euro`) | `3e_quartile_euro` == 0,
                DISP_Q321,
                `3e_quartile_euro`
              )
            ) %>%
            select(-DISP_Q121, -DISP_MED21, -DISP_Q321)

          message("Revenus completes avec FILOSOFI: ", filo_path)
        } else {
          message("Fichier FILOSOFI present mais colonnes attendues absentes: ", filo_path)
        }
      } else {
        message("Fichier FILOSOFI introuvable (BASE_TD_FILO_IRIS_2021_DISP.csv).")
      }

      iris_zone_income <- iris_zone_etude %>%
        left_join(iris_income_sub, by = "Code_IRIS")

      # Secours: reutiliser les valeurs deja presentes dans le rendu v5
      # pour les IRIS restant a NA/0 (ex: Nord-Est).
      unresolved_before <- iris_zone_income %>%
        st_drop_geometry() %>%
        filter(
          is.na(`1er_quartile_euro`) | `1er_quartile_euro` == 0 |
            is.na(Mediane_euro) | Mediane_euro == 0 |
            is.na(`3e_quartile_euro`) | `3e_quartile_euro` == 0
        )

      if (nrow(unresolved_before) > 0) {
        html_fallback <- parse_income_from_v5_html(
          file.path(cours_dir, "notebooks", "supermarket_implantation_v5.html")
        )
        if (nrow(html_fallback) > 0 && "iris_name" %in% names(iris_zone_income)) {
          iris_zone_income <- iris_zone_income %>%
            left_join(html_fallback, by = c("iris_name" = "iris_name")) %>%
            mutate(
              `1er_quartile_euro` = ifelse(
                is.na(`1er_quartile_euro`) | `1er_quartile_euro` == 0,
                q1_html,
                `1er_quartile_euro`
              ),
              Mediane_euro = ifelse(
                is.na(Mediane_euro) | Mediane_euro == 0,
                med_html,
                Mediane_euro
              ),
              `3e_quartile_euro` = ifelse(
                is.na(`3e_quartile_euro`) | `3e_quartile_euro` == 0,
                q3_html,
                `3e_quartile_euro`
              )
            ) %>%
            select(-any_of(c("q1_html", "med_html", "q3_html")))
          message("Revenus completes avec fallback depuis supermarket_implantation_v5.html")
        }
      }

      unresolved_after <- iris_zone_income %>%
        st_drop_geometry() %>%
        filter(
          is.na(`1er_quartile_euro`) | `1er_quartile_euro` == 0 |
            is.na(Mediane_euro) | Mediane_euro == 0 |
            is.na(`3e_quartile_euro`) | `3e_quartile_euro` == 0
        ) %>%
        select(Code_IRIS, iris_name, com_name, `1er_quartile_euro`, Mediane_euro, `3e_quartile_euro`)

      if (nrow(unresolved_after) > 0) {
        message("IRIS avec revenus encore manquants/0 apres correction: ", nrow(unresolved_after))
        print(unresolved_after)
      } else {
        message("Aucun revenu manquant/0 sur la zone Nord apres correction.")
      }

      p_income_q1 <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "1er_quartile_euro",
        title_txt = "1er quartile du revenu - zone Nord",
        legend_txt = "Q1 revenu (EUR)",
        palette_opt = "B"
      )
      save_map(
        p_income_q1,
        file.path(processed_plots_dir, "carte_revenu_q1_zone_nord_partie3_1_v5_equivalent.png")
      )

      p_income_med <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "Mediane_euro",
        title_txt = "Mediane du revenu - zone Nord",
        legend_txt = "Mediane (EUR)",
        palette_opt = "C"
      )
      save_map(
        p_income_med,
        file.path(processed_plots_dir, "carte_revenu_mediane_zone_nord_partie3_1_v5_equivalent.png")
      )

      p_income_q3 <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "3e_quartile_euro",
        title_txt = "3e quartile du revenu - zone Nord",
        legend_txt = "Q3 revenu (EUR)",
        palette_opt = "D"
      )
      save_map(
        p_income_q3,
        file.path(processed_plots_dir, "carte_revenu_q3_zone_nord_partie3_1_v5_equivalent.png")
      )

      # Variables en pourcentage pour affichage cartographique
      iris_zone_income <- iris_zone_income %>%
        mutate(
          Pct_actifs_pct = 100 * as.numeric(Pct_actifs),
          Pct_Menages_enfants_pct = 100 * as.numeric(Pct_Menages_enfants),
          Pct_Menages_voiture_pct = 100 * as.numeric(Pct_Menages_voiture),
          Pct_Cadres_sup_pct = 100 * as.numeric(Pct_Cadres_sup)
        )

      # Exports cartes socio-demo (parties 3.2 / 3.3 / 3.4)
      p_actifs <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "Pct_actifs_pct",
        title_txt = "Part des actifs (15-64 ans) - zone Nord",
        legend_txt = "% actifs",
        palette_opt = "B"
      )
      save_map(
        p_actifs,
        file.path(processed_plots_dir, "carte_pct_actifs_zone_nord_partie3_2_v5_equivalent.png")
      )

      p_enfants <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "Pct_Menages_enfants_pct",
        title_txt = "Part des menages avec enfants - zone Nord",
        legend_txt = "% menages avec enfants",
        palette_opt = "D"
      )
      save_map(
        p_enfants,
        file.path(processed_plots_dir, "carte_pct_menages_enfants_zone_nord_partie3_3_v5_equivalent.png")
      )

      p_voiture <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "Pct_Menages_voiture_pct",
        title_txt = "Part des menages avec voiture - zone Nord",
        legend_txt = "% menages avec voiture",
        palette_opt = "C"
      )
      save_map(
        p_voiture,
        file.path(processed_plots_dir, "carte_pct_menages_voiture_zone_nord_partie3_3_v5_equivalent.png")
      )

      p_cadres <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "Pct_Cadres_sup_pct",
        title_txt = "Part des cadres superieurs - zone Nord",
        legend_txt = "% cadres superieurs",
        palette_opt = "A"
      )
      save_map(
        p_cadres,
        file.path(processed_plots_dir, "carte_pct_cadres_zone_nord_partie3_4_v5_equivalent.png")
      )

      p_densite <- plot_income_map(
        iris_zone_etude = iris_zone_income,
        pdv_sf = sirene_zone_2025,
        value_col = "Densite_pop",
        title_txt = "Densite de population - zone Nord",
        legend_txt = "Densite (hab/km²)",
        palette_opt = "E"
      )
      save_map(
        p_densite,
        file.path(processed_plots_dir, "carte_densite_population_zone_nord_partie3_4_v5_equivalent.png")
      )

      # Base enrichie reutilisee pour OLS
      iris_zone_socio <- iris_zone_income
    }
  }
}

# ------------------------------
# Partie 4 - Cartes concurrence 2020/2025 + comptage IRIS
# ------------------------------
if (nrow(sirene_zone_2020) > 0) {
  p_2020 <- plot_concurrence_map(
    iris_zone_etude,
    sirene_zone_2020,
    "Concurrence super/hyper - 2020 (dataseed-sirene-2)"
  )
  save_map(
    p_2020,
    file.path(processed_plots_dir, "carte_concurrence_2020_partie4_v5_equivalent.png")
  )
}

p_2025 <- plot_concurrence_map(
  iris_zone_etude,
  sirene_zone_2025,
  "Concurrence super/hyper - 2025 (dataseed-sirene-1)"
)
save_map(
  p_2025,
  file.path(processed_plots_dir, "carte_concurrence_2025_partie4_v5_equivalent.png")
)

# Comptage par IRIS
join_2025 <- st_join(sirene_zone_2025, iris_zone_etude %>% dplyr::select(Code_IRIS), left = TRUE)
sirene_zone_2025$Code_IRIS <- as.character(st_drop_geometry(join_2025)$Code_IRIS)

compte_2025 <- sirene_zone_2025 %>%
  st_drop_geometry() %>%
  filter(!is.na(Code_IRIS), str_squish(Code_IRIS) != "") %>%
  count(Code_IRIS, name = "Nb_pdv_2025")

if (nrow(sirene_zone_2020) > 0) {
  join_2020 <- st_join(sirene_zone_2020, iris_zone_etude %>% dplyr::select(Code_IRIS), left = TRUE)
  sirene_zone_2020$Code_IRIS <- as.character(st_drop_geometry(join_2020)$Code_IRIS)
  compte_2020 <- sirene_zone_2020 %>%
    st_drop_geometry() %>%
    filter(!is.na(Code_IRIS), str_squish(Code_IRIS) != "") %>%
    count(Code_IRIS, name = "Nb_pdv_2020")
} else {
  compte_2020 <- tibble::tibble(Code_IRIS = character(), Nb_pdv_2020 = integer())
}

iris_count <- iris_zone_etude %>%
  left_join(compte_2020, by = "Code_IRIS") %>%
  left_join(compte_2025, by = "Code_IRIS") %>%
  mutate(
    Nb_pdv_2020 = coalesce(as.numeric(Nb_pdv_2020), 0),
    Nb_pdv_2025 = coalesce(as.numeric(Nb_pdv_2025), 0)
  )

max_count <- max(c(iris_count$Nb_pdv_2020, iris_count$Nb_pdv_2025), na.rm = TRUE)
if (!is.finite(max_count) || max_count <= 0) max_count <- 1

if (nrow(sirene_zone_2020) > 0) {
  p_count_2020 <- plot_iris_count_map(iris_count, "Nb_pdv_2020", max_count, "Comptage par IRIS - 2020")
  save_map(
    p_count_2020,
    file.path(processed_plots_dir, "carte_comptage_iris_2020_partie4_v5_equivalent.png")
  )
}

p_count_2025 <- plot_iris_count_map(iris_count, "Nb_pdv_2025", max_count, "Comptage par IRIS - 2025")
save_map(
  p_count_2025,
  file.path(processed_plots_dir, "carte_comptage_iris_2025_partie4_v5_equivalent.png")
)

# ------------------------------
# Partie 4.1 - Super/hyper vs drives
# ------------------------------
crossref_candidates <- c(
  file.path(cours_dir, "data", "processed", "crossref_supermarches_drives_zone_v3.csv"),
  file.path(cours_dir, "data", "processed", "crossref_supermarches_drives_zone.csv"),
  file.path(cours_dir, "data", "processed", "crossref_supermarches_drives.csv")
)
crossref_path <- crossref_candidates[file.exists(crossref_candidates)][1]

if (is.na(crossref_path)) {
  message("Aucun fichier crossref drives trouve dans data/processed.")
  drives_2025 <- sirene_zone_2025[0, ]
} else {
  crossref <- read_csv(crossref_path, show_col_types = FALSE) %>%
    mutate(
      siret = as.character(siret),
      drive_trouve = as.logical(drive_trouve)
    ) %>%
    filter(drive_trouve %in% TRUE) %>%
    distinct(siret, .keep_all = TRUE)

  drives_2025 <- sirene_zone_2025 %>%
    mutate(siret = as.character(siret)) %>%
    inner_join(crossref %>% dplyr::select(siret), by = "siret") %>%
    distinct(siret, .keep_all = TRUE)
}

p_vs_drives <- plot_super_hyper_vs_drives(iris_zone_etude, sirene_zone_2025, drives_2025)
save_map(
  p_vs_drives,
  file.path(processed_plots_dir, "carte_super_hyper_vs_drives_2025_partie4_1_v5_equivalent.png")
)

if (nrow(drives_2025) > 0) {
  if ("Code_IRIS" %in% names(drives_2025)) {
    drives_2025$Code_IRIS <- as.character(drives_2025$Code_IRIS)
  } else {
    join_drives <- st_join(drives_2025, iris_zone_etude %>% dplyr::select(Code_IRIS), left = TRUE)
    join_cols <- names(st_drop_geometry(join_drives))
    code_col <- join_cols[str_detect(join_cols, "^Code_IRIS(\\.|$)")][1]
    if (is.na(code_col)) {
      drives_2025$Code_IRIS <- NA_character_
    } else {
      drives_2025$Code_IRIS <- as.character(st_drop_geometry(join_drives)[[code_col]])
    }
  }
} else {
  drives_2025 <- drives_2025 %>% mutate(Code_IRIS = character(0))
}

compte_drives <- drives_2025 %>%
  st_drop_geometry() %>%
  filter(!is.na(Code_IRIS), str_squish(Code_IRIS) != "") %>%
  count(Code_IRIS, name = "Nb_drives_2025")

iris_drives <- iris_zone_etude %>%
  left_join(compte_drives, by = "Code_IRIS") %>%
  mutate(Nb_drives_2025 = coalesce(as.numeric(Nb_drives_2025), 0))

p_drives_iris <- plot_drives_by_iris(iris_drives)
save_map(
  p_drives_iris,
  file.path(processed_plots_dir, "carte_drives_par_iris_2025_partie4_1_v5_equivalent.png")
)

message("Drives detectes (2025): ", nrow(drives_2025))

# ------------------------------
# Partie 5 - Isochrones pied/voiture
# ------------------------------
strict_isochrones <- Sys.getenv("STRICT_ISOCHRONES", "0") == "1"
otp_autostart <- Sys.getenv("OTP_AUTOSTART", "0") == "1"
otp_allow_partial <- Sys.getenv("OTP_ALLOW_PARTIAL", "0") == "1"
local_otp_jar <- file.path(cours_dir, "data", "otp-1.5.0-shaded.jar")
path_otp <- if (file.exists(local_otp_jar)) local_otp_jar else NA_character_
path_data <- file.path(cours_dir, "data")

otpcon <- connect_otp_runtime(
  path_data = path_data,
  path_otp = path_otp,
  strict_isochrones = strict_isochrones,
  otp_autostart = otp_autostart
)

iso_walk <- build_multiformat_isochrones(
  points_sf = sirene_zone_2025,
  profile = "walk",
  otpcon = otpcon,
  strict = strict_isochrones,
  allow_partial = otp_allow_partial
)
iso_car <- build_multiformat_isochrones(
  points_sf = sirene_zone_2025,
  profile = "car",
  otpcon = otpcon,
  strict = strict_isochrones,
  allow_partial = otp_allow_partial
)

if (nrow(iso_walk) > 0) {
  p_iso_walk <- plot_iso_map(
    iris_zone_etude = iris_zone_etude,
    iso_sf = iso_walk,
    pdv_sf = sirene_zone_2025,
    title_txt = "Zones de chalandise (a pied) - 2025",
    fill_title = "Minutes (a pied)"
  )
  save_map(
    p_iso_walk,
    file.path(processed_plots_dir, "carte_isochrones_pied_2025_partie5_v5_equivalent.png")
  )
}

if (nrow(iso_car) > 0) {
  p_iso_car <- plot_iso_map(
    iris_zone_etude = iris_zone_etude,
    iso_sf = iso_car,
    pdv_sf = sirene_zone_2025,
    title_txt = "Zones de chalandise (en voiture) - 2025",
    fill_title = "Minutes (voiture)"
  )
  save_map(
    p_iso_car,
    file.path(processed_plots_dir, "carte_isochrones_voiture_2025_partie5_v5_equivalent.png")
  )
}

message(
  "Source isochrones pied: ",
  paste(sort(unique(iso_walk$iso_source)), collapse = ", ")
)
message(
  "Source isochrones voiture: ",
  paste(sort(unique(iso_car$iso_source)), collapse = ", ")
)

# ------------------------------
# Partie 5.1 - Recouvrement intra-enseigne (isochrones primaires voiture)
# ------------------------------
iso_primary_car <- iso_car %>%
  filter(
    (as.character(Type_PV) == "super" & as.numeric(minutes) == 10) |
      (as.character(Type_PV) == "hyper" & as.numeric(minutes) == 20)
  ) %>%
  st_make_valid()

iso_primary_car <- iso_primary_car %>%
  left_join(
    sirene_zone_2025 %>%
      st_drop_geometry() %>%
      dplyr::select(siret, groupe_reseau_nom_ref = groupe_reseau_nom),
    by = "siret"
  )

if ("groupe_reseau_nom" %in% names(iso_primary_car)) {
  iso_primary_car <- iso_primary_car %>%
    mutate(groupe_reseau_nom = dplyr::coalesce(
      as.character(groupe_reseau_nom),
      as.character(groupe_reseau_nom_ref)
    ))
} else {
  iso_primary_car <- iso_primary_car %>%
    mutate(groupe_reseau_nom = as.character(groupe_reseau_nom_ref))
}

iso_primary_car <- iso_primary_car %>%
  mutate(
    groupe_reseau_nom = ifelse(
      groupe_reseau_nom %in% c("Groupe Auchan", "Groupe Casino"),
      "Groupe Auchan/Casino",
      groupe_reseau_nom
    ),
    groupe_reseau_nom = ifelse(
      is.na(groupe_reseau_nom) | str_squish(groupe_reseau_nom) == "",
      "Independant/Autre",
      groupe_reseau_nom
    )
  ) %>%
  dplyr::select(-any_of("groupe_reseau_nom_ref"))

# ------------------------------
# Partie 8 - OLS (residus)
# ------------------------------
iris_ols <- iris_zone_socio

if (nrow(iso_primary_car) == 0) {
  iris_ols$Nb_pdv_access_primaire_voiture <- 0
} else {
  iris_ols$Nb_pdv_access_primaire_voiture <- rowSums(
    st_intersects(iris_ols, iso_primary_car, sparse = FALSE)
  )
}

predictors_candidates <- c(
  "Pct_actifs",
  "Pct_Menages_enfants",
  "Pct_Menages_voiture",
  "Pct_Cadres_sup",
  "Mediane_euro",
  "Densite_pop"
)
predictors_ok <- predictors_candidates[predictors_candidates %in% names(iris_ols)]
predictors_ok <- predictors_ok[vapply(predictors_ok, function(v) {
  vv <- suppressWarnings(as.numeric(iris_ols[[v]]))
  sum(is.finite(vv)) >= 5 && stats::var(vv[is.finite(vv)]) > 0
}, FUN.VALUE = logical(1))]

if (length(predictors_ok) < 1) {
  message("OLS non estime: aucun predicteur socio-demo valide.")
} else {
  model_df <- iris_ols %>%
    st_drop_geometry() %>%
    transmute(
      y = as.numeric(Nb_pdv_access_primaire_voiture),
      dplyr::across(dplyr::all_of(predictors_ok), ~ suppressWarnings(as.numeric(.x)))
    )

  complete_idx <- stats::complete.cases(model_df)
  model_cc <- model_df[complete_idx, , drop = FALSE]

  if (nrow(model_cc) < 8 || stats::var(model_cc$y) == 0) {
    message("OLS non estime: donnees insuffisantes apres filtrage complet.")
  } else {
    ols_formula <- stats::as.formula(paste("y ~", paste(predictors_ok, collapse = " + ")))
    ols_mod <- stats::lm(ols_formula, data = model_cc)

    iris_ols$ols_res <- NA_real_
    iris_ols$ols_res[complete_idx] <- stats::resid(ols_mod)

    p_ols_res <- ggplot() +
      geom_sf(data = iris_ols, aes(fill = ols_res), color = "white", linewidth = 0.25) +
      scale_fill_gradient2(
        low = "#2C7BB6",
        mid = "white",
        high = "#D7191C",
        midpoint = 0,
        na.value = "grey90",
        name = "Residus OLS"
      ) +
      geom_sf(
        data = separate_overlapping_points(sirene_zone_2025, offset_m = 22),
        aes(shape = Type_PV),
        color = "#263238",
        size = 1.7,
        alpha = 0.9
      ) +
      scale_shape_manual(values = c(super = 16, hyper = 17), name = "Type de point de vente") +
      labs(
        title = "Carte des residus OLS (accessibilite primaire voiture)",
        subtitle = "Modele OLS sur variables socio-demographiques"
      ) +
      theme_minimal()

    save_map(
      p_ols_res,
      file.path(processed_plots_dir, "carte_residus_ols_accessibilite_partie8_v5_equivalent.png")
    )

    message("OLS estime avec predicteurs: ", paste(predictors_ok, collapse = ", "))
    message("R2 ajuste OLS: ", round(summary(ols_mod)$adj.r.squared, 4))
  }
}

overlap_geoms <- list()
kp <- 0L

for (grp in unique(na.omit(iso_primary_car$groupe_reseau_nom))) {
  sub <- iso_primary_car %>% filter(groupe_reseau_nom == grp)
  if (nrow(sub) < 2) next

  inter_idx <- st_intersects(sub)
  for (i in seq_len(nrow(sub) - 1)) {
    j_candidates <- inter_idx[[i]]
    j_candidates <- j_candidates[j_candidates > i]
    if (length(j_candidates) == 0) next

    for (j in j_candidates) {
      inter_geom <- tryCatch(
        suppressWarnings(st_intersection(st_geometry(sub[i, ]), st_geometry(sub[j, ]))),
        error = function(e) NULL
      )
      if (is.null(inter_geom) || length(inter_geom) == 0) next
      inter_geom <- inter_geom[!st_is_empty(inter_geom)]
      if (length(inter_geom) == 0) next

      inter_union <- suppressWarnings(st_union(inter_geom))
      if (st_is_empty(inter_union)) next

      kp <- kp + 1L
      overlap_geoms[[kp]] <- st_sf(
        groupe_reseau_nom = grp,
        geometry = inter_union,
        crs = st_crs(iso_primary_car)
      )
    }
  }
}

if (length(overlap_geoms) == 0) {
  overlap_map_sf <- NULL
} else {
  overlap_map_sf <- do.call(rbind, overlap_geoms) %>%
    group_by(groupe_reseau_nom) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
}

p_overlap <- plot_overlap_map(iris_zone_etude, overlap_map_sf, sirene_zone_2025)
save_map(
  p_overlap,
  file.path(processed_plots_dir, "carte_recouvrement_intra_enseigne_2025_partie5_1_v5_equivalent.png")
)

message("Termine. PNG exportees dans: ", processed_plots_dir)

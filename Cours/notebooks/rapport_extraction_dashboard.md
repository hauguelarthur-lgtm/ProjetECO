Rapport d'extraction des données du flexdashboard

1. Vue d’ensemble du document

- Structure générale du dashboard : sections PARTIE avec sous-sections `###` et blocs `Row` flexdashboard.
- Nombre de sections : 24
- Principaux thèmes couverts : PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord), PARTIE 1.2: Table des ponctuels, PARTIE 1.2: Dictionnaire des variables, PARTIE 2.1 : Zones de chalandise (cadre methodologique), PARTIE 2.2 : Zones de chalandise (cadre methodologique), PARTIE 2.3 : Zones de chalandise (cadre methodologique), PARTIE 2.4 : Zones de chalandise (cadre methodologique), PARTIE 2.1 : Zones de chalandise (cadre methodologique), PARTIE 3.1: Cartographie du premier quartile et du revenu median, PARTIE 3.2: Cartographie du troisième quartile du revenu et du pourcentage d'actifs parmi les 15–64 ans, PARTIE 3.3: Cartographie des parts de ménages avec enfants/ avec voiture, PARTIE 3.4: Cartographie de la part de cadres supérieurs et de la densité de la population, PARTIE 4: Analyse de la concurrence, PARTIE 4.1: Comparaison super/hyper vs drives (2025), PARTIE 5: Zones de chalandises en 2025, PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025), PARTIE 6: Accessibilité, PARTIE 6.1: Couverture des IRIS, PARTIE 6.2: Comparaison 2020 vs 2025 (accessibilite et couverture), PARTIE 7: Autocorrelation spatiale, PARTIE 8: Carte des résidus OLS et Tests, PARTIE 9: Top 4 zones preferables pour une nouvelle implantation, PARTIE 10: CA potentiel (Pappers + projection Top 1), PARTIE 11: Analyse sectorielle redigee (a completer)
- Jeux de données repérés : CA_super_hyper_pappers_nombres.csv, GROUPE07_DicoDonnées_rapport1.csv, GROUPE07_TabPonctuels_rapport1.csv, crossref drives absent: tentative de fallback avec drives_grenoble.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv, dataseed-sirene-1.shp, drives_grenoble.csv, economicref-france-sirene-v3.csv, iris_isere.xlsx, nb_pdv_iris_iso_primaire_hierarchie_2025.csv, otp-1.5.0-shaded.jar
- Niveaux géographiques repérés : Agglomération complète (Grenoble+Nord+Ouest+Sud-Est), IRIS, IRIS, Commune, IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est), IRIS, Points établissements (SIRENE), IRIS, Points établissements (SIRENE), Zone cible Nord, non identifiable à partir du Rmd seul
- Types de visualisations repérés : bloc texte généré, carte tmap (choroplèthe et/ou points), graphique ggplot (carte points / diagnostics), indicateurs/tests spatiaux, non identifiable à partir du Rmd seul, tableau (DataTable)

2. Analyse descriptive section par section

Section 1 — PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord) ».

Visualisations présentes :
tableau (DataTable)

Variables / données concernées :
commune_norm, cp_norm, denominatio, drive_trouve, siret

Échelle géographique :
IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est)

Période :
2025

Source :
crossref drives absent: tentative de fallback avec drives_grenoble.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv, drives_grenoble.csv, economicref-france-sirene-v3.csv

Données extraites :
- Sous-sections détectées: Répartition des établissements dans la zone d'étude, Nombre de points de vente par enseigne
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 2
  - Répartition des établissements dans la zone d'étude (2 lignes)
  - Nombre de points de vente (agglo complète) (16 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Enseignes listées dans les tableaux de la section: ALDI MARCHE, AUCHAN SUPERMARCHE, CARREFOUR, CARREFOUR CITY, CARREFOUR CONTACT, CARREFOUR EXPRESS, CARREFOUR MARKET, CITY, INTERMARCHE, KAYI EXPRESS, LIDL, MARKET, MONOPRIX, NETTO, SUPER MARCHE SAVEURS ISTANBUL, UTILE

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - `iris_shp <- st_read(`
  - `sirene_shp <- st_read(`
  - `drive_info <- readr::read_csv(crossref_path, show_col_types = FALSE) %>%`
  - `drives_raw <- suppressMessages(readr::read_delim(`
- Jointures:
  - `left_join(sirene_csv, by = "siret") %>%`
  - `left_join(sirene_csv, by = "siret") %>%`
  - `joined_agglo <- st_join(`
  - `inner_join(drives_raw, by = c("cp_norm", "commune_norm", "voie_norm")) %>%`
  - `left_join(drive_info, by = "siret") %>%`
- Filtres:
  - `out %>% filter(code_naf %in% target_naf_codes, !is.na(Type_PV))`
  - `sirene_agglo_full_raw <- sirene_shp %>% filter(commune_norm %in% communes_agglo_norm)`
  - `sirene_zone_etude <- sirene_agglo_full %>% filter(commune_norm %in% communes_zone_cible_norm)`
  - `iris_agglo <- iris_shp %>% filter(commune_norm %in% communes_agglo_norm)`
  - `iris_zone_etude <- iris_agglo %>% filter(commune_norm %in% communes_zone_cible_norm)`
  - `filter(is.na(numero_sirene) | is.na(numero_drive) | numero_sirene == numero_drive) %>%`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Répartition des établissements dans la zone d'étude**

| Type d'établissement | Nombre |
| --- | --- |
| Hypermarchés | 2 |
| Supermarchés | 35 |

**Nombre de points de vente (agglo complète)**

| Enseigne de l'établissement | Nombre |
| --- | --- |
| ALDI MARCHE | 3 |
| AUCHAN SUPERMARCHE | 4 |
| CARREFOUR | 2 |
| CARREFOUR CITY | 12 |
| CARREFOUR CONTACT | 2 |
| CARREFOUR EXPRESS | 5 |
| CARREFOUR MARKET | 2 |
| CITY | 1 |
| INTERMARCHE | 3 |
| KAYI EXPRESS | 1 |
| LIDL | 5 |
| MARKET | 1 |
| MONOPRIX | 1 |
| NETTO | 1 |
| SUPER MARCHE SAVEURS ISTANBUL | 1 |
| UTILE | 2 |
Section 2 — PARTIE 1.2: Table des ponctuels

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 1.2: Table des ponctuels ».

Visualisations présentes :
tableau (DataTable)

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
Agglomération complète (Grenoble+Nord+Ouest+Sud-Est)

Période :
non identifiable à partir du Rmd seul

Source :
GROUPE07_TabPonctuels_rapport1.csv

Données extraites :
- Sous-sections détectées: Table des ponctuels
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Aperçu de la table des ponctuels (agglo complète) (6 lignes)

Tableau des données territoriales :
non applicable ou non identifiable à partir du Rmd seul

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Aperçu de la table des ponctuels (agglo complète)**

| SIRET | SIREN | Raison_sociale | Nom_usuel | Nom_usuel_source | Nom_usuel_alternatif | Date_changement_nom | Flag_changement_nom | Siege | Appartient_groupe | Nom_groupe | Date_creation | Drive_accole | Date_creation_drive | Drive_deporte | Code_NAF | Libelle_NAF | Adresse | Code_IRIS | Zone_etude |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 34513048823761 | 345130488 | NA | CARREFOUR CITY | enseigne_de | NA | NA | False | False | True | Commerce de détail en magasin non spécialisé à prédominance alimentaire | 2020-11-09 | False | NA | False | 4711D | NAFRev2 | 24 Rue DE NORMANDIE-NIEMEN, 38130, ECHIROLLES | 381510402 | False |
| 93143055700013 | 931430557 | NA | CARREFOUR CITY | enseigne_de | NA | NA | False | True | True | Commerce de détail en magasin non spécialisé à prédominance alimentaire | 2024-08-24 | False | NA | False | 4711D | NAFRev2 | 7 Square DES FUSILLES, 38000, GRENOBLE | 381850301 | True |
| 98486142700065 | 984861427 | NA | NA | denominatio | NA | NA | False | False | True | Commerce de détail en magasin non spécialisé à prédominance alimentaire | 2024-05-30 | False | NA | False | 4711F | NAFRev2 | 120 Boulevard PAUL LANGEVIN, 38600, FONTAINE | 381690110 | False |
| 85137596400026 | 851375964 | NA | CARREFOUR EXPRESS | enseigne_de | NA | NA | False | True | True | Commerce de détail en magasin non spécialisé à prédominance alimentaire | 2021-04-16 | False | NA | False | 4711D | NAFRev2 | 97 Avenue BENOIT FRACHON, 38400, SAINT-MARTIN-D'HERES | 384210201 | False |
| 94201937300011 | 942019373 | NA | NA | denominatio | NA | NA | False | True | True | Commerce de détail en magasin non spécialisé à prédominance alimentaire | 2025-03-13 | False | NA | False | 4711F | NAFRev2 | VOIE 24 VILLENEUVE, 38130, ECHIROLLES | NA | False |
| 44433064101075 | 444330641 | ALDI MARCHE | ALDI MARCHE | enseigne_de | NA | NA | False | False | True | Commerce de détail en magasin non spécialisé à prédominance alimentaire | 2021-04-26 | False | NA | False | 4711D | NAFRev2 | LES ILES, 38120, SAINT-EGREVE | 383820101 | True |
Section 3 — PARTIE 1.2: Dictionnaire des variables

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 1.2: Dictionnaire des variables ».

Visualisations présentes :
bloc texte généré; tableau (DataTable)

Variables / données concernées :
description

Échelle géographique :
IRIS, Commune, Points établissements (SIRENE), Zone cible Nord

Période :
2020, 2025

Source :
GROUPE07_DicoDonnées_rapport1.csv

Données extraites :
- Sous-sections détectées: Dictionnaire des variables
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Dictionnaire des variables (vivant) (38 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Dictionnaire des variables (vivant)**

| variable | type | description | source | regle_construction |
| --- | --- | --- | --- | --- |
| SIRET | character | Identifiant unique de l’établissement | SIRENE | Copie directe de siret |
| SIREN | character | Identifiant de l’unité légale | SIRENE | Copie directe de siren |
| Raison_sociale | character | Dénomination officielle de l’établissement | SIRENE | normalize_enseigne(denominatio) |
| Nom_usuel | character | Nom commercial/usuel retenu | SIRENE + nettoyage | enseigne_de sinon denominatio |
| Nom_usuel_source | character | Variable source du nom usuel retenu | Traitement interne | enseigne_de ou denominatio selon disponibilite |
| Nom_usuel_alternatif | character | Nom alternatif utile au suivi des changements d’enseigne | SIRENE + traitement interne | denomination alternative si differente |
| Date_changement_nom | character | Date de changement d’enseigne/dénomination (si connue) | A completer | Placeholder (source externe future) |
| Flag_changement_nom | logical | Indique une transition d’enseigne/dénomination | Traitement interne | FALSE par defaut |
| Siege | logical | Indique si l’établissement est siège | SIRENE | siret == siret_du_si |
| Appartient_groupe | logical | Appartenance à un groupe/réseau | SIRENE + mapping groupe | Nom_groupe non NA |
| Nom_groupe | character | Nom du groupe commercial | SIRENE + mapping groupe | groupe_de_l sinon mapping enseigne-&gt;groupe |
| Date_creation | character | Date de création de l’établissement | SIRENE | as_date_flexible(date_de_cre) |
| Drive_accole | logical | Présence d’un drive accolé | crossref drives / fallback | drive_trouve et type drive contient accole/voiture |
| Date_creation_drive | character | Date de création du drive | A completer | date_creation_drive si disponible sinon NA |
| Drive_deporte | logical | Présence d’un drive déporté | crossref drives / fallback | drive_trouve et non drive_accole |
| Code_NAF | character | Code NAF de l’établissement | SIRENE | normalize_naf(...) |
| Libelle_NAF | character | Libellé du code NAF | SIRENE | Copie nomenclatur |
| Adresse | character | Adresse postale complète | SIRENE | concat(adresse_de_, code_postal, commune_de_) |
| Code_IRIS | character | Code IRIS de localisation | INSEE IRIS + jointure spatiale | st_join point sur IRIS |
| Zone_etude | logical | True si point dans la zone cible Nord | Traitement interne | commune_norm appartient a communes_nord_norm |
| Nb_pdv_2020 | numeric | Nombre de super/hyper/superette par IRIS (2020) | SIRENE 2020 + jointure IRIS | Comptage par Code_IRIS |
| Nb_pdv_2025 | numeric | Nombre de super/hyper/superette par IRIS (2025) | SIRENE 2025 + jointure IRIS | Comptage par Code_IRIS |
| Nb_drives_2025 | numeric | Nombre de drives par IRIS (2025) | crossref drives + jointure IRIS | Comptage des points avec drive |
| Nb_pdv_access_primaire_voiture_2020 | numeric | Points accessibles en zone primaire voiture (2020) | OTP/fallback + SIRENE 2020 | rowSums(st_intersects(IRIS, iso primaire)) |
| Nb_pdv_access_primaire_voiture_2025 | numeric | Points accessibles en zone primaire voiture (2025) | OTP/fallback + SIRENE 2025 | rowSums(st_intersects(IRIS, iso primaire)) |
| Nb_pdv_access_primaire_voiture | numeric | Alias 2025 pour l'accessibilite primaire voiture | Traitement interne | Copie de Nb_pdv_access_primaire_voiture_2025 |
| Nb_supermarches | numeric | Nombre de super/hyper/superette accessibles a pied (10 min) | OTP/fallback | rowSums(st_intersects(IRIS, iso600)) |
| couverture_pct_2020 | numeric | Part de surface IRIS couverte (2020) | OTP/fallback + calcul surfacique | intersect_area / iris_area * 100 |
| couverture_pct_2025 | numeric | Part de surface IRIS couverte (2025) | OTP/fallback + calcul surfacique | intersect_area / iris_area * 100 |
| couverture_pct | numeric | Alias 2025 de la couverture | Traitement interne | Copie de couverture_pct_2025 |
| HHI_2020 | numeric | Indice Herfindahl-Hirschman par enseigne (2020) | SIRENE 2020 | Somme des parts au carre |
| HHI_2025 | numeric | Indice Herfindahl-Hirschman par enseigne (2025) | SIRENE 2025 | Somme des parts au carre |
| score_total_A | numeric | Score classement A (socio-demo uniquement) | Traitement interne | Agregation ponderee variables tache 1.2 |
| score_total_B | numeric | Score classement B (socio-demo + concurrence) | Traitement interne | Agregation ponderee tache 1.2 + 1.3 |
| score_offre_gap_B | numeric | Sous-score d'insuffisance d'offre (classement B) | Traitement interne | Combinaison concurrence/couverture/drives |
| rank_A | numeric | Rang selon classement A | Traitement interne | rank(-score_total_A) |
| rank_B | numeric | Rang selon classement B | Traitement interne | rank(-score_total_B) |
| delta_rang_A_B | numeric | Difference de rang entre classement A et B | Traitement interne | rank_A - rank_B |
Section 4 — PARTIE 2.1 : Zones de chalandise (cadre methodologique)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 2.1 : Zones de chalandise (cadre methodologique) ».

Visualisations présentes :
non identifiable à partir du Rmd seul

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
non identifiable à partir du Rmd seul

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Hypermarché
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
non applicable ou non identifiable à partir du Rmd seul

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 5 — PARTIE 2.2 : Zones de chalandise (cadre methodologique)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 2.2 : Zones de chalandise (cadre methodologique) ».

Visualisations présentes :
non identifiable à partir du Rmd seul

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
non identifiable à partir du Rmd seul

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Supermarché / discount
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
non applicable ou non identifiable à partir du Rmd seul

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 6 — PARTIE 2.3 : Zones de chalandise (cadre methodologique)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 2.3 : Zones de chalandise (cadre methodologique) ».

Visualisations présentes :
non identifiable à partir du Rmd seul

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
non identifiable à partir du Rmd seul

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Proximité urbaine (supérette, Carrefour City, Monoprix, etc.)
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
non applicable ou non identifiable à partir du Rmd seul

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 7 — PARTIE 2.4 : Zones de chalandise (cadre methodologique)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 2.4 : Zones de chalandise (cadre methodologique) ».

Visualisations présentes :
non identifiable à partir du Rmd seul

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
non identifiable à partir du Rmd seul

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Drive
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
non applicable ou non identifiable à partir du Rmd seul

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 8 — PARTIE 2.1 : Zones de chalandise (cadre methodologique)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 2.1 : Zones de chalandise (cadre methodologique) ».

Visualisations présentes :
non identifiable à partir du Rmd seul

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
non identifiable à partir du Rmd seul

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Parametrage retenu dans ce notebook (Partie 6), References bibliographiques
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
non applicable ou non identifiable à partir du Rmd seul

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 9 — PARTIE 3.1: Cartographie du premier quartile et du revenu median

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 3.1: Cartographie du premier quartile et du revenu median ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points); graphique ggplot (carte points / diagnostics); tableau (DataTable)

Variables / données concernées :
Classe, Enseigne, Type_PV, min_val, plage_attendue

Échelle géographique :
IRIS, Commune, Points établissements (SIRENE), Zone cible Nord

Période :
non identifiable à partir du Rmd seul

Source :
iris_isere.xlsx

Données extraites :
- Sous-sections détectées: Carte du premier quartile du revenu, Carte du revenu median, Commentaire d’analyse, Sources/justification (placeholder)
- Blocs Row détectés: 3
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 2
  - Controle de coherence des variables socio-demo (zone cible) (8 lignes)
  - Bloc standardise sources/justification (1 lignes)

Tableau des données territoriales :
Extraction exhaustive IRIS (zone cible) pour les variables cartographiées :

| Code_IRIS | iris_name | com_name | 1er_quartile_euro | Mediane_euro |
| --- | --- | --- | --- | --- |
| 381260000 | Corenc (commune non irisée) | Corenc | 0 | 0 |
| 381850503 | Abbaye | Grenoble | 8292 | 17754 |
| 381850311 | Abry | Grenoble | 11478 | 18002 |
| 381850214 | Aigle | Grenoble | 16166 | 24208 |
| 381850412 | Alliés-Clos d'Or | Grenoble | 8572 | 15360 |
| 381850413 | Alpins | Grenoble | 13230 | 19052 |
| 381850604 | Arlequin | Grenoble | 2532 | 7264 |
| 381850410 | Bajatière Est | Grenoble | 15508 | 22218 |
| 381850409 | Bajatière Ouest | Grenoble | 14414 | 22758 |
| 381850607 | Baladins | Grenoble | 6516 | 13254 |
| 381850414 | Beauvert | Grenoble | 14412 | 20888 |
| 381850108 | Berriat-Ampère | Grenoble | 12484 | 20498 |
| 381850411 | Capuche | Grenoble | 13252 | 20090 |
| 381850215 | Championnet | Grenoble | 15492 | 25340 |
| 381850405 | Clemenceau | Grenoble | 17122 | 24304 |
| 381850302 | Clinique Mutualiste | Grenoble | 12086 | 18594 |
| 381850605 | Constantine-Géants | Grenoble | 5444 | 11036 |
| 381850106 | Cours Berriat | Grenoble | 10704 | 18146 |
| 381850204 | Créqui-Victor Hugo | Grenoble | 20728 | 32398 |
| 381850404 | Diables Bleus | Grenoble | 15702 | 25711 |
| 381850104 | Diderot | Grenoble | 16214 | 23882 |
| 381850301 | Drac-Ampère | Grenoble | 13812 | 21426 |
| 381850305 | Eaux Claires-Champs Élysées | Grenoble | 14690 | 21164 |
| 381850307 | Eaux Claires-Painlevé | Grenoble | 15010 | 21598 |
| 381850201 | Esplanade | Grenoble | 15100 | 22670 |
| 381850103 | Europole | Grenoble | 12058 | 21830 |
| 381850407 | Ferrié-Stalingrad | Grenoble | 9147 | 17156 |
| 381850402 | Foch Est | Grenoble | 15408 | 22846 |
| 381850401 | Foch Ouest | Grenoble | 13960 | 20840 |
| 381850107 | Gabriel Péri | Grenoble | 13984 | 22166 |
| 381850105 | Gare | Grenoble | 15386 | 25514 |
| 381850611 | Grand-Place Alpexpo | Grenoble | 0 | 0 |
| 381850205 | Grenette | Grenoble | 15522 | 26672 |
| 381850403 | Gustave Rivet | Grenoble | 14880 | 23350 |
| 381850211 | Génissieu | Grenoble | 17928 | 26708 |
| 381850606 | Helbronner | Grenoble | 10788 | 18576 |
| 381850216 | Hoche | Grenoble | 8348 | 14854 |
| 381850308 | Houille Blanche | Grenoble | 13756 | 21135 |
| 381850213 | Hébert-Mutualité | Grenoble | 17542 | 27120 |
| 381850304 | Jaurès-Vallier | Grenoble | 14816 | 21490 |
| 381850203 | Jean Jaurès | Grenoble | 20284 | 29820 |
| 381850102 | Jean Macé | Grenoble | 12068 | 20790 |
| 381850502 | Jeanne d'Arc | Grenoble | 13658 | 20832 |
| 381850504 | Jouhaux | Grenoble | 4238 | 10144 |
| 381850602 | La Bruyère | Grenoble | 14804 | 21945 |
| 381850603 | Les Trembles | Grenoble | 0 | 0 |
| 381850111 | Lustucru | Grenoble | 15290 | 23954 |
| 381850601 | Malherbe | Grenoble | 8638 | 16554 |
| 381850309 | Mistral | Grenoble | 3930 | 8412 |
| 381850207 | Notre-Dame | Grenoble | 7826 | 15708 |
| 381850506 | Paul Cocat | Grenoble | 6124 | 11664 |
| 381850408 | Peretto | Grenoble | 17216 | 24128 |
| 381850101 | Polygone | Grenoble | 0 | 0 |
| 381850505 | Poterne | Grenoble | 7764 | 14908 |
| 381850212 | Préfecture | Grenoble | 18592 | 28024 |
| 381850406 | Reyniès | Grenoble | 16998 | 23992 |
| 381850310 | Rondeau-Libération | Grenoble | 13430 | 21720 |
| 381850206 | Saint-André | Grenoble | 18228 | 29142 |
| 381850109 | Saint-Bruno | Grenoble | 12492 | 21688 |
| 381850202 | Saint-Laurent-Lavalette | Grenoble | 8316 | 16496 |
| 381850306 | Sidi-Brahim | Grenoble | 15372 | 23576 |
| 381850507 | Teisseire | Grenoble | 5306 | 10448 |
| 381850208 | Trois Cours | Grenoble | 18154 | 27144 |
| 381850303 | Vallier | Grenoble | 13784 | 19410 |
| 381850501 | Valmy | Grenoble | 8908 | 17332 |
| 381850608 | Vigny-Musset | Grenoble | 8458 | 16038 |
| 381850609 | Village Olympique Nord | Grenoble | 4240 | 9724 |
| 381850610 | Village Olympique Sud | Grenoble | 4754 | 8470 |
| 381850110 | Waldeck-Rousseau | Grenoble | 11954 | 20220 |
| 381850210 | Île Verte-Maréchal Randon | Grenoble | 17238 | 25614 |
| 381850209 | Île Verte-Saint-Roch | Grenoble | 15842 | 26040 |
| 385160101 | Maquis du Grésivaudan Nord | La Tronche | 0 | 0 |
| 385160102 | Maquis du Grésivaudan Sud | La Tronche | 0 | 0 |
| 382290201 | Ayguinards | Meylan | 18616 | 26566 |
| 382290101 | Béalières | Meylan | 16526 | 25072 |
| 382290102 | Grand Pré-Buclos-Eyminées | Meylan | 18372 | 25904 |
| 382290302 | Haut Meylan | Meylan | 27748 | 41280 |
| 382290301 | Maupertuis-Saint-Mury-Charlaix | Meylan | 27604 | 39064 |
| 382290203 | Plaine Fleurie | Meylan | 19328 | 27192 |
| 382290202 | Revirée | Meylan | 18477 | 26478 |
| 382290404 | Zone Spécifique l'Île d'Amour | Meylan | 0 | 0 |
| 382290401 | Zone d'Activités 1 | Meylan | 0 | 0 |
| 382290402 | Zone d'Activités 2 | Meylan | 0 | 0 |
| 382290403 | Zone d'Activités 3 | Meylan | 0 | 0 |
| 384230102 | Nord-Est | Saint-Martin-le-Vinoux | 0 | 0 |
| 384230101 | Sud-Ouest et Z.I. | Saint-Martin-le-Vinoux | 0 | 0 |
| 383820104 | Barnave-Saint-Robert | Saint-Égrève | 22258 | 29932 |
| 383820103 | Champaviotte | Saint-Égrève | 14712 | 21520 |
| 383820201 | Fiancey-Brieux | Saint-Égrève | 17888 | 27360 |
| 383820105 | La Monta-Visancourt | Saint-Égrève | 15940 | 23674 |
| 383820202 | Moutonnées | Saint-Égrève | 17696 | 24388 |
| 383820203 | Prédieu | Saint-Égrève | 16966 | 22480 |
| 383820102 | Rochepleine | Saint-Égrève | 13640 | 22088 |
| 383820101 | Zone Industrielle | Saint-Égrève | 0 | 0 |

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - `iris_data <- read_excel(file.path(raw_dir, "iris_isere.xlsx"))`
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - `filter(is.finite(q1) & is.finite(med))`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Controle de coherence des variables socio-demo (zone cible)**

| variable | na_pct | min_val | max_val | plage_attendue | flag_plage |
| --- | --- | --- | --- | --- | --- |
| 1er_quartile_euro | 0 | 0.00 | 27748.00 | &gt;= 0 | True |
| Mediane_euro | 0 | 0.00 | 41280.00 | &gt;= 0 | True |
| 3e_quartile_euro | 0 | 0.00 | 60700.00 | &gt;= 0 | True |
| Pct_actifs | 2.13 | 0.36 | 0.89 | [0, 1] | True |
| Pct_Menages_enfants | 2.13 | 0.09 | 0.57 | [0, 1] | True |
| Pct_Menages_voiture | 2.13 | 0.33 | 0.69 | [0, 1] | True |
| Pct_Cadres_sup | 2.13 | 0.01 | 0.32 | [0, 1] | True |
| Densite_pop | 0 | 0.00 | 6.46 | &gt;= 0 | True |

**Bloc standardise sources/justification**

| famille_cartes | variables | source_principale | utilite_ca_potentiel | statut |
| --- | --- | --- | --- | --- |
| Partie 3.1 - Revenus (Q1 et mediane) | 1er_quartile_euro, Mediane_euro | A completer (INSEE / CREDOC / source metier) | Qualifier le pouvoir d'achat local pour estimer le potentiel de chiffre d'affaires. | Placeholder de redaction (a finaliser) |
Section 10 — PARTIE 3.2: Cartographie du troisième quartile du revenu et du pourcentage d'actifs parmi les 15–64 ans

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 3.2: Cartographie du troisième quartile du revenu et du pourcentage d'actifs parmi les 15–64 ans ».

Visualisations présentes :
bloc texte généré; tableau (DataTable)

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
IRIS

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Carte du troisième quartile du revenu, Carte du pourcentage d'actifs parmi les 15–64 ans, Commentaire d’analyse, Sources/justification (placeholder)
- Blocs Row détectés: 3
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Bloc standardise sources/justification (1 lignes)

Tableau des données territoriales :
Extraction exhaustive IRIS (zone cible) pour les variables cartographiées :

| Code_IRIS | iris_name | com_name | 3e_quartile_euro | Pct_actifs |
| --- | --- | --- | --- | --- |
| 381260000 | Corenc (commune non irisée) | Corenc | 0 | 0.724719101123595 |
| 381850503 | Abbaye | Grenoble | 26348 | 0.671986791414419 |
| 381850311 | Abry | Grenoble | 25482 | 0.780530973451327 |
| 381850214 | Aigle | Grenoble | 33334 | 0.741390513320338 |
| 381850412 | Alliés-Clos d'Or | Grenoble | 23054 | 0.761252446183953 |
| 381850413 | Alpins | Grenoble | 24570 | 0.775188916876574 |
| 381850604 | Arlequin | Grenoble | 12618 | 0.631578947368421 |
| 381850410 | Bajatière Est | Grenoble | 31618 | 0.758161225849434 |
| 381850409 | Bajatière Ouest | Grenoble | 32890 | 0.776767676767677 |
| 381850607 | Baladins | Grenoble | 20978 | 0.650145772594752 |
| 381850414 | Beauvert | Grenoble | 27014 | 0.744412607449857 |
| 381850108 | Berriat-Ampère | Grenoble | 29116 | 0.78175576262874 |
| 381850411 | Capuche | Grenoble | 28728 | 0.79002079002079 |
| 381850215 | Championnet | Grenoble | 36112 | 0.720123626373626 |
| 381850405 | Clemenceau | Grenoble | 32616 | 0.713533834586466 |
| 381850302 | Clinique Mutualiste | Grenoble | 25525 | 0.726164874551971 |
| 381850605 | Constantine-Géants | Grenoble | 19838 | 0.652738565782044 |
| 381850106 | Cours Berriat | Grenoble | 27608 | 0.699290060851927 |
| 381850204 | Créqui-Victor Hugo | Grenoble | 47446 | 0.667278287461774 |
| 381850404 | Diables Bleus | Grenoble | 37948 | 0.755823293172691 |
| 381850104 | Diderot | Grenoble | 33280 | 0.744518103008669 |
| 381850301 | Drac-Ampère | Grenoble | 30076 | 0.753092293054234 |
| 381850305 | Eaux Claires-Champs Élysées | Grenoble | 29784 | 0.803908355795148 |
| 381850307 | Eaux Claires-Painlevé | Grenoble | 29184 | 0.732658959537572 |
| 381850201 | Esplanade | Grenoble | 32518 | 0.543918918918919 |
| 381850103 | Europole | Grenoble | 32696 | 0.634865134865135 |
| 381850407 | Ferrié-Stalingrad | Grenoble | 24998 | 0.714932126696833 |
| 381850402 | Foch Est | Grenoble | 31786 | 0.656765676567657 |
| 381850401 | Foch Ouest | Grenoble | 29754 | 0.684273709483794 |
| 381850107 | Gabriel Péri | Grenoble | 32116 | 0.655667144906743 |
| 381850105 | Gare | Grenoble | 37232 | 0.639261744966443 |
| 381850611 | Grand-Place Alpexpo | Grenoble | 0 |  |
| 381850205 | Grenette | Grenoble | 39696 | 0.604107244723331 |
| 381850403 | Gustave Rivet | Grenoble | 32288 | 0.620336943441637 |
| 381850211 | Génissieu | Grenoble | 36488 | 0.630394857667585 |
| 381850606 | Helbronner | Grenoble | 28858 | 0.704069050554871 |
| 381850216 | Hoche | Grenoble | 23776 | 0.708686440677966 |
| 381850308 | Houille Blanche | Grenoble | 30830 | 0.726840855106888 |
| 381850213 | Hébert-Mutualité | Grenoble | 38696 | 0.694877505567929 |
| 381850304 | Jaurès-Vallier | Grenoble | 29890 | 0.729993493819128 |
| 381850203 | Jean Jaurès | Grenoble | 42154 | 0.683458134785568 |
| 381850102 | Jean Macé | Grenoble | 30836 | 0.709367493995196 |
| 381850502 | Jeanne d'Arc | Grenoble | 29410 | 0.72595596755504 |
| 381850504 | Jouhaux | Grenoble | 18714 | 0.679611650485437 |
| 381850602 | La Bruyère | Grenoble | 30958 | 0.695337620578778 |
| 381850603 | Les Trembles | Grenoble | 0 | 0.671186440677966 |
| 381850111 | Lustucru | Grenoble | 33414 | 0.797033898305085 |
| 381850601 | Malherbe | Grenoble | 24510 | 0.710633946830266 |
| 381850309 | Mistral | Grenoble | 13346 | 0.627049180327869 |
| 381850207 | Notre-Dame | Grenoble | 30214 | 0.604213694507148 |
| 381850506 | Paul Cocat | Grenoble | 20032 | 0.673728813559322 |
| 381850408 | Peretto | Grenoble | 34060 | 0.75383631713555 |
| 381850101 | Polygone | Grenoble | 0 |  |
| 381850505 | Poterne | Grenoble | 22960 | 0.359285714285714 |
| 381850212 | Préfecture | Grenoble | 41950 | 0.673199152542373 |
| 381850406 | Reyniès | Grenoble | 33606 | 0.755233494363929 |
| 381850310 | Rondeau-Libération | Grenoble | 31070 | 0.705972434915773 |
| 381850206 | Saint-André | Grenoble | 42076 | 0.667666466706659 |
| 381850109 | Saint-Bruno | Grenoble | 31128 | 0.755596162631339 |
| 381850202 | Saint-Laurent-Lavalette | Grenoble | 27164 | 0.693765281173594 |
| 381850306 | Sidi-Brahim | Grenoble | 33107 | 0.741803278688525 |
| 381850507 | Teisseire | Grenoble | 18092 | 0.699574468085106 |
| 381850208 | Trois Cours | Grenoble | 37738 | 0.67748279252704 |
| 381850303 | Vallier | Grenoble | 26720 | 0.68010752688172 |
| 381850501 | Valmy | Grenoble | 25404 | 0.573529411764706 |
| 381850608 | Vigny-Musset | Grenoble | 23860 | 0.765822784810127 |
| 381850609 | Village Olympique Nord | Grenoble | 16194 | 0.659701492537313 |
| 381850610 | Village Olympique Sud | Grenoble | 15392 | 0.540999057492931 |
| 381850110 | Waldeck-Rousseau | Grenoble | 29406 | 0.804745762711864 |
| 381850210 | Île Verte-Maréchal Randon | Grenoble | 38360 | 0.690588235294118 |
| 381850209 | Île Verte-Saint-Roch | Grenoble | 36482 | 0.679324894514768 |
| 385160101 | Maquis du Grésivaudan Nord | La Tronche | 0 | 0.682090147586757 |
| 385160102 | Maquis du Grésivaudan Sud | La Tronche | 0 | 0.617112299465241 |
| 382290201 | Ayguinards | Meylan | 36392 | 0.785079928952043 |
| 382290101 | Béalières | Meylan | 36274 | 0.714285714285714 |
| 382290102 | Grand Pré-Buclos-Eyminées | Meylan | 35940 | 0.761387163561077 |
| 382290302 | Haut Meylan | Meylan | 60700 | 0.712915601023018 |
| 382290301 | Maupertuis-Saint-Mury-Charlaix | Meylan | 56154 | 0.736416184971098 |
| 382290203 | Plaine Fleurie | Meylan | 37622 | 0.689303904923599 |
| 382290202 | Revirée | Meylan | 36180 | 0.793181818181818 |
| 382290404 | Zone Spécifique l'Île d'Amour | Meylan | 0 | 0.885245901639344 |
| 382290401 | Zone d'Activités 1 | Meylan | 0 | 0.65 |
| 382290402 | Zone d'Activités 2 | Meylan | 0 | 0.851428571428571 |
| 382290403 | Zone d'Activités 3 | Meylan | 0 | 0.853448275862069 |
| 384230102 | Nord-Est | Saint-Martin-le-Vinoux | 0 | 0.758474576271186 |
| 384230101 | Sud-Ouest et Z.I. | Saint-Martin-le-Vinoux | 0 | 0.751748251748252 |
| 383820104 | Barnave-Saint-Robert | Saint-Égrève | 39868 | 0.788440567066521 |
| 383820103 | Champaviotte | Saint-Égrève | 28922 | 0.817398119122257 |
| 383820201 | Fiancey-Brieux | Saint-Égrève | 37942 | 0.621301775147929 |
| 383820105 | La Monta-Visancourt | Saint-Égrève | 32162 | 0.807339449541284 |
| 383820202 | Moutonnées | Saint-Égrève | 32098 | 0.743634767339772 |
| 383820203 | Prédieu | Saint-Égrève | 29288 | 0.824598930481283 |
| 383820102 | Rochepleine | Saint-Égrève | 32098 | 0.81061038220194 |
| 383820101 | Zone Industrielle | Saint-Égrève | 0 | 0.779220779220779 |

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - `filter(is.finite(q3) & is.finite(actifs))`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Bloc standardise sources/justification**

| famille_cartes | variables | source_principale | utilite_ca_potentiel | statut |
| --- | --- | --- | --- | --- |
| Partie 3.2 - Q3 revenu et actifs | 3e_quartile_euro, Pct_actifs | A completer (INSEE / CREDOC / source metier) | Identifier les IRIS combinant revenus eleves et intensite d'activite favorable a la frequentation. | Placeholder de redaction (a finaliser) |
Section 11 — PARTIE 3.3: Cartographie des parts de ménages avec enfants/ avec voiture

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 3.3: Cartographie des parts de ménages avec enfants/ avec voiture ».

Visualisations présentes :
bloc texte généré; tableau (DataTable)

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
IRIS

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Carte de la part de ménages avec enfants, Carte de la part de ménages avec voiture, Commentaire d’analyse, Sources/justification (placeholder)
- Blocs Row détectés: 3
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Bloc standardise sources/justification (1 lignes)

Tableau des données territoriales :
Extraction exhaustive IRIS (zone cible) pour les variables cartographiées :

| Code_IRIS | iris_name | com_name | Pct_Menages_enfants | Pct_Menages_voiture |
| --- | --- | --- | --- | --- |
| 381260000 | Corenc (commune non irisée) | Corenc | 0.367941712204007 | 0.397692774741955 |
| 381850503 | Abbaye | Grenoble | 0.231843575418994 | 0.474162011173184 |
| 381850311 | Abry | Grenoble | 0.321483771251932 | 0.547140649149923 |
| 381850214 | Aigle | Grenoble | 0.213101160862355 | 0.477611940298507 |
| 381850412 | Alliés-Clos d'Or | Grenoble | 0.286501377410468 | 0.548209366391185 |
| 381850413 | Alpins | Grenoble | 0.352835283528353 | 0.537353735373537 |
| 381850604 | Arlequin | Grenoble | 0.442857142857143 | 0.404285714285714 |
| 381850410 | Bajatière Est | Grenoble | 0.255230125523013 | 0.561506276150628 |
| 381850409 | Bajatière Ouest | Grenoble | 0.225865209471767 | 0.5986642380085 |
| 381850607 | Baladins | Grenoble | 0.373937677053824 | 0.529745042492918 |
| 381850414 | Beauvert | Grenoble | 0.335249042145594 | 0.636973180076628 |
| 381850108 | Berriat-Ampère | Grenoble | 0.206874602164227 | 0.519414385741566 |
| 381850411 | Capuche | Grenoble | 0.214814814814815 | 0.537448559670782 |
| 381850215 | Championnet | Grenoble | 0.185402684563758 | 0.501258389261745 |
| 381850405 | Clemenceau | Grenoble | 0.229357798165138 | 0.569724770642202 |
| 381850302 | Clinique Mutualiste | Grenoble | 0.211389128559103 | 0.532355478861087 |
| 381850605 | Constantine-Géants | Grenoble | 0.414718614718615 | 0.511688311688312 |
| 381850106 | Cours Berriat | Grenoble | 0.16956802063185 | 0.448742746615087 |
| 381850204 | Créqui-Victor Hugo | Grenoble | 0.231932773109244 | 0.535294117647059 |
| 381850404 | Diables Bleus | Grenoble | 0.188486536675952 | 0.545961002785515 |
| 381850104 | Diderot | Grenoble | 0.21685173089484 | 0.531678641410843 |
| 381850301 | Drac-Ampère | Grenoble | 0.290402476780186 | 0.556037151702786 |
| 381850305 | Eaux Claires-Champs Élysées | Grenoble | 0.247377622377622 | 0.561188811188811 |
| 381850307 | Eaux Claires-Painlevé | Grenoble | 0.287179487179487 | 0.567521367521368 |
| 381850201 | Esplanade | Grenoble | 0.122324159021407 | 0.590214067278288 |
| 381850103 | Europole | Grenoble | 0.193690851735016 | 0.387381703470032 |
| 381850407 | Ferrié-Stalingrad | Grenoble | 0.233082706766917 | 0.564745196324144 |
| 381850402 | Foch Est | Grenoble | 0.170144462279294 | 0.467094703049759 |
| 381850401 | Foch Ouest | Grenoble | 0.213960546282246 | 0.47040971168437 |
| 381850107 | Gabriel Péri | Grenoble | 0.11276473955352 | 0.416141957641671 |
| 381850105 | Gare | Grenoble | 0.136136136136136 | 0.404404404404404 |
| 381850611 | Grand-Place Alpexpo | Grenoble |  |  |
| 381850205 | Grenette | Grenoble | 0.147951441578149 | 0.411229135053111 |
| 381850403 | Gustave Rivet | Grenoble | 0.188271604938272 | 0.48070987654321 |
| 381850211 | Génissieu | Grenoble | 0.0933940774487472 | 0.388382687927107 |
| 381850606 | Helbronner | Grenoble | 0.390965732087227 | 0.626168224299065 |
| 381850216 | Hoche | Grenoble | 0.251269035532995 | 0.536802030456853 |
| 381850308 | Houille Blanche | Grenoble | 0.284274193548387 | 0.539314516129032 |
| 381850213 | Hébert-Mutualité | Grenoble | 0.240793201133144 | 0.564683663833806 |
| 381850304 | Jaurès-Vallier | Grenoble | 0.182936833470057 | 0.538146021328958 |
| 381850203 | Jean Jaurès | Grenoble | 0.231549815498155 | 0.47970479704797 |
| 381850102 | Jean Macé | Grenoble | 0.218567639257294 | 0.553315649867374 |
| 381850502 | Jeanne d'Arc | Grenoble | 0.222805701425356 | 0.506376594148537 |
| 381850504 | Jouhaux | Grenoble | 0.408289241622575 | 0.515873015873016 |
| 381850602 | La Bruyère | Grenoble | 0.330935251798561 | 0.517985611510791 |
| 381850603 | Les Trembles | Grenoble | 0.424083769633508 | 0.37347294938918 |
| 381850111 | Lustucru | Grenoble | 0.191773504273504 | 0.585470085470085 |
| 381850601 | Malherbe | Grenoble | 0.332963374028857 | 0.607103218645949 |
| 381850309 | Mistral | Grenoble | 0.523611111111111 | 0.405555555555556 |
| 381850207 | Notre-Dame | Grenoble | 0.201431492842536 | 0.405930470347648 |
| 381850506 | Paul Cocat | Grenoble | 0.482954545454545 | 0.517992424242424 |
| 381850408 | Peretto | Grenoble | 0.253012048192771 | 0.602409638554217 |
| 381850101 | Polygone | Grenoble |  |  |
| 381850505 | Poterne | Grenoble | 0.239884393063584 | 0.453757225433526 |
| 381850212 | Préfecture | Grenoble | 0.214732453092425 | 0.507296733842946 |
| 381850406 | Reyniès | Grenoble | 0.193285859613428 | 0.619532044760936 |
| 381850310 | Rondeau-Libération | Grenoble | 0.346729708431836 | 0.557131599684791 |
| 381850206 | Saint-André | Grenoble | 0.183456183456183 | 0.451269451269451 |
| 381850109 | Saint-Bruno | Grenoble | 0.164391043145822 | 0.542326597487712 |
| 381850202 | Saint-Laurent-Lavalette | Grenoble | 0.209795918367347 | 0.424489795918367 |
| 381850306 | Sidi-Brahim | Grenoble | 0.18804920913884 | 0.5896309314587 |
| 381850507 | Teisseire | Grenoble | 0.43184421534937 | 0.455899198167239 |
| 381850208 | Trois Cours | Grenoble | 0.167202572347267 | 0.615219721329046 |
| 381850303 | Vallier | Grenoble | 0.189640768588137 | 0.532163742690059 |
| 381850501 | Valmy | Grenoble | 0.116655428186109 | 0.487525286581254 |
| 381850608 | Vigny-Musset | Grenoble | 0.388038447846209 | 0.585973656105376 |
| 381850609 | Village Olympique Nord | Grenoble | 0.565217391304348 | 0.534971644612476 |
| 381850610 | Village Olympique Sud | Grenoble | 0.342342342342342 | 0.34954954954955 |
| 381850110 | Waldeck-Rousseau | Grenoble | 0.229131175468484 | 0.481260647359455 |
| 381850210 | Île Verte-Maréchal Randon | Grenoble | 0.154711673699015 | 0.566104078762307 |
| 381850209 | Île Verte-Saint-Roch | Grenoble | 0.174110522331567 | 0.568508705526117 |
| 385160101 | Maquis du Grésivaudan Nord | La Tronche | 0.318345323741007 | 0.489208633093525 |
| 385160102 | Maquis du Grésivaudan Sud | La Tronche | 0.266994266994267 | 0.594594594594595 |
| 382290201 | Ayguinards | Meylan | 0.301768990634755 | 0.605619146722164 |
| 382290101 | Béalières | Meylan | 0.295034079844206 | 0.57935735150925 |
| 382290102 | Grand Pré-Buclos-Eyminées | Meylan | 0.274309723889556 | 0.57563025210084 |
| 382290302 | Haut Meylan | Meylan | 0.359511343804538 | 0.349912739965096 |
| 382290301 | Maupertuis-Saint-Mury-Charlaix | Meylan | 0.343042071197411 | 0.413430420711974 |
| 382290203 | Plaine Fleurie | Meylan | 0.240272373540856 | 0.602140077821012 |
| 382290202 | Revirée | Meylan | 0.285885167464115 | 0.600478468899522 |
| 382290404 | Zone Spécifique l'Île d'Amour | Meylan | 0.0930232558139535 | 0.488372093023256 |
| 382290401 | Zone d'Activités 1 | Meylan | 0.125 | 0.6875 |
| 382290402 | Zone d'Activités 2 | Meylan | 0.445544554455446 | 0.326732673267327 |
| 382290403 | Zone d'Activités 3 | Meylan | 0.373134328358209 | 0.492537313432836 |
| 384230102 | Nord-Est | Saint-Martin-le-Vinoux | 0.342747111681643 | 0.462130937098845 |
| 384230101 | Sud-Ouest et Z.I. | Saint-Martin-le-Vinoux | 0.382177033492823 | 0.553827751196172 |
| 383820104 | Barnave-Saint-Robert | Saint-Égrève | 0.37037037037037 | 0.545338441890166 |
| 383820103 | Champaviotte | Saint-Égrève | 0.332986472424558 | 0.569198751300728 |
| 383820201 | Fiancey-Brieux | Saint-Égrève | 0.382380506091846 | 0.447985004686036 |
| 383820105 | La Monta-Visancourt | Saint-Égrève | 0.428057553956835 | 0.47410071942446 |
| 383820202 | Moutonnées | Saint-Égrève | 0.397260273972603 | 0.533001245330012 |
| 383820203 | Prédieu | Saint-Égrève | 0.270481144343303 | 0.551365409622887 |
| 383820102 | Rochepleine | Saint-Égrève | 0.422586520947177 | 0.510928961748634 |
| 383820101 | Zone Industrielle | Saint-Égrève | 0.34 | 0.46 |

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - `filter(is.finite(enfants) & is.finite(voiture))`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Bloc standardise sources/justification**

| famille_cartes | variables | source_principale | utilite_ca_potentiel | statut |
| --- | --- | --- | --- | --- |
| Partie 3.3 - Menages et mobilite | Pct_Menages_enfants, Pct_Menages_voiture | A completer (INSEE / CREDOC / source metier) | Differencier potentiel de courses de stock (familles motorisees) et achats d'appoint. | Placeholder de redaction (a finaliser) |
Section 12 — PARTIE 3.4: Cartographie de la part de cadres supérieurs et de la densité de la population

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 3.4: Cartographie de la part de cadres supérieurs et de la densité de la population ».

Visualisations présentes :
bloc texte généré; tableau (DataTable)

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
IRIS

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Carte de la part de cadres supérieurs, Carte de la densité de la population, Commentaire d’analyse, Sources/justification (placeholder)
- Blocs Row détectés: 3
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Bloc standardise sources/justification (1 lignes)

Tableau des données territoriales :
Extraction exhaustive IRIS (zone cible) pour les variables cartographiées :

| Code_IRIS | iris_name | com_name | Pct_Cadres_sup | Densite_pop |
| --- | --- | --- | --- | --- |
| 381260000 | Corenc (commune non irisée) | Corenc | 0.271305947271613 | 6.45557350565428 |
| 381850503 | Abbaye | Grenoble | 0.108705457825891 | 1.38308189655172 |
| 381850311 | Abry | Grenoble | 0.0786729857819905 | 1.38362068965517 |
| 381850214 | Aigle | Grenoble | 0.243989769820972 | 1.22737068965517 |
| 381850412 | Alliés-Clos d'Or | Grenoble | 0.121087314662273 | 0.795258620689655 |
| 381850413 | Alpins | Grenoble | 0.063641105894627 | 1.29364224137931 |
| 381850604 | Arlequin | Grenoble | 0.0305466237942122 | 0.933728448275862 |
| 381850410 | Bajatière Est | Grenoble | 0.18448098663926 | 1.22467672413793 |
| 381850409 | Bajatière Ouest | Grenoble | 0.152718858465098 | 1.65301724137931 |
| 381850607 | Baladins | Grenoble | 0.0654349499615089 | 0.87176724137931 |
| 381850414 | Beauvert | Grenoble | 0.083088954056696 | 1.2667025862069 |
| 381850108 | Berriat-Ampère | Grenoble | 0.212005108556833 | 1.46066810344828 |
| 381850411 | Capuche | Grenoble | 0.141436464088398 | 1.11099137931034 |
| 381850215 | Championnet | Grenoble | 0.287323130233901 | 2.15301724137931 |
| 381850405 | Clemenceau | Grenoble | 0.174698795180723 | 1.11961206896552 |
| 381850302 | Clinique Mutualiste | Grenoble | 0.108996539792388 | 1.06896551724138 |
| 381850605 | Constantine-Géants | Grenoble | 0.0508788159111933 | 1.54148706896552 |
| 381850106 | Cours Berriat | Grenoble | 0.209156193895871 | 1.34806034482759 |
| 381850204 | Créqui-Victor Hugo | Grenoble | 0.289320388349515 | 1.27693965517241 |
| 381850404 | Diables Bleus | Grenoble | 0.245294117647059 | 1.05226293103448 |
| 381850104 | Diderot | Grenoble | 0.244375827084252 | 1.45366379310345 |
| 381850301 | Drac-Ampère | Grenoble | 0.154059405940594 | 1.65463362068966 |
| 381850305 | Eaux Claires-Champs Élysées | Grenoble | 0.165742793791574 | 1.13362068965517 |
| 381850307 | Eaux Claires-Painlevé | Grenoble | 0.0936721474838067 | 1.28771551724138 |
| 381850201 | Esplanade | Grenoble | 0.185213414634146 | 0.758620689655172 |
| 381850103 | Europole | Grenoble | 0.237940630797774 | 1.38146551724138 |
| 381850407 | Ferrié-Stalingrad | Grenoble | 0.156852527357999 | 1.20096982758621 |
| 381850402 | Foch Est | Grenoble | 0.167016806722689 | 1.13577586206897 |
| 381850401 | Foch Ouest | Grenoble | 0.151277013752456 | 1.25431034482759 |
| 381850107 | Gabriel Péri | Grenoble | 0.221940928270042 | 1.39493534482759 |
| 381850105 | Gare | Grenoble | 0.213692946058091 | 0.861530172413793 |
| 381850611 | Grand-Place Alpexpo | Grenoble |  | 0 |
| 381850205 | Grenette | Grenoble | 0.223690899847483 | 1.17349137931034 |
| 381850403 | Gustave Rivet | Grenoble | 0.153313550939664 | 1.25754310344828 |
| 381850211 | Génissieu | Grenoble | 0.219341563786008 | 1.41002155172414 |
| 381850606 | Helbronner | Grenoble | 0.0858843537414966 | 0.799030172413793 |
| 381850216 | Hoche | Grenoble | 0.124501197126895 | 0.835129310344828 |
| 381850308 | Houille Blanche | Grenoble | 0.144713526284702 | 1.05064655172414 |
| 381850213 | Hébert-Mutualité | Grenoble | 0.206733298264072 | 1.1573275862069 |
| 381850304 | Jaurès-Vallier | Grenoble | 0.171151776103337 | 1.13631465517241 |
| 381850203 | Jean Jaurès | Grenoble | 0.303927148548662 | 1.11099137931034 |
| 381850102 | Jean Macé | Grenoble | 0.215400624349636 | 1.87284482758621 |
| 381850502 | Jeanne d'Arc | Grenoble | 0.158649398704903 | 1.33890086206897 |
| 381850504 | Jouhaux | Grenoble | 0.0755182625863771 | 1.41810344827586 |
| 381850602 | La Bruyère | Grenoble | 0.112903225806452 | 1.14655172413793 |
| 381850603 | Les Trembles | Grenoble | 0.0558712121212121 | 0.766163793103448 |
| 381850111 | Lustucru | Grenoble | 0.232833739979087 | 1.78502155172414 |
| 381850601 | Malherbe | Grenoble | 0.0529265255292653 | 1.10398706896552 |
| 381850309 | Mistral | Grenoble | 0.0122033898305085 | 1.08836206896552 |
| 381850207 | Notre-Dame | Grenoble | 0.184889434889435 | 0.989762931034483 |
| 381850506 | Paul Cocat | Grenoble | 0.0690872415532022 | 1.46390086206897 |
| 381850408 | Peretto | Grenoble | 0.217328170377541 | 1.31411637931034 |
| 381850101 | Polygone | Grenoble |  | 0 |
| 381850505 | Poterne | Grenoble | 0.03248670998228 | 1.03286637931034 |
| 381850212 | Préfecture | Grenoble | 0.238736842105263 | 1.46336206896552 |
| 381850406 | Reyniès | Grenoble | 0.209230769230769 | 0.982758620689655 |
| 381850310 | Rondeau-Libération | Grenoble | 0.161262798634812 | 1.56788793103448 |
| 381850206 | Saint-André | Grenoble | 0.317845828933474 | 1.171875 |
| 381850109 | Saint-Bruno | Grenoble | 0.288659793814433 | 1.59428879310345 |
| 381850202 | Saint-Laurent-Lavalette | Grenoble | 0.23068669527897 | 1.17672413793103 |
| 381850306 | Sidi-Brahim | Grenoble | 0.182686902754356 | 1.09213362068966 |
| 381850507 | Teisseire | Grenoble | 0.0384122919334187 | 1.14870689655172 |
| 381850208 | Trois Cours | Grenoble | 0.238334529791816 | 0.835668103448276 |
| 381850303 | Vallier | Grenoble | 0.116605934409162 | 1.18049568965517 |
| 381850501 | Valmy | Grenoble | 0.103926096997691 | 1.25269396551724 |
| 381850608 | Vigny-Musset | Grenoble | 0.13416149068323 | 3.4207974137931 |
| 381850609 | Village Olympique Nord | Grenoble | 0.0208333333333333 | 0.948275862068966 |
| 381850610 | Village Olympique Sud | Grenoble | 0.0115702479338843 | 0.789331896551724 |
| 381850110 | Waldeck-Rousseau | Grenoble | 0.245901639344262 | 1.10129310344828 |
| 381850210 | Île Verte-Maréchal Randon | Grenoble | 0.264425261244889 | 1.31411637931034 |
| 381850209 | Île Verte-Saint-Roch | Grenoble | 0.235617808904452 | 1.23383620689655 |
| 385160101 | Maquis du Grésivaudan Nord | La Tronche | 0.231336405529954 | 5.88062015503876 |
| 385160102 | Maquis du Grésivaudan Sud | La Tronche | 0.177310924369748 | 4.41860465116279 |
| 382290201 | Ayguinards | Meylan | 0.184146341463415 | 1.55830670926518 |
| 382290101 | Béalières | Meylan | 0.19023282226008 | 1.6908945686901 |
| 382290102 | Grand Pré-Buclos-Eyminées | Meylan | 0.169152419061477 | 2.59185303514377 |
| 382290302 | Haut Meylan | Meylan | 0.236176596656665 | 2.18051118210863 |
| 382290301 | Maupertuis-Saint-Mury-Charlaix | Meylan | 0.295822676896846 | 2.19408945686901 |
| 382290203 | Plaine Fleurie | Meylan | 0.127748691099476 | 1.68769968051118 |
| 382290202 | Revirée | Meylan | 0.164502164502165 | 1.31150159744409 |
| 382290404 | Zone Spécifique l'Île d'Amour | Meylan | 0.0533333333333333 | 0.0654952076677316 |
| 382290401 | Zone d'Activités 1 | Meylan | 0.0612244897959184 | 0.0391373801916933 |
| 382290402 | Zone d'Activités 2 | Meylan | 0.0878048780487805 | 0.20926517571885 |
| 382290403 | Zone d'Activités 3 | Meylan | 0.177777777777778 | 0.137380191693291 |
| 384230102 | Nord-Est | Saint-Martin-le-Vinoux | 0.205738705738706 | 1.93447580645161 |
| 384230101 | Sud-Ouest et Z.I. | Saint-Martin-le-Vinoux | 0.0805194805194805 | 3.8679435483871 |
| 383820104 | Barnave-Saint-Robert | Saint-Égrève | 0.187321937321937 | 1.5939226519337 |
| 383820103 | Champaviotte | Saint-Égrève | 0.100307692307692 | 1.87016574585635 |
| 383820201 | Fiancey-Brieux | Saint-Égrève | 0.159682399647111 | 2.55340699815838 |
| 383820105 | La Monta-Visancourt | Saint-Égrève | 0.17630408410837 | 2.96316758747698 |
| 383820202 | Moutonnées | Saint-Égrève | 0.123314065510597 | 1.77255985267035 |
| 383820203 | Prédieu | Saint-Égrève | 0.121989121989122 | 1.40055248618785 |
| 383820102 | Rochepleine | Saint-Égrève | 0.177560975609756 | 2.3720073664825 |
| 383820101 | Zone Industrielle | Saint-Égrève | 0.0283018867924528 | 0.114180478821363 |

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - `filter(is.finite(cadres) & is.finite(densite))`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Bloc standardise sources/justification**

| famille_cartes | variables | source_principale | utilite_ca_potentiel | statut |
| --- | --- | --- | --- | --- |
| Partie 3.4 - Cadres et densite | Pct_Cadres_sup, Densite_pop | A completer (INSEE / CREDOC / source metier) | Arbitrer entre potentiel de panier premium et masse critique de clientele locale. | Placeholder de redaction (a finaliser) |
Section 13 — PARTIE 4: Analyse de la concurrence

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 4: Analyse de la concurrence ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable)

Variables / données concernées :
Enseigne, Nb_pdv_2020, Nb_pdv_2025, commune_norm

Échelle géographique :
IRIS, Commune, Points établissements (SIRENE), Zone cible Nord

Période :
2020, 2025

Source :
otp-1.5.0-shaded.jar

Données extraites :
- Sous-sections détectées: Cartographie des Points de vente en 2020, Cartographie des Points de vente en 2025, Comptage IRIS super/hyper/supérette 2020, Comptage IRIS super/hyper/supérette 2025, Commentaire d’analyse, Transitions d’enseigne 2020 → 2025 (SIRET commun), Focus transitions impliquant Casino (signalement)
- Blocs Row détectés: 4
- Titres cartographiques détectés (tm_layout): Comptage par IRIS - 2020, Comptage par IRIS - 2025
- Tableaux DataTables détectés: 2
  - Transitions enseigne (1 lignes)
  - Focus Casino (1 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - `sirene_shp_avant <- st_read(`
- Jointures:
  - `joined_2020 <- st_join(`
  - `joined_2025 <- st_join(`
  - `left_join(compte_avant, by = "Code_IRIS") %>%`
  - `left_join(compte_2025, by = "Code_IRIS") %>%`
  - `transitions_enseigne <- full_join(`
- Filtres:
  - `filter(commune_norm %in% communes_zone_cible_norm & etatadminis == "Actif")`
  - `filter(code_naf %in% target_naf_codes, !is.na(Type_PV))`
  - `filter(`
  - `filter(`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Transitions enseigne**

| Info |
| --- |
| Aucune transition détectée sur SIRET communs. |

**Focus Casino**

| Info |
| --- |
| Aucun changement impliquant Casino détecté sur SIRET communs. |
Section 14 — PARTIE 4.1: Comparaison super/hyper vs drives (2025)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 4.1: Comparaison super/hyper vs drives (2025) ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points)

Variables / données concernées :
Code_IRIS, Enseigne, Nb_drives_2025, Nb_drives_2025_map, Type_PV, drives_types, groupe_reseau_nom, match_method, siret, siret_chr

Échelle géographique :
IRIS, Points établissements (SIRENE)

Période :
2025

Source :
crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv

Données extraites :
- Sous-sections détectées: Carte comparative: tous les super/hyper vs points avec drive, Comptage IRIS drives 2025 (accoles + deportes), Commentaire d’analyse
- Blocs Row détectés: 2
- Titres cartographiques détectés (tm_layout): Super/hyper (2025) vs points avec drive, Comptage drives par IRIS - 2025
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - `crossref_drives <- readr::read_csv(crossref_path, show_col_types = FALSE) %>%`
- Jointures:
  - `inner_join(crossref_drives, by = "siret_chr") %>%`
  - `drives_join <- st_join(`
  - `left_join(compte_drives_2025, by = "Code_IRIS") %>%`
- Filtres:
  - `filter(as.character(Type_PV) %in% c("super", "hyper")) %>%`
  - `filter(drive_trouve %in% TRUE) %>%`
  - `filter(!is.na(Code_IRIS) & str_squish(Code_IRIS) != "") %>%`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 15 — PARTIE 5: Zones de chalandises en 2025

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 5: Zones de chalandises en 2025 ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points)

Variables / données concernées :
Classe, Enseigne, Pct_Menages_voiture, Pct_actifs, Type_PV, minutes

Échelle géographique :
IRIS, Points établissements (SIRENE)

Période :
2025

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Zones de chalandise (à pied), Zones de chalandise (en voiture), Commentaire d’analyse
- Blocs Row détectés: 2
- Titres cartographiques détectés (tm_layout): Zones de chalandise (à pied), Zones de chalandise (en voiture)
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - `dplyr::left_join(ref_enseigne, by = "siret") %>%`
  - `dplyr::left_join(ref_enseigne, by = "siret") %>%`
- Filtres:
  - `iso600 <- iso %>% filter(time == 600)`
  - `filter(`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 16 — PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) ».

Visualisations présentes :
carte tmap (choroplèthe et/ou points); tableau (DataTable)

Variables / données concernées :
Enseigne, Type_PV, couverture_pct_2025, groupe_reseau_nom, siret

Échelle géographique :
IRIS, Points établissements (SIRENE)

Période :
2020, 2025

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Tableau de recouvrement intra-enseigne, Carte synthèse du recouvrement intra-enseigne
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): Recouvrement intra-enseigne: aucun recouvrement detecte, Synthese recouvrement intra-enseigne (2025)
- Tableaux DataTables détectés: 1
  - Recouvrement intra-enseigne 2025 (isochrones primaires voiture) (4 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - `left_join(`
  - `left_join(cov_2025 %>% dplyr::select(dplyr::all_of(c(iris_id_col, "couverture_pct_2025"))), by = iris_id_col) %>%`
  - `left_join(cov_2020 %>% dplyr::select(dplyr::all_of(c(iris_id_col, "couverture_pct_2020"))), by = iris_id_col) %>%`
- Filtres:
  - `sub <- iso_overlap_input %>% filter(groupe_reseau_nom == grp)`
  - `filter(`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Recouvrement intra-enseigne 2025 (isochrones primaires voiture)**

| groupe_reseau_nom | nb_recouvrements | aire_totale_km2 |
| --- | --- | --- |
| Groupe Carrefour | 48 | 56.0789 |
| Aldi | 5 | 3.0385 |
| Independant/Autre | 3 | 1.9837 |
| Groupe Auchan/Casino | 1 | 0.0511 |
Section 17 — PARTIE 6: Accessibilité

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 6: Accessibilité ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points)

Variables / données concernées :
Classe, Enseigne, Nb_pdv_access_primaire_voiture, Nb_supermarches, Type_PV

Échelle géographique :
IRIS, Points établissements (SIRENE)

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Accessibilité IRIS, Accessibilité des supermarchés (à pied), Commentaire d’analyse
- Blocs Row détectés: 2
- Titres cartographiques détectés (tm_layout): Accessibilite IRIS (hyper>super>superette), Accessibilité des supermarchés (à pied)
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 18 — PARTIE 6.1: Couverture des IRIS

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 6.1: Couverture des IRIS ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points)

Variables / données concernées :
Classe, Enseigne, Type_PV, couverture_pct_2025

Échelle géographique :
IRIS, Points établissements (SIRENE)

Période :
2020, 2025

Source :
nb_pdv_iris_iso_primaire_hierarchie_2025.csv

Données extraites :
- Sous-sections détectées: Couverture des IRIS, Commentaire d’analyse
- Blocs Row détectés: 2
- Titres cartographiques détectés (tm_layout): Couverture des IRIS (zone primaire hierarchisee hyper>super>superette)
- Tableaux DataTables détectés: aucun tableau explicitement rattaché dans le HTML.

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - `left_join(cov_2025 %>% dplyr::select(dplyr::all_of(c(iris_id_col, "couverture_pct_2025"))), by = iris_id_col)`
  - `left_join(cov_2020 %>% dplyr::select(dplyr::all_of(c(iris_id_col, "couverture_pct_2020"))), by = iris_id_col)`
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.
Section 19 — PARTIE 6.2: Comparaison 2020 vs 2025 (accessibilite et couverture)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 6.2: Comparaison 2020 vs 2025 (accessibilite et couverture) ».

Visualisations présentes :
tableau (DataTable)

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
IRIS

Période :
2020, 2025

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Tableau comparatif 2020 vs 2025
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Comparaison verifiable 2020 vs 2025 (2 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Comparaison verifiable 2020 vs 2025**

| indicateur | moyenne_2020 | mediane_2020 | moyenne_2025 | mediane_2025 | delta_moyenne |
| --- | --- | --- | --- | --- | --- |
| Nb_pdv_access_primaire_voiture | 5.5 | 5 | 5.745 | 5 | 0.245 |
| couverture_pct | 94.416 | 100 | 94.341 | 100 | -0.075 |
Section 20 — PARTIE 7: Autocorrelation spatiale

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 7: Autocorrelation spatiale ».

Visualisations présentes :
graphique ggplot (carte points / diagnostics); indicateurs/tests spatiaux; tableau (DataTable)

Variables / données concernées :
HHI, entropie_relative

Échelle géographique :
IRIS, Points établissements (SIRENE), Zone cible Nord

Période :
2020, 2025

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Voisins spatiaux, Commentaire d’analyse, Indices de concentration spatiale
- Blocs Row détectés: 2
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Concentration concurrentielle par groupes (HHI/entropie, 2020 vs 2025) (2 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Concentration concurrentielle par groupes (HHI/entropie, 2020 vs 2025)**

| annee | HHI | entropie_relative_groupes | nb_groupes |
| --- | --- | --- | --- |
| 2020 | 0.25 | 0.8308 | 7 |
| 2025 | 0.2505 | 0.8484 | 7 |
Section 21 — PARTIE 8: Carte des résidus OLS et Tests

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 8: Carte des résidus OLS et Tests ».

Visualisations présentes :
graphique ggplot (carte points / diagnostics); indicateurs/tests spatiaux; tableau (DataTable)

Variables / données concernées :
AIC, moran_I

Échelle géographique :
IRIS

Période :
non identifiable à partir du Rmd seul

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Test de Moran, Commentaire d’analyse, Specification et estimation SAR/SEM
- Blocs Row détectés: 2
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Comparaison OLS/SAR/SEM (AIC et parametre spatial) (3 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
non applicable

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Comparaison OLS/SAR/SEM (AIC et parametre spatial)**

| modele | AIC | param_spatial | note |
| --- | --- | --- | --- |
| OLS | 460.38 | NA |  |
| SAR | 358.02 | 0.8867 |  |
| SEM | 361.77 | 0.9206 |  |
Section 22 — PARTIE 9: Top 4 zones preferables pour une nouvelle implantation

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 9: Top 4 zones preferables pour une nouvelle implantation ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable)

Variables / données concernées :
Nb_pdv_access_primaire_voiture_2025, abs_delta, grey85, s_q1, score_total_A, zone_top4

Échelle géographique :
IRIS, Commune

Période :
2025

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Classement A (tache 1.2 uniquement) {data-width=33}, Classement B (taches 1.2 + 1.3) {data-width=33}, Delta de rang A vs B {data-width=34}, Carte des 4 zones prioritaires (classement B), Commentaire d’analyse
- Blocs Row détectés: 3
- Titres cartographiques détectés (tm_layout): Top 4 des zones d'implantation preferables (classement B)
- Tableaux DataTables détectés: 3
  - Top 4 - Classement A (4 lignes)
  - Top 4 - Classement B (4 lignes)
  - IRIS les plus impactes par l'ajout des indicateurs concurrence/accessibilite (10 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - `left_join(top4_join, by = "iris_name") %>%`
- Filtres:
  - `tm_shape(iris_top4_map %>% filter(zone_top4 == "Autres zones")) +`
  - `tm_shape(iris_top4_map %>% filter(zone_top4 != "Autres zones")) +`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Top 4 - Classement A**

| rank_A | iris_name | com_name | score_total_A_100 |
| --- | --- | --- | --- |
| 1 | Maupertuis-Saint-Mury-Charlaix | Meylan | 75.2 |
| 2 | Haut Meylan | Meylan | 73.1 |
| 3 | Barnave-Saint-Robert | Saint-Égrève | 66.5 |
| 4 | Créqui-Victor Hugo | Grenoble | 64.1 |

**Top 4 - Classement B**

| rank_B | iris_name | com_name | score_total_B_100 | score_total_A_100 | Nb_pdv_access_primaire_voiture_2025 | couverture_pct_2025 | Nb_pdv_2025 | Nb_drives_2025 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | Haut Meylan | Meylan | 72.9 | 73.1 | 2 | 66.501168 | 1 | 0 |
| 2 | Maupertuis-Saint-Mury-Charlaix | Meylan | 71.3 | 75.2 | 2 | 100 | 0 | 0 |
| 3 | Fiancey-Brieux | Saint-Égrève | 68.4 | 57.4 | 3 | 37.658322 | 0 | 0 |
| 4 | Barnave-Saint-Robert | Saint-Égrève | 64.9 | 66.5 | 3 | 100 | 0 | 0 |

**IRIS les plus impactes par l'ajout des indicateurs concurrence/accessibilite**

| iris_name | com_name | rank_A | rank_B | delta_rang_A_B | score_total_A_100 | score_total_B_100 |
| --- | --- | --- | --- | --- | --- | --- |
| Préfecture | Grenoble | 15 | 88 | -73 | 58 | 39 |
| Arlequin | Grenoble | 85 | 25 | 60 | 29.1 | 53.7 |
| Maquis du Grésivaudan Nord | La Tronche | 76 | 22 | 54 | 38 | 55.3 |
| Jean Jaurès | Grenoble | 9 | 60 | -51 | 60.6 | 47 |
| Nord-Est | Saint-Martin-le-Vinoux | 82 | 32 | 50 | 33.5 | 53.2 |
| Corenc (commune non irisée) | Corenc | 71 | 23 | 48 | 39.6 | 54.8 |
| Championnet | Grenoble | 16 | 56 | -40 | 58 | 48.4 |
| Helbronner | Grenoble | 48 | 11 | 37 | 49.1 | 60.7 |
| Aigle | Grenoble | 32 | 63 | -31 | 54.1 | 45 |
| Saint-Laurent-Lavalette | Grenoble | 67 | 37 | 30 | 42.6 | 52.2 |
Section 23 — PARTIE 10: CA potentiel (Pappers + projection Top 1)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 10: CA potentiel (Pappers + projection Top 1) ».

Visualisations présentes :
bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable)

Variables / données concernées :
CA observe / estime, Code_IRIS, Enseigne, Type_PV, breaks, ca_class, ca_value, capital_social_num, drive_trouve, grey95, hhi_enseignes_local, include.lowest, iris_name, labels, log_densite, nb_enseignes_access, ols_resid_access, pred_ca_2024, recouv_intra_enseigne_local, right, shape_cat, siret14

Échelle géographique :
IRIS, Points établissements (SIRENE)

Période :
2021, 2024, 2025

Source :
CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv

Données extraites :
- Sous-sections détectées: Carte CA 2021 {data-width=50}, Carte CA 2024 {data-width=50}, Prediction CA - implantation Top 1 {data-width=33}, Selection des variables du modele {data-width=34}, Structure attendue (placeholder conserve) {data-width=33}, Statut et analyse (placeholder), Sources / citations (placeholder)
- Blocs Row détectés: 3
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 4
  - Prediction du CA 2024 pour notre supermarche (IRIS Top 1) (1 lignes)
  - Screening univarie des predicteurs (adj R2) et variables retenues (24 lignes)
  - Template CA potentiel (avec pre-remplissage Top 1 si disponible) (1 lignes)
  - Sources mobilisees pour la section CA (etat courant) (4 lignes)

Tableau des données territoriales :
Niveau IRIS détecté, mais les valeurs exactes par IRIS pour cette section ne sont pas directement lisibles dans le HTML; voir annexe 3.1 et les tableaux dédiés.

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - `pappers_ca <- suppressMessages(readr::read_delim(`
  - `drive_ca <- readr::read_csv(crossref_ca_path, show_col_types = FALSE) %>%`
- Jointures:
  - `left_join(pappers_ca, by = "siret14", suffix = c("", "_pappers")) %>%`
  - `left_join(drive_ca, by = "siret14") %>%`
  - `left_join(iris_res_tbl, by = "Code_IRIS")`
  - `left_join(overlap_local, by = "iris_name")`
  - `left_join(iris_features_ca %>% select(-iris_name), by = "Code_IRIS")`
- Filtres:
  - `filter(as.character(Type_PV) %in% c("super", "hyper")) %>%`
  - `filter(!is.na(siret14)) %>%`
  - `filter(!is.na(siret14)) %>%`
  - `filter(rang == 1) %>%`
  - `top1_geom <- iris_zone_etude %>% filter(iris_name == top1_iris_name)`
  - `pts_super <- pts %>% filter(shape_cat == "Super")`
  - `pts_hyper <- pts %>% filter(shape_cat == "Hyper")`
  - `pts_implant <- pts %>% filter(shape_cat == "Notre implantation")`
  - `filter(!is.na(ca_2021), !is.na(capital_social_num), capital_social_num < 1000000)`
  - `filter(!is.na(ca_2024), !is.na(capital_social_num), capital_social_num < 1000000)`
  - `filter(is.finite(ca_2024), !is.na(ca_2024))`
  - `filter(complete.cases(.))`
  - `filter(complete.cases(.))`
  - `filter(complete.cases(.))`
  - `filter(Code_IRIS == top1_code_iris) %>%`

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Prediction du CA 2024 pour notre supermarche (IRIS Top 1)**

| iris_cible | code_iris | type_implantation | pred_ca_2024 | classe_carte_2024 | methode | statut |
| --- | --- | --- | --- | --- | --- | --- |
| Haut Meylan | 382290302 | super | 136420 | -8,930 - 1,304,000 | LM log(CA_2024) entraine sur Pappers | Estime |

**Screening univarie des predicteurs (adj R2) et variables retenues**

| variable | adj_r2_univ | selected |
| --- | --- | --- |
| couverture_pct_2025 | 0.5147 | True |
| couverture_inverse | 0.5147 | True |
| Densite_pop | 0.4366 | True |
| log_densite | 0.3471 | True |
| densite_x_actifs | 0.2603 | True |
| Type_PV | 0.0398 | True |
| Pct_Cadres_sup | 0.1922 | False |
| Pct_Menages_voiture | 0.1247 | False |
| concurrence_inverse | 0.0589 | False |
| hhi_enseignes_local | 0.0573 | False |
| ols_resid_access | 0.039 | False |
| super_ratio_local | 0.0268 | False |
| access_x_cover | 0.0225 | False |
| nb_enseignes_access | 0.0188 | False |
| Nb_pdv_access_primaire_voiture_2025 | -0.0024 | False |
| recouv_intra_enseigne_local | -0.0193 | False |
| drive_trouve | -0.0275 | False |
| Nb_drives_2025 | -0.0307 | False |
| Pct_actifs | -0.0331 | False |
| Pct_Menages_enfants | -0.0595 | False |
| revenu_x_densite | -0.0614 | False |
| Mediane_euro | -0.0619 | False |
| Nb_supermarches | -0.0625 | False |
| Nb_pdv_2025 | -0.0625 | False |

**Template CA potentiel (avec pre-remplissage Top 1 si disponible)**

| Code_IRIS | iris_name | population_cible | depense_alimentaire_par_menage | taux_capture_estime | ca_potentiel_estime | source_donnees | statut |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 382290302 | Haut Meylan | NA | NA | NA | 136420.292951 | Pappers + LM | Estimation provisoire |

**Sources mobilisees pour la section CA (etat courant)**

| source | usage | statut |
| --- | --- | --- |
| Pappers (CA par SIRET) | Variable cible d'entrainement (CA_2021/CA_2024) | Integre |
| SIRENE geolocalise (dataseed-sirene-1) | Geolocalisation super/hyper + typologie | Integre |
| IRIS INSEE + variables socio-demo | Predicteurs de la demande locale | Integre |
| Hypotheses CA potentiel (a completer) | Parametrage final capture et conversion en CA potentiel | A finaliser |
Section 24 — PARTIE 11: Analyse sectorielle redigee (a completer)

Objectif apparent :
Cette section présente les éléments affichés/calculés relatifs à « PARTIE 11: Analyse sectorielle redigee (a completer) ».

Visualisations présentes :
tableau (DataTable)

Variables / données concernées :
non identifiable à partir du Rmd seul

Échelle géographique :
non identifiable à partir du Rmd seul

Période :
2020, 2025

Source :
non identifiable à partir du Rmd seul

Données extraites :
- Sous-sections détectées: Trame de redaction
- Blocs Row détectés: 1
- Titres cartographiques détectés (tm_layout): non identifiable à partir du Rmd seul
- Tableaux DataTables détectés: 1
  - Plan de la partie sectorielle a finaliser (4 lignes)

Tableau des données territoriales :
non applicable ou non identifiable à partir du Rmd seul

Éléments de concurrence :
- Informations de concurrence présentes via cartes/tableaux; détail chiffré selon tableaux affichés ci-dessous.

Éléments techniques repérés dans le code R :
- Jeux de données / lectures:
  - non identifiable à partir du Rmd seul
- Jointures:
  - non identifiable à partir du Rmd seul
- Filtres:
  - non identifiable à partir du Rmd seul

Informations directement réutilisables pour une future analyse rédigée :
- Liste des variables cartographiées/agrégées dans la section.
- Valeurs tabulaires affichées dans les DataTables associées.
- Périmètre géographique et période détectés dans le code.

Tableaux extraits (valeurs affichées) :

**Plan de la partie sectorielle a finaliser**

| section | contenu_attendu | statut |
| --- | --- | --- |
| Contexte concurrentiel local | Structure de marche, formats, groupes dominants | A rediger |
| Dynamique des enseignes (2020-2025) | Transitions enseigne, reconfigurations, drives | A rediger |
| Synthese des risques/éléments par zone | Lecture croisee demande/concurrence/accessibilite | A rediger |
| Argumentaire final de localisation | Justification finale du choix Top 1 | A rediger |

3. Annexes

3.1 Tableau récapitulatif de toutes les variables repérées

| Section | Nom de variable | Libellé | Description factuelle | Niveau géographique | Unité | Source | Présente dans quelle visualisation |
| --- | --- | --- | --- | --- | --- | --- | --- |
| PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord) | commune_norm | commune_norm | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est) | non identifiable à partir du Rmd seul | crossref drives absent: tentative de fallback avec drives_grenoble.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv, drives_grenoble.csv, economicref-france-sirene-v3.csv | tableau (DataTable) |
| PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord) | cp_norm | cp_norm | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est) | non identifiable à partir du Rmd seul | crossref drives absent: tentative de fallback avec drives_grenoble.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv, drives_grenoble.csv, economicref-france-sirene-v3.csv | tableau (DataTable) |
| PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord) | denominatio | denominatio | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est) | non identifiable à partir du Rmd seul | crossref drives absent: tentative de fallback avec drives_grenoble.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv, drives_grenoble.csv, economicref-france-sirene-v3.csv | tableau (DataTable) |
| PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord) | drive_trouve | drive_trouve | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est) | non identifiable à partir du Rmd seul | crossref drives absent: tentative de fallback avec drives_grenoble.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv, drives_grenoble.csv, economicref-france-sirene-v3.csv | tableau (DataTable) |
| PARTIE 1.1: Vue d'ensemble de la zone d’étude (agglo complete + zone cible Nord) | siret | siret | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est) | non identifiable à partir du Rmd seul | crossref drives absent: tentative de fallback avec drives_grenoble.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv, drives_grenoble.csv, economicref-france-sirene-v3.csv | tableau (DataTable) |
| PARTIE 1.2: Dictionnaire des variables | description | description | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | GROUPE07_DicoDonnées_rapport1.csv | bloc texte généré; tableau (DataTable) |
| PARTIE 3.1: Cartographie du premier quartile et du revenu median | Classe | Classe | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | iris_isere.xlsx | bloc texte généré; carte tmap (choroplèthe et/ou points); graphique ggplot (carte points / diagnostics); tableau (DataTable) |
| PARTIE 3.1: Cartographie du premier quartile et du revenu median | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | iris_isere.xlsx | bloc texte généré; carte tmap (choroplèthe et/ou points); graphique ggplot (carte points / diagnostics); tableau (DataTable) |
| PARTIE 3.1: Cartographie du premier quartile et du revenu median | Type_PV | Type_PV | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | iris_isere.xlsx | bloc texte généré; carte tmap (choroplèthe et/ou points); graphique ggplot (carte points / diagnostics); tableau (DataTable) |
| PARTIE 3.1: Cartographie du premier quartile et du revenu median | min_val | min_val | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | iris_isere.xlsx | bloc texte généré; carte tmap (choroplèthe et/ou points); graphique ggplot (carte points / diagnostics); tableau (DataTable) |
| PARTIE 3.1: Cartographie du premier quartile et du revenu median | plage_attendue | plage_attendue | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | iris_isere.xlsx | bloc texte généré; carte tmap (choroplèthe et/ou points); graphique ggplot (carte points / diagnostics); tableau (DataTable) |
| PARTIE 4: Analyse de la concurrence | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | otp-1.5.0-shaded.jar | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 4: Analyse de la concurrence | Nb_pdv_2020 | Nb_pdv_2020 | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | compte | otp-1.5.0-shaded.jar | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 4: Analyse de la concurrence | Nb_pdv_2025 | Nb_pdv_2025 | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | compte | otp-1.5.0-shaded.jar | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 4: Analyse de la concurrence | commune_norm | commune_norm | Variable repérée dans code/visualisation de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | otp-1.5.0-shaded.jar | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | Code_IRIS | Code_IRIS | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | Nb_drives_2025 | Nb_drives_2025 | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | compte | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | Nb_drives_2025_map | Nb_drives_2025_map | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | compte | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | Type_PV | Type_PV | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | drives_types | drives_types | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | groupe_reseau_nom | groupe_reseau_nom | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | match_method | match_method | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | siret | siret | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | siret_chr | siret_chr | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 5: Zones de chalandises en 2025 | Classe | Classe | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 5: Zones de chalandises en 2025 | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 5: Zones de chalandises en 2025 | Pct_Menages_voiture | Pct_Menages_voiture | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | proportion | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 5: Zones de chalandises en 2025 | Pct_actifs | Pct_actifs | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | proportion | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 5: Zones de chalandises en 2025 | Type_PV | Type_PV | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 5: Zones de chalandises en 2025 | minutes | minutes | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) | Type_PV | Type_PV | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) | couverture_pct_2025 | couverture_pct_2025 | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) | groupe_reseau_nom | groupe_reseau_nom | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) | siret | siret | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 6: Accessibilité | Classe | Classe | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6: Accessibilité | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6: Accessibilité | Nb_pdv_access_primaire_voiture | Nb_pdv_access_primaire_voiture | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | compte | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6: Accessibilité | Nb_supermarches | Nb_supermarches | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | compte | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6: Accessibilité | Type_PV | Type_PV | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6.1: Couverture des IRIS | Classe | Classe | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | nb_pdv_iris_iso_primaire_hierarchie_2025.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6.1: Couverture des IRIS | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | nb_pdv_iris_iso_primaire_hierarchie_2025.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6.1: Couverture des IRIS | Type_PV | Type_PV | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | nb_pdv_iris_iso_primaire_hierarchie_2025.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 6.1: Couverture des IRIS | couverture_pct_2025 | couverture_pct_2025 | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | nb_pdv_iris_iso_primaire_hierarchie_2025.csv | bloc texte généré; carte tmap (choroplèthe et/ou points) |
| PARTIE 7: Autocorrelation spatiale | HHI | HHI | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | graphique ggplot (carte points / diagnostics); indicateurs/tests spatiaux; tableau (DataTable) |
| PARTIE 7: Autocorrelation spatiale | entropie_relative | entropie_relative | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE), Zone cible Nord | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | graphique ggplot (carte points / diagnostics); indicateurs/tests spatiaux; tableau (DataTable) |
| PARTIE 8: Carte des résidus OLS et Tests | AIC | AIC | Variable repérée dans code/visualisation de la section | IRIS | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | graphique ggplot (carte points / diagnostics); indicateurs/tests spatiaux; tableau (DataTable) |
| PARTIE 8: Carte des résidus OLS et Tests | moran_I | moran_I | Variable repérée dans code/visualisation de la section | IRIS | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | graphique ggplot (carte points / diagnostics); indicateurs/tests spatiaux; tableau (DataTable) |
| PARTIE 9: Top 4 zones preferables pour une nouvelle implantation | Nb_pdv_access_primaire_voiture_2025 | Nb_pdv_access_primaire_voiture_2025 | Variable repérée dans code/visualisation de la section | IRIS, Commune | compte | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 9: Top 4 zones preferables pour une nouvelle implantation | abs_delta | abs_delta | Variable repérée dans code/visualisation de la section | IRIS, Commune | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 9: Top 4 zones preferables pour une nouvelle implantation | grey85 | grey85 | Variable repérée dans code/visualisation de la section | IRIS, Commune | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 9: Top 4 zones preferables pour une nouvelle implantation | s_q1 | s_q1 | Variable repérée dans code/visualisation de la section | IRIS, Commune | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 9: Top 4 zones preferables pour une nouvelle implantation | score_total_A | score_total_A | Variable repérée dans code/visualisation de la section | IRIS, Commune | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 9: Top 4 zones preferables pour une nouvelle implantation | zone_top4 | zone_top4 | Variable repérée dans code/visualisation de la section | IRIS, Commune | non identifiable à partir du Rmd seul | non identifiable à partir du Rmd seul | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | CA observe / estime | CA observe / estime | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | Code_IRIS | Code_IRIS | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | Enseigne | Enseigne | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | Type_PV | Type_PV | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | breaks | breaks | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | ca_class | ca_class | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | euro | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | ca_value | ca_value | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | euro | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | capital_social_num | capital_social_num | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | drive_trouve | drive_trouve | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | grey95 | grey95 | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | hhi_enseignes_local | hhi_enseignes_local | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | include.lowest | include.lowest | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | iris_name | iris_name | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | labels | labels | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | log_densite | log_densite | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | nb_enseignes_access | nb_enseignes_access | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | ols_resid_access | ols_resid_access | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | pred_ca_2024 | pred_ca_2024 | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | euro | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | recouv_intra_enseigne_local | recouv_intra_enseigne_local | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | right | right | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | shape_cat | shape_cat | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |
| PARTIE 10: CA potentiel (Pappers + projection Top 1) | siret14 | siret14 | Variable repérée dans code/visualisation de la section | IRIS, Points établissements (SIRENE) | non identifiable à partir du Rmd seul | CA_super_hyper_pappers_nombres.csv, crossref_supermarches_drives.csv, crossref_supermarches_drives_zone.csv, crossref_supermarches_drives_zone_v3.csv | bloc texte généré; carte tmap (choroplèthe et/ou points); tableau (DataTable) |

3.2 Tableau récapitulatif de tous les jeux de données repérés

| Nom de l’objet R / dataset | Rôle dans le dashboard | Variables principales utilisées | Niveau géographique | Remarques techniques |
| --- | --- | --- | --- | --- |
| iris_shp | Chargement de données source | non identifiable à partir du Rmd seul | IRIS, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est) | iris_shp <- st_read( |
| sirene_shp | Chargement de données source | non identifiable à partir du Rmd seul | IRIS, Points établissements (SIRENE), Zone cible Nord, Agglomération complète (Grenoble+Nord+Ouest+Sud-Est) | sirene_shp <- st_read( |
| iris_data | Chargement de données source | non identifiable à partir du Rmd seul | IRIS, Points établissements (SIRENE) | iris_data <- read_excel(file.path(raw_dir, "iris_isere.xlsx")) |
| sirene_shp_avant | Chargement de données source | non identifiable à partir du Rmd seul | Points établissements (SIRENE) | sirene_shp_avant <- st_read( |

3.3 Tableau récapitulatif des cartes

| Section | Type de carte | Variable cartographiée | Niveau géographique | Couches additionnelles | Légende / classes | Données exploitables à reprendre dans un rapport |
| --- | --- | --- | --- | --- | --- | --- |
| PARTIE 4: Analyse de la concurrence | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Comptage par IRIS - 2020 |
| PARTIE 4: Analyse de la concurrence | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Commune, Points établissements (SIRENE), Zone cible Nord | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Comptage par IRIS - 2025 |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Super/hyper (2025) vs points avec drive |
| PARTIE 4.1: Comparaison super/hyper vs drives (2025) | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Comptage drives par IRIS - 2025 |
| PARTIE 5: Zones de chalandises en 2025 | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Zones de chalandise (à pied) |
| PARTIE 5: Zones de chalandises en 2025 | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Zones de chalandise (en voiture) |
| PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Recouvrement intra-enseigne: aucun recouvrement detecte |
| PARTIE 5.1: Recouvrement intra-enseigne (isochrones 2025) | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Synthese recouvrement intra-enseigne (2025) |
| PARTIE 6: Accessibilité | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Accessibilite IRIS (hyper>super>superette) |
| PARTIE 6: Accessibilité | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Accessibilité des supermarchés (à pied) |
| PARTIE 6.1: Couverture des IRIS | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Points établissements (SIRENE) | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Couverture des IRIS (zone primaire hierarchisee hyper>super>superette) |
| PARTIE 9: Top 4 zones preferables pour une nouvelle implantation | Carte tmap | Variables selon tm_polygons/tm_fill/tm_symbols de la section | IRIS, Commune | Points SIRENE, polygones IRIS, couches drives/isochrones selon section | Classes: voir style tmap (quantile/fixed) et légendes HTML | Top 4 des zones d'implantation preferables (classement B) |
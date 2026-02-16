# Organisation du dossier `Cours`

## Structure

- `data/raw/`: données sources (shapefiles, xlsx, package OTP)
- `data/processed/`: sorties générées par les scripts/notebooks (`.gpkg`, `.csv`)
- `data/`: données OTP locales (`Graph.obj`, GTFS, jar, etc.)
- `notebooks/`: fichiers `.Rmd` de travail
- `scripts/`: scripts `.R` utilitaires
- `reports/`: exports HTML

## Convention de chemins

Les scripts/notebooks utilisent une résolution automatique de `cours_dir` pour fonctionner :

- depuis la racine du repo
- ou depuis `Cours/`
- ou depuis `Cours/notebooks`

Les lectures de données passent par `data/raw` et les sorties sont écrites dans `data/processed` ou `reports`.

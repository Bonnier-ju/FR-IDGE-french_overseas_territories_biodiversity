# =============================================================================
# FR-IDGE / Overseas component
# Extraction of a taxonomic group from TAXREF for French overseas territories,
# enriched with conservation statuses, territory attributes and range metrics
#
# Goal: from the raw TAXREF file, build a single analysis-ready dataset that
#       carries everything needed downstream, so that no information has to be
#       fetched or recomputed later.
#
# What this version adds to the original extraction:
#   1. territory attributes      (area, region, basin, administrative status)
#   2. range metrics per taxon   (how many territories, regions, basins)
#   3. conservation statuses     (IUCN red lists, invasive listings, protection)
#   4. derived conservation flags (threatened, endemic AND threatened, ...)
#   5. a data dictionary         (one row per column, exported with the data)
#
# The script stays taxon-agnostic: change the TAXON_* parameters to run it on
# birds, vascular plants, reptiles, fishes, etc. Each added block is optional
# and switches itself off if its input file is missing, so the script always
# runs to completion.
#
# Author: Julien Bonnier
# =============================================================================

library(data.table)

# --- Files -------------------------------------------------------------------

WORKING_DIR <- "C:/Users/jbonnier/OneDrive/University/Projects/FR-IDGE/Axe_2_Pan_OM_oiseaux/data_analysis"

TAXREF_VERSION <- "v18"
TAXREF_FILE    <- "data/raw/TAXREF_v18_2025/TAXREFv18.txt"   # raw TAXREF file (tab-separated)
OUTPUT_DIR     <- "data/clean"
CSV_SEP <- ";"
CSV_BOM <- TRUE

# Optional inputs. Each block is skipped, with a message, if its file is absent.
META_FILE <- "data/territories_metadata.csv"   # territory attributes
BDC_FILE  <- "data/raw/BDC_18/bdc_18_01.csv"         # BDC-Statuts v18, TAB separated


# =============================================================================
# PARAMETERS -- the only section to edit
# =============================================================================

# The TAXREF zip also ships statuts_note.csv, habitats_note.csv and
# rangs_note.csv, which hold the official meaning of the status, habitat and
# rank codes for that exact version. When TRUE, they are read from the same
# folder as TAXREF_FILE and override the built-in reference tables below --
# the safest way to stay correct when TAXREF is updated.
# Missing or unreadable files simply fall back to the built-in tables.

USE_OFFICIAL_NOTES <- TRUE

# Blocks that can be switched off entirely, independently of file availability.
ADD_TERRITORY_META <- TRUE
ADD_RANGE_METRICS  <- TRUE
ADD_STATUTS        <- TRUE


# --- Taxonomic group ---------------------------------------------------------
# TAXON_COLUMN  : the TAXREF column used to filter (see available columns below)
# TAXON_VALUES  : one or several values to keep in that column
# TAXON_LABEL   : short slug reused in output file names
#
# Available filtering columns in TAXREF:
#   REGNE       e.g. "Animalia", "Plantae", "Fungi"
#   PHYLUM      e.g. "Chordata", "Tracheophyta"
#   CLASSE      e.g. "Aves", "Mammalia", "Reptilia", "Insecta"
#   ORDRE       e.g. "Passeriformes", "Lepidoptera"
#   FAMILLE     e.g. "Orchidaceae"
#   GROUP2_INPN e.g. "Oiseaux", "Mammiferes", "Angiospermes" (INPN groupings)
#
#
# Examples:
#   Birds            -> TAXON_COLUMN "CLASSE",      TAXON_VALUES "Aves"
#   Vascular plants  -> TAXON_COLUMN "PHYLUM",      TAXON_VALUES "Tracheophyta"
#   Reptiles + amph. -> TAXON_COLUMN "GROUP2_INPN", TAXON_VALUES c("Reptiles", "Amphibiens")

TAXON_COLUMN <- "CLASSE"
TAXON_VALUES <- "Aves"
TAXON_LABEL  <- "birds"


# --- Taxonomic rank ----------------------------------------------------------
# Ranks to keep. "ES" = species, "SSES" = subspecies.
# Use "ES" alone for a strictly species-level dataset.

KEEP_RANKS <- c("ES", "SSES")

# Keep only valid names (CD_NOM == CD_REF), dropping synonyms.
# Synonyms point to the same biological entity and would
# duplicate records. Set to FALSE only to inspect the synonymy itself.

KEEP_VALID_ONLY <- TRUE

# --- Optional filtering (OFF by default: exploratory extraction) -------------
# DROP_STATUS_CODES : status codes to discard entirely. character(0) keeps all.
#   A common minimal cleanup is c("A", "Q") -- explicitly absent, and mentioned
#   in error. Even those carry information on survey effort, hence kept here.
# FILTER_TO_PRESENT : TRUE keeps only records flagged is_present (see below),
#   i.e. the taxon is actually there today, whatever its origin.

DROP_STATUS_CODES <- character(0)
FILTER_TO_PRESENT <- FALSE

# --- Territories -------------------------------------------------------------
# Overseas territory columns in TAXREF, with readable labels.
# Set INCLUDE_METROPOLITAN to TRUE to also keep mainland France ("FR").
#
# IMPORTANT: the "TAAF" column of TAXREF is a COMPUTED field, aggregating
# "SA" (subantarctic islands) and "TA" (Terre Adelie). It is deliberately left
# out below: keeping it would count those taxa twice. Note that the Iles
# Eparses, although administratively part of the TAAF, have their own "EPA"
# column and are NOT included in the computed "TAAF" field.
# Source: Gargominy et al. 2022, TAXREF methodology report (PatriNat).

OM_TERRITORIES <- c(
  GF   = "Guyane",
  GUA  = "Guadeloupe",
  MAR  = "Martinique",
  SM   = "Saint-Martin",
  SB   = "Saint-Barthelemy",
  SPM  = "Saint-Pierre-et-Miquelon",
  MAY  = "Mayotte",
  REU  = "La Reunion",
  EPA  = "Iles Eparses",
  SA   = "Iles subantarctiques (Kerguelen, Crozet, Saint-Paul et Amsterdam)",
  TA   = "Terre Adelie",
  NC   = "Nouvelle-Caledonie",
  WF   = "Wallis-et-Futuna",
  PF   = "Polynesie francaise",
  CLI  = "Clipperton"
)

INCLUDE_METROPOLITAN <- FALSE

# Cold territories: very specific biota, with little overlap with the rest.
# Flagged so they can easily be excluded from the main comparison if needed.
AUSTRAL_CODES <- c("SA", "TA")

# --- Conservation statuses ---------------------------------------------------
# BDC-Statuts groups its status types in REGROUPEMENT_TYPE. Rather than listing
# exact wordings, which change between versions, each family is selected by a
# regular expression. The script prints the full vocabulary it found, so these
# can be refined if needed.

STATUS_FAMILY_PATTERNS <- c(
  liste_rouge = "liste rouge",
  protection  = "protection",
  directive   = "directive",
  convention  = "convention",
  pna         = "plan national"
)

# Invasive-species regulation. BDC-Statuts has NO family named after invasive
# alien species: the relevant acts sit under "Reglementation", which ALSO
# contains "Reglementation sans objet" -- 655 taxa for which no regulation
# applies. Selecting that family wholesale would flag them as regulated, the
# exact opposite of the truth, so the two relevant types are named explicitly.
#   REGLII    = interdiction d'introduction
#   REGLLUTTE = lutte contre certaines especes
EEE_TYPES <- c("REGLII", "REGLLUTTE")

# IUCN categories, ordered from most to least threatened. DD / NA / NE are not
# categories of risk. Used to pick one value when several assessments coexist.
IUCN_ORDER <- c("EX", "EW", "RE", "CR", "EN", "VU", "NT", "LC", "DD", "NA", "NE")

# Categories counted as "threatened" in the usual IUCN sense.
THREATENED_CATEGORIES <- c("CR", "EN", "VU")

# Some BDC territorial labels differ too much from ours for an automatic match.
# Add any further pair here as "BDC label" = "our label".
MANUAL_TERRITORY_MATCHES <- c(
  # BDC names the three TAAF districts separately, which is exactly our own
  # breakdown. The aggregate "Terres australes et antarctiques francaises" is
  # deliberately left unmatched: mapping it onto one district would repeat the
  # double-counting trap of the computed TAAF column of TAXREF.
  "taaf terre adelie"          = "Terre Adelie",
  "taaf iles sub antarctiques" = "Iles subantarctiques (Kerguelen, Crozet, Saint-Paul et Amsterdam)",
  "taaf iles eparses"          = "Iles Eparses",
  "reunion"                    = "La Reunion",
  # New Caledonia carries NO territory-level assessment in BDC: its red lists
  # are provincial. Both provinces are mapped onto the territory so that NC is
  # not silently left without any status; combined with worst_status(), a taxon
  # threatened in one province counts as threatened for the territory.
  "province nord"              = "Nouvelle-Caledonie",
  "province sud"               = "Nouvelle-Caledonie"
)

# =============================================================================
# REFERENCE TABLES -- TAXREF code meanings
#
# These tables are fallbacks. The TAXREF zip ships three companion files --
# statuts_note.csv, habitats_note.csv and rangs_note.csv -- that carry the
# official labels for the version at hand. They are read automatically when
# found (see USE_OFFICIAL_NOTES above), which keeps the script correct across
# TAXREF versions.
# =============================================================================

STATUS_LABELS <- c(
  P = "Present (indigene ou indetermine)",
  E = "Endemique",
  S = "Subendemique (endemisme regional)",
  C = "Cryptogene (origine inconnue)",
  I = "Introduit, etabli",
  J = "Introduit envahissant",
  M = "Introduit non etabli (dont domestique/cultive)",
  B = "Occasionnel (non nicheur, accidentel)",
  D = "Douteux (presence a confirmer)",
  A = "Absent du territoire",
  W = "Disparu du territoire (non eteint globalement)",
  X = "Eteint globalement",
  Y = "Introduit puis disparu/eteint",
  Z = "Endemique eteint",
  Q = "Mentionne par erreur",
  G = "ADNe uniquement (nouveaute v18)"
)

# Coarse grouping of the statuses, handy for summaries and plots.
STATUS_CLASS <- c(
  P = "Indigene",
  E = "Indigene",
  S = "Indigene",
  C = "Cryptogene",
  I = "Introduit",
  J = "Introduit",
  M = "Introduit",
  B = "Occasionnel",
  D = "Incertain",
  G = "Incertain",
  Q = "Erreur",
  A = "Absent",
  W = "Disparu / eteint",
  X = "Disparu / eteint",
  Y = "Disparu / eteint",
  Z = "Disparu / eteint"
)

# Status codes meaning "the taxon is there today", whatever its origin.
# Used only to build the is_present flag -- no row is dropped because of it.
PRESENT_CODES    <- c("P", "E", "S", "C", "I", "J", "B")
NATIVE_CODES     <- c("P", "E", "S")
INTRODUCED_CODES <- c("I", "J", "M")
EXTINCT_CODES    <- c("W", "X", "Y", "Z")
UNCERTAIN_CODES  <- c("D", "G", "Q")

# Erosion in the conservation sense. Y is deliberately excluded: a failed or
# eradicated introduction is not a loss of biodiversity, and counting it would
# carry the opposite sign.
EROSION_CODES    <- c("W", "X", "Z")

# --- Taxonomic rank codes (table 3, infraspecific ranks only) ----------------

RANK_LABELS <- c(
  ES   = "Espece",
  SSES = "Sous-espece",
  NAT  = "Natio",
  VAR  = "Variete",
  SVAR = "Sous-variete",
  FO   = "Forme",
  SSFO = "Sous-forme",
  AGES = "Agregat",
  RACE = "Race",
  CAR  = "Cultivar",
  AB   = "Abberatio"
)

# --- Habitat codes -----------------------------------------------------------

HABITAT_LABELS <- c(
  "1" = "Marin",
  "2" = "Eau douce",
  "3" = "Terrestre",
  "4" = "Marin et eau douce (diadrome/euryhalin)",
  "5" = "Marin et terrestre (ex. oiseaux marins, tortues)",
  "6" = "Eau saumatre",
  "7" = "Continental (terrestre et/ou eau douce, indetermine)",
  "8" = "Continental (terrestre ET eau douce)"
)

# Coarse habitat grouping, so marine and continental taxa can be compared
# without juggling eight codes.
HABITAT_CLASS <- c(
  "1" = "Marin strict",
  "2" = "Eau douce",
  "3" = "Terrestre / continental",
  "4" = "Marin et continental",
  "5" = "Marin et continental",
  "6" = "Eau saumatre",
  "7" = "Terrestre / continental",
  "8" = "Terrestre / continental"
)

# =============================================================================
# PROCESSING -- no edit needed below
# =============================================================================

# --- Set the working directory -----------------------------------------------

if (!is.null(WORKING_DIR)) {
  if (!dir.exists(WORKING_DIR)) {
    stop("Working directory not found: ", WORKING_DIR,
         "\nCheck WORKING_DIR, and use forward slashes '/' in the path.")
  }
  setwd(WORKING_DIR)
}

cat("Working directory:", getwd(), "\n")

# --- Locate the raw TAXREF file ----------------------------------------------

if (!file.exists(TAXREF_FILE)) {
  stop("TAXREF file not found: ", normalizePath(TAXREF_FILE, mustWork = FALSE),
       "\nExpected either a path relative to the working directory",
       " (e.g. 'data/raw/TAXREFv18.txt') or a full absolute path.",
       "\nDownload TAXREF from https://inpn.mnhn.fr/telechargement/referentielEspece/taxref")
}

cat("TAXREF file:", normalizePath(TAXREF_FILE), "\n")

# --- Encoding-safe reader ----------------------------------------------------
# INPN reference files come in two broken flavours, and both must be handled:
#   (a) genuine Windows-1252 bytes, which fail validUTF8(). Note it is really
#       Windows-1252 and not Latin-1: the files use curly quotes and
#       apostrophes, which live in the 0x80-0x9F range that ISO-8859-1 leaves
#       undefined.
#   (b) UTF-8 written out a second time as if it were Windows-1252
#       ("mojibake"): "Donnees" becomes "DonnA©es". Those bytes ARE valid
#       UTF-8, so validUTF8() passes and the damage goes unnoticed.

# Repairs only the ELEMENTS that look doubly encoded, and keeps the
# repair only if it yields valid UTF-8. Applying this column-wide destroys
# correctly encoded strings such as "Ruppell": converting them to
# Windows-1252 and relabelling them UTF-8 makes them invalid, which then
# breaks trimws() further down.
fix_mojibake <- function(x) {
  idx <- which(grepl("\u00c3[\u0080-\u00bf]|\u00c2[\u0080-\u00bf]", x))
  if (length(idx) == 0) return(x)
  cand <- iconv(x[idx], from = "UTF-8", to = "windows-1252")
  ok   <- !is.na(cand)
  if (any(ok)) {
    repaired <- cand[ok]
    Encoding(repaired) <- "UTF-8"
    good <- validUTF8(repaired)
    x[idx[ok][good]] <- repaired[good]
  }
  x
}

read_text_table <- function(path, ...) {
  tab <- fread(path, encoding = "unknown", colClasses = "character", ...)
  for (col in names(tab)) {
    x <- tab[[col]]
    if (!is.character(x)) next
    if (any(!validUTF8(x), na.rm = TRUE)) {
      x <- iconv(x, from = "windows-1252", to = "UTF-8", sub = "")
    } else {
      Encoding(x) <- "UTF-8"
      x <- fix_mojibake(x)
    }
    set(tab, j = col, value = x)
  }
  tab[]
}

# Accent- and case-insensitive key, used to match labels between referentials
# without hand-writing a lookup table.
# Makes the enrichment blocks re-runnable. Without this, running a block twice
# in the same session feeds merge() a table that already carries the columns it
# is about to add: data.table then suffixes them .x and .y, and every derived
# flag downstream fails to find its input. Also catches suffixed leftovers from
# an interrupted run.
drop_existing <- function(dt, cols) {
  pat <- paste0("^(", paste(cols, collapse = "|"), ")(\\.x|\\.y)?$")
  present <- grep(pat, names(dt), value = TRUE)
  if (length(present) > 0) dt[, (present) := NULL]
  invisible(present)
}

normalise <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  gsub("[^a-z0-9]+", " ", x)
}

# --- Load the official code tables shipped with TAXREF, if available ---------
#   statuts_note.csv   ORDRE;STATUT;DESCRIPTION;DEFINITION
#   habitats_note.csv  HABITAT;LB_HABITAT;DEFINITION
#   rangs_note.csv     RG_LEVEL;RANG;DETAIL;DETAIL_EN
# Columns are therefore selected by name, never by position.

read_note_file <- function(filename, code_col, label_col) {
  path <- file.path(dirname(TAXREF_FILE), filename)
  if (!file.exists(path)) return(NULL)
  
  tryCatch({
    tab <- read_text_table(path, sep = ";")
    setnames(tab, trimws(sub("^\ufeff", "", names(tab))))   # strip a BOM
    
    if (!all(c(code_col, label_col) %in% names(tab))) {
      warning("Columns '", code_col, "' / '", label_col, "' not found in ",
              filename, " (found: ", paste(names(tab), collapse = ", "),
              "). Falling back to the built-in table.")
      return(NULL)
    }
    
    codes  <- trimws(tab[[code_col]])
    labels <- trimws(tab[[label_col]])
    keep   <- !is.na(codes) & codes != "" & !is.na(labels)
    if (!any(keep)) return(NULL)
    setNames(labels[keep], codes[keep])
  }, error = function(e) {
    warning("Could not read ", filename, " (", conditionMessage(e),
            "). Falling back to the built-in table.")
    NULL
  })
}

if (USE_OFFICIAL_NOTES) {
  official_status  <- read_note_file("statuts_note.csv",  "STATUT",  "DESCRIPTION")
  official_habitat <- read_note_file("habitats_note.csv", "HABITAT", "LB_HABITAT")
  official_rank    <- read_note_file("rangs_note.csv",    "RANG",    "DETAIL")
  
  if (!is.null(official_status)) {
    STATUS_LABELS <- official_status
    cat("Status labels read from statuts_note.csv (",
        length(STATUS_LABELS), " codes)\n", sep = "")
  }
  if (!is.null(official_habitat)) {
    HABITAT_LABELS <- official_habitat
    cat("Habitat labels read from habitats_note.csv (",
        length(HABITAT_LABELS), " codes)\n", sep = "")
  }
  if (!is.null(official_rank)) {
    RANK_LABELS <- official_rank
    cat("Rank labels read from rangs_note.csv (",
        length(RANK_LABELS), " codes)\n", sep = "")
  }
}

territories <- OM_TERRITORIES
if (INCLUDE_METROPOLITAN) {
  territories <- c(FR = "France metropolitaine", territories)
}

# --- Read raw TAXREF ---------------------------------------------------------
# The TAXREF export is tab-separated and every value is wrapped in double quotes.

taxref <- read_text_table(
  TAXREF_FILE,
  sep        = "\t",
  quote      = "\"",
  na.strings = c("", "NA")
)

cat("Raw TAXREF rows:", nrow(taxref), "\n")

# --- Check that the requested columns exist ----------------------------------

required_cols <- c(TAXON_COLUMN, "CD_NOM", "CD_REF", "RANG", "LB_NOM",
                   names(territories))

missing_cols <- setdiff(required_cols, names(taxref))
if (length(missing_cols) > 0) {
  stop("Missing columns in TAXREF: ", paste(missing_cols, collapse = ", "),
       "\nCheck the column names against your TAXREF version.")
}

# --- Filter: taxonomic group, rank, valid names ------------------------------

taxa <- taxref[get(TAXON_COLUMN) %in% TAXON_VALUES & RANG %in% KEEP_RANKS]

if (KEEP_VALID_ONLY) {
  taxa <- taxa[CD_NOM == CD_REF]
}

if (nrow(taxa) == 0) {
  stop("No taxon found for ", TAXON_COLUMN, " = ",
       paste(TAXON_VALUES, collapse = ", "),
       ". Check the spelling against the values present in TAXREF.")
}

cat("Taxa kept (", TAXON_COLUMN, " = ", paste(TAXON_VALUES, collapse = ", "),
    "): ", nrow(taxa), "\n", sep = "")

# --- Keep useful columns -----------------------------------------------------
# Exploratory extraction: keep every descriptive column available, so that no
# information has to be fetched again from the raw file later on.

wanted_cols <- c(
  # identifiers and hierarchy
  "CD_NOM", "CD_REF", "CD_SUP", "CD_TAXSUP", "CD_BA", "RANG",
  # taxonomy
  "REGNE", "PHYLUM", "CLASSE", "ORDRE", "FAMILLE", "SOUS_FAMILLE", "TRIBU",
  # INPN groupings
  "GROUP1_INPN", "GROUP2_INPN", "GROUP3_INPN",
  # names
  "LB_NOM", "LB_AUTEUR", "NOMENCLATURAL_COMMENT",
  "NOM_COMPLET", "NOM_VALIDE", "NOM_VERN", "NOM_VERN_ENG",
  # ecology and links
  "HABITAT", "URL", "URL_INPN"
)

keep_cols <- c(intersect(wanted_cols, names(taxa)), names(territories))
taxa <- taxa[, ..keep_cols]

# --- Reshape to long format: one row per taxon x territory -------------------

id_cols <- setdiff(keep_cols, names(territories))

taxa_long <- melt(
  taxa,
  id.vars       = id_cols,
  measure.vars  = names(territories),
  variable.name = "territory_code",
  value.name    = "status_code"
)

# Keep every record carrying information. An empty cell means "no information
# for this territory" and is the only thing dropped here.
taxa_long <- taxa_long[!is.na(status_code) & status_code != ""]

# --- Decode codes into labels and flags --------------------------------------

taxa_long[, territory      := territories[as.character(territory_code)]]
taxa_long[, status_label   := STATUS_LABELS[status_code]]
taxa_long[, status_class   := STATUS_CLASS[status_code]]
taxa_long[, habitat_label  := HABITAT_LABELS[as.character(HABITAT)]]
taxa_long[, habitat_class  := HABITAT_CLASS[as.character(HABITAT)]]
taxa_long[, rank_label     := RANK_LABELS[as.character(RANG)]]

# Full scientific name with its authority, as required when first citing a
# taxon in a publication.
if (all(c("LB_NOM", "LB_AUTEUR") %in% names(taxa_long))) {
  taxa_long[, nom_autorite := trimws(paste(LB_NOM,
                                           fifelse(is.na(LB_AUTEUR), "", LB_AUTEUR)))]
}

# Flags -- all derived from status_code, none of them drops a record.
taxa_long[, is_present    := status_code %in% PRESENT_CODES]
taxa_long[, is_native     := status_code %in% NATIVE_CODES]
taxa_long[, is_endemic    := status_code %in% c("E", "Z")]
taxa_long[, is_introduced := status_code %in% INTRODUCED_CODES]
taxa_long[, is_invasive_taxref := status_code == "J"]
taxa_long[, is_cryptogenic := status_code == "C"]
taxa_long[, is_occasional := status_code == "B"]
taxa_long[, is_extinct    := status_code %in% EXTINCT_CODES]
taxa_long[, is_eroded     := status_code %in% EROSION_CODES]
taxa_long[, is_uncertain  := status_code %in% UNCERTAIN_CODES]
taxa_long[, is_absent     := status_code == "A"]
taxa_long[, is_austral    := territory_code %in% AUSTRAL_CODES]
taxa_long[, is_species    := RANG == "ES"]

# Warn about status codes absent from the reference tables (new TAXREF version)
unknown_status <- sort(unique(taxa_long[is.na(status_label), status_code]))
if (length(unknown_status) > 0) {
  warning("Status codes not found in STATUS_LABELS: ",
          paste(unknown_status, collapse = ", "),
          ". Update the reference table against your TAXREF version.")
}


# --- Optional filtering ------------------------------------------------------

if (length(DROP_STATUS_CODES) > 0) {
  n_before  <- nrow(taxa_long)
  taxa_long <- taxa_long[!status_code %in% DROP_STATUS_CODES]
  cat("Dropped", n_before - nrow(taxa_long), "records with status in",
      paste(DROP_STATUS_CODES, collapse = ", "), "\n")
}

if (FILTER_TO_PRESENT) {
  n_before  <- nrow(taxa_long)
  taxa_long <- taxa_long[is_present == TRUE]
  cat("Dropped", n_before - nrow(taxa_long), "records not flagged is_present\n")
}


# =============================================================================
# BLOCK 1 -- TERRITORY ATTRIBUTES
#
# Area, biogeographic region, ocean basin, administrative and EU status.
# Carrying them in the dataset makes species-area analyses, regional
# comparisons and governance-related questions possible without a further join.
# =============================================================================

if (ADD_TERRITORY_META && file.exists(META_FILE)) {
  
  meta <- read_text_table(META_FILE, sep = CSV_SEP, na.strings = c("", "NA"))
  setnames(meta, trimws(sub("^\ufeff", "", names(meta))))
  
  if (!"territory" %in% names(meta)) {
    warning("No 'territory' column in ", META_FILE, " -- block skipped.")
  } else {
    # Guard: an unmatched territory would silently carry NA attributes.
    unmatched <- setdiff(unique(taxa_long$territory), meta$territory)
    if (length(unmatched) > 0) {
      warning("Territories absent from the metadata table: ",
              paste(unmatched, collapse = " | "))
    }
    
    meta_cols <- setdiff(names(meta), c("territory_code", "territory_fr"))
    drop_existing(taxa_long, setdiff(meta_cols, "territory"))
    taxa_long <- merge(taxa_long, meta[, ..meta_cols],
                       by = "territory", all.x = TRUE, sort = FALSE)
    
    if ("area_km2" %in% names(taxa_long)) {
      taxa_long[, area_km2 := as.numeric(area_km2)]
    }
    
    cat("\nTerritory attributes joined:",
        length(setdiff(meta_cols, "territory")), "columns\n")
  }
  
} else if (ADD_TERRITORY_META) {
  cat("\nTerritory attributes skipped: ", META_FILE, " not found.\n", sep = "")
}


# =============================================================================
# BLOCK 2 -- RANGE METRICS PER TAXON
#
# How widely a taxon is spread across the overseas territories. Computed on
# present records only: an absence or an error record says nothing about range.
# These are taxon-level attributes, repeated on every row of that taxon.
# =============================================================================

if (ADD_RANGE_METRICS) {
  
  present_only <- taxa_long[is_present == TRUE]
  
  range_cols <- list(n_territoires = quote(uniqueN(territory)))
  if ("region" %in% names(present_only)) {
    range_cols$n_regions <- quote(uniqueN(region[!is.na(region)]))
  }
  if ("basin" %in% names(present_only)) {
    range_cols$n_bassins <- quote(uniqueN(basin[!is.na(basin)]))
  }
  
  rng <- present_only[, lapply(range_cols, eval, envir = .SD),
                      by = CD_REF,
                      .SDcols = intersect(c("territory", "region", "basin"),
                                          names(present_only))]
  
  rng[, est_endemique_stricte := n_territoires == 1]
  
  drop_existing(taxa_long, setdiff(names(rng), "CD_REF"))
  taxa_long <- merge(taxa_long, rng, by = "CD_REF", all.x = TRUE, sort = FALSE)
  
  # Taxa that are never present anywhere (only A, Q, D records) get 0.
  taxa_long[is.na(n_territoires), n_territoires := 0L]
  
  cat("\nRange metrics computed for", nrow(rng), "present taxa\n")
  cat("  occupying a single territory:",
      rng[n_territoires == 1, .N],
      paste0("(", round(100 * rng[n_territoires == 1, .N] / nrow(rng), 1), " %)"),
      "\n")
}


# =============================================================================
# BLOCK 3 -- CONSERVATION STATUSES (BDC-Statuts)
#
# BDC-Statuts is the national knowledge base on species statuses. It is keyed
# on the same CD_REF as TAXREF, so the taxonomic join is direct. Its
# TERRITORIAL vocabulary, however, is its own: the labels are matched here on
# an accent-insensitive key, and whatever fails to match is printed rather
# than dropped silently.
#
# Three scopes are distinguished, because they answer different questions:
#   monde      -> the IUCN global assessment, same everywhere
#   france     -> the national assessment
#   territoire -> the assessment made for that territory specifically
# A taxon can be LC worldwide and CR on one island. Collapsing the three would
# destroy exactly the signal of interest.
# =============================================================================

has_statuts <- FALSE

if (ADD_STATUTS && file.exists(BDC_FILE)) {
  
  # BDC-Statuts ships as a COMMA separated file with every value quoted,
  # unlike TAXREF which is tab separated. fread detects the separator on its
  # own; forcing the wrong one yields a single column and a misleading
  # "missing columns" warning further down.
  bdc <- read_text_table(BDC_FILE, quote = "\"")
  setnames(bdc, trimws(gsub("^\ufeff|^\"|\"$", "", names(bdc))))
  
  cat("\nBDC-Statuts rows:", format(nrow(bdc), big.mark = " "),
      "|", ncol(bdc), "columns\n")
  
  if (ncol(bdc) == 1) {
    stop("BDC-Statuts was read as a single column: the separator was not ",
         "detected.\nOpen the file and pass it explicitly, e.g. ",
         "read_text_table(BDC_FILE, sep = \",\", quote = \"\\\"\").")
  }
  
  needed <- c("CD_REF", "REGROUPEMENT_TYPE", "CD_TYPE_STATUT",
              "LB_TYPE_STATUT", "CODE_STATUT", "LB_ADM_TR")
  if (!all(needed %in% names(bdc))) {
    warning("BDC-Statuts is missing ",
            paste(setdiff(needed, names(bdc)), collapse = ", "),
            " -- block skipped.")
  } else {
    
    # Restrict to our taxa. Filtering on CD_REF rather than on a taxonomic
    # column keeps this working whatever TAXON_COLUMN was set to.
    our_refs <- unique(taxa_long$CD_REF)
    bdc <- bdc[CD_REF %in% our_refs]
    
    cat("Status records for our taxa:", format(nrow(bdc), big.mark = " "),
        "covering", uniqueN(bdc$CD_REF), "taxa\n")
    
    cat("\n-- Status families found (REGROUPEMENT_TYPE) --\n")
    print(bdc[, .(n_records = .N, n_taxa = uniqueN(CD_REF)),
              by = REGROUPEMENT_TYPE][order(-n_records)])
    
    cat("\n-- Status types found --\n")
    print(bdc[, .(n_records = .N, n_taxa = uniqueN(CD_REF)),
              by = .(REGROUPEMENT_TYPE, CD_TYPE_STATUT, LB_TYPE_STATUT)
    ][order(-n_records)])
    
    # --- Match BDC territorial labels to ours ------------------------------
    
    our_terr <- data.table(territory = sort(unique(taxa_long$territory)))
    our_terr[, terr_key := normalise(territory)]
    
    bdc_terr <- unique(bdc[, .(LB_ADM_TR)])
    bdc_terr[, terr_key := normalise(LB_ADM_TR)]
    bdc_terr <- merge(bdc_terr, our_terr, by = "terr_key", all.x = TRUE)
    
    # NOTE: "key" is a reserved argument of data.table(), so a column of that
    # name cannot be created in the constructor -- it would be taken as the
    # key to set, and the call fails. Hence terr_key throughout.
    manual <- data.table(terr_key = normalise(names(MANUAL_TERRITORY_MATCHES)),
                         manual_territory = unname(MANUAL_TERRITORY_MATCHES))
    bdc_terr <- merge(bdc_terr, manual, by = "terr_key", all.x = TRUE)
    bdc_terr[is.na(territory), territory := manual_territory]
    bdc_terr[, c("manual_territory", "terr_key") := NULL]
    
    cat("\n-- Territorial scopes matched --\n")
    print(bdc_terr[!is.na(territory)][order(territory)])
    
    cat("\n-- Scopes NOT matched (world, France, foreign, metropolitan) --\n")
    print(bdc_terr[is.na(territory), .(LB_ADM_TR)][order(LB_ADM_TR)])
    cat("  -> any OVERSEAS territory in this list needs a line in",
        "MANUAL_TERRITORY_MATCHES.\n")
    
    bdc <- merge(bdc, bdc_terr, by = "LB_ADM_TR", all.x = TRUE)
    
    # "France" and "France metropolitaine" both denote the national scope. For
    # an overseas analysis the latter is the mainland chapter and says nothing
    # about these territories, but it is captured rather than silently lost.
    bdc[, scope := fcase(
      normalise(LB_ADM_TR) == "monde",                                "monde",
      normalise(LB_ADM_TR) == "europe",                               "europe",
      normalise(LB_ADM_TR) %in% c("france", "france metropolitaine"), "france",
      !is.na(territory),                                              "territoire",
      default = "autre"
    )]
    
    # --- Red lists ---------------------------------------------------------
    # When several assessments coexist for one scope (successive editions of a
    # list), the most threatened category is kept, which is the usual
    # convention.
    
    worst_status <- function(x) {
      x <- unique(x[!is.na(x) & x != ""])
      if (length(x) == 0) return(NA_character_)
      known <- x[x %in% IUCN_ORDER]
      if (length(known) == 0) return(paste(sort(x), collapse = "|"))
      known[which.min(match(known, IUCN_ORDER))]
    }
    
    lr <- bdc[grepl(STATUS_FAMILY_PATTERNS["liste_rouge"], REGROUPEMENT_TYPE,
                    ignore.case = TRUE)]
    
    lr_monde  <- lr[scope == "monde",
                    .(lr_mondiale = worst_status(CODE_STATUT)), by = CD_REF]
    lr_france <- lr[scope == "france",
                    .(lr_france = worst_status(CODE_STATUT)), by = CD_REF]
    lr_europe <- lr[scope == "europe",
                    .(lr_europeenne = worst_status(CODE_STATUT)), by = CD_REF]
    lr_terr   <- lr[scope == "territoire",
                    .(lr_territoriale = worst_status(CODE_STATUT)),
                    by = .(CD_REF, territory)]
    
    # The red list a territorial assessment comes from, needed to cite it.
    if ("FULL_CITATION" %in% names(lr)) {
      lr_source <- lr[scope == "territoire",
                      .(lr_source = paste(sort(unique(FULL_CITATION)),
                                          collapse = " ; ")),
                      by = .(CD_REF, territory)]
    } else {
      lr_source <- NULL
    }
    
    cat("\nRed lists -- global:", nrow(lr_monde),
        "| European:", nrow(lr_europe),
        "| national (mainland chapter):", nrow(lr_france),
        "| territorial (taxon x territory):", nrow(lr_terr), "\n")
    
    cat("\n-- Territorial assessments per territory --\n")
    print(lr_terr[, .N, by = territory][order(-N)])
    
    # --- Invasive alien species listings -----------------------------------
    # A regulatory act, not a biogeographic judgement. Kept strictly apart
    # from the TAXREF "J" status so the two can be compared rather than
    # conflated.
    
    eee <- bdc[CD_TYPE_STATUT %in% EEE_TYPES]
    
    eee_any <- eee[, .(eee_listes = paste(sort(unique(CD_TYPE_STATUT)),
                                          collapse = "|")), by = CD_REF]
    eee_terr <- eee[scope == "territoire",
                    .(eee_territoriale = paste(sort(unique(CD_TYPE_STATUT)),
                                               collapse = "|")),
                    by = .(CD_REF, territory)]
    
    cat("Invasive listings:", nrow(eee), "records for",
        uniqueN(eee$CD_REF), "taxa\n")
    
    # --- Protection, directives, action plans ------------------------------
    
    collapse_family <- function(pattern, colname) {
      sub <- bdc[grepl(pattern, REGROUPEMENT_TYPE, ignore.case = TRUE)]
      if (nrow(sub) == 0) return(NULL)
      out <- sub[, .(v = paste(sort(unique(CD_TYPE_STATUT)), collapse = "|")),
                 by = CD_REF]
      setnames(out, "v", colname)
      out
    }
    
    prot      <- collapse_family(STATUS_FAMILY_PATTERNS["protection"], "protection")
    directive <- collapse_family(STATUS_FAMILY_PATTERNS["directive"],  "directive")
    convention <- collapse_family(STATUS_FAMILY_PATTERNS["convention"], "conventions")
    pna       <- collapse_family(STATUS_FAMILY_PATTERNS["pna"],        "pna")
    
    # --- Join everything ---------------------------------------------------
    
    # Re-running this block on an already-enriched taxa_long would suffix every
    # status column .x / .y and break the derived flags below.
    drop_existing(taxa_long,
                  c("lr_mondiale", "lr_europeenne", "lr_france",
                    "lr_territoriale", "lr_source",
                    "eee_listes", "eee_territoriale",
                    "protection", "directive", "conventions", "pna",
                    "menacee_mondiale", "menacee_territoriale",
                    "est_evaluee", "eee_listee", "est_protegee"))
    
    n_before <- nrow(taxa_long)
    
    join_by_ref <- function(dt, x) {
      if (is.null(x)) return(dt)
      merge(dt, x, by = "CD_REF", all.x = TRUE, sort = FALSE)
    }
    join_by_ref_terr <- function(dt, x) {
      if (is.null(x)) return(dt)
      merge(dt, x, by = c("CD_REF", "territory"), all.x = TRUE, sort = FALSE)
    }
    
    taxa_long <- join_by_ref(taxa_long, lr_monde)
    taxa_long <- join_by_ref(taxa_long, lr_europe)
    taxa_long <- join_by_ref(taxa_long, lr_france)
    taxa_long <- join_by_ref_terr(taxa_long, lr_terr)
    taxa_long <- join_by_ref_terr(taxa_long, lr_source)
    taxa_long <- join_by_ref(taxa_long, eee_any)
    taxa_long <- join_by_ref_terr(taxa_long, eee_terr)
    taxa_long <- join_by_ref(taxa_long, prot)
    taxa_long <- join_by_ref(taxa_long, directive)
    taxa_long <- join_by_ref(taxa_long, convention)
    taxa_long <- join_by_ref(taxa_long, pna)
    
    stopifnot(nrow(taxa_long) == n_before)
    
    # --- Derived conservation flags ----------------------------------------
    
    taxa_long[, menacee_mondiale := lr_mondiale %in% THREATENED_CATEGORIES]
    if ("lr_territoriale" %in% names(taxa_long)) {
      taxa_long[, menacee_territoriale := lr_territoriale %in% THREATENED_CATEGORIES]
    }
    taxa_long[, est_evaluee := !is.na(lr_mondiale) |
                (!is.na(lr_territoriale) & lr_territoriale != "")]
    taxa_long[, eee_listee := !is.na(eee_listes)]
    if ("protection" %in% names(taxa_long)) {
      taxa_long[, est_protegee := !is.na(protection)]
    }
    
    # No composite "responsibility" flag is built here. Crossing endemism with
    # threat status is an analytical choice -- which endemic codes to include,
    # which assessment scale to trust where territorial lists are missing --
    # and it belongs in the analysis, not in the dataset. Every ingredient is
    # present: status_code, is_endemic, menacee_mondiale, menacee_territoriale.
    
    has_statuts <- TRUE
    cat("\nConservation statuses joined. Records unchanged:", nrow(taxa_long), "\n")
  }
  
} else if (ADD_STATUTS) {
  cat("\nConservation statuses skipped: ", BDC_FILE, " not found.\n",
      "Download https://assets.patrinat.fr/files/referentiel/BDC.zip\n", sep = "")
}


# --- Column order ------------------------------------------------------------

front_cols <- c("CD_REF", "CD_NOM", "RANG", "rank_label", "is_species",
                "LB_NOM", "LB_AUTEUR", "nom_autorite", "NOM_VERN",
                "ORDRE", "FAMILLE",
                "HABITAT", "habitat_label", "habitat_class",
                "territory_code", "territory", "region", "basin", "area_km2",
                "status_code", "status_label", "status_class",
                "lr_mondiale", "lr_europeenne", "lr_france", "lr_territoriale",
                "eee_listes", "eee_territoriale", "protection",
                "n_territoires", "n_regions", "n_bassins",
                "is_present", "is_native", "is_endemic", "is_introduced",
                "is_invasive_taxref", "eee_listee",
                "menacee_mondiale", "menacee_territoriale",
                "is_cryptogenic", "is_occasional", "is_extinct", "is_eroded",
                "is_uncertain", "is_absent", "is_austral")
setcolorder(taxa_long, intersect(front_cols, names(taxa_long)))

# --- Summaries ---------------------------------------------------------------

cat("\n==================== SUMMARY ====================\n")
cat("Taxon x territory records:", nrow(taxa_long), "\n")
cat("Distinct species (RANG = ES)   :",
    taxa_long[RANG == "ES", uniqueN(CD_REF)], "\n")
cat("Distinct subspecies (RANG=SSES):",
    taxa_long[RANG == "SSES", uniqueN(CD_REF)], "\n")

cat("\n-- Records by status --\n")
print(taxa_long[, .(n_records = .N, n_taxa = uniqueN(CD_REF)),
                by = .(status_code, status_label)][order(-n_records)])

cat("\n-- Taxa by territory and status class --\n")
print(dcast(taxa_long, territory ~ status_class,
            value.var = "CD_REF", fun.aggregate = uniqueN))

if (ADD_RANGE_METRICS) {
  cat("\n-- Present taxa by number of territories occupied --\n")
  print(unique(taxa_long[is_present == TRUE, .(CD_REF, n_territoires)]
  )[, .N, by = n_territoires][order(n_territoires)])
}

if (has_statuts) {
  
  cat("\n-- Red-list coverage of present taxa, by territory --\n")
  cov <- taxa_long[is_present == TRUE, .(
    n_taxa        = uniqueN(CD_REF),
    eval_mondiale = uniqueN(CD_REF[!is.na(lr_mondiale)]),
    eval_territ   = uniqueN(CD_REF[!is.na(lr_territoriale)])
  ), by = territory]
  cov[, `:=`(pct_mondiale = round(100 * eval_mondiale / n_taxa, 1),
             pct_territ   = round(100 * eval_territ   / n_taxa, 1))]
  print(cov[order(-pct_territ)])
  
  # Sanity check only: every IUCN category should be a known one, and the
  # share of NA tells how much of the avifauna has simply never been assessed.
  cat("\n-- Global IUCN categories, present taxa (join sanity check) --\n")
  print(unique(taxa_long[is_present == TRUE, .(CD_REF, lr_mondiale)]
  )[, .N, by = lr_mondiale][order(-N)])
}

#--- Export ------------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

out_stem <- paste0(TAXON_LABEL, "_om_", TAXREF_VERSION)

# Single writer, so every output shares the same separator and BOM setting.
write_csv_out <- function(x, filename) {
  path <- file.path(OUTPUT_DIR, filename)
  fwrite(x, path, sep = CSV_SEP, bom = CSV_BOM)
  path
}

write_csv_out(taxa_long, paste0(out_stem, "_long.csv"))

# Wide format (taxon x territory matrix of status codes), handy for sharing
wide_id <- intersect(c("CD_REF", "LB_NOM", "LB_AUTEUR", "NOM_VERN", "RANG",
                       "rank_label", "ORDRE", "FAMILLE", "HABITAT",
                       "habitat_label", "habitat_class",
                       "lr_mondiale", "lr_europeenne", "eee_listes",
                       "n_territoires"),
                     names(taxa_long))

taxa_wide <- dcast(
  taxa_long,
  as.formula(paste(paste(wide_id, collapse = " + "), "~ territory")),
  value.var = "status_code"
)

write_csv_out(taxa_wide, paste0(out_stem, "_wide.csv"))

# Code reference tables, exported so the labels travel with the data
write_csv_out(data.table(status_code  = names(STATUS_LABELS),
                         status_label = unname(STATUS_LABELS),
                         status_class = unname(STATUS_CLASS[names(STATUS_LABELS)])),
              "ref_status_codes.csv")

write_csv_out(data.table(habitat_code  = names(HABITAT_LABELS),
                         habitat_label = unname(HABITAT_LABELS),
                         habitat_class = unname(HABITAT_CLASS[names(HABITAT_LABELS)])),
              "ref_habitat_codes.csv")

write_csv_out(data.table(rank_code  = names(RANK_LABELS),
                         rank_label = unname(RANK_LABELS)),
              "ref_rank_codes.csv")

# --- Data dictionary ---------------------------------------------------------
# One row per column of the exported dataset. Needed to hand the data over to
# someone else, and required when depositing a dataset with a DOI.

COLUMN_DESCRIPTIONS <- c(
  CD_REF          = "Identifiant du taxon de reference (cle de jointure universelle)",
  CD_NOM          = "Identifiant du nom; egal a CD_REF pour les noms valides",
  RANG            = "Code de rang taxonomique (ES = espece, SSES = sous-espece)",
  rank_label      = "Libelle du rang",
  is_species      = "VRAI si le taxon est une espece et non une sous-espece",
  LB_NOM          = "Nom scientifique sans l'autorite",
  LB_AUTEUR       = "Auteur et annee de description",
  nom_autorite    = "Nom scientifique suivi de son autorite (exige par les revues)",
  NOM_VERN        = "Nom vernaculaire francais",
  ORDRE           = "Ordre taxonomique",
  FAMILLE         = "Famille taxonomique",
  HABITAT         = "Code habitat TAXREF (1 a 8)",
  habitat_label   = "Libelle de l'habitat",
  habitat_class   = "Regroupement grossier de l'habitat (marin, terrestre, ...)",
  territory_code  = "Code TAXREF du territoire",
  territory       = "Nom du territoire",
  region          = "Region biogeographique",
  basin           = "Bassin oceanique",
  area_km2        = "Superficie du territoire en km2",
  status_code     = "Code de statut biogeographique TAXREF pour ce territoire",
  status_label    = "Libelle du statut",
  status_class    = "Regroupement grossier du statut",
  lr_mondiale     = "Categorie UICN mondiale",
  lr_europeenne   = "Categorie UICN europeenne",
  lr_france       = "Categorie UICN nationale, chapitre metropolitain (peu pertinent en Outre-mer)",
  lr_territoriale = "Categorie UICN propre a ce territoire",
  lr_source       = "Liste rouge territoriale dont provient l'evaluation",
  eee_listes      = "Reglementation EEE: REGLII interdiction d'introduction, REGLLUTTE lutte",
  eee_territoriale= "Reglementation EEE propre a ce territoire",
  protection      = "Statuts de protection reglementaire",
  directive       = "Directives europeennes citant le taxon",
  conventions     = "Conventions internationales citant le taxon (Bonn, Berne, ...)",
  pna             = "Plans nationaux d'action",
  n_territoires   = "Nombre de territoires d'Outre-mer ou le taxon est present",
  n_regions       = "Nombre de regions biogeographiques occupees",
  n_bassins       = "Nombre de bassins oceaniques occupes",
  est_endemique_stricte = "VRAI si le taxon n'occupe qu'un seul territoire",
  is_present      = "VRAI si le taxon est present aujourd'hui, quelle que soit son origine",
  is_native       = "VRAI si indigene (P, E ou S)",
  is_endemic      = "VRAI si endemique du territoire (E ou Z)",
  is_introduced   = "VRAI si introduit (I, J ou M)",
  is_invasive_taxref = "VRAI si TAXREF le classe envahissant (J) sur ce territoire",
  eee_listee      = "VRAI si le taxon fait l'objet d'une reglementation EEE (introduction ou lutte)",
  menacee_mondiale = "VRAI si CR, EN ou VU sur la liste rouge mondiale",
  menacee_territoriale = "VRAI si CR, EN ou VU sur la liste rouge du territoire",
  est_evaluee     = "VRAI si le taxon a fait l'objet d'au moins une evaluation",
  est_protegee    = "VRAI si le taxon beneficie d'une protection reglementaire",
  is_cryptogenic  = "VRAI si origine indeterminee (C)",
  is_occasional   = "VRAI si presence non etablie (B)",
  is_extinct      = "VRAI si disparu ou eteint (W, X, Y ou Z)",
  is_eroded       = "VRAI si perte de biodiversite averee (W, X ou Z); exclut Y",
  is_uncertain    = "VRAI si statut incertain (D, G ou Q)",
  is_absent       = "VRAI si absent; liste de veille nationale, jamais un signal territorial",
  is_austral      = "VRAI pour les territoires australs (SA, TA)"
)

dictionary <- data.table(
  colonne     = names(taxa_long),
  description = unname(COLUMN_DESCRIPTIONS[names(taxa_long)])
)
dictionary[is.na(description),
           description := "(non documentee -- colonne brute issue de TAXREF)"]

write_csv_out(dictionary, paste0(out_stem, "_dictionnaire.csv"))

cat("\nDone. Files written to ", normalizePath(OUTPUT_DIR),
    " with prefix '", out_stem, "'\n",
    "  ", out_stem, "_long.csv          one row per taxon x territory\n",
    "  ", out_stem, "_wide.csv          taxon x territory matrix\n",
    "  ", out_stem, "_dictionnaire.csv  description of every column\n",
    "  ref_*.csv                        code reference tables\n",
    "CSV written as UTF-8", if (CSV_BOM) " with BOM" else "",
    ", separator '", CSV_SEP, "'\n", sep = "")


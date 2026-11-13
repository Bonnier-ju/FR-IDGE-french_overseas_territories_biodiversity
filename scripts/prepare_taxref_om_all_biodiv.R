# =============================================================================
# FR-IDGE / Overseas component
# FULL BIODIVERSITY extraction from TAXREF for French overseas territories
#
# Goal: extract EVERY taxon of the referential occurring in the 15 French
#       overseas territories -- all kingdoms, all groups -- and export a
#       clean, ready-to-use dataset.
#
# This is the broad counterpart of prepare_taxref_om.R, which extracts one
# taxonomic group at a time. Same logic, same outputs, same code tables;
# only the defaults differ, and a few options are added for the scale:
#   - no taxonomic filter (TAXON_VALUES is NULL)
#   - wide export off by default (one column per territory over hundreds of
#     thousands of rows is expensive and barely usable)
#   - optional per-group export, one file per INPN group
#   - richer summaries: by kingdom, by INPN group, by territory
#
# Expect a long run (several minutes) and large output files.
#
# NOTE: the two scripts share their processing code. A fix applied to one
# should be carried over to the other.
#
# Author: Julien Bonnier
# =============================================================================

library(data.table)

# --- Files -------------------------------------------------------------------

WORKING_DIR <- "C:/Users/jbonnier/OneDrive/University/Projects/FR-IDGE/Axe_2_Pan_OM_oiseaux/data_analysis"

TAXREF_VERSION <- "v18"
TAXREF_FILE    <- "data/raw/TAXREF_v18_2025/TAXREFv18.txt"   # raw TAXREF file (tab-separated)
OUTPUT_DIR     <- "data/clean_allbiodiv"   # kept apart from the bird outputs
CSV_SEP <- ";"
CSV_BOM <- TRUE

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
#
# ALL BIODIVERSITY: set TAXON_VALUES <- NULL to drop the taxonomic filter
# entirely and keep every taxon of the referential. TAXON_COLUMN is then
# ignored. Expect a much larger output -- see EXPORT_WIDE below.
#   All taxa         -> TAXON_VALUES NULL,          TAXON_LABEL "allbiodiv"

# NULL = no taxonomic filter at all. TAXON_COLUMN is then ignored.
# Set them to a group (e.g. "CLASSE" / "Aves") to narrow the run down.

TAXON_COLUMN <- NULL
TAXON_VALUES <- NULL
TAXON_LABEL  <- "allbiodiv"


# --- Taxonomic rank ----------------------------------------------------------
# Ranks to keep. "ES" = species, "SSES" = subspecies.
# Use "ES" alone for a strictly species-level dataset.
#
# Subspecies are kept by default, and this matters a lot in overseas
# territories: on the bird run, dropping them removed 54 % of the endemic
# taxa and 50 % of the subendemic ones. Island differentiation is largely
# infraspecific, and those endemic subspecies are exactly the distinct
# genetic units the indicators are about.
# The output carries RANG and is_species, so species-level counts stay easy:
#   taxa_long[RANG == "ES", uniqueN(CD_REF)]
#
# Across all kingdoms, plants and fungi also carry endemism at the variety
# and form level. Add them if infraspecific plant diversity matters to you:
#   KEEP_RANKS <- c("ES", "SSES", "VAR", "SVAR", "FO")
# This inflates the volume and mixes taxonomic levels, so keep RANG in mind
# whenever you count.

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

# --- Export options ----------------------------------------------------------
# EXPORT_WIDE : the wide table (one row per taxon, one column per territory).
#   Cheap for a single taxonomic group, but costly on a full-referential run
#   (hundreds of thousands of rows). Set to FALSE to skip it.
# Note that on a full run the long file easily exceeds Excel's 1 048 576-row
# limit: work on it in R, or filter before exporting.
#
# Each table is written in up to two formats, both UTF-8 and both carrying
# the BOM set by CSV_BOM:
#   EXPORT_CSV -> ".csv", separator CSV_SEP (";" by default, for Excel)
#   EXPORT_TXT -> ".txt", tab separated, like the raw TAXREF files themselves
#
# SLIM_COLUMNS : drop columns that are redundant or derivable from others
#   (URL and URL_INPN rebuild from CD_NOM, NOM_COMPLET from LB_NOM +
#   LB_AUTEUR, and REGNE/PHYLUM/CLASSE are constant within a narrow run).
#   Cuts the exported text by about 40 % on a broad extraction.

# SPLIT_BY_GROUP : also write one file per INPN group (GROUP2_INPN) into an
#   OUTPUT_DIR/by_group/ subfolder. Far more practical than a single huge
#   file when different people work on different groups.

EXPORT_WIDE    <- FALSE   # off: too costly and barely usable at this scale
EXPORT_CSV     <- TRUE
EXPORT_TXT     <- TRUE
SLIM_COLUMNS   <- FALSE
SPLIT_BY_GROUP <- TRUE

# Columns removed when SLIM_COLUMNS is TRUE.
SLIM_DROP_COLS <- c("URL", "URL_INPN", "NOM_COMPLET", "NOM_VALIDE",
                    "NOMENCLATURAL_COMMENT", "CD_TAXSUP", "CD_BA")

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

t_start <- Sys.time()

cat("Working directory:", getwd(), "\n")

# --- Locate the raw TAXREF file ----------------------------------------------

if (!file.exists(TAXREF_FILE)) {
  stop("TAXREF file not found: ", normalizePath(TAXREF_FILE, mustWork = FALSE),
       "\nExpected either a path relative to the working directory",
       " (e.g. 'data/raw/TAXREFv18.txt') or a full absolute path.",
       "\nDownload TAXREF from https://inpn.mnhn.fr/telechargement/referentielEspece/taxref")
}

cat("TAXREF file:", normalizePath(TAXREF_FILE), "\n")

# --- Load the official code tables shipped with TAXREF, if available ---------
# Each note file is expected to hold the code in its first column and the
# label in its second one. Anything unreadable falls back silently.


read_text_table <- function(path, ...) {
  tab <- fread(path, encoding = "unknown", colClasses = "character", ...)
  for (col in names(tab)) {
    x <- tab[[col]]
    if (!is.character(x)) next
    if (any(!validUTF8(x), na.rm = TRUE)) {
      # The companion note files are Windows-1252, not Latin-1: they use
      # curly quotes and apostrophes, which live in the 0x80-0x9F range that
      # ISO-8859-1 leaves undefined.
      x <- iconv(x, from = "windows-1252", to = "UTF-8", sub = "")
    } else {
      Encoding(x) <- "UTF-8"
    }
    set(tab, j = col, value = x)
  }
  tab[]
}

#Load :
#   statuts_note.csv   ORDRE;STATUT;DESCRIPTION;DEFINITION
#   habitats_note.csv  HABITAT;LB_HABITAT;DEFINITION
#   rangs_note.csv     RG_LEVEL;RANG;DETAIL;DETAIL_EN
# Columns are therefore selected by name, never by position.

read_note_file <- function(filename, code_col, label_col) {
  path <- file.path(dirname(TAXREF_FILE), filename)
  if (!file.exists(path)) return(NULL)
  
  tryCatch({
    tab <- read_text_table(path, sep = ";")
    setnames(tab, trimws(sub("^﻿", "", names(tab))))   # strip a BOM
    
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

required_cols <- c(if (!is.null(TAXON_VALUES)) TAXON_COLUMN,
                   "CD_NOM", "CD_REF", "RANG", "LB_NOM",
                   names(territories))

missing_cols <- setdiff(required_cols, names(taxref))
if (length(missing_cols) > 0) {
  stop("Missing columns in TAXREF: ", paste(missing_cols, collapse = ", "),
       "\nCheck the column names against your TAXREF version.")
}

# --- Filter: taxonomic group, rank, valid names ------------------------------

if (is.null(TAXON_VALUES)) {
  taxa <- taxref[RANG %in% KEEP_RANKS]              # whole referential
  taxon_desc <- "no taxonomic filter (all taxa)"
} else {
  taxa <- taxref[get(TAXON_COLUMN) %in% TAXON_VALUES & RANG %in% KEEP_RANKS]
  taxon_desc <- paste0(TAXON_COLUMN, " = ",
                       paste(TAXON_VALUES, collapse = ", "))
}

if (KEEP_VALID_ONLY) {
  taxa <- taxa[CD_NOM == CD_REF]
}

if (nrow(taxa) == 0) {
  stop("No taxon found for ", taxon_desc,
       ". Check the spelling against the values present in TAXREF.")
}

cat("Taxa kept (", taxon_desc, "): ", nrow(taxa), "\n", sep = "")

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

taxa_long[, territory     := territories[as.character(territory_code)]]
taxa_long[, status_label  := STATUS_LABELS[status_code]]
taxa_long[, status_class  := STATUS_CLASS[status_code]]
taxa_long[, habitat_label := HABITAT_LABELS[as.character(HABITAT)]]
taxa_long[, rank_label    := RANK_LABELS[as.character(RANG)]]

# Flags -- all derived from status_code, none of them drops a record.
taxa_long[, is_present    := status_code %in% PRESENT_CODES]
taxa_long[, is_native     := status_code %in% NATIVE_CODES]
taxa_long[, is_endemic    := status_code %in% c("E", "Z")]
taxa_long[, is_introduced := status_code %in% INTRODUCED_CODES]
taxa_long[, is_cryptogenic := status_code == "C"]
taxa_long[, is_occasional := status_code == "B"]
taxa_long[, is_extinct    := status_code %in% EXTINCT_CODES]
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

# --- Column order ------------------------------------------------------------

front_cols <- c("CD_REF", "CD_NOM", "RANG", "rank_label", "is_species",
                "LB_NOM", "NOM_VERN", "ORDRE", "FAMILLE",
                "HABITAT", "habitat_label",
                "territory_code", "territory",
                "status_code", "status_label", "status_class",
                "is_present", "is_native", "is_endemic", "is_introduced",
                "is_cryptogenic", "is_occasional", "is_extinct",
                "is_uncertain", "is_absent", "is_austral")
setcolorder(taxa_long, intersect(front_cols, names(taxa_long)))

# --- Summaries ---------------------------------------------------------------

cat("\nTaxon x territory records:", nrow(taxa_long), "\n")
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

# On a broad run, the INPN groupings are the most readable overview.
if ("REGNE" %in% names(taxa_long)) {
  cat("\n-- Present taxa by kingdom --\n")
  print(taxa_long[is_present == TRUE,
                  .(n_taxa = uniqueN(CD_REF)),
                  by = REGNE][order(-n_taxa)])
}

if ("GROUP2_INPN" %in% names(taxa_long)) {
  cat("\n-- Present taxa by INPN group (GROUP2_INPN) --\n")
  print(taxa_long[is_present == TRUE,
                  .(n_taxa = uniqueN(CD_REF)),
                  by = GROUP2_INPN][order(-n_taxa)])
}

cat("\n-- Present taxa by territory --\n")
print(taxa_long[is_present == TRUE,
                .(n_taxa    = uniqueN(CD_REF),
                  n_species = uniqueN(CD_REF[RANG == "ES"]),
                  n_endemic = uniqueN(CD_REF[status_code == "E"])),
                by = territory][order(-n_taxa)])

#--- Export ------------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

out_stem <- paste0(TAXON_LABEL, "_om_", TAXREF_VERSION)

# Single writer, so every output shares the same settings.
# It takes a file name WITHOUT extension and writes the enabled formats.
write_table_out <- function(x, stem, dir = OUTPUT_DIR) {
  written <- character(0)
  if (EXPORT_CSV) {
    path <- file.path(dir, paste0(stem, ".csv"))
    fwrite(x, path, sep = CSV_SEP, bom = CSV_BOM)
    written <- c(written, path)
  }
  if (EXPORT_TXT) {
    path <- file.path(dir, paste0(stem, ".txt"))
    fwrite(x, path, sep = "\t", bom = CSV_BOM)
    written <- c(written, path)
  }
  written
}

# Drop the redundant columns before writing anything out.
if (SLIM_COLUMNS) {
  drop_now <- intersect(SLIM_DROP_COLS, names(taxa_long))
  if (length(drop_now) > 0) {
    taxa_long[, (drop_now) := NULL]
    cat("Dropped", length(drop_now), "redundant columns:",
        paste(drop_now, collapse = ", "), "\n")
  }
}

write_table_out(taxa_long, paste0(out_stem, "_long"))

if (nrow(taxa_long) > 1048576) {
  cat("NOTE: the long file exceeds Excel's row limit (", nrow(taxa_long),
      " rows). Work on it in R, or filter before opening it.\n", sep = "")
}

# Wide format (taxon x territory matrix of status codes), handy for sharing
if (EXPORT_WIDE) {
  taxa_wide <- dcast(
    taxa_long,
    CD_REF + LB_NOM + NOM_VERN + RANG + rank_label + ORDRE + FAMILLE + HABITAT + habitat_label ~ territory,
    value.var = "status_code"
  )
  
  write_table_out(taxa_wide, paste0(out_stem, "_wide"))
} else {
  cat("Wide export skipped (EXPORT_WIDE = FALSE)\n")
}

# Code reference tables, exported so the labels travel with the data
write_table_out(data.table(status_code  = names(STATUS_LABELS),
                           status_label = unname(STATUS_LABELS),
                           status_class = unname(STATUS_CLASS[names(STATUS_LABELS)])),
                "ref_status_codes")

write_table_out(data.table(habitat_code  = names(HABITAT_LABELS),
                           habitat_label = unname(HABITAT_LABELS)),
                "ref_habitat_codes")

write_table_out(data.table(rank_code  = names(RANK_LABELS),
                           rank_label = unname(RANK_LABELS)),
                "ref_rank_codes")

# --- One file per INPN group -------------------------------------------------

if (SPLIT_BY_GROUP && "GROUP2_INPN" %in% names(taxa_long)) {
  group_dir <- file.path(OUTPUT_DIR, "by_group")
  dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
  
  groups <- sort(unique(taxa_long$GROUP2_INPN))
  groups <- groups[!is.na(groups) & groups != ""]
  
  cat("\nWriting one file per INPN group into ", group_dir,
      " (", length(groups), " groups)\n", sep = "")
  
  for (g in groups) {
    # Accent-free, filesystem-safe file name
    slug <- iconv(g, to = "ASCII//TRANSLIT")
    if (is.na(slug)) slug <- g
    slug <- gsub("[^A-Za-z0-9]+", "_", slug)
    slug <- gsub("^_+|_+$", "", slug)
    
    sub <- taxa_long[GROUP2_INPN == g]
    write_table_out(sub, paste0(TAXON_LABEL, "_", slug, "_", TAXREF_VERSION),
                    dir = group_dir)
    cat("  ", slug, ": ", nrow(sub), " records, ",
        uniqueN(sub$CD_REF), " taxa\n", sep = "")
  }
}

cat("\nDone. Files written to ", normalizePath(OUTPUT_DIR),
    " with prefix '", out_stem, "'\n",
    "Formats: ",
    paste(c(if (EXPORT_CSV) paste0(".csv (sep '", CSV_SEP, "')"),
            if (EXPORT_TXT) ".txt (tab)"), collapse = " + "),
    " -- UTF-8", if (CSV_BOM) " with BOM" else "", "\n", sep = "")

cat("Total run time: ",
    round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1),
    " min\n", sep = "")


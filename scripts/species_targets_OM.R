# =============================================================================
# FR-IDGE / Overseas component
# Species targets for the overseas territories: how many species to monitor,
# in which territory and in which taxonomic group
#
# Working targets, not project decisions: set in 2026, confronted with the data
# that can actually be mobilised, fixed in 2028. Every choice that shapes the
# targets is a parameter at the top, so a recalibration is a one-line change,
# logged in Notion (Axe 4 > Objectifs d'espèces suivies).
#
# Reference rule -- Hebert K., Pollock L., Hoban S. 2026. How much monitoring
# is needed to reliably track progress towards genetic diversity targets?
# Biological Conservation 317, 111824.
# https://doi.org/10.1016/j.biocon.2026.111824
#
# Author: Julien Bonnier
# =============================================================================

# =============================================================================
# 01. SETUP, DATA IMPORT AND SPECIES POOLS
# =============================================================================

library(data.table)
library(ggplot2)

# --- Paths -------------------------------------------------------------------

WORKING_DIR <- "C:/Users/jbonnier/OneDrive/University/Projects/FR-IDGE/Axe_2_Pan_OM_oiseaux/data_analysis"

DATA_FILE   <- "data/clean_allbiodiv/allbiodiv_om_v18_long.csv"
META_FILE   <- "data/territories_metadata.csv"
THEME_FILE  <- "scripts/fridge_theme.R"
RESULTS_DIR <- "species_targets_OM_results"
FIGURES_DIR <- file.path(RESULTS_DIR, "figures")
TABLES_DIR  <- file.path(RESULTS_DIR, "tables")
CSV_SEP     <- ";"

setwd(WORKING_DIR)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR,  recursive = TRUE, showWarnings = FALSE)

if (!file.exists(THEME_FILE)) stop("Plotting charter not found: ", THEME_FILE)
source(THEME_FILE, encoding = "UTF-8")

# --- Target-setting parameters ------------------------------------------------
# Pool definition (decided 13/11/2026):
#   native species (P, E, S), species rank, animals, plants and fungi;
#   marine species included -- reef biota is a territorial responsibility;
#   migrants on passage included -- they are coded P, and their numbers on
#   passage also inform the conservation state of the territory.

NATIVE_CODES  <- c("P", "E", "S")
TARGET_RANK   <- "ES"
KEEP_KINGDOMS <- c("Animalia", "Plantae", "Fungi")   # Hebert's empirical scope
DROP_TAXA     <- c("Homo sapiens")                   # coded native, not a target

ENVELOPES     <- c(100, 150, 200, 300)               # overall overseas targets

STRATUM_ORDER <- c("Oiseaux", "Mammifères", "Reptiles et amphibiens",
                   "Poissons", "Insectes", "Mollusques", "Crustacés",
                   "Autres invertébrés", "Plantes vasculaires",
                   "Bryophytes et algues", "Champignons et lichens")

# --- Read the extraction and the territory metadata ----------------------------

taxa <- fread(DATA_FILE, sep = ";", encoding = "UTF-8", na.strings = c("", "NA"))
meta <- fread(META_FILE, sep = ";", encoding = "UTF-8", na.strings = c("", "NA"))

unmatched <- setdiff(unique(taxa$territory), meta$territory)
if (length(unmatched) > 0) {
  stop("Territories absent from the metadata table: ",
       paste(unmatched, collapse = " | "))
}

taxa <- merge(taxa, meta, by = "territory", all.x = TRUE, sort = FALSE)

REGION_ORDER <- c("Guyane", "Caraibes", "Atlantique NO",
                  "Ocean Indien", "Austral", "Pacifique")

meta[, region := factor(region, levels = REGION_ORDER)]
short_order <- meta[order(region, -area_km2), territory_short_fr]

taxa[, region             := factor(region, levels = REGION_ORDER)]
taxa[, territory_short_fr := factor(territory_short_fr, levels = short_order)]

# --- Coarse taxonomic strata ----------------------------------------------------
# Named groups first; everything else falls back on its kingdom, so no
# GROUP2_INPN label can be lost. Check the audit table printed below.

assign_stratum <- function(group, kingdom) {
  fcase(
    group == "Oiseaux",                                       "Oiseaux",
    group == "Mammifères",                                    "Mammifères",
    group %in% c("Reptiles", "Amphibiens"),                   "Reptiles et amphibiens",
    grepl("Poissons", group),                                 "Poissons",
    group == "Insectes",                                      "Insectes",
    grepl("Gastéropodes|Bivalves|Céphalopodes|Mollusques|Polyplacophores|Scaphopodes",
          group),                                             "Mollusques",
    group == "Crustacés",                                     "Crustacés",
    grepl("Angiospermes|Gymnospermes|Ptéridophytes", group),  "Plantes vasculaires",
    kingdom == "Animalia",                                    "Autres invertébrés",
    kingdom == "Plantae",                                     "Bryophytes et algues",
    kingdom == "Fungi",                                       "Champignons et lichens",
    default = NA_character_
  )
}

# --- The species pool ------------------------------------------------------------

sp <- taxa[status_code %in% NATIVE_CODES &
             RANG == TARGET_RANK &
             REGNE %in% KEEP_KINGDOMS &
             !LB_NOM %in% DROP_TAXA]

sp[, stratum := factor(assign_stratum(GROUP2_INPN, REGNE), levels = STRATUM_ORDER)]

if (sp[is.na(stratum), .N] > 0) {
  stop("Some records have no stratum -- check assign_stratum():\n",
       paste(unique(sp[is.na(stratum), GROUP2_INPN]), collapse = " | "))
}

cat("\n== Species pool ==\n")
cat("Native species (rank ES), all kingdoms:",
    format(taxa[status_code %in% NATIVE_CODES & RANG == TARGET_RANK,
                uniqueN(CD_REF)], big.mark = " "), "\n")
cat("Kept (animals, plants, fungi)         :",
    format(uniqueN(sp$CD_REF), big.mark = " "), "\n")

cat("\n-- Native species dropped, by kingdom --\n")
print(taxa[status_code %in% NATIVE_CODES & RANG == TARGET_RANK &
             !REGNE %in% KEEP_KINGDOMS,
           .(n_species = uniqueN(CD_REF)), by = REGNE][order(-n_species)])

# Audit: every INPN group and the stratum it landed in. Read this first.
stratum_map <- sp[, .(n_species = uniqueN(CD_REF)), by = .(stratum, GROUP2_INPN)]
setorder(stratum_map, stratum, -n_species)

cat("\n-- Mapping of INPN groups onto strata --\n")
print(stratum_map, nrows = 200)

fwrite(stratum_map, file.path(TABLES_DIR, "01_stratum_mapping.csv"),
       sep = CSV_SEP, bom = TRUE)

# --- Pools: territory x stratum -------------------------------------------------

pools <- sp[, .(pool = uniqueN(CD_REF)),
            by = .(region, territory_short_fr, stratum)]

pools_wide <- dcast(pools, territory_short_fr ~ stratum,
                    value.var = "pool", fill = 0)
pools_wide[, Total := rowSums(.SD), .SDcols = -"territory_short_fr"]

cat("\n-- Native species by territory and stratum --\n")
print(pools_wide)

fwrite(pools, file.path(TABLES_DIR, "01_species_pools.csv"),
       sep = CSV_SEP, bom = TRUE)


# =============================================================================
# 02. HEBERT IDEAL: HOW MANY SPECIES EACH TERRITORY WOULD NEED
#
# The rule is applied to the pool of the WHOLE territory (reporting level
# decided 13/11/2026), then split between strata in proportion to their
# richness, i.e. a stratified random sample of the territory's species.
# This is the ideal "if data were not missing"; no envelope yet.
# =============================================================================

# Share of a pool to monitor for a Ne > 500 indicator with a 5 % risk of error,
# random species selection, annual monitoring (Hebert et al. 2026, section 3.4
# and Fig. 6). Read as a step function. The 600-1 000 gap is not reported and
# is bridged by log-linear interpolation between 23 % and 8 %. Tested pools
# range from 100 to 5 000 species: above 5 000 the 8 % is an extrapolation.

HEBERT_MAX_TESTED <- 5000

hebert_share <- function(n) {
  interp <- exp(log(0.23) + (log(n) - log(600)) / (log(1000) - log(600)) *
                  (log(0.08) - log(0.23)))
  fcase(n < 100,  0.56,
        n < 300,  0.31,
        n < 600,  0.23,
        n < 1000, interp,
        default = 0.08)
}

# Largest-remainder rounding: integer shares that always sum to the total.
# Reused at every allocation step of the script.
allocate_lr <- function(weights, total) {
  if (total == 0 || sum(weights) == 0) return(integer(length(weights)))
  raw   <- total * weights / sum(weights)
  alloc <- floor(raw)
  short <- total - sum(alloc)
  if (short > 0) {
    idx <- order(-(raw - alloc))[seq_len(short)]
    alloc[idx] <- alloc[idx] + 1
  }
  as.integer(alloc)
}

# --- Territory ideal ----------------------------------------------------------

terr_ideal <- pools[, .(pool = sum(pool)), by = .(region, territory_short_fr)]
terr_ideal[, share_ideal  := hebert_share(pool)]
terr_ideal[, n_ideal      := as.integer(ceiling(share_ideal * pool))]
terr_ideal[, hebert_range := fifelse(pool > HEBERT_MAX_TESTED,
                                     "Extrapolé (> 5 000)", "Gamme testée")]
setorder(terr_ideal, -n_ideal)

total_pool  <- terr_ideal[, sum(pool)]
total_ideal <- terr_ideal[, sum(n_ideal)]

cat("\n== Hebert ideal by territory ==\n")
print(terr_ideal[, .(territory_short_fr, pool,
                     share_pct = round(100 * share_ideal, 1),
                     n_ideal, hebert_range)])

cat("\nAll overseas territories:", format(total_ideal, big.mark = " "),
    "species to monitor, out of", format(total_pool, big.mark = " "), "\n")
for (env in ENVELOPES) {
  cat(sprintf("  envelope %3d species = %5.1f %% of the ideal\n",
              env, 100 * env / total_ideal))
}

# --- Ideal split between strata, within each territory -------------------------

cells <- merge(pools,
               terr_ideal[, .(territory_short_fr, n_ideal_terr = n_ideal)],
               by = "territory_short_fr")
cells[, n_ideal := allocate_lr(pool, n_ideal_terr[1]), by = territory_short_fr]

cat("\n-- Guyane: ideal split between strata --\n")
print(cells[territory_short_fr == "Guyane",
            .(stratum, pool, n_ideal)][order(-n_ideal)])

ideal_wide <- dcast(cells, territory_short_fr ~ stratum,
                    value.var = "n_ideal", fill = 0)

cat("\n-- Ideal by territory and stratum --\n")
print(ideal_wide)

fwrite(terr_ideal, file.path(TABLES_DIR, "02_hebert_ideal_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)
fwrite(cells, file.path(TABLES_DIR, "02_hebert_ideal_by_cell.csv"),
       sep = CSV_SEP, bom = TRUE)

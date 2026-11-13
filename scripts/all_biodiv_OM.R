# =============================================================================
# FR-IDGE / Overseas component
# All-biodiversity analysis of French overseas territories (TAXREF v18)
#
# Author: Julien Bonnier
# =============================================================================

# =============================================================================
# 01. SETUP AND DATA IMPORT
# =============================================================================

library(data.table)
library(ggplot2)

# --- Paths -------------------------------------------------------------------
# Everything below is relative to WORKING_DIR. Forward slashes on Windows too.

WORKING_DIR  <- "C:/Users/jbonnier/OneDrive/University/Projects/FR-IDGE/Axe_2_Pan_OM_oiseaux/data_analysis"

DATA_FILE    <- "data/clean_allbiodiv/allbiodiv_om_v18_long.csv"
META_FILE    <- "data/territories_metadata.csv"
THEME_FILE   <- "scripts/fridge_theme.R"        # adjust if stored elsewhere
RESULTS_DIR  <- "all_biodiv_OM_results"
FIGURES_DIR  <- file.path(RESULTS_DIR, "figures")
TABLES_DIR   <- file.path(RESULTS_DIR, "tables")
CSV_SEP <- ";"

setwd(WORKING_DIR)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR,  recursive = TRUE, showWarnings = FALSE)

cat("Working directory:", getwd(), "\n")

# --- Plotting charter --------------------------------------------------------

if (!file.exists(THEME_FILE)) {
  stop("Plotting charter not found: ", THEME_FILE,
       "\nPut fridge_theme.R there, or point THEME_FILE at it.")
}
source(THEME_FILE, encoding = "UTF-8")

# --- Read the extraction -----------------------------------------------------
# The file is UTF-8 with a BOM and ";" separated; fread handles both.
# It is large, so this takes a moment.

taxa <- fread(DATA_FILE, sep = ";", encoding = "UTF-8", na.strings = c("", "NA"))

cat("Records read:", format(nrow(taxa), big.mark = " "),
    "|", ncol(taxa), "columns\n")

meta <- fread(META_FILE, sep = ";", encoding = "UTF-8", na.strings = c("", "NA"))

# Guard: every territory of the extraction must be matched, or later figures
# would silently drop records.
unmatched <- setdiff(unique(taxa$territory), meta$territory)
if (length(unmatched) > 0) {
  stop("Territories absent from the metadata table: ",
       paste(unmatched, collapse = " | "),
       "\nCheck that the labels match the ones in OM_TERRITORIES.")
}

taxa <- merge(taxa, meta, by = "territory", all.x = TRUE, sort = FALSE)

cat("Metadata joined:", nrow(meta), "territories,",
    ncol(meta) - 1, "attributes added\n")

# --- Factor ordering ---------------------------------------------------------
# Fixed level orders so every figure of the project reads the same way.
# Territories are grouped by region, then by decreasing area within it.

REGION_ORDER <- c("Guyane", "Caraibes", "Atlantique NO",
                  "Ocean Indien", "Austral", "Pacifique")

meta[, region := factor(region, levels = REGION_ORDER)]
territory_order <- meta[order(region, -area_km2), territory]

taxa[, territory := factor(territory, levels = territory_order)]
taxa[, region    := factor(region,    levels = REGION_ORDER)]
taxa[, basin     := factor(basin, levels = c("Atlantique", "Indien",
                                             "Pacifique", "Austral"))]

# --- Analysis subsets --------------------------------------------------------
# Counting convention agreed for the whole project:
#   unit    = taxon (species AND subspecies together, counted by CD_REF)
#   scope   = taxa actually present today (is_present), whatever their origin
# "taxa" keeps every record, including absences, for data-availability work.

pres <- taxa[is_present == TRUE]

cat("\nPresent taxon x territory records:",
    format(nrow(pres), big.mark = " "), "\n")
cat("Distinct taxa present (all ranks):",
    format(uniqueN(pres$CD_REF), big.mark = " "), "\n")
cat("  of which species (RANG = ES)   :",
    format(pres[RANG == "ES", uniqueN(CD_REF)], big.mark = " "), "\n")
cat("  of which subspecies (SSES)     :",
    format(pres[RANG == "SSES", uniqueN(CD_REF)], big.mark = " "), "\n")

# --- Sanity checks -----------------------------------------------------------

cat("\n-- Present taxa by territory --\n")
print(pres[, .(n_taxa = uniqueN(CD_REF)), by = .(region, territory)][order(region, -n_taxa)])

cat("\n-- Present taxa by INPN group --\n")
print(pres[, .(n_taxa = uniqueN(CD_REF)), by = GROUP2_INPN][order(-n_taxa)])

cat("\n-- Coverage of the fields used downstream (share of NA) --\n")
for (col in c("GROUP2_INPN", "REGNE", "HABITAT", "status_code")) {
  if (col %in% names(pres)) {
    cat(sprintf("  %-14s %5.1f %% missing\n",
                col, 100 * mean(is.na(pres[[col]]))))
  }
}


# =============================================================================
# 02. TAXONOMIC PROFILE BY TERRITORY
#
# Question: does each territory have a distinct taxonomic profile, or are they
#           all dominated by insects and plants?
# =============================================================================

# --- Axis ordering -----------------------------------------------------------
# territory_short_fr is what the figures display, so it needs the same level
# order as `territory`. Reversed, because ggplot draws the first level at the
# bottom of a discrete y axis.

short_order <- meta[order(region, -area_km2), territory_short_fr]
pres[, territory_short_fr := factor(territory_short_fr,
                                    levels = rev(short_order))]

# --- Grouping decision -------------------------------------------------------
# GROUP2_INPN holds 34 categories; the charter allows 6 colours. The N largest
# groups are named, everything else is pooled.
#
# Note on "Autres": INPN already uses that label for its own residual group
# (sponges, worms, minor phyla -- 9 521 taxa, 3rd largest overall). 

N_NAMED_GROUPS <- 5
POOL_LABEL     <- "Autres et groupes mineurs"
INPN_RESIDUAL  <- "Autres"

group_totals <- pres[, .(n_taxa = uniqueN(CD_REF)), by = GROUP2_INPN][order(-n_taxa)]

named_groups <- group_totals[GROUP2_INPN != INPN_RESIDUAL
][seq_len(N_NAMED_GROUPS), GROUP2_INPN]

pres[, group_plot := factor(
  fifelse(GROUP2_INPN %in% named_groups, GROUP2_INPN, POOL_LABEL),
  levels = c(named_groups, POOL_LABEL)
)]

cat("\nNamed groups:", paste(named_groups, collapse = ", "), "\n")
cat("Pooled into '", POOL_LABEL, "': ",
    group_totals[!GROUP2_INPN %in% named_groups, .N], " groups, ",
    format(pres[group_plot == POOL_LABEL, uniqueN(CD_REF)], big.mark = " "),
    " taxa\n", sep = "")

# --- Table: taxa per territory x group ---------------------------------------

tax_profile <- pres[, .(n_taxa = uniqueN(CD_REF)),
                    by = .(region, territory, territory_short_fr, group_plot)]

tax_profile[, total_territory := sum(n_taxa), by = territory]
tax_profile[, share := n_taxa / total_territory]

tax_profile_wide <- dcast(tax_profile,
                          region + territory + total_territory ~ group_plot,
                          value.var = "n_taxa", fill = 0)
setorder(tax_profile_wide, region, -total_territory)

fwrite(tax_profile_wide,
       file.path(TABLES_DIR, "02_taxonomic_profile_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Dominant group in each territory --\n")
print(tax_profile[, .SD[which.max(share)], by = territory
][order(-share),
  .(territory, group_plot,
    pct = round(100 * share, 1), total_territory)])

# --- Figure 01: taxonomic profile --------------------------------------------
# Stacked shares, so profiles stay comparable despite a 30-fold range in
# richness; the absolute total is printed at the end of each bar so the
# reader keeps the magnitude in view.

FIG_ID    <- "fig01_profil_taxonomique"
FIG_TITLE <- "Profil taxonomique des territoires d'Outre-mer"

fig_palette <- c(setNames(FRIDGE_CAT[seq_along(named_groups)], named_groups),
                 setNames(FRIDGE_OTHER, POOL_LABEL))

totals_lab <- unique(tax_profile[, .(region, territory_short_fr,
                                     total_territory)])

p_profile <- ggplot(tax_profile,
                    aes(x = n_taxa, y = territory_short_fr, fill = group_plot)) +
  geom_col(position = "fill", width = 0.72) +
  geom_text(data = totals_lab,
            aes(x = 1, y = territory_short_fr,
                label = format(total_territory, big.mark = " ")),
            inherit.aes = FALSE,
            hjust = -0.12, size = 2.9, colour = FRIDGE_INK2) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.14))) +
  scale_fill_manual(values = fig_palette) +
  labs(
    title    = FIG_TITLE,
    subtitle = paste("Part de chaque grand groupe taxonomique parmi les taxons",
                     "présents  ·  effectif total en bout de barre"),
    x = NULL, y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · taxons présents, espèces et ",
                     "sous-espèces confondues · regroupement GROUP2_INPN.\n",
                     "La richesse reflète aussi l'effort d'inventaire, très ",
                     "inégal entre territoires.")
  ) +
  theme_fridge(grid = "x") +
  theme(strip.placement   = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        panel.spacing.y   = unit(0.6, "lines"),
        legend.position   = "top")

fridge_save(p_profile, FIG_ID, width = 10, height = 6.5, dir = FIGURES_DIR)

cat("\nFigure written:", file.path(FIGURES_DIR, paste0(FIG_ID, ".png")), "\n")

# --- Composition of the pooled category, per territory -----------------------

pool_detail <- pres[group_plot == POOL_LABEL,
                    .(n_taxa = uniqueN(CD_REF)),
                    by = .(region, territory, GROUP2_INPN)]
pool_detail[, total_pool := sum(n_taxa), by = territory]
pool_detail[, share_pool := round(100 * n_taxa / total_pool, 1)]

setorder(pool_detail, territory, -n_taxa)

cat("\n-- Top 5 groups inside the pool, per territory --\n")
print(pool_detail[, head(.SD, 5), by = .(region, territory),
                  .SDcols = c("GROUP2_INPN", "n_taxa", "share_pool")])

fwrite(pool_detail,
       file.path(TABLES_DIR, "03_pool_composition_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)

# --- Figure 02: group x territory matrix, no pooling -------------------------
# A sequential ramp escapes the 6-colour limit, so every group can be shown.
# A group is kept when it reaches SHARE_THRESHOLD in at least one territory --
# territory-wise, not overall, so locally dominant groups survive.

SHARE_THRESHOLD <- 0.03
FIG_ID2    <- "fig02_matrice_groupes_territoires"
FIG_TITLE2 <- "Composition taxonomique détaillée des territoires d'Outre-mer"

full_profile <- pres[, .(n_taxa = uniqueN(CD_REF)),
                     by = .(region, territory, territory_short_fr, GROUP2_INPN)]
full_profile[, share := n_taxa / sum(n_taxa), by = territory]

keep_groups <- full_profile[, .(mx = max(share)), by = GROUP2_INPN
][mx >= SHARE_THRESHOLD, GROUP2_INPN]

hm <- full_profile[GROUP2_INPN %in% keep_groups]
hm[GROUP2_INPN == INPN_RESIDUAL, GROUP2_INPN := "Autres (INPN)"]

group_order <- hm[, .(tot = sum(n_taxa)), by = GROUP2_INPN][order(-tot), GROUP2_INPN]
hm[, GROUP2_INPN := factor(GROUP2_INPN, levels = group_order)]

cat("\nGroups kept in the matrix:", length(keep_groups), "of",
    uniqueN(full_profile$GROUP2_INPN), "\n")

p_heat <- ggplot(hm, aes(x = GROUP2_INPN, y = territory_short_fr, fill = share)) +
  geom_tile(colour = FRIDGE_SURF, linewidth = 0.9) +
  geom_text(data = hm[share >= 0.05],
            aes(label = round(100 * share), colour = share > 0.28),
            size = 2.5, show.legend = FALSE) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_gradientn(colours = FRIDGE_SEQ, transform = "sqrt",
                       labels = scales::percent_format(accuracy = 1),
                       name = NULL) +
  scale_colour_manual(values = c(`FALSE` = FRIDGE_INK, `TRUE` = "white")) +
  labs(
    title    = FIG_TITLE2,
    subtitle = paste0("Part de chaque groupe dans les taxons présents du ",
                      "territoire  ·  valeurs affichées à partir de 5 %"),
    x = NULL, y = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · taxons présents, espèces et ",
                     "sous-espèces confondues.\nGroupes retenus : ceux ",
                     "atteignant ", 100 * SHARE_THRESHOLD,
                     " % dans au moins un territoire · échelle en racine carrée.")
  ) +
  theme_fridge(grid = "none") +
  theme(axis.text.x       = element_text(angle = 45, hjust = 1, size = 10, vjust = 1),
        strip.placement   = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        panel.spacing.y   = unit(0.4, "lines"),
        legend.position   = "right",
        legend.key.height = unit(2, "lines"))
p_heat

fridge_save(p_heat, FIG_ID2, width = 11, height = 7, dir = FIGURES_DIR)


# =============================================================================
# 04. MARINE VS CONTINENTAL PROFILE
#
# Question: are the territory profiles driven by biology, or by what has
#           actually been surveyed? HABITAT is a TAXREF field, filled for
#           100 % of the records, so it tests this directly.
# =============================================================================

# TAXREF habitat codes regrouped into 5 readable classes (8 codes, 6 colours).
HABITAT_CLASS <- c(
  "1" = "Marin strict",
  "4" = "Marin et continental",
  "5" = "Marin et continental",
  "6" = "Eau saumâtre",
  "2" = "Eau douce",
  "3" = "Terrestre / continental",
  "7" = "Terrestre / continental",
  "8" = "Terrestre / continental"
)
HABITAT_ORDER <- c("Marin strict", "Marin et continental", "Eau saumâtre",
                   "Eau douce", "Terrestre / continental")

pres[, habitat_class := factor(HABITAT_CLASS[as.character(HABITAT)],
                               levels = HABITAT_ORDER)]

hab_profile <- pres[, .(n_taxa = uniqueN(CD_REF)),
                    by = .(region, territory, territory_short_fr, habitat_class)]
hab_profile[, total_territory := sum(n_taxa), by = territory]
hab_profile[, share := n_taxa / total_territory]

hab_wide <- dcast(hab_profile, region + territory + total_territory ~ habitat_class,
                  value.var = "n_taxa", fill = 0)
setorder(hab_wide, region, -total_territory)

fwrite(hab_wide, file.path(TABLES_DIR, "04_habitat_profile_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Marine share of each territory --\n")
print(hab_profile[habitat_class %in% c("Marin strict", "Marin et continental"),
                  .(marine_pct = round(100 * sum(share), 1)),
                  by = .(region, territory)][order(-marine_pct)])

# --- Figure 03 ---------------------------------------------------------------

FIG_ID3    <- "fig03_profil_habitat"
FIG_TITLE3 <- "Milieux de vie des taxons recensés, par territoire"

hab_palette <- setNames(FRIDGE_CAT[seq_along(HABITAT_ORDER)], HABITAT_ORDER)

p_hab <- ggplot(hab_profile,
                aes(x = n_taxa, y = territory_short_fr, fill = habitat_class)) +
  geom_col(position = "fill", width = 0.72) +
  geom_text(data = unique(hab_profile[, .(region, territory_short_fr,
                                          total_territory)]),
            aes(x = 1, y = territory_short_fr,
                label = format(total_territory, big.mark = " ")),
            inherit.aes = FALSE,
            hjust = -0.12, size = 2.9, colour = FRIDGE_INK2) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.14))) +
  scale_fill_manual(values = hab_palette) +
  labs(
    title    = FIG_TITLE3,
    subtitle = paste("Part marine et continentale des taxons présents",
                     " ·  effectif total en bout de barre"),
    x = NULL, y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN), champ HABITAT renseigné à ",
                     "100 % · taxons présents.\nUne part marine très élevée ",
                     "signale souvent un territoire inventorié surtout par ",
                     "campagnes océanographiques.")
  ) +
  theme_fridge(grid = "x") +
  theme(strip.placement   = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        panel.spacing.y   = unit(0.6, "lines"),
        legend.position   = "top")

p_hab
fridge_save(p_hab, FIG_ID3, width = 10, height = 6.5, dir = FIGURES_DIR)


# =============================================================================
# 05. SPECIES-AREA RELATIONSHIP
#
# Question: does the species-area relationship hold across the overseas
#           territories, and which ones are richer than their size predicts?
#
# Terre Adelie is excluded: its 432 000 km2 are a claimed Antarctic ice
# sector, not a comparable land area (area_comparable = FALSE).
# =============================================================================

terr_rich <- pres[, .(n_taxa = uniqueN(CD_REF)),
                  by = .(territory, territory_short_fr, region,
                         area_km2, area_comparable)]

sar_data <- terr_rich[area_comparable == TRUE]

# --- Fit: log10(S) = log10(c) + z * log10(A) ---------------------------------

sar_fit <- lm(log10(n_taxa) ~ log10(area_km2), data = sar_data)

sar_c  <- coef(sar_fit)[1]
sar_z  <- coef(sar_fit)[2]
sar_r2 <- summary(sar_fit)$r.squared

cat("\n-- Species-area relationship --\n")
cat(sprintf("  log10(S) = %.3f + %.3f * log10(A)   R2 = %.3f   n = %d\n",
            sar_c, sar_z, sar_r2, nrow(sar_data)))
cat("  z typically ranges 0.20-0.35 for island systems\n")
print(summary(sar_fit))

# Residuals: positive = richer than its area predicts
sar_data[, predicted := 10^(sar_c + sar_z * log10(area_km2))]
sar_data[, resid_log := log10(n_taxa) - log10(predicted)]
setorder(sar_data, -resid_log)

fwrite(sar_data, file.path(TABLES_DIR, "05_species_area_residuals.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Residuals (positive = richer than expected) --\n")
print(sar_data[, .(territory, region, area_km2, n_taxa,
                   predicted = round(predicted),
                   resid_log = round(resid_log, 3))])

# --- Figure 04a: the relationship --------------------------------------------

FIG_ID4    <- "fig04a_relation_aire_especes"
FIG_TITLE4 <- "Relation aire-espèces dans les territoires d'Outre-mer"

axis_num <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)

p_sar <- ggplot(sar_data, aes(x = area_km2, y = n_taxa)) +
  geom_abline(intercept = sar_c, slope = sar_z,
              colour = FRIDGE_MUTED, linewidth = 0.6, linetype = "22") +
  geom_point(aes(colour = region), size = 3.2) +
  scale_x_log10(labels = axis_num) +
  scale_y_log10(labels = axis_num) +
  scale_colour_fridge_region() +
  labs(
    title    = FIG_TITLE4,
    subtitle = sprintf(paste("log10(S) = %.2f + %.3f x log10(A)  ·  R² = %.2f",
                             " ·  z attendu entre 0,20 et 0,35 pour des îles"),
                       sar_c, sar_z, sar_r2),
    x = "Superficie terrestre (km², échelle log)",
    y = "Taxons présents (échelle log)",
    colour = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · taxons présents, espèces et ",
                     "sous-espèces confondues · 14 territoires.\n",
                     "Terre Adélie exclue : secteur antarctique revendiqué, ",
                     "surface non comparable.")
  ) +
  theme_fridge(grid = "both")
p_sar

fridge_save(p_sar, FIG_ID4, width = 9, height = 6.5, dir = FIGURES_DIR)

# --- Figure 04b: residuals ---------------------------------------------------
# The same information read as a ranking: who stands above or below the line.

FIG_ID4B    <- "fig04b_residus_aire_especes"
FIG_TITLE4B <- "Richesse observée par rapport à la richesse attendue"

p_resid <- ggplot(sar_data,
                  aes(x = resid_log,
                      y = reorder(territory_short_fr, resid_log),
                      fill = region)) +
  geom_col(width = 0.68) +
  geom_vline(xintercept = 0, colour = FRIDGE_INK2, linewidth = 0.5) +
  scale_fill_fridge_region() +
  scale_x_continuous(expand = expansion(mult = 0.08)) +
  labs(
    title    = FIG_TITLE4B,
    subtitle = paste("Écart à la relation aire-espèces, en log10  ·",
                     "positif = plus riche que ne le prédit sa superficie"),
    x = "Résidu (log10)", y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · 14 territoires.\n",
                     "Un écart positif peut traduire une richesse réelle ",
                     "comme un effort d'inventaire plus soutenu.")
  ) +
  theme_fridge(grid = "x")

p_resid
fridge_save(p_resid, FIG_ID4B, width = 9, height = 5.5, dir = FIGURES_DIR)


# =============================================================================
# 06. ENDEMISM
#
# Question: where is the endemism concentrated, in which groups, and in which
#           habitats? Endemics are the taxa for which a territory carries sole
#           responsibility => they cannot be monitored anywhere else.
# =============================================================================

# --- Diagnostic: which status codes are actually in the data ------------------
# Printed from the data itself rather than assumed, so the labels below can be
# checked against TAXREF's own vocabulary.

cat("\n-- Status codes among present taxon x territory records --\n")
print(pres[, .(n_records = .N, n_taxa = uniqueN(CD_REF)),
           by = .(status_code, status_label)][order(-n_records)])

# --- Endemism flags ----------------------------------------------------------
# E  = endemic to the territory
# S  = subendemic, i.e. restricted to a regional biogeographic area that the
#      territory belongs to (Petites Antilles, Mascareignes, Plateau des
#      Guyanes, ...). See the endemism_area column of the metadata.

ENDEMIC_CODE    <- "E"
SUBENDEMIC_CODE <- "S"

pres[, endemism := factor(
  fifelse(status_code == ENDEMIC_CODE,    "Endemique",
          fifelse(status_code == SUBENDEMIC_CODE, "Subendemique", "Autre")),
  levels = c("Endemique", "Subendemique", "Autre")
)]

# Sanity check: an endemic taxon should occur in one territory only.
endemic_multi <- pres[endemism == "Endemique",
                      .(n_terr = uniqueN(territory)), by = CD_REF][n_terr > 1]

cat("\nEndemic taxa flagged in more than one territory:", nrow(endemic_multi), "\n")
if (nrow(endemic_multi) > 0) {
  cat("  -> worth inspecting; E should be territory-exclusive.\n")
  print(head(endemic_multi[order(-n_terr)], 10))
}

# --- Table: endemism by territory --------------------------------------------

endem_terr <- pres[, .(
  n_total        = uniqueN(CD_REF),
  n_endemic      = uniqueN(CD_REF[endemism == "Endemique"]),
  n_subendemic   = uniqueN(CD_REF[endemism == "Subendemique"])
), by = .(region, territory, territory_short_fr, area_km2, endemism_area)]

endem_terr[, `:=`(
  pct_endemic    = 100 * n_endemic    / n_total,
  pct_subendemic = 100 * n_subendemic / n_total,
  pct_restricted = 100 * (n_endemic + n_subendemic) / n_total
)]

setorder(endem_terr, -pct_endemic)

fwrite(endem_terr, file.path(TABLES_DIR, "06_endemism_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Endemism by territory --\n")
print(endem_terr[, .(territory_short_fr, n_total, n_endemic,
                     pct_endemic    = round(pct_endemic, 1),
                     n_subendemic,
                     pct_restricted = round(pct_restricted, 1))])

# --- Figure 05a: endemism rate by territory ----------------------------------
# Sorted by endemism rate rather than grouped by region: the rate is the
# message here, and the region is carried by the colour.

FIG_ID <- "fig05a_taux_endemisme"

endem_long <- melt(
  endem_terr[, .(territory_short_fr, region, n_total,
                 Endemique = pct_endemic, Subendemique = pct_subendemic)],
  id.vars       = c("territory_short_fr", "region", "n_total"),
  variable.name = "endemism",
  value.name    = "pct"
)

rate_order <- endem_terr[order(pct_restricted), territory_short_fr]
endem_long[, territory_short_fr := factor(territory_short_fr, levels = rate_order)]
endem_long[, endemism := factor(endemism, levels = c("Subendemique", "Endemique"))]

tot_lab <- unique(endem_terr[, .(territory_short_fr, pct_restricted, n_endemic)])
tot_lab[, territory_short_fr := factor(territory_short_fr, levels = rate_order)]

p_endem <- ggplot(endem_long,
                  aes(x = pct, y = territory_short_fr, fill = endemism)) +
  geom_col(width = 0.72) +
  geom_text(data = tot_lab,
            aes(x = pct_restricted, y = territory_short_fr,
                label = paste0(format(n_endemic, big.mark = " "), " end.")),
            inherit.aes = FALSE,
            hjust = -0.15, size = 2.9, colour = FRIDGE_INK2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = function(x) paste0(x, " %")) +
  scale_fill_manual(values = c(Endemique    = FRIDGE_CAT[1],
                               Subendemique = FRIDGE_CAT[3])) +
  labs(
    title    = "Endémisme des territoires d'Outre-mer",
    subtitle = paste("Part des taxons présents endémiques du territoire ou",
                     "subendémiques de sa région"),
    x = "Part des taxons présents", y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · statuts E et S · espèces et ",
                     "sous-espèces confondues.\n",
                     "Le subendémisme se définit par aire biogéographique ",
                     "régionale, variable selon le territoire.")
  ) +
  theme_fridge(grid = "x") +
  theme(legend.position = "top")

p_endem
fridge_save(p_endem, FIG_ID, width = 9.5, height = 6, dir = FIGURES_DIR)

# --- Endemism by taxonomic group ---------------------------------------------
# Computed on the full GROUP2_INPN, not the 6-colour grouping: the question is
# which groups concentrate endemism, and the pool would hide exactly that.

endem_group <- pres[, .(
  n_total   = uniqueN(CD_REF),
  n_endemic = uniqueN(CD_REF[endemism == "Endemique"])
), by = GROUP2_INPN]

endem_group[, pct_endemic := 100 * n_endemic / n_total]
setorder(endem_group, -n_endemic)

fwrite(endem_group, file.path(TABLES_DIR, "06_endemism_by_group.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Endemism by INPN group (groups with >= 50 endemics) --\n")
print(endem_group[n_endemic >= 50,
                  .(GROUP2_INPN, n_total, n_endemic,
                    pct_endemic = round(pct_endemic, 1))])

# --- Figure 05b: richness vs endemism rate across groups ---------------------
# Two independent dimensions: how many taxa a group holds, and what share of
# them is territory-exclusive. Point area is proportional to endemic count.

FIG_ID <- "fig05b_endemisme_par_groupe"

grp_plot <- endem_group[n_total >= 200]

p_grp <- ggplot(grp_plot, aes(x = n_total, y = pct_endemic)) +
  geom_point(aes(size = n_endemic), colour = FRIDGE_CAT[1], alpha = 0.75) +
  ggrepel::geom_text_repel(aes(label = GROUP2_INPN),
                           size = 2.9, colour = FRIDGE_INK2,
                           max.overlaps = 20, seed = 1) +
  scale_x_log10(labels = scales::label_number(big.mark = " ")) +
  scale_y_continuous(labels = function(x) paste0(x, " %")) +
  scale_size_area(max_size = 11,
                  labels = scales::label_number(big.mark = " ")) +
  labs(
    title    = "Richesse et endémisme des grands groupes taxonomiques",
    subtitle = paste("Groupes de plus de 200 taxons présents  ·  l'aire du point",
                     "est proportionnelle au nombre d'endémiques"),
    x = "Taxons présents en Outre-mer (échelle log)",
    y = "Part de taxons endémiques",
    size = "Endémiques",
    caption = paste0("Source : TAXREF v18 (INPN) · statut E · espèces et ",
                     "sous-espèces confondues.\n",
                     "Le taux d'endémisme suit l'inverse de la capacité de ",
                     "dispersion des groupes.")
  ) +
  theme_fridge(grid = "both")

p_grp
fridge_save(p_grp, FIG_ID, width = 9, height = 6, dir = FIGURES_DIR)



# --- Endemism and habitat ----------------------------------------------------
# Expectation to test: endemism should be overwhelmingly terrestrial, since
# marine taxa disperse. Reuses habitat_class from section 04.

if ("habitat_class" %in% names(pres)) {
  
  endem_hab <- pres[, .(
    n_total   = uniqueN(CD_REF),
    n_endemic = uniqueN(CD_REF[endemism == "Endemique"])
  ), by = habitat_class]
  
  endem_hab[, pct_endemic := 100 * n_endemic / n_total]
  setorder(endem_hab, -n_endemic)
  
  cat("\n-- Endemism by habitat class --\n")
  print(endem_hab[, .(habitat_class, n_total, n_endemic,
                      pct_endemic = round(pct_endemic, 1))])
  
  fwrite(endem_hab, file.path(TABLES_DIR, "06_endemism_by_habitat.csv"),
         sep = CSV_SEP, bom = TRUE)
  
} else {
  cat("\nhabitat_class not found -- run section 04 first for the habitat cross.\n")
}

# --- Endemic birds by territory (axis 2 working list) ------------------------
# The operational core of axis 2: taxa for which a single territory carries
# sole responsibility, in the focal group.

birds_endem <- pres[GROUP2_INPN == "Oiseaux" & endemism == "Endemique",
                    .(region, territory, territory_short_fr, CD_REF,
                      LB_NOM, NOM_VERN, RANG, HABITAT, status_code)]

setorder(birds_endem, region, territory, LB_NOM)

fwrite(birds_endem, file.path(TABLES_DIR, "06_oiseaux_endemiques.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Endemic birds by territory --\n")
print(birds_endem[, .(n_endemic = uniqueN(CD_REF)),
                  by = .(region, territory_short_fr)][order(-n_endemic)])

cat("\nTotal endemic bird taxa:", uniqueN(birds_endem$CD_REF), "\n")
cat("  species    :", birds_endem[RANG == "ES",   uniqueN(CD_REF)], "\n")
cat("  subspecies :", birds_endem[RANG == "SSES", uniqueN(CD_REF)], "\n")

# =============================================================================
# 07. INTRODUCTION PRESSURE
#
# Question: how much of each territory's documented biota is introduced, which
#           share of it is classed invasive, and in which groups and habitats
#           is the pressure concentrated?
# =============================================================================

# --- Origin classification ---------------------------------------------------
# P / E / S are native (indigenous, endemic, subendemic).
# I / J are introduced, J being the invasive subset.
# C (cryptogenic) belongs to neither and is kept apart rather than forced into
#   one camp: its origin is genuinely undetermined.
# B (casual) is a non-established occurrence and is excluded from the rate
#   denominator, since counting it would inflate or deflate it arbitrarily.
#
# Levels are kept ASCII to stay usable as dcast column names; French labels are
# applied in the figures only.

ORIGIN_NATIVE     <- c("P", "E", "S")
ORIGIN_INTRODUCED <- c("I")
ORIGIN_INVASIVE   <- c("J")
ORIGIN_UNCERTAIN  <- c("C")
ORIGIN_CASUAL     <- c("B")

pres[, origin := factor(fcase(
  status_code %in% ORIGIN_NATIVE,     "Indigene",
  status_code %in% ORIGIN_INTRODUCED, "Introduit",
  status_code %in% ORIGIN_INVASIVE,   "Envahissant",
  status_code %in% ORIGIN_UNCERTAIN,  "Cryptogene",
  status_code %in% ORIGIN_CASUAL,     "Occasionnel",
  default = NA_character_
), levels = c("Indigene", "Introduit", "Envahissant",
              "Cryptogene", "Occasionnel"))]

cat("\n-- Origin classes --\n")
print(pres[, .(n_records = .N, n_taxa = uniqueN(CD_REF)), by = origin][order(-n_records)])

# Guard: no status code should fall through the classification.
if (pres[is.na(origin), .N] > 0) {
  cat("\nUnclassified status codes:\n")
  print(pres[is.na(origin), .N, by = status_code])
}

# --- The national watchlist (status A), for reference -------------------------
# A-coded taxa are absent but monitored. They are not in "pres" by definition.
# For birds the watchlist was uniform across territories (a national list, not
# a territorial signal); this checks whether that holds across all kingdoms.

watchlist <- taxa[status_code == "A",
                  .(n_taxa = uniqueN(CD_REF)), by = .(region, territory_short_fr)]
setorder(watchlist, -n_taxa)

cat("\n-- Watchlist (status A), taxa per territory --\n")
print(watchlist)
cat("Distinct watchlisted taxa overall:",
    format(taxa[status_code == "A", uniqueN(CD_REF)], big.mark = " "), "\n")
cat("  -> near-identical counts across territories = national list,",
    "not a territorial signal.\n")

# --- Table: introduction pressure by territory -------------------------------

origin_terr <- dcast(
  pres[!is.na(origin), .(n = uniqueN(CD_REF)),
       by = .(region, territory, territory_short_fr, area_km2, origin)],
  region + territory + territory_short_fr + area_km2 ~ origin,
  value.var = "n", fill = 0
)

origin_terr[, `:=`(
  n_introduced = Introduit + Envahissant,
  base         = Indigene + Introduit + Envahissant
)]

origin_terr[, `:=`(
  pct_introduced = 100 * n_introduced / base,
  pct_invasive   = 100 * Envahissant  / base,
  pct_inv_of_int = fifelse(n_introduced > 0,
                           100 * Envahissant / n_introduced, NA_real_),
  pct_cryptogene = 100 * Cryptogene / base
)]

setorder(origin_terr, -pct_introduced)

fwrite(origin_terr, file.path(TABLES_DIR, "07_introduction_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Introduction pressure by territory --\n")
print(origin_terr[, .(territory_short_fr, base, Indigene,
                      Introduit, Envahissant, Cryptogene, Occasionnel,
                      pct_introduced = round(pct_introduced, 1),
                      pct_inv_of_int = round(pct_inv_of_int, 1))])

# --- Figure 06a: introduction rate by territory ------------------------------
# The two segments sum to the introduction rate, split by invasive status, so
# the figure carries both the pressure and its composition.

FIG_ID <- "fig06a_pression_introduction"

intro_long <- melt(
  origin_terr[, .(territory_short_fr, region,
                  Introduit = 100 * Introduit   / base,
                  Envahissant = 100 * Envahissant / base)],
  id.vars       = c("territory_short_fr", "region"),
  variable.name = "origin",
  value.name    = "pct"
)

intro_order <- origin_terr[order(pct_introduced), territory_short_fr]
intro_long[, territory_short_fr := factor(territory_short_fr, levels = intro_order)]
intro_long[, origin := factor(origin, levels = c("Envahissant", "Introduit"))]

intro_lab <- unique(origin_terr[, .(territory_short_fr, pct_introduced, n_introduced)])
intro_lab[, territory_short_fr := factor(territory_short_fr, levels = intro_order)]

p_intro <- ggplot(intro_long,
                  aes(x = pct, y = territory_short_fr, fill = origin)) +
  geom_col(width = 0.72) +
  geom_text(data = intro_lab,
            aes(x = pct_introduced, y = territory_short_fr,
                label = format(n_introduced, big.mark = " ")),
            inherit.aes = FALSE,
            hjust = -0.18, size = 2.9, colour = FRIDGE_INK2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = function(x) paste0(x, " %")) +
  scale_fill_manual(values = c(Introduit   = FRIDGE_CAT[2],
                               Envahissant = FRIDGE_CAT[6]),
                    labels = c(Envahissant = "Introduit envahissant",
                               Introduit   = "Introduit, non classé envahissant"),
                    breaks = c("Introduit", "Envahissant")) +
  labs(
    title    = "Pression d'introduction dans les territoires d'Outre-mer",
    subtitle = paste("Part de taxons introduits parmi les taxons présents",
                     "d'origine déterminée  ·  effectif introduit en bout de",
                     "barre"),
    x = "Part des taxons présents", y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · statuts I et J rapportés à ",
                     "P+E+S+I+J · espèces et sous-espèces confondues.\n",
                     "Les cryptogènes (C) et les occasionnels (B) sont exclus ",
                     "du dénominateur. Le taux dépend aussi de l'effort de ",
                     "détection des introductions.")
  ) +
  theme_fridge(grid = "x") +
  theme(legend.position = "top")

p_intro
fridge_save(p_intro, FIG_ID, width = 9.5, height = 6, dir = FIGURES_DIR)

# --- Introduction pressure by taxonomic group --------------------------------

origin_group <- dcast(
  pres[!is.na(origin), .(n = uniqueN(CD_REF)), by = .(GROUP2_INPN, origin)],
  GROUP2_INPN ~ origin, value.var = "n", fill = 0
)

origin_group[, `:=`(
  n_introduced = Introduit + Envahissant,
  base         = Indigene + Introduit + Envahissant
)]
origin_group[, pct_introduced := 100 * n_introduced / base]
setorder(origin_group, -n_introduced)

fwrite(origin_group, file.path(TABLES_DIR, "07_introduction_by_group.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Introduction pressure by INPN group (>= 50 introduced) --\n")
print(origin_group[n_introduced >= 50,
                   .(GROUP2_INPN, base, n_introduced, Envahissant,
                     pct_introduced = round(pct_introduced, 1))])

# --- Introduction pressure and habitat ---------------------------------------
# Expectation: introductions are overwhelmingly terrestrial and freshwater;
# marine introductions are harder to detect and rarer in the referential.

if ("habitat_class" %in% names(pres)) {
  
  origin_hab <- dcast(
    pres[!is.na(origin), .(n = uniqueN(CD_REF)), by = .(habitat_class, origin)],
    habitat_class ~ origin, value.var = "n", fill = 0
  )
  
  origin_hab[, `:=`(
    n_introduced = Introduit + Envahissant,
    base         = Indigene + Introduit + Envahissant
  )]
  origin_hab[, pct_introduced := 100 * n_introduced / base]
  setorder(origin_hab, -n_introduced)
  
  cat("\n-- Introduction pressure by habitat class --\n")
  print(origin_hab[, .(habitat_class, base, n_introduced, Envahissant,
                       pct_introduced = round(pct_introduced, 1))])
  
  fwrite(origin_hab, file.path(TABLES_DIR, "07_introduction_by_habitat.csv"),
         sep = CSV_SEP, bom = TRUE)
  
} else {
  cat("\nhabitat_class not found -- run section 04 first for the habitat cross.\n")
}


# --- Introduced vertebrate predators and disease-relevant taxa ----------------
# A taxon count is a poor proxy for impact: a single introduced rat reshapes an
# island avifauna. This lists the introduced taxa in the groups that actually
# drive insular bird decline, so the pressure can be described by identity
# rather than by volume.

PREDATOR_GROUPS <- c("Mammifères", "Reptiles", "Amphibiens")

pred_intro <- pres[origin %in% c("Introduit", "Envahissant") &
                     GROUP2_INPN %in% PREDATOR_GROUPS,
                   .(region, territory_short_fr, GROUP2_INPN,
                     LB_NOM, NOM_VERN, status_code)]

setorder(pred_intro, region, territory_short_fr, GROUP2_INPN, LB_NOM)

cat("\n-- Introduced vertebrates by territory --\n")
print(pred_intro[, .(n = uniqueN(LB_NOM)),
                 by = .(territory_short_fr, GROUP2_INPN)][order(-n)])

fwrite(pred_intro, file.path(TABLES_DIR, "07_vertebres_introduits.csv"),
       sep = CSV_SEP, bom = TRUE)





# =============================================================================
# 08. EROSION
#
# Question: where has biodiversity loss already been recorded, in which groups,
#           and how much of the endemic heritage is already gone?
#
# =============================================================================

# --- Diagnostic: the full status vocabulary, from the data --------------------
# Printed before any interpretation, so the code choices below can be checked
# against TAXREF's own labels.

cat("\n-- All status codes in the full extraction --\n")
print(taxa[, .(n_records = .N,
               n_taxa    = uniqueN(CD_REF),
               n_terr    = uniqueN(territory),
               present   = sum(is_present)),
           by = .(status_code, status_label)][order(-n_records)])

# --- Erosion and native codes ------------------------------------------------
# W = Disparu · X = Eteint · Z = Endemique eteint
# Y (= "Introduit eteint") is deliberately excluded: a failed or eradicated
# introduction is not a biodiversity loss, and would carry the opposite sign.
EROSION_CODES <- c("W", "X", "Z")

# The denominator is the native pool only. An erosion rate computed over
# established introductions would be diluted by taxa whose loss is not a
# conservation concern.
NATIVE_CODES  <- c("P", "E", "S")

# --- The denominator: native taxa ever recorded in a territory ---------------
# Native taxa still present plus native taxa recorded then lost.

ever <- taxa[status_code %in% c(NATIVE_CODES, EROSION_CODES)]
ever[, lost := status_code %in% EROSION_CODES]

cat("\nNative taxon x territory records ever recorded:",
    format(nrow(ever), big.mark = " "), "\n")
cat("  of which lost:", format(ever[lost == TRUE, .N], big.mark = " "), "\n")
cat("Distinct native taxa ever recorded:",
    format(uniqueN(ever$CD_REF), big.mark = " "), "\n")

# --- Table: erosion by territory ---------------------------------------------

eros_terr <- ever[, .(
  n_ever = uniqueN(CD_REF),
  n_lost = uniqueN(CD_REF[lost == TRUE])
), by = .(region, territory, territory_short_fr, area_km2)]

eros_terr[, pct_lost := 100 * n_lost / n_ever]
setorder(eros_terr, -pct_lost)

fwrite(eros_terr, file.path(TABLES_DIR, "08_erosion_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Erosion by territory --\n")
print(eros_terr[, .(territory_short_fr, n_ever, n_lost,
                    pct_lost = round(pct_lost, 2))])

# --- Breakdown by erosion code, per territory --------------------------------
# The codes are not equivalent: local disappearance (W) and global extinction
# (X, Z) carry very different meanings, and only the latter is irreversible.

eros_code <- dcast(
  ever[lost == TRUE, .(n = uniqueN(CD_REF)),
       by = .(region, territory_short_fr, status_code)],
  region + territory_short_fr ~ status_code, value.var = "n", fill = 0
)

cat("\n-- Lost taxa by code and territory --\n")
print(eros_code)

fwrite(eros_code, file.path(TABLES_DIR, "08_erosion_by_code.csv"),
       sep = CSV_SEP, bom = TRUE)

# --- Figure 07: erosion by territory -----------------------------------------

FIG_ID <- "fig07_erosion_par_territoire"

eros_order <- eros_terr[order(pct_lost), territory_short_fr]
eros_plot  <- copy(eros_terr)
eros_plot[, territory_short_fr := factor(territory_short_fr, levels = eros_order)]

p_eros <- ggplot(eros_plot,
                 aes(x = pct_lost, y = territory_short_fr, fill = region)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = format(n_lost, big.mark = " ")),
            hjust = -0.2, size = 2.9, colour = FRIDGE_INK2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16)),
                     labels = function(x) paste0(x, " %")) +
  scale_fill_fridge_region() +
  labs(
    title    = "Érosion documentée dans les territoires d'Outre-mer",
    subtitle = paste("Part de taxons disparus ou éteints parmi les taxons",
                     "indigènes signalés  ·  effectif perdu en bout de barre"),
    x = "Part des taxons indigènes signalés", y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · statuts W, X et Z rapportés ",
                     "à P+E+S+W+X+Z · espèces et sous-espèces confondues.\n",
                     "La perte documentée dépend de l'ancienneté et de la ",
                     "qualité des inventaires : un territoire sans histoire ",
                     "naturaliste ancienne paraît épargné.")
  ) +
  theme_fridge(grid = "x") +
  theme(legend.position = "top")

p_eros
fridge_save(p_eros, FIG_ID, width = 9.5, height = 6, dir = FIGURES_DIR)


# --- Erosion by taxonomic group ----------------------------------------------

eros_group <- ever[, .(
  n_ever = uniqueN(CD_REF),
  n_lost = uniqueN(CD_REF[lost == TRUE])
), by = GROUP2_INPN]

eros_group[, pct_lost := 100 * n_lost / n_ever]
setorder(eros_group, -n_lost)

fwrite(eros_group, file.path(TABLES_DIR, "08_erosion_by_group.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Erosion by INPN group (groups with any loss) --\n")
print(eros_group[n_lost > 0,
                 .(GROUP2_INPN, n_ever, n_lost,
                   pct_lost = round(pct_lost, 2))])

# --- Endemic erosion, read directly from the Z code ---------------------------
# Z is endemic-specific, so no cross with the E status is needed -- nor possible,
# since a taxon carries one status per territory.

endem_eros <- ever[status_code %in% c("E", "Z"),
                   .(vivants = uniqueN(CD_REF[status_code == "E"]),
                     eteints = uniqueN(CD_REF[status_code == "Z"])),
                   by = .(region, territory_short_fr)]

endem_eros[, pct_eteints := 100 * eteints / (vivants + eteints)]
setorder(endem_eros, -eteints)

cat("\n-- Endemic erosion by territory --\n")
print(endem_eros[, .(territory_short_fr, vivants, eteints,
                     pct_eteints = round(pct_eteints, 1))])

fwrite(endem_eros, file.path(TABLES_DIR, "08_erosion_endemique.csv"),
       sep = CSV_SEP, bom = TRUE)

endem_eros_group <- ever[status_code %in% c("E", "Z"),
                         .(vivants = uniqueN(CD_REF[status_code == "E"]),
                           eteints = uniqueN(CD_REF[status_code == "Z"])),
                         by = GROUP2_INPN][eteints > 0]

endem_eros_group[, pct_eteints := 100 * eteints / (vivants + eteints)]
setorder(endem_eros_group, -eteints)

cat("\n-- Endemic erosion by group --\n")
print(endem_eros_group[, .(GROUP2_INPN, vivants, eteints,
                           pct_eteints = round(pct_eteints, 1))])

fwrite(endem_eros_group, file.path(TABLES_DIR, "08_erosion_endemique_groupe.csv"),
       sep = CSV_SEP, bom = TRUE)

# --- Environmental-DNA-only taxa (code G) -------------------------------------
# New in TAXREF v18, and directly relevant to FR-IDGE: a genetic detection
# method already carried by the national referential.

cat("\n-- eDNA-only taxa (status G) --\n")
print(taxa[status_code == "G",
           .(n_taxa = uniqueN(CD_REF)),
           by = .(territory, GROUP2_INPN)][order(-n_taxa)])


# --- Check: which groups drive endemic extinction, territory by territory ----
cat("\n-- Endemic extinctions (Z) by territory and group --\n")
print(ever[status_code == "Z", .N,
           by = .(territory_short_fr, GROUP2_INPN)][order(-N)])

# --- Check: local disappearances (W) in Martinique ----------------------------
cat("\n-- Taxa recorded as disappeared (W) in Martinique --\n")
print(ever[territory_short_fr == "Martinique" & status_code == "W",
           .(GROUP2_INPN, LB_NOM, NOM_VERN)][order(GROUP2_INPN, LB_NOM)])


# =============================================================================
# 09. TAXA SHARED BETWEEN TERRITORIES  --  NATIVE TAXA ONLY
#
# Question: how much of each territory's native biota is shared with the
#           others, is sharing structured by region (are the Caribbean islands
#           a pool?), and which native taxa are the most widely distributed
#           across the overseas territories?
#
# Scope: this whole section runs on native taxa only (P, E, S).
#   Introduced taxa (I) and invasive introduced taxa (J) are excluded, because
#   they are shared BY CONSTRUCTION: a rat, an Argentine ant or a Lantana
#   reaches every warm territory through traffic, not through biogeography.
#   Counting them would manufacture exactly the pattern this section tests --
#   co-occurrence would then measure the shared history of invasion rather
#   than a common natural heritage, and would inflate precisely the
#   cross-basin pairs that have no biogeographic basis.
#   Casual occurrences (B) are excluded too: a non-established record is not a
#   presence. Cryptogenic taxa (C) are excluded by default, their origin being
#   genuinely undetermined -- see INCLUDE_CRYPTOGENIC below.
#
# Two complementary readings of every territory pair:
#   jaccard        = shared / union, symmetric, comparable across all pairs
#   pct_of_smaller = shared / richness of the poorer territory, asymmetric,
#                    readable when richness differs by a factor of 30
#     (Jaccard is capped at the richness ratio of the pair, so a small biota
#      fully nested in a large one still scores low)
#
# Caveat carried from earlier sections: species and subspecies are counted
# separately (project convention); a lineage split across ranks inflates both
# richness and apparent turnover.
# =============================================================================

# --- Scope: the native pool ---------------------------------------------------
# `origin` comes from section 07: Indigene = P/E/S, Introduit = I,
# Envahissant = J, Cryptogene = C, Occasionnel = B.

INCLUDE_CRYPTOGENIC <- FALSE   # TRUE folds the 408 C taxa into the native pool

NATIVE_ORIGINS <- if (INCLUDE_CRYPTOGENIC) c("Indigene", "Cryptogene") else "Indigene"

nat <- pres[origin %in% NATIVE_ORIGINS]

# --- What the restriction removes, and from where -----------------------------
# Printed rather than assumed: the introduction rate varies by a factor of 10
# between territories (Réunion 15.6 %, Guyane 1.5 %), so the restriction does
# not shift every territory by the same amount.

cat("\n== Scope: native taxa only ==\n")
cat("Present taxon x territory records:", format(nrow(pres), big.mark = " "), "\n")
cat("  kept as native                 :", format(nrow(nat),  big.mark = " "),
    sprintf("(%.1f %%)\n", 100 * nrow(nat) / nrow(pres)))
cat("Distinct taxa present:", format(uniqueN(pres$CD_REF), big.mark = " "),
    "-> native:", format(uniqueN(nat$CD_REF), big.mark = " "), "\n")

cat("\n-- Records dropped, by origin class --\n")
print(pres[!origin %in% NATIVE_ORIGINS,
           .(n_records = .N, n_taxa = uniqueN(CD_REF)), by = origin][order(-n_records)])

cat("\n-- Effect of the restriction, by territory --\n")
scope_terr <- merge(
  pres[, .(n_all    = uniqueN(CD_REF)), by = territory_short_fr],
  nat [, .(n_native = uniqueN(CD_REF)), by = territory_short_fr],
  by = "territory_short_fr", all.x = TRUE
)
scope_terr[is.na(n_native), n_native := 0L]
scope_terr[, pct_dropped := round(100 * (n_all - n_native) / n_all, 1)]
print(scope_terr[order(-pct_dropped)])

fwrite(scope_terr, file.path(TABLES_DIR, "09_scope_native_restriction.csv"),
       sep = CSV_SEP, bom = TRUE)

# --- Helper: pairwise sharing from any subset of `nat` ------------------------
# Returns one row per ordered territory pair, diagonal included, so richness
# can be read off the diagonal and the matrix rebuilt without a second pass.

pairwise_sharing <- function(dt, label) {
  
  occ <- unique(dt[, .(CD_REF, territory_short_fr)])
  occ[, present := 1L]
  
  mat <- dcast(occ, CD_REF ~ territory_short_fr,
               value.var = "present", fill = 0L)
  
  m <- as.matrix(mat[, -1L, with = FALSE])
  storage.mode(m) <- "double"
  
  shared <- crossprod(m)          # shared[i, j] = taxa present in both
  rich   <- diag(shared)          # diagonal = richness of each territory
  
  res <- as.data.table(expand.grid(terr_a = colnames(shared),
                                   terr_b = colnames(shared),
                                   stringsAsFactors = FALSE))
  
  res[, n_shared := shared[cbind(terr_a, terr_b)]]
  res[, rich_a   := rich[terr_a]]
  res[, rich_b   := rich[terr_b]]
  
  res[, jaccard        := n_shared / (rich_a + rich_b - n_shared)]
  res[, pct_of_smaller := 100 * n_shared / pmin(rich_a, rich_b)]
  res[, subset         := label]
  
  res[]
}

# --- Regional attribution for both members of a pair -------------------------

region_lookup <- meta[, .(territory_short_fr, region)]

add_pair_regions <- function(res) {
  res <- merge(res, setnames(copy(region_lookup),
                             c("terr_a", "region_a")), by = "terr_a", all.x = TRUE)
  res <- merge(res, setnames(copy(region_lookup),
                             c("terr_b", "region_b")), by = "terr_b", all.x = TRUE)
  res[, same_region := region_a == region_b]
  res[]
}

# --- Main run: all native taxa ------------------------------------------------

share_all <- add_pair_regions(pairwise_sharing(nat, "Tous taxons indigènes"))

# Unordered pairs only, diagonal dropped, for every ranking and summary below.
share_pairs <- share_all[terr_a != terr_b]
share_pairs <- share_pairs[
  match(terr_a, short_order) < match(terr_b, short_order)
]

# The ceiling of Jaccard for a pair is the richness ratio itself: a fully
# nested biota cannot score higher. Saturation says how close a pair sits to
# its own ceiling, which is what the raw index hides.
share_pairs[, jaccard_max := pmin(rich_a, rich_b) / pmax(rich_a, rich_b)]
share_pairs[, saturation  := 100 * jaccard / jaccard_max]

setorder(share_pairs, -jaccard)

fwrite(share_pairs[, .(terr_a, terr_b, region_a, region_b, same_region,
                       rich_a, rich_b, n_shared,
                       jaccard = round(jaccard, 4),
                       jaccard_max = round(jaccard_max, 4),
                       saturation = round(saturation, 1),
                       pct_of_smaller = round(pct_of_smaller, 1))],
       file.path(TABLES_DIR, "09_pairwise_sharing.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Most similar territory pairs (Jaccard, native taxa) --\n")
print(head(share_pairs[, .(terr_a, terr_b, n_shared,
                           jaccard = round(jaccard, 3),
                           pct_of_smaller = round(pct_of_smaller, 1))], 15))

cat("\n-- Highest overlap read on the poorer territory --\n")
print(head(share_pairs[order(-pct_of_smaller),
                       .(terr_a, terr_b, rich_a, rich_b, n_shared,
                         pct_of_smaller = round(pct_of_smaller, 1),
                         jaccard = round(jaccard, 3))], 15))

cat("\n-- Closest to their own ceiling (nested biotas) --\n")
print(head(share_pairs[order(-saturation),
                       .(terr_a, terr_b, jaccard = round(jaccard, 3),
                         jaccard_max = round(jaccard_max, 3),
                         saturation = round(saturation, 1))], 15))

# --- Is sharing structured by region? ----------------------------------------

cat("\n-- Within-region vs between-region similarity --\n")
print(share_pairs[, .(n_pairs         = .N,
                      jaccard_median  = round(median(jaccard), 3),
                      jaccard_max     = round(max(jaccard), 3),
                      pct_smaller_med = round(median(pct_of_smaller), 1)),
                  by = same_region])

cat("\n-- Similarity within each region --\n")
print(share_pairs[same_region == TRUE,
                  .(n_pairs         = .N,
                    jaccard_median  = round(median(jaccard), 3),
                    pct_smaller_med = round(median(pct_of_smaller), 1),
                    shared_median   = median(n_shared)),
                  by = .(region = region_a)][order(-jaccard_median)])

# Caribbean focus: the hypothesis that the Lesser Antilles form one pool.
CARIB <- meta[region == "Caraibes", territory_short_fr]

cat("\n-- Caribbean pairs --\n")
print(share_pairs[terr_a %in% CARIB & terr_b %in% CARIB,
                  .(terr_a, terr_b, rich_a, rich_b, n_shared,
                    jaccard = round(jaccard, 3),
                    pct_of_smaller = round(pct_of_smaller, 1))][order(-jaccard)])

# Cross-basin pairs: the pattern most likely to have been carried by
# cosmopolitan introductions, and therefore the one to re-read here.
cat("\n-- Strongest cross-region pairs, native taxa only --\n")
print(head(share_pairs[same_region == FALSE][order(-pct_of_smaller),
                                             .(terr_a, terr_b, region_a, region_b, n_shared,
                                               pct_of_smaller = round(pct_of_smaller, 1),
                                               jaccard = round(jaccard, 3))], 12))

# --- Figure 08a: sharing heatmap ---------------------------------------------
# Lower triangle only: the matrix is symmetric for Jaccard, and a full square
# doubles every value without adding information. Territories keep the project
# order (region, then decreasing area) so regional blocks sit together.

FIG_ID    <- "fig08a_partage_territoires"
FIG_TITLE <- "Taxons indigènes partagés entre les territoires d'Outre-mer"

FIG_CAPTION_NATIVE <- paste0(
  "Source : TAXREF v18 (INPN) · taxons indigènes uniquement (statuts P, E, S) ",
  "· espèces et sous-espèces confondues.\nLes introduits (I, J), les ",
  "occasionnels (B) et les cryptogènes (C) sont exclus : partagés par le ",
  "trafic et non par la biogéographie, ils gonfleraient le partage mesuré."
)

hm_share <- copy(share_pairs)
hm_share[, terr_a := factor(terr_a, levels = short_order)]
hm_share[, terr_b := factor(terr_b, levels = rev(short_order))]

p_share <- ggplot(hm_share, aes(x = terr_a, y = terr_b, fill = jaccard)) +
  geom_tile(colour = FRIDGE_SURF, linewidth = 0.9) +
  geom_text(data = hm_share[jaccard >= 0.02],
            aes(label = format(n_shared, big.mark = " "),
                colour = jaccard > 0.10),
            size = 2.4, show.legend = FALSE) +
  scale_fill_gradientn(colours = FRIDGE_SEQ, transform = "sqrt",
                       labels = scales::number_format(accuracy = 0.01),
                       name = "Jaccard") +
  scale_colour_manual(values = c(`FALSE` = FRIDGE_INK, `TRUE` = "white")) +
  coord_fixed() +
  labs(
    title    = FIG_TITLE,
    subtitle = paste("Indice de Jaccard entre territoires  ·  nombre de taxons",
                     "partagés affiché à partir de 0,02"),
    x = NULL, y = NULL,
    caption = FIG_CAPTION_NATIVE
  ) +
  theme_fridge(grid = "none") +
  theme(axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position   = "right",
        legend.key.height = unit(2, "lines"))

p_share
fridge_save(p_share, FIG_ID, width = 10, height = 8.5, dir = FIGURES_DIR)

# --- Figure 08b: the same pairs, read on the poorer territory ----------------
# Asymmetric view: what share of the smaller biota the larger one also holds.
# This is where a small island nested inside a regional pool becomes visible,
# whereas Jaccard only shows the richness gap.

FIG_ID2 <- "fig08b_partage_territoires_relatif"

p_share_rel <- ggplot(hm_share,
                      aes(x = terr_a, y = terr_b, fill = pct_of_smaller)) +
  geom_tile(colour = FRIDGE_SURF, linewidth = 0.9) +
  geom_text(data = hm_share[pct_of_smaller >= 5],
            aes(label = round(pct_of_smaller),
                colour = pct_of_smaller > 45),
            size = 2.4, show.legend = FALSE) +
  scale_fill_gradientn(colours = FRIDGE_SEQ,
                       labels = function(x) paste0(x, " %"),
                       name = NULL) +
  scale_colour_manual(values = c(`FALSE` = FRIDGE_INK, `TRUE` = "white")) +
  coord_fixed() +
  labs(
    title    = "Imbrication des faunes et flores indigènes ultramarines",
    subtitle = paste("Part des taxons indigènes du territoire le moins riche",
                     "de la paire également présents dans l'autre"),
    x = NULL, y = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · taxons indigènes (P, E, S).\n",
                     "Lecture asymétrique, insensible à l'écart de richesse : ",
                     "une valeur élevée signale un territoire emboîté dans un ",
                     "pool régional plus large.")
  ) +
  theme_fridge(grid = "none") +
  theme(axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position   = "right",
        legend.key.height = unit(2, "lines"))

p_share_rel
fridge_save(p_share_rel, FIG_ID2, width = 10, height = 8.5, dir = FIGURES_DIR)

# --- Control runs: what actually drives the sharing --------------------------
# Three subsets of the native pool, same metric, to separate the causes:
#   no endemics   -> removes the exclusivity floor
#   terrestrial   -> expected low sharing (poor dispersal, insular endemism)
#   marine strict -> expected high sharing (connected basins)

share_variants <- rbindlist(list(
  share_all,
  add_pair_regions(pairwise_sharing(nat[endemism != "Endemique"],
                                    "Indigènes hors endémiques")),
  add_pair_regions(pairwise_sharing(nat[habitat_class == "Terrestre / continental"],
                                    "Indigènes terrestres")),
  add_pair_regions(pairwise_sharing(nat[habitat_class == "Marin strict"],
                                    "Indigènes marins stricts"))
), use.names = TRUE)

variant_pairs <- share_variants[
  terr_a != terr_b &
    match(terr_a, short_order) < match(terr_b, short_order)
]

cat("\n-- Sharing by subset (median over all pairs) --\n")
print(variant_pairs[, .(n_pairs         = .N,
                        jaccard_median  = round(median(jaccard), 3),
                        jaccard_max     = round(max(jaccard), 3),
                        pct_smaller_med = round(median(pct_of_smaller), 1)),
                    by = subset])

cat("\n-- Same, within region only --\n")
print(variant_pairs[same_region == TRUE,
                    .(jaccard_median  = round(median(jaccard), 3),
                      pct_smaller_med = round(median(pct_of_smaller), 1)),
                    by = .(subset, region = region_a)][order(subset, -jaccard_median)])

fwrite(variant_pairs[, .(subset, terr_a, terr_b, same_region,
                         rich_a, rich_b, n_shared,
                         jaccard = round(jaccard, 4),
                         pct_of_smaller = round(pct_of_smaller, 1))],
       file.path(TABLES_DIR, "09_pairwise_sharing_subsets.csv"),
       sep = CSV_SEP, bom = TRUE)

# =============================================================================
# 09b. THE MOST WIDELY SHARED NATIVE TAXA
# =============================================================================

# --- Occupancy: how many territories hold each native taxon -------------------
# `statuses` keeps the P/E/S detail: a taxon endemic in one territory cannot be
# present in another, so any taxon above one territory is necessarily P or S.

occupancy <- nat[, .(
  n_terr      = uniqueN(territory),
  n_regions   = uniqueN(region),
  territories = paste(sort(unique(as.character(territory_short_fr))),
                      collapse = " | "),
  statuses    = paste(sort(unique(status_code)), collapse = "/")
), by = .(CD_REF, LB_NOM, NOM_VERN, RANG, GROUP2_INPN, habitat_class)]

setorder(occupancy, -n_terr, GROUP2_INPN, LB_NOM)

fwrite(occupancy, file.path(TABLES_DIR, "09_taxon_occupancy.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Distribution of native taxa by number of territories occupied --\n")
print(occupancy[, .(n_taxa = .N,
                    pct    = round(100 * .N / nrow(occupancy), 2)),
                by = n_terr][order(n_terr)])

cat("\n-- The 40 most widely distributed native taxa --\n")
print(head(occupancy[, .(LB_NOM, NOM_VERN, GROUP2_INPN,
                         habitat_class, n_terr, n_regions, statuses)], 40))

# --- Who are the widespread native taxa? --------------------------------------
# A threshold rather than a top-N: the point is the profile of the widespread
# pool, not a leaderboard. With introductions removed, whatever remains here is
# a genuinely shared natural heritage -- the pan-OM monitoring candidates.

WIDESPREAD_MIN <- 8   # of 15 territories

widespread <- occupancy[n_terr >= WIDESPREAD_MIN]

cat("\nNative taxa present in", WIDESPREAD_MIN, "territories or more:",
    format(nrow(widespread), big.mark = " "),
    sprintf("(%.2f %% of native taxa)\n",
            100 * nrow(widespread) / nrow(occupancy)))

cat("\n-- Their taxonomic groups --\n")
print(widespread[, .(n_taxa = .N), by = GROUP2_INPN][order(-n_taxa)])

cat("\n-- Their habitats --\n")
print(widespread[, .(n_taxa = .N), by = habitat_class][order(-n_taxa)])

cat("\n-- Their native status --\n")
print(widespread[, .(n_taxa = .N), by = statuses][order(-n_taxa)])

fwrite(widespread, file.path(TABLES_DIR, "09_widespread_taxa.csv"),
       sep = CSV_SEP, bom = TRUE)

# Birds specifically: the pan-OM axis needs native taxa shared across
# territories, which is exactly the opposite selection to the endemic list.
cat("\n-- Most widespread native bird taxa --\n")
print(head(occupancy[GROUP2_INPN == "Oiseaux",
                     .(LB_NOM, NOM_VERN, RANG, habitat_class,
                       n_terr, n_regions, territories)], 30))

fwrite(occupancy[GROUP2_INPN == "Oiseaux"][n_terr >= 3],
       file.path(TABLES_DIR, "09_oiseaux_partages.csv"),
       sep = CSV_SEP, bom = TRUE)

# --- Figure 09a: occupancy spectrum ------------------------------------------
# Habitat fill answers the real question behind "which taxa are shared": the
# widespread pool is expected to be marine, the exclusive one terrestrial.

FIG_ID3 <- "fig09a_spectre_occupation"

occ_spectrum <- occupancy[!is.na(habitat_class),
                          .(n_taxa = .N), by = .(n_terr, habitat_class)]
occ_tot <- occ_spectrum[, .(n_tot = sum(n_taxa)), by = n_terr]

p_spectrum <- ggplot(occ_spectrum,
                     aes(x = factor(n_terr), y = n_taxa, fill = habitat_class)) +
  geom_col(position = "fill", width = 0.76) +
  geom_text(data = occ_tot,
            aes(x = factor(n_terr), y = 1, label = format(n_tot, big.mark = " ")),
            inherit.aes = FALSE, vjust = -0.5, size = 2.9, colour = FRIDGE_INK2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = hab_palette) +
  labs(
    title    = "Spectre d'occupation des taxons indigènes ultramarins",
    subtitle = paste("Milieu de vie des taxons selon le nombre de territoires",
                     "occupés  ·  effectif total au-dessus de chaque barre"),
    x = "Nombre de territoires occupés", y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · taxons indigènes (P, E, S).\n",
                     "La part marine croît avec le nombre de territoires ",
                     "occupés : le partage large est marin.")
  ) +
  theme_fridge(grid = "y") +
  theme(legend.position = "top")

p_spectrum
fridge_save(p_spectrum, FIG_ID3, width = 9.5, height = 6, dir = FIGURES_DIR)

# --- Figure 09b: shared vs exclusive, per territory --------------------------
# Operational reading for species selection: a territory whose native biota is
# largely exclusive cannot borrow monitoring from its neighbours.

FIG_ID4 <- "fig09b_exclusivite_territoires"

occ_terr <- merge(
  unique(nat[, .(CD_REF, territory_short_fr, region)]),
  occupancy[, .(CD_REF, n_terr)],
  by = "CD_REF", all.x = TRUE
)

excl_terr <- occ_terr[, .(
  n_total     = uniqueN(CD_REF),
  n_exclusive = uniqueN(CD_REF[n_terr == 1])
), by = .(territory_short_fr, region)]

excl_terr[, pct_exclusive := 100 * n_exclusive / n_total]
setorder(excl_terr, -pct_exclusive)

fwrite(excl_terr, file.path(TABLES_DIR, "09_exclusivity_by_territory.csv"),
       sep = CSV_SEP, bom = TRUE)

cat("\n-- Share of each native biota found in no other overseas territory --\n")
print(excl_terr[, .(territory_short_fr, n_total, n_exclusive,
                    pct_exclusive = round(pct_exclusive, 1))])

excl_plot <- copy(excl_terr)
excl_plot[, territory_short_fr := factor(territory_short_fr,
                                         levels = excl_terr[order(pct_exclusive),
                                                            territory_short_fr])]

p_excl <- ggplot(excl_plot,
                 aes(x = pct_exclusive, y = territory_short_fr, fill = region)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = format(n_exclusive, big.mark = " ")),
            hjust = -0.18, size = 2.9, colour = FRIDGE_INK2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16)),
                     labels = function(x) paste0(x, " %")) +
  scale_fill_fridge_region() +
  labs(
    title    = "Exclusivité des biotes indigènes ultramarins",
    subtitle = paste("Part des taxons indigènes présents dans ce seul",
                     "territoire parmi les 15  ·  effectif en bout de barre"),
    x = "Part des taxons indigènes", y = NULL, fill = NULL,
    caption = paste0("Source : TAXREF v18 (INPN) · taxons indigènes (P, E, S).\n",
                     "Exclusivité au sein des Outre-mer français seulement : ",
                     "un taxon peut être partagé avec un pays voisin sans ",
                     "apparaître ici.")
  ) +
  theme_fridge(grid = "x") +
  theme(legend.position = "top")

p_excl
fridge_save(p_excl, FIG_ID4, width = 9.5, height = 6, dir = FIGURES_DIR)

# =============================================================================
# 09c. SMALL MULTIPLES: WHAT DRIVES THE SHARING
#
# The same pairwise matrix, four times, on a common colour scale. Reading the
# four panels side by side answers what no single panel can: whether a regional
# pool is a biological fact or an artefact of endemics and marine dispersal.
# With introductions already out of scope, a pattern surviving here is
# biogeographic.
# =============================================================================

SUBSET_ORDER <- c("Tous taxons indigènes", "Indigènes hors endémiques",
                  "Indigènes terrestres", "Indigènes marins stricts")

# --- Long table for the facets -----------------------------------------------
# Territories keep the project order so the four panels are strictly
# comparable, cell by cell. A pair absent from a subset (no taxon of that
# habitat in one of the two territories) stays empty rather than being drawn
# as a zero: no data and no overlap are different statements.

hm_facet <- copy(variant_pairs)
hm_facet[, subset := factor(subset, levels = SUBSET_ORDER)]
hm_facet[, terr_a := factor(terr_a, levels = short_order)]
hm_facet[, terr_b := factor(terr_b, levels = rev(short_order))]

cat("\n-- Pairs per panel --\n")
print(hm_facet[, .(n_pairs = .N,
                   jaccard_max = round(max(jaccard), 3)), by = subset])

# --- Helper: one faceted map per metric --------------------------------------

facet_sharing_map <- function(metric, fig_id, fig_title, fig_subtitle,
                              legend_name, legend_labels, transform = "sqrt") {
  
  p <- ggplot(hm_facet, aes(x = terr_a, y = terr_b, fill = .data[[metric]])) +
    geom_tile(colour = FRIDGE_SURF, linewidth = 0.4) +
    facet_wrap(~ subset, ncol = 2) +
    scale_fill_gradientn(colours = FRIDGE_SEQ, transform = transform,
                         labels = legend_labels, name = legend_name,
                         na.value = FRIDGE_SURF) +
    coord_fixed() +
    labs(
      title    = fig_title,
      subtitle = fig_subtitle,
      x = NULL, y = NULL,
      caption = paste0("Source : TAXREF v18 (INPN) · taxons indigènes (P, E, ",
                       "S) · espèces et sous-espèces confondues · échelle ",
                       "commune aux quatre panneaux.\nUne case vide signale ",
                       "une paire sans taxon du sous-ensemble considéré, non ",
                       "une absence de partage.")
    ) +
    theme_fridge(grid = "none") +
    theme(axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1,
                                           size = 7),
          axis.text.y       = element_text(size = 7),
          strip.text        = element_text(hjust = 0),
          panel.spacing     = unit(1.1, "lines"),
          legend.position   = "right",
          legend.key.height = unit(2, "lines"))
  
  fridge_save(p, fig_id, width = 11, height = 11, dir = FIGURES_DIR)
  p
}

# --- Figure 10a: Jaccard ------------------------------------------------------

p_facet_jac <- facet_sharing_map(
  metric        = "jaccard",
  fig_id        = "fig10a_partage_sous_ensembles",
  fig_title     = "Ce qui porte le partage entre territoires d'Outre-mer",
  fig_subtitle  = paste("Indice de Jaccard, même matrice sur quatre",
                        "sous-ensembles de taxons indigènes"),
  legend_name   = "Jaccard",
  legend_labels = scales::number_format(accuracy = 0.01)
)
p_facet_jac

# --- Figure 10b: the asymmetric reading --------------------------------------
# Same panels, imbrication instead of similarity: the small territories only
# become readable here.

p_facet_rel <- facet_sharing_map(
  metric        = "pct_of_smaller",
  fig_id        = "fig10b_imbrication_sous_ensembles",
  fig_title     = "Imbrication des biotes indigènes selon le type de taxons",
  fig_subtitle  = paste("Part des taxons indigènes du territoire le moins",
                        "riche de la paire également présents dans l'autre"),
  legend_name   = NULL,
  legend_labels = function(x) paste0(x, " %"),
  transform     = "identity"
)
p_facet_rel

# --- The numbers behind the panels -------------------------------------------
# A figure shows the contrast; these tables are what gets quoted.

cat("\n-- Caribbean pairs across the four subsets --\n")
print(dcast(hm_facet[terr_a %in% CARIB & terr_b %in% CARIB],
            terr_a + terr_b ~ subset,
            value.var = "jaccard", fun.aggregate = function(x) round(x, 3)))

cat("\n-- Median Jaccard, within region, by subset --\n")
print(dcast(hm_facet[same_region == TRUE,
                     .(med = round(median(jaccard), 3)),
                     by = .(region = region_a, subset)],
            region ~ subset, value.var = "med"))

cat("\n-- Terrestrial vs marine ratio, per region --\n")
ratio_tab <- dcast(hm_facet[same_region == TRUE,
                            .(med = median(jaccard)),
                            by = .(region = region_a, subset)],
                   region ~ subset, value.var = "med")

setnames(ratio_tab,
         c("Indigènes terrestres", "Indigènes marins stricts"),
         c("terrestre", "marin"), skip_absent = TRUE)

if (all(c("terrestre", "marin") %in% names(ratio_tab))) {
  ratio_tab[, ratio_terr_marin := round(terrestre / marin, 2)]
  print(ratio_tab[order(ratio_terr_marin),
                  .(region,
                    terrestre = round(terrestre, 3),
                    marin     = round(marin, 3),
                    ratio_terr_marin)])
  cat("  ratio > 1 : partage porté par le terrestre, pool régional réel\n")
  cat("  ratio < 1 : partage porté par le marin, mutualisation limitée\n")
}

fwrite(hm_facet[, .(subset, terr_a, terr_b, region_a, region_b, same_region,
                    rich_a, rich_b, n_shared,
                    jaccard = round(jaccard, 4),
                    pct_of_smaller = round(pct_of_smaller, 1))],
       file.path(TABLES_DIR, "09_sharing_small_multiples.csv"),
       sep = CSV_SEP, bom = TRUE)


# =============================================================================
# FR-IDGE / Plotting charter
# ggplot2 theme, palettes and helpers following the FR-IDGE brand guidelines
#
# Usage: source this file at the top of any analysis script.
#   source("scripts/fridge_theme.R")
#   ggplot(...) + ... + scale_fill_fridge_d() + theme_fridge()
#
# The brand colours are designed for logos and slides, not for data series:
# used raw they fail on lightness, chroma and colour-vision separation. The
# categorical palette below keeps the brand hues but re-steps them so that
# every adjacent pair stays distinguishable, including for colour-blind
# readers, and so that every series reaches 3:1 contrast on the chart surface.
#
# Author: Julien Bonnier
# =============================================================================

library(ggplot2)
library(grid)

# =============================================================================
# 1. BRAND REFERENCE (do not use directly for data series)
# =============================================================================

FRIDGE_BRAND <- c(
  azur_fonce    = "#275662",   # FR-IDGE
  cyan_fonce    = "#008c8e",   # Indicateurs
  cyan          = "#66c1bf",   # Genetiques
  gris_fonce    = "#0d0d0d",
  gris          = "#a6a6a6",
  vert          = "#00bf63",
  vert_20       = "#80dfb1",
  vert_herbe    = "#7ed957",
  vert_pastel   = "#beecab",
  jaune         = "#ffde59",
  jaune_clair   = "#ffe16a",
  saumon        = "#ff751f",
  orange_pastel = "#ffba8f",
  rouge_vif     = "#ff3131",
  rouge_pastel  = "#fe9797"
)

# Brand colours keep their role in titles, logos and key figures.
FRIDGE_INK   <- "#0d0d0d"   # primary text
FRIDGE_INK2  <- "#4a4a4a"   # secondary text
FRIDGE_MUTED <- "#8a8a8a"   # axis text, captions
FRIDGE_GRID  <- "#e6e6e4"   # grid lines
FRIDGE_SURF  <- "#fcfcfb"   # chart surface (light)
FRIDGE_ACCENT <- "#275662"  # titles, reference lines

# =============================================================================
# 2. DATA PALETTES
# =============================================================================

# --- Categorical: fixed order, NEVER cycled ----------------------------------
# Validated (light surface #fcfcfb): lightness band, chroma floor, CVD
# separation, normal-vision floor and contrast all pass.
# Beyond 6 categories, group the tail into "Autres" or use facets -- do not
# generate a 7th hue.

FRIDGE_CAT <- c(
  "#00968d",   # 1 teal      -- derived from cyan fonce, the FR-IDGE signature
  "#d2691e",   # 2 orange    -- derived from saumon
  "#1f6fb2",   # 3 blue      -- derived from azur fonce
  "#479e3c",   # 4 green     -- derived from vert herbe
  "#7a5ea8",   # 5 violet    -- added: the brand offers no cool counterpoint
  "#c0392b"    # 6 red       -- derived from rouge vif
)

# Dark-mode steps of the same hues (validated on surface #1a1a19).
FRIDGE_CAT_DARK <- c("#149e92", "#d4742c", "#4e93d6",
                     "#52a447", "#9b82c7", "#dd5a50")

# Neutral slot for "Autres" / "Non renseigne". Never a series colour.
FRIDGE_OTHER <- "#9a9a98"

# --- Sequential: one hue, light to dark --------------------------------------
# For magnitude (richness, counts, densities). Brand teal.

FRIDGE_SEQ <- c("#e4f2f1", "#b7dedb", "#84c5c1", "#4da8a3", "#1a8781", "#0a5f5c")

# --- Diverging: two poles, neutral grey midpoint -----------------------------
# For polarity only (difference from a reference, gain/loss). Never for
# magnitude -- a diverging ramp on a one-way quantity invents a midpoint.

FRIDGE_DIV <- c("#0a5f5c", "#4da8a3", "#b7dedb",
                "#eeeeec",
                "#ffba8f", "#d2691e", "#9c3d10")

# --- Status: reserved, never reused as series colours ------------------------

FRIDGE_STATUS <- c(
  good     = "#2f8f4e",
  warning  = "#b08a00",
  serious  = "#d2691e",
  critical = "#c0392b"
)

# --- Territories: one fixed colour each --------------------------------------
#
# HONEST LIMIT, read this before using it. Fifteen mutually distinguishable
# colours do not exist: any 15-slot palette has pairs that normal vision
# cannot separate, let alone colour-blind readers. So the scheme is built in
# two layers instead:
#
#   - HUE carries the BIOGEOGRAPHIC REGION. The six regional anchors are
#     validated: lightness band, chroma floor, CVD separation, normal-vision
#     floor and contrast all pass.
#   - LIGHTNESS separates territories WITHIN a region. Those steps are NOT
#     perceptually safe on their own (adjacent Caribbean shades sit around
#     dE 6 in normal vision).
#
# Consequence: a figure coloured by territory MUST carry direct labels, or
# order the bars so the region reads as a block. Colour alone never carries
# territory identity. For a strict comparison of a few territories, use the
# categorical palette above and treat them as ordinary series.

FRIDGE_REGION <- c(
  "Guyane"         = "#008d82",   # teal -- FR-IDGE signature, lone continental
  "Caraibes"       = "#d2691e",   # orange
  "Pacifique"      = "#1f6fb2",   # blue
  "Ocean Indien"   = "#479e3c",   # green
  "Austral"        = "#96479b",   # violet
  "Atlantique NO"  = "#b03a2e"    # red
)

# Territory codes to region, reused for faceting and grouping.
FRIDGE_TERRITORY_REGION <- c(
  GF = "Guyane",
  GUA = "Caraibes", MAR = "Caraibes", SM = "Caraibes", SB = "Caraibes",
  SPM = "Atlantique NO",
  MAY = "Ocean Indien", REU = "Ocean Indien", EPA = "Ocean Indien",
  SA = "Austral", TA = "Austral",
  NC = "Pacifique", WF = "Pacifique", PF = "Pacifique", CLI = "Pacifique"
)

# One colour per territory: regional hue + lightness step within the region.
# Keyed on the territory LABELS used in the extraction output.
FRIDGE_TERRITORY <- c(
  "Guyane"                   = "#008d82",

  "Guadeloupe"               = "#9c4e12",
  "Martinique"               = "#c2621a",
  "Saint-Martin"             = "#d98a44",
  "Saint-Barthelemy"         = "#ecaf7f",
  "Saint-Barthélemy"         = "#ecaf7f",

  "Saint-Pierre-et-Miquelon" = "#b03a2e",

  "Mayotte"                  = "#2e6f2a",
  "La Reunion"               = "#479e3c",
  "La Réunion"               = "#479e3c",
  "Iles Eparses"             = "#7cbd6b",
  "Îles Éparses"             = "#7cbd6b",

  "Iles subantarctiques (Kerguelen, Crozet, Saint-Paul et Amsterdam)" = "#96479b",
  "Iles subantarctiques"     = "#96479b",
  "Terre Adelie"             = "#bb86bf",
  "Terre Adélie"             = "#bb86bf",

  "Nouvelle-Caledonie"       = "#15517f",
  "Nouvelle-Calédonie"       = "#15517f",
  "Polynesie francaise"      = "#1f6fb2",
  "Polynésie française"      = "#1f6fb2",
  "Wallis-et-Futuna"         = "#4e93d6",
  "Clipperton"               = "#8bbce8"
)

# --- Project-specific fixed mappings -----------------------------------------
# Status classes carry meaning, so they get a stable mapping rather than an
# arbitrary slot. Keeps every figure of the project consistent.

FRIDGE_STATUS_CLASS <- c(
  "Indigene"         = "#00968d",
  "Indigène"         = "#00968d",
  "Cryptogene"       = "#7a5ea8",
  "Cryptogène"       = "#7a5ea8",
  "Introduit"        = "#d2691e",
  "Occasionnel"      = "#1f6fb2",
  "Incertain"        = "#b08a00",
  "Disparu / eteint" = "#c0392b",
  "Disparu / éteint" = "#c0392b",
  "Absent"           = "#9a9a98",
  "Erreur"           = "#c8c8c6"
)

# =============================================================================
# 3. FONTS
# =============================================================================
# Brand fonts: League Spartan (titles, bold) + Raleway (body).
# They are only used when actually installed, so the charter degrades
# gracefully on a machine that lacks them. Install once with:
#   install.packages("sysfonts"); install.packages("showtext")
#   sysfonts::font_add_google("League Spartan"); sysfonts::font_add_google("Raleway")
#   showtext::showtext_auto()

fridge_fonts <- function() {
  available <- tryCatch(names(grDevices::postscriptFonts()), error = function(e) character(0))
  sys_fonts <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character(0))
  list(
    title = if ("League Spartan" %in% sys_fonts) "League Spartan" else "sans",
    body  = if ("Raleway"        %in% sys_fonts) "Raleway"        else "sans"
  )
}

# =============================================================================
# 4. THEME
# =============================================================================

theme_fridge <- function(base_size = 12, grid = c("y", "x", "both", "none")) {
  grid  <- match.arg(grid)
  fonts <- fridge_fonts()

  th <- theme_minimal(base_size = base_size, base_family = fonts$body) +
    theme(
      plot.title    = element_text(family = fonts$title, face = "bold",
                                   size = base_size * 1.35, colour = FRIDGE_ACCENT,
                                   margin = margin(b = 4), hjust = 0),
      plot.subtitle = element_text(size = base_size * 0.95, colour = FRIDGE_INK2,
                                   margin = margin(b = 12), hjust = 0),
      plot.caption  = element_text(size = base_size * 0.75, colour = FRIDGE_MUTED,
                                   hjust = 0, margin = margin(t = 12)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      axis.title   = element_text(size = base_size * 0.9, colour = FRIDGE_INK2),
      axis.text    = element_text(size = base_size * 0.85, colour = FRIDGE_MUTED),
      axis.ticks   = element_blank(),
      axis.line.x  = element_line(colour = FRIDGE_GRID, linewidth = 0.4),

      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = FRIDGE_GRID, linewidth = 0.4),

      legend.position   = "top",
      legend.justification = "left",
      legend.title      = element_text(size = base_size * 0.85, colour = FRIDGE_INK2),
      legend.text       = element_text(size = base_size * 0.85, colour = FRIDGE_INK2),
      legend.key.size   = unit(0.9, "lines"),
      legend.margin     = margin(b = 4),

      strip.text  = element_text(family = fonts$title, face = "bold",
                                 size = base_size * 0.9, colour = FRIDGE_INK,
                                 hjust = 0, margin = margin(b = 4)),

      plot.background  = element_rect(fill = FRIDGE_SURF, colour = NA),
      panel.background = element_rect(fill = FRIDGE_SURF, colour = NA),
      plot.margin      = margin(16, 20, 12, 16)
    )

  th + switch(
    grid,
    y    = theme(panel.grid.major.x = element_blank()),
    x    = theme(panel.grid.major.y = element_blank()),
    both = theme(),
    none = theme(panel.grid.major = element_blank())
  )
}

# =============================================================================
# 5. SCALES
# =============================================================================

fridge_pal_d <- function(dark = FALSE) {
  pal <- if (dark) FRIDGE_CAT_DARK else FRIDGE_CAT
  function(n) {
    if (n > length(pal)) {
      stop("FR-IDGE categorical palette holds ", length(pal), " slots, ", n,
           " requested.\nGroup the tail into 'Autres' (FRIDGE_OTHER) or use ",
           "facets -- do not cycle the hues.")
    }
    pal[seq_len(n)]
  }
}

scale_colour_fridge_d <- function(dark = FALSE, ...)
  discrete_scale("colour", palette = fridge_pal_d(dark), ...)
scale_color_fridge_d <- scale_colour_fridge_d

scale_fill_fridge_d <- function(dark = FALSE, ...)
  discrete_scale("fill", palette = fridge_pal_d(dark), ...)

scale_fill_fridge_c <- function(...)
  scale_fill_gradientn(colours = FRIDGE_SEQ, ...)
scale_colour_fridge_c <- function(...)
  scale_colour_gradientn(colours = FRIDGE_SEQ, ...)

scale_fill_fridge_div <- function(midpoint = 0, ...)
  scale_fill_gradientn(colours = FRIDGE_DIV,
                       rescaler = ~ scales::rescale_mid(.x, mid = midpoint), ...)

# Fixed mapping for the TAXREF status classes.
scale_fill_fridge_status <- function(...)
  scale_fill_manual(values = FRIDGE_STATUS_CLASS, na.value = FRIDGE_OTHER, ...)
scale_colour_fridge_status <- function(...)
  scale_colour_manual(values = FRIDGE_STATUS_CLASS, na.value = FRIDGE_OTHER, ...)

# Fixed mapping for the 15 territories. See the warning above FRIDGE_TERRITORY:
# always pair it with direct labels or a region-ordered axis.
scale_fill_fridge_territory <- function(...)
  scale_fill_manual(values = FRIDGE_TERRITORY, na.value = FRIDGE_OTHER, ...)
scale_colour_fridge_territory <- function(...)
  scale_colour_manual(values = FRIDGE_TERRITORY, na.value = FRIDGE_OTHER, ...)

# Colour by region instead -- the perceptually safe option when the figure
# cannot carry a label per territory.
scale_fill_fridge_region <- function(...)
  scale_fill_manual(values = FRIDGE_REGION, na.value = FRIDGE_OTHER, ...)
scale_colour_fridge_region <- function(...)
  scale_colour_manual(values = FRIDGE_REGION, na.value = FRIDGE_OTHER, ...)

# =============================================================================
# 6. HELPERS
# =============================================================================

# Collapse a factor to the n most frequent levels, the rest into "Autres".
# Enforces the 6-slot rule instead of silently overflowing the palette.
fridge_lump <- function(x, n = 5, other = "Autres") {
  tab  <- sort(table(x), decreasing = TRUE)
  keep <- names(tab)[seq_len(min(n, length(tab)))]
  factor(ifelse(x %in% keep, as.character(x), other),
         levels = c(keep, other))
}

# Shorten the long territory labels for axes.
FRIDGE_TERRITORY_SHORT <- c(
  "Iles subantarctiques (Kerguelen, Crozet, Saint-Paul et Amsterdam)" = "Iles subantarctiques",
  "Saint-Pierre-et-Miquelon" = "St-Pierre-et-Miquelon",
  "Polynesie francaise"      = "Polynesie fr.",
  "Nouvelle-Caledonie"       = "Nouvelle-Caledonie"
)

fridge_short_territory <- function(x) {
  out <- FRIDGE_TERRITORY_SHORT[x]
  ifelse(is.na(out), x, out)
}

# Single save function, so every figure of the project shares its geometry.
# Default ratio suits a Notion page or a slide body.
fridge_save <- function(plot, filename, width = 9, height = 5.5, dpi = 350,
                        dir = "figures", formats = c("png", "svg")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (fmt in formats) {
    ggsave(file.path(dir, paste0(filename, ".", fmt)), plot = plot,
           width = width, height = height, dpi = dpi, bg = FRIDGE_SURF)
  }
  invisible(file.path(dir, paste0(filename, ".", formats)))
}

cat("FR-IDGE plotting charter loaded",
    "(theme_fridge, scale_*_fridge_*, fridge_save)\n")

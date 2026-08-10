# ============================================================================
# East Africa regional locator map (admin0 + admin1 boundaries)
# ============================================================================
#
# Clean, minimal reference map covering Somalia, Kenya and South Sudan plus
# their neighbours (Ethiopia, Eritrea, Djibouti, Sudan, Uganda, Tanzania,
# DR Congo, Rwanda, Burundi). All countries render grey with white admin1
# (state/province) outlines and a country-name label; the 3 focus countries
# render light blue with a heavier admin0 (national) boundary and larger,
# bold labels so they stand out against their neighbours. A specific set of
# admin1 units within the 3 focus countries (HIGHLIGHT_PCODES) render in a
# darker blue on top of that.
#
# Every country's admin1 *and* admin0 layer is its official OCHA COD-AB (or,
# for Djibouti, the GADM boundaries OCHA itself catalogues as that country's
# COD-AB) -- see data/gis/SOURCES.md for the full provenance table and
# retrieval dates for each.
#
# Fill and the national outline are drawn from each country's own ADM0
# file, not by dissolving ADM1 units: several of the raw ADM1 layers have
# small topology gaps between adjacent units (Djibouti's GADM layer has 7
# such gaps in its mainland alone -- checked via ring-counting a planar
# union), which show up as stray unfilled holes or odd doubled-looking
# border segments if you dissolve ADM1 into the country outline. Official
# ADM0 files are single, clean rings with no such gaps, so ADM1 is used
# here purely as an internal-line overlay (fill = NA) on top of the ADM0
# fill, and gaps in it no longer punch through to the surface.

suppressMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
})

# Some source layers (notably DRC/Uganda) have self-intersections that the
# spherical (s2) engine refuses to repair; the planar GEOS engine fixes them
# without issue and plotting accuracy doesn't need spherical precision.
sf::sf_use_s2(FALSE)

out_pdf <- "outputs/maps/east_africa_regional_map.pdf"

FOCUS_ADM0 <- list(
  Kenya       = "data/gis/ken_adm_ocha_20250108_ab_shp/ken_admin0.shp",
  Somalia     = "data/gis/som_adm_ocha_20250108_ab_shp/som_admin0.shp",
  `South Sudan` = "data/gis/ssd_admbnda_imwg_nbs_20230829_shp/ssd_admbnda_adm0_imwg_nbs_20230829.shp"
)
FOCUS_ADM1 <- list(
  Kenya       = "data/gis/ken_adm_ocha_20250108_ab_shp/ken_admin1.shp",
  Somalia     = "data/gis/som_adm_ocha_20250108_ab_shp/som_admin1.shp",
  `South Sudan` = "data/gis/ssd_admbnda_imwg_nbs_20230829_shp/ssd_admbnda_adm1_imwg_nbs_20230829.shp"
)
FOCUS_COUNTRIES <- names(FOCUS_ADM0)
# Column holding the admin1 P-code in each focus country's ADM1 attribute
# table (case differs by source agency).
FOCUS_PCODE_COL <- list(Kenya = "adm1_pcode", Somalia = "adm1_pcode",
                         `South Sudan` = "ADM1_PCODE")
FOCUS_NAME_COL  <- list(Kenya = "adm1_name", Somalia = "adm1_name",
                         `South Sudan` = "ADM1_EN")

# Admin1 units to render in a darker blue, e.g. specific conflict/priority
# areas -- pcodes as supplied and verified against each country's official
# ADM1 attribute table (name + pcode both matched).
HIGHLIGHT_PCODES <- c(
  "KE011", "KE010", "KE025", "KE031", "KE042", "KE040", "KE047", "KE003",
  "KE006", "KE009",
  "SS02", "SS08", "SS09",
  "SO21", "SO19",
  "SO25", "SO22", "SO24", "SO20", "SO28", "SO18"
)

# Sudan's ADM1 layer includes "Abyei PCA" (adm1_pcode SD19) as its own unit
# -- the official OCHA treatment of the Abyei Permanent Court of Arbitration
# area. Ethiopia's layer likewise includes a region named "Contested". Both
# are rendered here like any other admin1 unit (thin white boundary, no
# special hatching) since neither Sudan nor Ethiopia is a focus country.
NEIGHBOUR_ADM0 <- list(
  Ethiopia  = "data/gis/eth_adm_ocha_20260624_shp/eth_admin0.shp",
  Eritrea   = "data/gis/eri_adm_ocha_20260624_shp/eri_admin0.shp",
  Sudan     = "data/gis/sdn_adm_ocha_20260624_shp/sdn_admin0.shp",
  Uganda    = "data/gis/uga_adm_ocha_20260624_shp/uga_admin0.shp",
  `Democratic Republic of the Congo` =
    "data/gis/cod_adm_ocha_20260624_shp/cod_admin0.shp",
  Djibouti  = "data/gis/dji_adm_gadm_2022_shp/dji_admbnda_gadm_adm0_2022.shp",
  Tanzania  = "data/gis/tza_adm_ocha_20181019_shp/tza_admbnda_adm0_20181019.shp",
  Rwanda    = "data/gis/rwa_adm_nisr_20181002_shp/rwa_adm0_2006_NISR_WGS1984_20181002.shp",
  Burundi   = "data/gis/bdi_adm_ocha_20260624_shp/bdi_admin0.shp"
)
NEIGHBOUR_ADM1 <- list(
  Ethiopia  = "data/gis/eth_adm_ocha_20260624_shp/eth_admin1.shp",
  Eritrea   = "data/gis/eri_adm_ocha_20260624_shp/eri_admin1.shp",
  Sudan     = "data/gis/sdn_adm_ocha_20260624_shp/sdn_admin1.shp",
  Uganda    = "data/gis/uga_adm_ocha_20260624_shp/uga_admin1.shp",
  `Democratic Republic of the Congo` =
    "data/gis/cod_adm_ocha_20260624_shp/cod_admin1.shp",
  Djibouti  = "data/gis/dji_adm_gadm_2022_shp/dji_admbnda_gadm_adm1_2022.shp",
  Tanzania  = "data/gis/tza_adm_ocha_20181019_shp/tza_admbnda_adm1_20181019.shp",
  Rwanda    = "data/gis/rwa_adm_nisr_20181002_shp/rwa_adm1_2006_NISR_WGS1984_20181002.shp",
  Burundi   = "data/gis/bdi_adm_ocha_20260624_shp/bdi_admin1.shp"
)

FILL_FOCUS        <- "#BFE0EC"
FILL_HIGHLIGHT    <- "#5B9BC2"
FILL_OTHER        <- "grey85"
BORDER_ADM1       <- "white"
BORDER_ADM0       <- "grey35"
BORDER_ADM0_FOCUS <- "grey15"

load_layer <- function(shp_paths, is_focus) {
  bind_rows(lapply(names(shp_paths), function(cty) {
    st_read(shp_paths[[cty]], quiet = TRUE) |>
      st_make_valid() |>
      transmute(COUNTRY = cty, is_focus = is_focus)
  }))
}

# Focus-country ADM1 loaded separately (rather than via load_layer) so the
# pcode column -- needed to pick out HIGHLIGHT_PCODES -- survives; its name
# varies by source agency (see FOCUS_PCODE_COL).
load_focus_adm1 <- function() {
  bind_rows(lapply(names(FOCUS_ADM1), function(cty) {
    st_read(FOCUS_ADM1[[cty]], quiet = TRUE) |>
      st_make_valid() |>
      transmute(COUNTRY = cty, is_focus = TRUE,
                 pcode = .data[[FOCUS_PCODE_COL[[cty]]]],
                 adm1_name = .data[[FOCUS_NAME_COL[[cty]]]])
  }))
}

adm0 <- bind_rows(load_layer(FOCUS_ADM0, TRUE), load_layer(NEIGHBOUR_ADM0, FALSE))
adm1_focus <- load_focus_adm1()
adm1 <- bind_rows(select(adm1_focus, -pcode, -adm1_name), load_layer(NEIGHBOUR_ADM1, FALSE)) |>
  st_transform(st_crs(adm0))
adm1_highlight <- filter(adm1_focus, pcode %in% HIGHLIGHT_PCODES) |>
  st_transform(st_crs(adm0))

# Label points for the highlighted admin1 units -- these are small and
# fully inside the view window (unlike the country labels), so a plain
# point-on-surface with no clipping is enough. Warrap defaults to a point
# that lands right under the bold "South Sudan" country label, so it's
# manually moved elsewhere within its own borders (verified with
# st_intersects to still be inside the polygon). Laikipia and Galgaduud
# used to need the same treatment against the country labels' old
# positions, but those labels have since moved well clear (see
# COUNTRY_LABEL_NUDGE below), so both now sit at their true geometric
# centre (point-on-surface). Eastern Equatoria's geometric centre
# (34.0, 4.76) is close to true-centred but is a long word that still
# reaches back into "Juba" (31.58, 4.85) at this font size purely from
# text width, even though the two anchor points themselves are a
# reasonable distance apart -- nudged slightly north, short of the full
# corner-nudge used before, to clear that without leaving the centre.
LABEL_NUDGE <- list(
  SS08 = c(lon = 28.9,  lat = 8.7),  # Warrap -- shift north, away from "South Sudan"
  SS02 = c(lon = 33.8,  lat = 5.6),  # Eastern Equatoria -- nudge up-left, off "Juba", stay near centre
  SS09 = c(lon = 28.2,  lat = 7.3),  # Western Bahr el Ghazal -- nudge down-right from its centre, then back up a touch
  SO22 = c(lon = 44.9,  lat = 2.5)   # Banadir -- too small/close to Mogadishu to label in place; moved inland with a leader line (see BANADIR_LEADER)
)
# Banadir's true location, for the leader line pointing back from its
# moved label to the (tiny) region itself.
BANADIR_TRUE_POS <- c(lon = 45.33, lat = 2.08)
highlight_label_pts <- adm1_highlight |>
  st_point_on_surface() |>
  mutate(lon = st_coordinates(geometry)[, 1],
         lat = st_coordinates(geometry)[, 2])
for (pc in names(LABEL_NUDGE)) {
  i <- highlight_label_pts$pcode == pc
  highlight_label_pts$lon[i] <- LABEL_NUDGE[[pc]][["lon"]]
  highlight_label_pts$lat[i] <- LABEL_NUDGE[[pc]][["lat"]]
}

# National capitals of the 3 focus countries. Nairobi's capital marker
# lands on almost exactly the same point as the Nairobi *county* label
# above (the county is small and city-sized), so that admin1 label is
# dropped in favour of the capital one rather than drawing "Nairobi" twice
# on top of itself.
CAPITALS <- data.frame(
  country = c("Kenya", "Somalia", "South Sudan"),
  city    = c("Nairobi", "Mogadishu", "Juba"),
  lon     = c(36.8219, 45.3182, 31.5825),
  lat     = c(-1.2921, 2.0469, 4.8517)
)
highlight_label_pts <- filter(highlight_label_pts, pcode != "KE047")

adm0_other <- filter(adm0, !is_focus)
adm0_focus <- filter(adm0, is_focus)

# View window: bbox of the 3 focus countries plus a buffer so neighbouring
# countries show as partial context at the edges (same treatment the
# reference map gives Russia/Ukraine) without needing their full extent.
focus_bbox <- st_bbox(adm0_focus)
buffer_deg <- 2
map_xlim <- c(focus_bbox[["xmin"]] - buffer_deg, focus_bbox[["xmax"]] + buffer_deg)
map_ylim <- c(focus_bbox[["ymin"]] - buffer_deg, focus_bbox[["ymax"]] + buffer_deg)

mean_lat   <- mean(map_ylim)
map_aspect <- (diff(map_xlim) * cos(mean_lat * pi / 180)) / diff(map_ylim)
fig_height <- 8
fig_width  <- fig_height * map_aspect

# Country-name label points: countries that run off the edge of the crop
# (DRC, Ethiopia, Sudan, Tanzania...) have their true centroid well outside
# the visible area, so labels are placed on the *visible portion* of each
# country instead -- clip adm0 to the view window first, then take a
# guaranteed-inside point of what's left (st_point_on_surface, unlike a
# centroid, always lands inside an irregular/concave shape).
view_box <- st_as_sfc(st_bbox(c(xmin = map_xlim[1], xmax = map_xlim[2],
                                 ymin = map_ylim[1], ymax = map_ylim[2]),
                               crs = st_crs(adm0)))
label_pts <- adm0 |>
  st_intersection(view_box) |>
  st_point_on_surface() |>
  mutate(lon = st_coordinates(geometry)[, 1],
         lat = st_coordinates(geometry)[, 2],
         # "Democratic Republic of the Congo" is too long to fit on one
         # line at this label size without running off the visible sliver
         # of DRC shown at this crop; wrapped onto two lines instead of
         # shrinking just this one label's font size.
         label_text = ifelse(COUNTRY == "Democratic Republic of the Congo",
                              "Democratic Republic of\nCongo", COUNTRY))

# The 3 focus-country labels default to landing on top of one of that
# country's darker-blue highlighted admin1 units (e.g. South Sudan's label
# sits right on Warrap). Moved into a lighter, non-highlighted part of the
# same country instead -- verified against every HIGHLIGHT_PCODES polygon,
# not just the one originally underneath.
COUNTRY_LABEL_NUDGE <- list(
  Somalia       = c(lon = 46.5, lat = 9.0),   # Sool, in the north -- clear of the whole southern/central highlight cluster
  Kenya         = c(lon = 38.0, lat = -0.6),  # Kitui county -- right of and above Nairobi, on light-blue (non-highlighted) ground
  `South Sudan` = c(lon = 30.5, lat = 6.5)    # shift SE, off Warrap
)
for (cty in names(COUNTRY_LABEL_NUDGE)) {
  i <- label_pts$COUNTRY == cty
  label_pts$lon[i] <- COUNTRY_LABEL_NUDGE[[cty]][["lon"]]
  label_pts$lat[i] <- COUNTRY_LABEL_NUDGE[[cty]][["lat"]]
}

# Standard UN cartographic disclaimer -- required on UN-produced maps
# regardless of how minimal the rest of the design is.
disclaimer <- "The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations."

# Small legend, bottom-right corner. Positioned in the blank ocean area
# past Somalia's coast (Somalia's own coastline stays north of ~-2 lat, so
# this sits below and clear of it) and well east of Kenya/Tanzania -- it's
# allowed to overlap the empty corner of the *map panel*, just not any
# country's actual territory or labels.
# Row order fixed to match the annotate() calls below (grey swatch,
# focus-blue swatch, highlight-blue swatch, thin border, thick border,
# capital dot).
legend_box  <- data.frame(xmin = 45.0, xmax = 53.3, ymin = -6.7, ymax = -2.2)
legend_rows <- data.frame(
  y     = c(-2.7, -3.38, -4.06, -4.74, -5.42, -6.10),
  label = c("Neighbouring East Africa countries", "Study focus countries",
            "Admin1 areas covered by survey", "Country border",
            "Focus country border", "National capital")
)
legend_sw_x0 <- 45.5
legend_sw_x1 <- 46.3
legend_txt_x <- 46.55

p <- ggplot(adm0) +
  geom_sf(aes(fill = is_focus), colour = NA) +
  geom_sf(data = adm1_highlight, fill = FILL_HIGHLIGHT, colour = NA) +
  geom_sf(data = adm1, fill = NA, colour = BORDER_ADM1, linewidth = 0.15) +
  geom_sf(data = adm0_other, fill = NA, colour = BORDER_ADM0, linewidth = 0.4) +
  geom_sf(data = adm0_focus, fill = NA, colour = BORDER_ADM0_FOCUS, linewidth = 1.1) +
  scale_fill_manual(values = c(`TRUE` = FILL_FOCUS, `FALSE` = FILL_OTHER), guide = "none") +
  geom_text(
    data = filter(label_pts, !is_focus), aes(x = lon, y = lat, label = label_text),
    inherit.aes = FALSE, size = 6.2, fontface = "bold", colour = "grey15", lineheight = 0.9
  ) +
  geom_text(
    data = filter(label_pts, is_focus), aes(x = lon, y = lat, label = COUNTRY),
    inherit.aes = FALSE, size = 8.4, fontface = "bold", colour = "grey15"
  ) +
  # Leader line for Banadir: its label sits inland (see SO22 in
  # LABEL_NUDGE), so a thin line points back from there to the actual
  # (tiny) region, drawn before the label text so the line reads as
  # emerging from behind it rather than crossing on top.
  annotate("segment", x = BANADIR_TRUE_POS[["lon"]], y = BANADIR_TRUE_POS[["lat"]],
           xend = LABEL_NUDGE$SO22[["lon"]], yend = LABEL_NUDGE$SO22[["lat"]],
           colour = "grey25", linewidth = 0.45, linetype = "dashed") +
  geom_text(
    data = highlight_label_pts, aes(x = lon, y = lat, label = adm1_name),
    inherit.aes = FALSE, size = 4.2, fontface = "plain", colour = "grey5"
  ) +
  geom_point(
    data = CAPITALS, aes(x = lon, y = lat),
    inherit.aes = FALSE, shape = 21, size = 2.2, fill = "grey5", colour = "white", stroke = 0.5
  ) +
  geom_text(
    data = CAPITALS, aes(x = lon, y = lat, label = city),
    inherit.aes = FALSE, size = 5.0, fontface = "bold", colour = "grey5",
    hjust = 0, nudge_x = 0.35
  ) +
  # Legend, drawn last so it sits on top of everything else. Each swatch is
  # its own geom_rect with a literal (unmapped) fill colour rather than an
  # aes(fill=...) mapping, so it doesn't collide with the scale_fill_manual
  # already governing the country-fill aesthetic above.
  geom_rect(
    data = legend_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "white", colour = "grey60", linewidth = 0.3
  ) +
  annotate("rect", xmin = legend_sw_x0, xmax = legend_sw_x1,
           ymin = legend_rows$y[1] - 0.28, ymax = legend_rows$y[1] + 0.28,
           fill = FILL_OTHER, colour = "grey40", linewidth = 0.2) +
  annotate("rect", xmin = legend_sw_x0, xmax = legend_sw_x1,
           ymin = legend_rows$y[2] - 0.28, ymax = legend_rows$y[2] + 0.28,
           fill = FILL_FOCUS, colour = "grey40", linewidth = 0.2) +
  annotate("rect", xmin = legend_sw_x0, xmax = legend_sw_x1,
           ymin = legend_rows$y[3] - 0.28, ymax = legend_rows$y[3] + 0.28,
           fill = FILL_HIGHLIGHT, colour = "grey40", linewidth = 0.2) +
  annotate("segment", x = legend_sw_x0, xend = legend_sw_x1,
           y = legend_rows$y[4], yend = legend_rows$y[4],
           colour = BORDER_ADM0, linewidth = 0.4) +
  annotate("segment", x = legend_sw_x0, xend = legend_sw_x1,
           y = legend_rows$y[5], yend = legend_rows$y[5],
           colour = BORDER_ADM0_FOCUS, linewidth = 1.1) +
  annotate("point", x = (legend_sw_x0 + legend_sw_x1) / 2, y = legend_rows$y[6],
           shape = 21, size = 2.2, fill = "grey5", colour = "white", stroke = 0.5) +
  geom_text(
    data = legend_rows, aes(x = legend_txt_x, y = y, label = label),
    inherit.aes = FALSE, size = 3.0, fontface = "plain", colour = "grey15", hjust = 0
  ) +
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)

# Two versions: the standard one carries the UN cartographic disclaimer
# caption (required on UN-produced maps); the "_no_disclaimer" one is the
# identical map with that caption omitted, for contexts where it isn't
# wanted.
p_disclaimer <- p +
  labs(caption = disclaimer) +
  theme(plot.caption = element_text(size = 7.2, colour = "grey40",
                                     hjust = 0.5, margin = margin(t = 6)))
ggsave(out_pdf, p_disclaimer, width = fig_width, height = fig_height + 0.3,
       units = "in", device = cairo_pdf)
cat("Saved:", out_pdf, sprintf("(%.1f x %.1f in)\n", fig_width, fig_height))

out_pdf_no_disclaimer <- sub("\\.pdf$", "_no_disclaimer.pdf", out_pdf)
ggsave(out_pdf_no_disclaimer, p, width = fig_width, height = fig_height,
       units = "in", device = cairo_pdf)
cat("Saved:", out_pdf_no_disclaimer, sprintf("(%.1f x %.1f in)\n", fig_width, fig_height))

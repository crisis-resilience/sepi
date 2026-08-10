# GIS boundary sources

## SEPI focus countries (used throughout the indicator pipeline)

| Folder | Country | Source | Retrieved |
|---|---|---|---|
| `ken_adm_ocha_20250108_ab_shp` | Kenya | OCHA COD-AB | 2025-01-08 |
| `som_adm_ocha_20250108_ab_shp` | Somalia | OCHA COD-AB | 2025-01-08 |
| `ssd_admbnda_imwg_nbs_20230829_shp` | South Sudan | OCHA / IMWG / NBS COD-AB | 2023-08-29 |

## East Africa regional map — neighbouring/context countries

Downloaded 2026-07-23 from the Humanitarian Data Exchange (data.humdata.org),
each country's official "Subnational Administrative Boundaries" (COD-AB)
dataset, for use in `R/build_east_africa_regional_map.R`.

| Folder | Country | HDX dataset | Source agency | Metadata last modified |
|---|---|---|---|---|
| `eth_adm_ocha_20260624_shp` | Ethiopia | [cod-ab-eth](https://data.humdata.org/dataset/cod-ab-eth) | CSA + Regional BoFED | 2026-06-24 |
| `sdn_adm_ocha_20260624_shp` | Sudan | [cod-ab-sdn](https://data.humdata.org/dataset/cod-ab-sdn) | OCHA / IMWG | 2026-06-24 |
| `uga_adm_ocha_20260624_shp` | Uganda | [cod-ab-uga](https://data.humdata.org/dataset/cod-ab-uga) | Uganda Bureau of Statistics | 2026-06-24 |
| `cod_adm_ocha_20260624_shp` | DR Congo | [cod-ab-cod](https://data.humdata.org/dataset/cod-ab-cod) | Referentiel Geographique Commun | 2026-06-24 |
| `dji_adm_gadm_2022_shp` | Djibouti | [cod-ab-dji](https://data.humdata.org/dataset/cod-ab-dji) | GADM (P-coded/catalogued by OCHA) | 2025-06-27 |
| `tza_adm_ocha_20181019_shp` | Tanzania | [cod-ab-tza](https://data.humdata.org/dataset/cod-ab-tza) | Tanzania NBS / OCHA ROSA | boundary vintage 2018-10-19 |
| `rwa_adm_nisr_20181002_shp` | Rwanda | [cod-ab-rwa](https://data.humdata.org/dataset/cod-ab-rwa) | National Institute of Statistics of Rwanda (NISR) | boundary vintage 2018-10-02 |
| `bdi_adm_ocha_20260624_shp` | Burundi | [cod-ab-bdi](https://data.humdata.org/dataset/cod-ab-bdi) | IGEBU and OCHA Burundi | 2026-06-24 |
| `eri_adm_ocha_20260624_shp` | Eritrea | [cod-ab-eri](https://data.humdata.org/dataset/cod-ab-eri) | OCHA ROSEA | 2026-06-24 |

Notes:
- Djibouti's COD-AB is GADM-sourced rather than an OCHA field survey — it is
  still the dataset OCHA itself catalogues as the Djibouti COD-AB, but the
  underlying digitisation authority is GADM, not a national statistics body.
- Tanzania's HDX page flags that admin1/2/3 topology has known overlaps/gaps
  requiring cleanup; not fully re-validated here beyond `st_make_valid()`.
- Sudan's admin1 layer includes "Abyei PCA" (adm1_pcode `SD19`) as its own
  unit — the official OCHA treatment of the Abyei Permanent Court of
  Arbitration area. This map currently renders it like any other Sudan
  admin1 region (thin white boundary, grey fill, no special hatching) since
  Sudan is background/context here, not a focus country. If this map is ever
  repurposed to foreground Sudan/South Sudan specifically, UN cartographic
  convention would call for Abyei to get distinct (e.g. hatched, undetermined
  status) treatment rather than blending into either country.
- Ethiopia's admin1 layer likewise includes a region named "Contested" —
  left as-is for the same reason.

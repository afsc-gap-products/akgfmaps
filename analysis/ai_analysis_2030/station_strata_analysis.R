# Script for analysis of old/new AI bathymetry comparison
# Code managed and updated by Rebecca Howard

library(gapindex)
library(dplyr)
library(tidyr)
library(sf)
library(data.table)
library(ggplot2)
library(forcats)

## Increase default text size by 2 for all plots (title, axes, labels, legends)
theme_set(theme_gray(base_size = 13))

## Connect to Oracle
channel <- gapindex::get_connected()

## Load report species list and complex definitions
ai_spp_list <- read.csv(here::here("data", "ai_report_specieslist.csv"))
complex_lookup <- read.csv(here::here("data", "complex_lookup.csv"))

## Extract individual species codes and complex constituent species codes for AI
single_spp <- ai_spp_list |>
  filter(report == 1, !is.na(as.numeric(species_code))) |>
  pull(species_code) |>
  as.numeric()

complex_spp <- complex_lookup |>
  filter(
    region == "AI",
    complex %in% (ai_spp_list |> filter(report == 1, is.na(as.numeric(species_code))) |> pull(species_code))
  ) |>
  pull(species_code) |>
  as.numeric()

report_spp_codes <- sort(unique(c(single_spp, complex_spp)))

## Pull data from 1991-2026 for all report species & complex constituents (69 species)
gapindex_data <- gapindex::get_data(
  year_set = c(1991:2026),
  survey_set = "AI",
  spp_codes = report_spp_codes,
  haul_type = 3,
  abundance_haul = "Y",
  pull_lengths = FALSE,
  channel = channel)

## Pull new strata
ai_strata_2030 <-
  sf::st_read(
    dsn = here::here("data", "ai_strata_2030.gpkg")
  ) |>
  dplyr::mutate(AREA_TYPE = "STRATUM") 

## Fill in zeros and calculate CPUE
cpue <- gapindex::calc_cpue(gapdata = gapindex_data)

## Calculate stratum-level biomass, population abundance, mean CPUE and 
## associated variances
biomass_stratum <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data,
  cpue = cpue)

## Calculate aggregated biomass and population abundance across subareas,
## management areas, and regions
biomass_subareas <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data,
  biomass_stratum = biomass_stratum)

## -------------------------------------------------------------------------
## 1. Footprint comparison (2026 vs. 2030 design)
## -------------------------------------------------------------------------

## Stratum-level area comparison
strata_comparison <- gapindex_data$strata |>
  as_tibble() |>
  select(STRATUM, AREA_NAME, DESCRIPTION, AREA_KM2_2026 = AREA_KM2) |>
  left_join(
    gapindex_data$subarea |>
      as_tibble() |>
      filter(AREA_TYPE == "INPFC") |>
      select(AREA_ID, SUBAREA = AREA_NAME) |>
      inner_join(
        as_tibble(gapindex_data$stratum_groups) |> select(AREA_ID, STRATUM),
        by = "AREA_ID"
      ) |>
      select(STRATUM, SUBAREA),
    by = "STRATUM"
  ) |>
  left_join(
    ai_strata_2030 |>
      sf::st_drop_geometry() |>
      select(
        STRATUM = AREA_ID,
        DEPTH_MIN_M,
        DEPTH_MAX_M,
        AREA_KM2_2030 = AREA_KM2
      ),
    by = "STRATUM"
  ) |>
  mutate(
    SUBAREA = factor(
      SUBAREA,
      levels = c(
        "Western Aleutians",
        "Central Aleutians",
        "Eastern Aleutians",
        "Southern Bering Sea"
      )
    ),
    DEPTH_RANGE = factor(
      paste0(DEPTH_MIN_M, "-", DEPTH_MAX_M, " m"),
      levels = c("1-100 m", "101-200 m", "201-300 m", "301-500 m")
    ),
    STRATUM_LABEL = paste0(STRATUM, " (", AREA_NAME, ")"),
    DIFF_KM2 = AREA_KM2_2030 - AREA_KM2_2026,
    PCT_CHANGE = 100 * DIFF_KM2 / AREA_KM2_2026
  )

## Subarea, depth range, and total region area comparison
subarea_comparison <- gapindex_data$subarea |>
  as_tibble() |>
  select(AREA_TYPE, AREA_ID, AREA_NAME, DESCRIPTION) |>
  inner_join(as_tibble(gapindex_data$stratum_groups), by = "AREA_ID") |>
  inner_join(
    strata_comparison |> select(STRATUM, AREA_KM2_2026, AREA_KM2_2030),
    by = "STRATUM"
  ) |>
  group_by(AREA_TYPE, AREA_ID, AREA_NAME, DESCRIPTION) |>
  summarise(
    N_STRATA = n(),
    AREA_KM2_2026 = sum(AREA_KM2_2026),
    AREA_KM2_2030 = sum(AREA_KM2_2030),
    DIFF_KM2 = AREA_KM2_2030 - AREA_KM2_2026,
    PCT_CHANGE = 100 * DIFF_KM2 / AREA_KM2_2026,
    .groups = "drop"
  )

## -------------------------------------------------------------------------
## 2. 2030 biomass comparison workflow
## -------------------------------------------------------------------------

gapindex_data_2030 <- data.table::copy(gapindex_data)

## Replace 2026 stratum areas with 2030 stratum areas
gapindex_data_2030$strata <- gapindex_data_2030$strata |>
  rows_update(
    ai_strata_2030 |>
      sf::st_drop_geometry() |>
      select(STRATUM = AREA_ID, AREA_KM2),
    by = "STRATUM"
  ) |>
  as.data.table()

cpue_2030 <- gapindex::calc_cpue(gapdata = gapindex_data_2030)

biomass_stratum_2030 <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_2030,
  cpue = cpue_2030
)

biomass_subareas_2030 <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_2030,
  biomass_stratum = biomass_stratum_2030
)

## Compare stratum-level biomass across all survey years (1991-2026)
biomass_stratum_comparison <- biomass_stratum |>
  as_tibble() |>
  select(
    YEAR,
    SPECIES_CODE,
    STRATUM,
    BIOMASS_MT_CURRENT = BIOMASS_MT,
    BIOMASS_VAR_CURRENT = BIOMASS_VAR
  ) |>
  inner_join(
    biomass_stratum_2030 |>
      as_tibble() |>
      select(
        YEAR,
        SPECIES_CODE,
        STRATUM,
        BIOMASS_MT_2030 = BIOMASS_MT,
        BIOMASS_VAR_2030 = BIOMASS_VAR
      ),
    by = c("YEAR", "SPECIES_CODE", "STRATUM")
  ) |>
  left_join(
    strata_comparison |> select(STRATUM, AREA_NAME, DEPTH_RANGE),
    by = "STRATUM"
  ) |>
  mutate(
    DIFF_BIOMASS_MT = BIOMASS_MT_2030 - BIOMASS_MT_CURRENT,
    PCT_CHANGE_BIOMASS = if_else(
      BIOMASS_MT_CURRENT > 0,
      100 * DIFF_BIOMASS_MT / BIOMASS_MT_CURRENT,
      NA_real_
    )
  )

## Compare subarea/regional biomass across all survey years (1991-2026)
biomass_subarea_comparison <- biomass_subareas |>
  as_tibble() |>
  select(
    YEAR,
    SPECIES_CODE,
    AREA_ID,
    BIOMASS_MT_CURRENT = BIOMASS_MT,
    BIOMASS_VAR_CURRENT = BIOMASS_VAR
  ) |>
  inner_join(
    biomass_subareas_2030 |>
      as_tibble() |>
      select(
        YEAR,
        SPECIES_CODE,
        AREA_ID,
        BIOMASS_MT_2030 = BIOMASS_MT,
        BIOMASS_VAR_2030 = BIOMASS_VAR
      ),
    by = c("YEAR", "SPECIES_CODE", "AREA_ID")
  ) |>
  left_join(
    subarea_comparison |> select(AREA_ID, AREA_TYPE, AREA_NAME, DESCRIPTION),
    by = "AREA_ID"
  ) |>
  mutate(
    DIFF_BIOMASS_MT = BIOMASS_MT_2030 - BIOMASS_MT_CURRENT,
    PCT_CHANGE_BIOMASS = if_else(
      BIOMASS_MT_CURRENT > 0,
      100 * DIFF_BIOMASS_MT / BIOMASS_MT_CURRENT,
      NA_real_
    )
  )

## -------------------------------------------------------------------------
## Map constituent species to 33 report taxa (single species + complexes)
## -------------------------------------------------------------------------

single_map <- ai_spp_list |>
  filter(report == 1, !is.na(as.numeric(species_code))) |>
  mutate(SPECIES_CODE = as.integer(species_code)) |>
  select(reportorder, group, REPORT_CODE = species_code, REPORT_NAME = spp_name_informal, SPECIES_CODE)

complex_map <- ai_spp_list |>
  filter(report == 1, is.na(as.numeric(species_code))) |>
  inner_join(
    complex_lookup |> filter(region == "AI") |> select(complex, SPECIES_CODE = species_code),
    by = c("species_code" = "complex")
  ) |>
  mutate(SPECIES_CODE = as.integer(SPECIES_CODE)) |>
  select(reportorder, group, REPORT_CODE = species_code, REPORT_NAME = spp_name_informal, SPECIES_CODE)

all_report_map <- bind_rows(single_map, complex_map) |>
  arrange(reportorder, SPECIES_CODE)

## Aggregate subarea and depth-level biomass by report taxa (single species & complexes)
biomass_report_comparison <- all_report_map |>
  inner_join(
    as_tibble(biomass_subareas) |>
      select(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT_CURRENT = BIOMASS_MT, BIOMASS_VAR_CURRENT = BIOMASS_VAR),
    by = "SPECIES_CODE",
    relationship = "many-to-many"
  ) |>
  inner_join(
    as_tibble(biomass_subareas_2030) |>
      select(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT_2030 = BIOMASS_MT, BIOMASS_VAR_2030 = BIOMASS_VAR),
    by = c("YEAR", "SPECIES_CODE", "AREA_ID")
  ) |>
  group_by(reportorder, group, REPORT_CODE, REPORT_NAME, YEAR, AREA_ID) |>
  summarise(
    BIOMASS_MT_CURRENT = sum(BIOMASS_MT_CURRENT, na.rm = TRUE),
    BIOMASS_MT_2030 = sum(BIOMASS_MT_2030, na.rm = TRUE),
    BIOMASS_VAR_CURRENT = sum(BIOMASS_VAR_CURRENT, na.rm = TRUE),
    BIOMASS_VAR_2030 = sum(BIOMASS_VAR_2030, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    subarea_comparison |> select(AREA_ID, AREA_TYPE, AREA_NAME, DESCRIPTION),
    by = "AREA_ID"
  ) |>
  mutate(
    DIFF_BIOMASS_MT = BIOMASS_MT_2030 - BIOMASS_MT_CURRENT,
    PCT_CHANGE_BIOMASS = if_else(
      BIOMASS_MT_CURRENT > 0,
      100 * DIFF_BIOMASS_MT / BIOMASS_MT_CURRENT,
      NA_real_
    )
  )

## -------------------------------------------------------------------------
## 3. Visualizations of area changes (2026 vs. 2030)
## -------------------------------------------------------------------------

## Plot 1: Subarea by INPFC area changes
ggplot(
  subarea_comparison |>
    filter(AREA_TYPE %in% "INPFC") |>
    mutate(
      DESCRIPTION = fct_reorder(DESCRIPTION, DIFF_KM2),
      PCT_LABEL = sprintf("%+.2f%%", PCT_CHANGE)
    ),
  aes(x = DIFF_KM2, y = DESCRIPTION)
) +
  geom_col() +
  geom_text(
    aes(
      label = PCT_LABEL,
      hjust = if_else(DIFF_KM2 >= 0, -0.15, 1.15)
    ),
    size = 4
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = expansion(mult = c(0.50, 0.55))) +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = NULL,
    title = "Footprint Change by Subareas"
  ) +
  theme_linedraw(base_size = 16)

## Plot 1b: Footprint change by depth bin (with percentage labels)
ggplot(
  subarea_comparison |>
    filter(AREA_TYPE == "DEPTH") |>
    mutate(
      DESCRIPTION = fct_reorder(DESCRIPTION, DIFF_KM2),
      PCT_LABEL = sprintf("%+.2f%%", PCT_CHANGE)
    ),
  aes(x = DIFF_KM2, y = DESCRIPTION)
) +
  geom_col() +
  geom_text(
    aes(
      label = PCT_LABEL,
      hjust = if_else(DIFF_KM2 >= 0, -0.15, 1.15)
    ),
    size = 4
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = expansion(mult = c(0.40, 0.40))) +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = NULL,
    title = "Footprint Change by Depth Bin"
  ) +
  theme_linedraw(base_size = 16)

## Plot 2: Subarea by INPFC by depth changes (colored by subarea)
ggplot(
  subarea_comparison |>
    filter(AREA_TYPE %in% "INPFC BY DEPTH") |>
    mutate(
      AREA_NAME = factor(
        AREA_NAME,
        levels = c(
          "Western Aleutians",
          "Central Aleutians",
          "Eastern Aleutians",
          "Southern Bering Sea"
        )
      ),
      DESCRIPTION = fct_reorder(DESCRIPTION, DIFF_KM2)
    ),
  aes(x = DIFF_KM2, y = DESCRIPTION, fill = AREA_NAME)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = NULL,
    fill = "Subarea",
    title = "Footprint Change by Subarea and Depth"
  ) +
  theme_linedraw(base_size = 16)


## Plot 2a_depth: Subarea by INPFC by depth changes (colored by depth bin)
ggplot(
  subarea_comparison |>
    filter(AREA_TYPE == "INPFC BY DEPTH") |>
    mutate(
      DEPTH_BIN = factor(
        sub(".*\\s(\\d+\\sm\\s-\\s\\d+\\sm)$", "\\1", DESCRIPTION),
        levels = c(
          "1 m - 100 m",
          "101 m - 200 m",
          "201 m - 300 m",
          "301 m - 500 m"
        )
      ),
      DESCRIPTION = fct_reorder(DESCRIPTION, DIFF_KM2)
    ),
  aes(x = DIFF_KM2, y = DESCRIPTION, fill = DEPTH_BIN)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = NULL,
    fill = "Depth bin",
    title = "Footprint Change by Subarea and Depth (Colored by Depth Bin)"
  ) +
  theme_linedraw(base_size = 16)

## Plot 2b: Stratum-level area changes faceted by depth range
ggplot(
  strata_comparison |>
    mutate(STRATUM = fct_reorder(as.character(STRATUM), DIFF_KM2)),
  aes(x = DIFF_KM2, y = STRATUM, fill = SUBAREA)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ DEPTH_RANGE, scales = "free_y", ncol = 2) +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = "Stratum ID",
    fill = "Subarea",
    title = "Stratum Area Change by Depth Range"
  ) +
  theme_bw(base_size = 16)

## Plot 3a: Combined report-species biomass estimates across every survey year (1991-2026) by region and INPFC subarea
ggplot(
  biomass_report_comparison |>
    filter(AREA_TYPE %in% c("REGION", "INPFC")) |>
    group_by(YEAR, AREA_ID, AREA_TYPE, DESCRIPTION) |>
    summarise(
      BIOMASS_MT_CURRENT = sum(BIOMASS_MT_CURRENT, na.rm = TRUE),
      BIOMASS_MT_2030 = sum(BIOMASS_MT_2030, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c(BIOMASS_MT_CURRENT, BIOMASS_MT_2030),
      names_to = "DESIGN",
      values_to = "BIOMASS_MT"
    ) |>
    mutate(
      DESIGN = recode(
        DESIGN,
        "BIOMASS_MT_CURRENT" = "Current design (1991-2026)",
        "BIOMASS_MT_2030" = "New design (2030)"
      )
    ),
  aes(x = YEAR, y = BIOMASS_MT / 1000, color = DESIGN, linetype = DESIGN)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = unique(biomass_report_comparison$YEAR)) +
  facet_wrap(~ DESCRIPTION, scales = "free_y", ncol = 2) +
  labs(
    x = "Survey year",
    y = "Combined biomass (1,000 mt)",
    color = "Stratum design",
    linetype = "Stratum design",
    title = "Combined Report-Species Biomass (1991-2026): Current vs. 2030 Design"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) 

## Plot 3b: Annual percentage change in combined report-species biomass by subarea/region
ggplot(
  biomass_report_comparison |>
    filter(AREA_TYPE %in% c("REGION", "INPFC")) |>
    group_by(YEAR, AREA_ID, AREA_TYPE, DESCRIPTION) |>
    summarise(
      BIOMASS_MT_CURRENT = sum(BIOMASS_MT_CURRENT, na.rm = TRUE),
      BIOMASS_MT_2030 = sum(BIOMASS_MT_2030, na.rm = TRUE),
      PCT_CHANGE_BIOMASS = 100 * (BIOMASS_MT_2030 - BIOMASS_MT_CURRENT) / BIOMASS_MT_CURRENT,
      .groups = "drop"
    ),
  aes(x = factor(YEAR), y = PCT_CHANGE_BIOMASS)
) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ DESCRIPTION, ncol = 2, scales = "fixed") +
  labs(
    x = "Survey year",
    y = "Biomass change (%)",
    title = "Annual Combined Report-Species Biomass Change (%) by Subarea (2030 vs. Current)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_linedraw(base_size = 16)

## Plot 3c: Combined report-species biomass estimates across every survey year (1991-2026) for entire AI region
ggplot(
  biomass_report_comparison |>
    filter(AREA_TYPE == "REGION") |>
    group_by(YEAR, DESCRIPTION) |>
    summarise(
      BIOMASS_MT_CURRENT = sum(BIOMASS_MT_CURRENT, na.rm = TRUE),
      BIOMASS_MT_2030 = sum(BIOMASS_MT_2030, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c(BIOMASS_MT_CURRENT, BIOMASS_MT_2030),
      names_to = "DESIGN",
      values_to = "BIOMASS_MT"
    ) |>
    mutate(
      DESIGN = recode(
        DESIGN,
        "BIOMASS_MT_CURRENT" = "Current design (1991-2026)",
        "BIOMASS_MT_2030" = "New design (2030)"
      )
    ),
  aes(x = YEAR, y = BIOMASS_MT / 1000, color = DESIGN, linetype = DESIGN)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = unique(biomass_report_comparison$YEAR)) +
  labs(
    x = "Survey year",
    y = "Combined biomass (1,000 mt)",
    color = "Stratum design",
    linetype = "Stratum design",
    title = "Combined Report-Species Biomass (1991-2026): Entire AI Region"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  theme_linedraw(base_size = 16)

## Plot 3d: Combined report-species biomass estimates across every survey year (1991-2026) by depth bin
ggplot(
  biomass_report_comparison |>
    filter(AREA_TYPE == "DEPTH") |>
    group_by(YEAR, AREA_ID, DESCRIPTION) |>
    summarise(
      BIOMASS_MT_CURRENT = sum(BIOMASS_MT_CURRENT, na.rm = TRUE),
      BIOMASS_MT_2030 = sum(BIOMASS_MT_2030, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      DESCRIPTION = factor(
        DESCRIPTION,
        levels = c(
          "All areas 1-100 meters",
          "All areas 101-200 meters",
          "All areas 201-300 meters",
          "All areas 301-500 meters"
        )
      )
    ) |>
    pivot_longer(
      cols = c(BIOMASS_MT_CURRENT, BIOMASS_MT_2030),
      names_to = "DESIGN",
      values_to = "BIOMASS_MT"
    ) |>
    mutate(
      DESIGN = recode(
        DESIGN,
        "BIOMASS_MT_CURRENT" = "Current design (1991-2026)",
        "BIOMASS_MT_2030" = "New design (2030)"
      )
    ),
  aes(x = YEAR, y = BIOMASS_MT / 1000, color = DESIGN, linetype = DESIGN)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = unique(biomass_report_comparison$YEAR)) +
  facet_wrap(~ DESCRIPTION, scales = "free_y", ncol = 2) +
  labs(
    x = "Survey year",
    y = "Combined biomass (1,000 mt)",
    color = "Stratum design",
    linetype = "Stratum design",
    title = "Combined Report-Species Biomass by Depth Bin (1991-2026): Current vs. 2030 Design"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  theme_linedraw(base_size = 16)

## Plot 3e: Annual percentage change in combined report-species biomass by depth bin for every survey year
ggplot(
  biomass_report_comparison |>
    filter(AREA_TYPE == "DEPTH") |>
    group_by(YEAR, AREA_ID, DESCRIPTION) |>
    summarise(
      BIOMASS_MT_CURRENT = sum(BIOMASS_MT_CURRENT, na.rm = TRUE),
      BIOMASS_MT_2030 = sum(BIOMASS_MT_2030, na.rm = TRUE),
      PCT_CHANGE_BIOMASS = 100 * (BIOMASS_MT_2030 - BIOMASS_MT_CURRENT) / BIOMASS_MT_CURRENT,
      .groups = "drop"
    ) |>
    mutate(
      DESCRIPTION = factor(
        DESCRIPTION,
        levels = c(
          "All areas 1-100 meters",
          "All areas 101-200 meters",
          "All areas 201-300 meters",
          "All areas 301-500 meters"
        )
      )
    ),
  aes(x = factor(YEAR), y = PCT_CHANGE_BIOMASS)
) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ DESCRIPTION, scales = "fixed", ncol = 2) +
  labs(
    x = "Survey year",
    y = "Biomass change (%)",
    title = "Annual Combined Report-Species Biomass Change (%) by Depth Bin (2030 vs. Current)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  theme_linedraw(base_size = 16)

## -------------------------------------------------------------------------
## Separate species biomass plots by subarea
## -------------------------------------------------------------------------

plot_subarea_species_biomass <- function(subarea_name, species_per_plot = 11) {
  ranked_species <- biomass_report_comparison |>
    filter(AREA_TYPE == "INPFC", AREA_NAME == subarea_name) |>
    group_by(REPORT_NAME) |>
    summarise(MEAN_BIOMASS = mean(BIOMASS_MT_CURRENT, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(MEAN_BIOMASS)) |>
    pull(REPORT_NAME)

  species_chunks <- split(
    ranked_species,
    ceiling(seq_along(ranked_species) / species_per_plot)
  )

  plots <- lapply(seq_along(species_chunks), function(i) {
    chunk_spp <- species_chunks[[i]]
    sub_df <- biomass_report_comparison |>
      filter(AREA_TYPE == "INPFC", AREA_NAME == subarea_name, REPORT_NAME %in% chunk_spp) |>
      pivot_longer(
        cols = c(BIOMASS_MT_CURRENT, BIOMASS_MT_2030),
        names_to = "DESIGN",
        values_to = "BIOMASS_MT"
      ) |>
      mutate(
        DESIGN = recode(
          DESIGN,
          "BIOMASS_MT_CURRENT" = "Current design (1991-2026)",
          "BIOMASS_MT_2030" = "New design (2030)"
        ),
        REPORT_NAME = factor(REPORT_NAME, levels = chunk_spp)
      )

    ggplot(sub_df, aes(x = YEAR, y = BIOMASS_MT / 1000, color = DESIGN, linetype = DESIGN)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.3) +
      scale_x_continuous(breaks = c(1991, 2000, 2010, 2020, 2026)) +
      facet_wrap(~ REPORT_NAME, scales = "free_y", ncol = 3) +
      labs(
        x = "Survey year",
        y = "Biomass (1,000 mt)",
        color = "Stratum design",
        linetype = "Stratum design",
        title = paste0(subarea_name, " - Taxa Biomass (1991-2026)"),
        subtitle = paste0(
          "Set ", i, " of ", length(species_chunks),
          " (Species ", (i - 1) * species_per_plot + 1, "-",
          min(i * species_per_plot, length(ranked_species)),
          " ranked by mean biomass)"
        )
      ) +
      theme_bw() +
      theme(
        plot.margin = margin(t = 20, r = 15, b = 10, l = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10),
        panel.spacing.y = unit(1.2, "lines"),
        legend.position = "bottom"
      )
  })

  for (p in plots) {
    print(p)
  }

  invisible(plots)
}

## Generate separate biomass plots for each of the four INPFC subareas
plot_subarea_species_biomass("Western Aleutians")
plot_subarea_species_biomass("Central Aleutians")
plot_subarea_species_biomass("Eastern Aleutians")
plot_subarea_species_biomass("Southern Bering Sea")

## -------------------------------------------------------------------------
## Separate species biomass plots by depth bin
## -------------------------------------------------------------------------

plot_depth_species_biomass <- function(depth_name, species_per_plot = 11) {
  ranked_species <- biomass_report_comparison |>
    filter(AREA_TYPE == "DEPTH", DESCRIPTION == depth_name) |>
    group_by(REPORT_NAME) |>
    summarise(MEAN_BIOMASS = mean(BIOMASS_MT_CURRENT, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(MEAN_BIOMASS)) |>
    pull(REPORT_NAME)

  species_chunks <- split(
    ranked_species,
    ceiling(seq_along(ranked_species) / species_per_plot)
  )

  plots <- lapply(seq_along(species_chunks), function(i) {
    chunk_spp <- species_chunks[[i]]
    depth_df <- biomass_report_comparison |>
      filter(AREA_TYPE == "DEPTH", DESCRIPTION == depth_name, REPORT_NAME %in% chunk_spp) |>
      pivot_longer(
        cols = c(BIOMASS_MT_CURRENT, BIOMASS_MT_2030),
        names_to = "DESIGN",
        values_to = "BIOMASS_MT"
      ) |>
      mutate(
        DESIGN = recode(
          DESIGN,
          "BIOMASS_MT_CURRENT" = "Current design (1991-2026)",
          "BIOMASS_MT_2030" = "New design (2030)"
        ),
        REPORT_NAME = factor(REPORT_NAME, levels = chunk_spp)
      )

    ggplot(depth_df, aes(x = YEAR, y = BIOMASS_MT / 1000, color = DESIGN, linetype = DESIGN)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.3) +
      scale_x_continuous(breaks = c(1991, 2000, 2010, 2020, 2026)) +
      facet_wrap(~ REPORT_NAME, scales = "free_y", ncol = 3) +
      labs(
        x = "Survey year",
        y = "Biomass (1,000 mt)",
        color = "Stratum design",
        linetype = "Stratum design",
        title = paste0(depth_name, " - Taxa Biomass (1991-2026)"),
        subtitle = paste0(
          "Set ", i, " of ", length(species_chunks),
          " (Species ", (i - 1) * species_per_plot + 1, "-",
          min(i * species_per_plot, length(ranked_species)),
          " ranked by mean biomass)"
        )
      ) +
      theme_bw() +
      theme(
        plot.margin = margin(t = 20, r = 15, b = 10, l = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10),
        panel.spacing.y = unit(1.2, "lines"),
        legend.position = "bottom"
      )
  })

  for (p in plots) {
    print(p)
  }

  invisible(plots)
}

## Generate separate biomass plots for each of the four depth bins
plot_depth_species_biomass("All areas 1-100 meters")
plot_depth_species_biomass("All areas 101-200 meters")
plot_depth_species_biomass("All areas 201-300 meters")
plot_depth_species_biomass("All areas 301-500 meters")

## -------------------------------------------------------------------------
## Separate species biomass plots for the entire AI region
## -------------------------------------------------------------------------

plot_region_species_biomass <- function(species_per_plot = 11) {
  ranked_species <- biomass_report_comparison |>
    filter(AREA_TYPE == "REGION") |>
    group_by(REPORT_NAME) |>
    summarise(MEAN_BIOMASS = mean(BIOMASS_MT_CURRENT, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(MEAN_BIOMASS)) |>
    pull(REPORT_NAME)

  species_chunks <- split(
    ranked_species,
    ceiling(seq_along(ranked_species) / species_per_plot)
  )

  plots <- lapply(seq_along(species_chunks), function(i) {
    chunk_spp <- species_chunks[[i]]
    reg_df <- biomass_report_comparison |>
      filter(AREA_TYPE == "REGION", REPORT_NAME %in% chunk_spp) |>
      pivot_longer(
        cols = c(BIOMASS_MT_CURRENT, BIOMASS_MT_2030),
        names_to = "DESIGN",
        values_to = "BIOMASS_MT"
      ) |>
      mutate(
        DESIGN = recode(
          DESIGN,
          "BIOMASS_MT_CURRENT" = "Current design (1991-2026)",
          "BIOMASS_MT_2030" = "New design (2030)"
        ),
        REPORT_NAME = factor(REPORT_NAME, levels = chunk_spp)
      )

    ggplot(reg_df, aes(x = YEAR, y = BIOMASS_MT / 1000, color = DESIGN, linetype = DESIGN)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.3) +
      scale_x_continuous(breaks = c(1991, 2000, 2010, 2020, 2026)) +
      facet_wrap(~ REPORT_NAME, scales = "free_y", ncol = 3) +
      labs(
        x = "Survey year",
        y = "Biomass (1,000 mt)",
        color = "Stratum design",
        linetype = "Stratum design",
        title = "AI Region - Taxa Biomass (1991-2026)",
        subtitle = paste0(
          "Set ", i, " of ", length(species_chunks),
          " (Species ", (i - 1) * species_per_plot + 1, "-",
          min(i * species_per_plot, length(ranked_species)),
          " ranked by mean biomass)"
        )
      ) +
      theme(
        plot.margin = margin(t = 20, r = 15, b = 10, l = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(size = 10),
        panel.spacing.y = unit(1.2, "lines"),
        legend.position = "bottom"
      )
  })

  for (p in plots) {
    print(p)
  }

  invisible(plots)
}

## Generate separate biomass plots for the entire AI region
plot_region_species_biomass()

## -------------------------------------------------------------------------
## 4. Spatial polygon boundary comparison (2026 akgfmaps vs. ai_strata_2030)
## -------------------------------------------------------------------------

ai_base_2026 <- akgfmaps::get_base_layers(
  select.region = "ai",
  set.crs = sf::st_crs(ai_strata_2030)
)

ai_strata_2026 <- ai_base_2026$survey.strata |>
  sf::st_make_valid() |>
  mutate(STRATUM = as.integer(STRATUM))

ai_strata_2030_valid <- ai_strata_2030 |>
  sf::st_make_valid() |>
  mutate(STRATUM = as.integer(AREA_ID))

## Pairwise polygon intersection across all 2026 and 2030 strata
strata_ix <- suppressWarnings(
  sf::st_intersection(
    ai_strata_2026 |> select(STRATUM_2026 = STRATUM),
    ai_strata_2030_valid |> select(STRATUM_2030 = STRATUM)
  )
) |>
  mutate(IX_AREA_KM2 = as.numeric(sf::st_area(geometry)) / 1e6) |>
  sf::st_drop_geometry()

## Per-stratum spatial boundary breakdown
strata_spatial_comparison <- strata_comparison |>
  select(STRATUM, AREA_NAME, DEPTH_RANGE) |>
  left_join(
    ai_strata_2026 |>
      mutate(POLY_KM2_2026 = as.numeric(sf::st_area(ai_strata_2026)) / 1e6) |>
      sf::st_drop_geometry() |>
      select(STRATUM, POLY_KM2_2026),
    by = "STRATUM"
  ) |>
  left_join(
    ai_strata_2030_valid |>
      mutate(POLY_KM2_2030 = as.numeric(sf::st_area(ai_strata_2030_valid)) / 1e6) |>
      sf::st_drop_geometry() |>
      select(STRATUM, POLY_KM2_2030),
    by = "STRATUM"
  ) |>
  left_join(
    strata_ix |>
      filter(STRATUM_2026 == STRATUM_2030) |>
      select(STRATUM = STRATUM_2026, SAME_STRATUM_KM2 = IX_AREA_KM2),
    by = "STRATUM"
  ) |>
  left_join(
    strata_ix |>
      filter(STRATUM_2026 != STRATUM_2030) |>
      group_by(STRATUM = STRATUM_2026) |>
      summarise(LOST_TO_OTHER_STRATA_KM2 = sum(IX_AREA_KM2), .groups = "drop"),
    by = "STRATUM"
  ) |>
  left_join(
    strata_ix |>
      filter(STRATUM_2026 != STRATUM_2030) |>
      group_by(STRATUM = STRATUM_2030) |>
      summarise(GAINED_FROM_OTHER_STRATA_KM2 = sum(IX_AREA_KM2), .groups = "drop"),
    by = "STRATUM"
  ) |>
  mutate(
    across(
      c(SAME_STRATUM_KM2, LOST_TO_OTHER_STRATA_KM2, GAINED_FROM_OTHER_STRATA_KM2),
      ~ replace_na(.x, 0)
    ),
    LOST_OUTSIDE_FOOTPRINT_KM2 = pmax(
      0,
      POLY_KM2_2026 - SAME_STRATUM_KM2 - LOST_TO_OTHER_STRATA_KM2
    ),
    GAINED_OUTSIDE_FOOTPRINT_KM2 = pmax(
      0,
      POLY_KM2_2030 - SAME_STRATUM_KM2 - GAINED_FROM_OTHER_STRATA_KM2
    ),
    PCT_2026_RETAINED = 100 * SAME_STRATUM_KM2 / POLY_KM2_2026
  )

## Plot 4: Spatial boundary turnover per stratum (Gained vs. Lost components)
ggplot(
  strata_spatial_comparison |>
    mutate(
      LOST_TO_OTHER_STRATA_KM2 = -LOST_TO_OTHER_STRATA_KM2,
      LOST_OUTSIDE_FOOTPRINT_KM2 = -LOST_OUTSIDE_FOOTPRINT_KM2,
      NET_DIFF_KM2 = POLY_KM2_2030 - POLY_KM2_2026,
      STRATUM = fct_reorder(as.character(STRATUM), NET_DIFF_KM2)
    ) |>
    pivot_longer(
      cols = c(
        GAINED_FROM_OTHER_STRATA_KM2,
        GAINED_OUTSIDE_FOOTPRINT_KM2,
        LOST_TO_OTHER_STRATA_KM2,
        LOST_OUTSIDE_FOOTPRINT_KM2
      ),
      names_to = "CHANGE_TYPE",
      values_to = "AREA_KM2"
    ) |>
    mutate(
      CHANGE_TYPE = recode(
        CHANGE_TYPE,
        "GAINED_FROM_OTHER_STRATA_KM2" = "Gained from adjacent strata",
        "GAINED_OUTSIDE_FOOTPRINT_KM2" = "Added outside 2026 footprint",
        "LOST_TO_OTHER_STRATA_KM2" = "Lost to adjacent strata",
        "LOST_OUTSIDE_FOOTPRINT_KM2" = "Removed outside 2030 footprint"
      )
    ),
  aes(x = AREA_KM2, y = STRATUM, fill = CHANGE_TYPE)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ DEPTH_RANGE, scales = "free_y", ncol = 2) +
  labs(
    x = "Polygon area gained (+) or lost (-), km²",
    y = "Stratum ID",
    fill = "Boundary change",
    title = "Spatial Polygon Boundary Turnover (2026 vs. 2030)"
  ) +
  theme_bw(base_size = 16)

## Build spatial polygon layer classifying footprint changes
ix_sf <- suppressWarnings(
  sf::st_intersection(
    ai_strata_2026 |> select(STRATUM_2026 = STRATUM),
    ai_strata_2030_valid |> select(STRATUM_2030 = STRATUM, AREA_NAME)
  )
) |>
  mutate(
    CHANGE_CATEGORY = if_else(
      STRATUM_2026 == STRATUM_2030,
      "Unchanged stratum",
      "Reclassified between strata"
    )
  ) |>
  select(STRATUM = STRATUM_2030, CHANGE_CATEGORY) |>
  sf::st_make_valid()

footprint_2026_union <- sf::st_union(ai_strata_2026)
footprint_2030_union <- sf::st_union(ai_strata_2030_valid)

added_sf <- suppressWarnings(
  sf::st_difference(
    ai_strata_2030_valid |> select(STRATUM),
    footprint_2026_union
  )
) |>
  mutate(CHANGE_CATEGORY = "Added outside 2026 footprint") |>
  select(STRATUM, CHANGE_CATEGORY) |>
  sf::st_make_valid()

removed_sf <- suppressWarnings(
  sf::st_difference(
    ai_strata_2026 |> select(STRATUM),
    footprint_2030_union
  )
) |>
  mutate(CHANGE_CATEGORY = "Removed outside 2030 footprint") |>
  select(STRATUM, CHANGE_CATEGORY) |>
  sf::st_make_valid()

sf::st_geometry(ix_sf) <- "geometry"
sf::st_geometry(added_sf) <- "geometry"
sf::st_geometry(removed_sf) <- "geometry"

stratum_inpfc <- gapindex_data$subarea |>
  as_tibble() |>
  filter(AREA_TYPE == "INPFC") |>
  select(AREA_ID, INPFC_AREA = AREA_NAME) |>
  inner_join(
    as_tibble(gapindex_data$stratum_groups) |> select(AREA_ID, STRATUM),
    by = "AREA_ID"
  ) |>
  select(STRATUM, INPFC_AREA)

footprint_change_sf <- bind_rows(ix_sf, added_sf, removed_sf) |>
  filter(!sf::st_is_empty(geometry)) |>
  left_join(stratum_inpfc, by = "STRATUM") |>
  mutate(
    CHANGE_CATEGORY = factor(
      CHANGE_CATEGORY,
      levels = c(
        "Unchanged stratum",
        "Reclassified between strata",
        "Added outside 2026 footprint",
        "Removed outside 2030 footprint"
      )
    )
  )

## Calculate area and percentage for each boundary change category across the full AI region
region_spatial_comparison <- footprint_change_sf |>
  mutate(AREA_KM2 = as.numeric(sf::st_area(geometry)) / 1e6) |>
  sf::st_drop_geometry() |>
  group_by(CHANGE_CATEGORY) |>
  summarise(AREA_KM2 = sum(AREA_KM2), .groups = "drop") |>
  mutate(
    AREA_KM2_2026 = subarea_comparison$AREA_KM2_2026[subarea_comparison$AREA_TYPE == "REGION"],
    AREA_KM2_2030 = subarea_comparison$AREA_KM2_2030[subarea_comparison$AREA_TYPE == "REGION"],
    PCT_OF_2026 = 100 * AREA_KM2 / AREA_KM2_2026,
    PCT_OF_2030 = 100 * AREA_KM2 / AREA_KM2_2030,
    CATEGORY_LABEL = sprintf(
      "%s: %s km² (%.1f%%)",
      CHANGE_CATEGORY,
      format(round(AREA_KM2, 0), big.mark = ",", trim = TRUE),
      PCT_OF_2026
    )
  )

## Plot 5: Full-region map of added, removed, and reclassified polygons with area and % labels
ggplot(
  footprint_change_sf |>
    left_join(
      region_spatial_comparison |> select(CHANGE_CATEGORY, CATEGORY_LABEL),
      by = "CHANGE_CATEGORY"
    ) |>
    mutate(
      CATEGORY_LABEL = factor(
        CATEGORY_LABEL,
        levels = region_spatial_comparison$CATEGORY_LABEL[
          region_spatial_comparison$CHANGE_CATEGORY != "Unchanged stratum"
        ]
      )
    )
) +
  geom_sf(
    data = ai_base_2026$akland,
    fill = "grey35",
    color = NA
  ) +
  geom_sf(
    data = footprint_change_sf |> filter(CHANGE_CATEGORY == "Unchanged stratum"),
    fill = "grey82",
    color = NA
  ) +
  geom_sf(
    data = footprint_change_sf |>
      filter(CHANGE_CATEGORY != "Unchanged stratum") |>
      left_join(
        region_spatial_comparison |> select(CHANGE_CATEGORY, CATEGORY_LABEL),
        by = "CHANGE_CATEGORY"
      ) |>
      mutate(
        CATEGORY_LABEL = factor(
          CATEGORY_LABEL,
          levels = region_spatial_comparison$CATEGORY_LABEL[
            region_spatial_comparison$CHANGE_CATEGORY != "Unchanged stratum"
          ]
        )
      ),
    aes(fill = CATEGORY_LABEL),
    color = NA
  ) +
  coord_sf(
    xlim = ai_base_2026$plot.boundary$x,
    ylim = ai_base_2026$plot.boundary$y
  ) +
  guides(fill = guide_legend(ncol = 1, title.position = "top")) +
  labs(
    fill = "Boundary change (% of 2026 regional footprint)",
    title = "AI Survey Footprint Changes (2026 vs. 2030)",
    subtitle = sprintf(
      "Unchanged stratum (light grey): %s km² (%.1f%% of 2026 area)",
      format(
        round(
          region_spatial_comparison$AREA_KM2[
            region_spatial_comparison$CHANGE_CATEGORY == "Unchanged stratum"
          ],
          0
        ),
        big.mark = ",",
        trim = TRUE
      ),
      region_spatial_comparison$PCT_OF_2026[
        region_spatial_comparison$CHANGE_CATEGORY == "Unchanged stratum"
      ]
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

## Calculate area and percentage for each boundary change category by INPFC subarea
subarea_spatial_comparison <- footprint_change_sf |>
  mutate(AREA_KM2 = as.numeric(sf::st_area(geometry)) / 1e6) |>
  sf::st_drop_geometry() |>
  group_by(INPFC_AREA, CHANGE_CATEGORY) |>
  summarise(AREA_KM2 = sum(AREA_KM2), .groups = "drop") |>
  left_join(
    subarea_comparison |>
      filter(AREA_TYPE == "INPFC") |>
      select(INPFC_AREA = AREA_NAME, AREA_KM2_2026, AREA_KM2_2030),
    by = "INPFC_AREA"
  ) |>
  mutate(
    INPFC_AREA = factor(
      INPFC_AREA,
      levels = c(
        "Western Aleutians",
        "Central Aleutians",
        "Eastern Aleutians",
        "Southern Bering Sea"
      )
    ),
    PCT_OF_2026 = 100 * AREA_KM2 / AREA_KM2_2026,
    PCT_OF_2030 = 100 * AREA_KM2 / AREA_KM2_2030
  ) |>
  arrange(INPFC_AREA, CHANGE_CATEGORY)

## Helper to map footprint changes for a single INPFC subarea with area and % labels
plot_subarea_footprint <- function(subarea_name) {
  subarea_stats <- subarea_spatial_comparison |>
    filter(INPFC_AREA == subarea_name) |>
    mutate(
      CATEGORY_LABEL = sprintf(
        "%s: %s km² (%.1f%%)",
        CHANGE_CATEGORY,
        format(round(AREA_KM2, 0), big.mark = ",", trim = TRUE),
        PCT_OF_2026
      )
    )

  unchanged_stat <- subarea_stats |>
    filter(CHANGE_CATEGORY == "Unchanged stratum")

  subarea_sf <- footprint_change_sf |>
    filter(INPFC_AREA == subarea_name) |>
    left_join(
      subarea_stats |> select(CHANGE_CATEGORY, CATEGORY_LABEL),
      by = "CHANGE_CATEGORY"
    ) |>
    mutate(
      CATEGORY_LABEL = factor(
        CATEGORY_LABEL,
        levels = subarea_stats$CATEGORY_LABEL[
          subarea_stats$CHANGE_CATEGORY != "Unchanged stratum"
        ]
      )
    )

  bbox <- sf::st_bbox(subarea_sf)

  ggplot(subarea_sf) +
    geom_sf(
      data = ai_base_2026$akland,
      fill = "grey35",
      color = NA
    ) +
    geom_sf(
      data = subarea_sf |> filter(CHANGE_CATEGORY == "Unchanged stratum"),
      fill = "grey82",
      color = NA
    ) +
    geom_sf(
      data = subarea_sf |> filter(CHANGE_CATEGORY != "Unchanged stratum"),
      aes(fill = CATEGORY_LABEL),
      color = NA
    ) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    guides(fill = guide_legend(ncol = 1, title.position = "top")) +
    labs(
      fill = "Boundary change (% of 2026 subarea)",
      title = paste0(subarea_name, ": Footprint Changes (2026 vs. 2030)"),
      subtitle = sprintf(
        "Unchanged stratum (light grey): %s km² (%.1f%% of 2026 area)",
        format(round(unchanged_stat$AREA_KM2, 0), big.mark = ",", trim = TRUE),
        unchanged_stat$PCT_OF_2026
      )
    ) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom")
}

## Separate spatial maps for each of the four subareas
plot_subarea_footprint("Western Aleutians")
plot_subarea_footprint("Central Aleutians")
plot_subarea_footprint("Eastern Aleutians")
plot_subarea_footprint("Southern Bering Sea")

## -------------------------------------------------------------------------
## 5. Trawlability status comparison (2026 vs. 2030 station grids)
## -------------------------------------------------------------------------

ai_stations_2026 <- sf::st_read(
  dsn = here::here("data", "current_ai_stations.gpkg"),
  quiet = TRUE
)

ai_stations_2030 <- sf::st_read(
  dsn = here::here("data", "ai_stations_2030.gpkg"),
  quiet = TRUE
)

stns_2026_df <- ai_stations_2026 |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  mutate(
    TRAWLABLE = coalesce(TRAWLABLE, "UNK"),
    AREA_KM2 = AREA_M2 / 1e6
  )

stns_2030_df <- ai_stations_2030 |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  mutate(
    TRAWLABLE = coalesce(TRAWLABLE, "UNK"),
    AREA_KM2 = as.numeric(AREA_KM2)
  )

## Regional and subarea trawlability comparison
trawl_region_comparison <- stns_2026_df |>
  group_by(TRAWLABLE) |>
  summarise(
    N_STATIONS_2026 = n(),
    AREA_KM2_2026 = sum(AREA_KM2),
    .groups = "drop"
  ) |>
  full_join(
    stns_2030_df |>
      group_by(TRAWLABLE) |>
      summarise(
        N_STATIONS_2030 = n(),
        AREA_KM2_2030 = sum(AREA_KM2),
        .groups = "drop"
      ),
    by = "TRAWLABLE"
  ) |>
  mutate(
    SUBAREA = "All AI Region",
    TRAWLABLE_STATUS = factor(
      recode(
        TRAWLABLE,
        "Y" = "Trawlable (Y)",
        "N" = "Untrawlable (N)",
        "UNK" = "Unknown (UNK)"
      ),
      levels = c("Trawlable (Y)", "Untrawlable (N)", "Unknown (UNK)")
    ),
    DIFF_STATIONS = N_STATIONS_2030 - N_STATIONS_2026,
    PCT_CHANGE_STATIONS = 100 * DIFF_STATIONS / N_STATIONS_2026,
    DIFF_KM2 = AREA_KM2_2030 - AREA_KM2_2026,
    PCT_CHANGE_KM2 = 100 * DIFF_KM2 / AREA_KM2_2026
  )

trawl_subarea_comparison <- stns_2026_df |>
  left_join(
    strata_comparison |> select(STRATUM, SUBAREA, DEPTH_RANGE),
    by = "STRATUM"
  ) |>
  group_by(SUBAREA, TRAWLABLE) |>
  summarise(
    N_STATIONS_2026 = n(),
    AREA_KM2_2026 = sum(AREA_KM2),
    .groups = "drop"
  ) |>
  full_join(
    stns_2030_df |>
      left_join(
        strata_comparison |> select(STRATUM, SUBAREA, DEPTH_RANGE),
        by = "STRATUM"
      ) |>
      group_by(SUBAREA, TRAWLABLE) |>
      summarise(
        N_STATIONS_2030 = n(),
        AREA_KM2_2030 = sum(AREA_KM2),
        .groups = "drop"
      ),
    by = c("SUBAREA", "TRAWLABLE")
  ) |>
  mutate(
    TRAWLABLE_STATUS = factor(
      recode(
        TRAWLABLE,
        "Y" = "Trawlable (Y)",
        "N" = "Untrawlable (N)",
        "UNK" = "Unknown (UNK)"
      ),
      levels = c("Trawlable (Y)", "Untrawlable (N)", "Unknown (UNK)")
    ),
    DIFF_STATIONS = N_STATIONS_2030 - N_STATIONS_2026,
    PCT_CHANGE_STATIONS = 100 * DIFF_STATIONS / N_STATIONS_2026,
    DIFF_KM2 = AREA_KM2_2030 - AREA_KM2_2026,
    PCT_CHANGE_KM2 = 100 * DIFF_KM2 / AREA_KM2_2026
  )

## Depth range trawlability comparison
trawl_depth_comparison <- stns_2026_df |>
  left_join(
    strata_comparison |> select(STRATUM, SUBAREA, DEPTH_RANGE),
    by = "STRATUM"
  ) |>
  group_by(DEPTH_RANGE, TRAWLABLE) |>
  summarise(
    N_STATIONS_2026 = n(),
    AREA_KM2_2026 = sum(AREA_KM2),
    .groups = "drop"
  ) |>
  full_join(
    stns_2030_df |>
      left_join(
        strata_comparison |> select(STRATUM, SUBAREA, DEPTH_RANGE),
        by = "STRATUM"
      ) |>
      group_by(DEPTH_RANGE, TRAWLABLE) |>
      summarise(
        N_STATIONS_2030 = n(),
        AREA_KM2_2030 = sum(AREA_KM2),
        .groups = "drop"
      ),
    by = c("DEPTH_RANGE", "TRAWLABLE")
  ) |>
  mutate(
    TRAWLABLE_STATUS = factor(
      recode(
        TRAWLABLE,
        "Y" = "Trawlable (Y)",
        "N" = "Untrawlable (N)",
        "UNK" = "Unknown (UNK)"
      ),
      levels = c("Trawlable (Y)", "Untrawlable (N)", "Unknown (UNK)")
    ),
    DIFF_STATIONS = N_STATIONS_2030 - N_STATIONS_2026,
    PCT_CHANGE_STATIONS = 100 * DIFF_STATIONS / N_STATIONS_2026,
    DIFF_KM2 = AREA_KM2_2030 - AREA_KM2_2026,
    PCT_CHANGE_KM2 = 100 * DIFF_KM2 / AREA_KM2_2026
  ) +
  theme_bw(base_size = 16)

## Plot 7a: Regional and subarea area change (km² and %) by trawlability status
ggplot(
  bind_rows(
    trawl_region_comparison |> mutate(SUBAREA = as.character(SUBAREA)),
    trawl_subarea_comparison |> mutate(SUBAREA = as.character(SUBAREA))
  ) |>
    mutate(
      SUBAREA = factor(
        SUBAREA,
        levels = rev(c(
          "All AI Region",
          "Western Aleutians",
          "Central Aleutians",
          "Eastern Aleutians",
          "Southern Bering Sea"
        ))
      ),
      CHANGE_LABEL = sprintf("%+.0f km² (%+.1f%%)", DIFF_KM2, PCT_CHANGE_KM2)
    ),
  aes(x = DIFF_KM2, y = SUBAREA)
) +
  geom_col() +
  geom_text(
    aes(
      label = CHANGE_LABEL,
      hjust = if_else(DIFF_KM2 >= 0, -0.06, 1.06)
    ),
    size = 5.0
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = expansion(mult = c(0.60, 0.60))) +
  facet_wrap(~ TRAWLABLE_STATUS, scales = "free_x", ncol = 1) +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = NULL,
    title = "Footprint Change by Trawlability Status Across Region and Subareas"
  ) +
  theme_bw(base_size = 16)

## Plot 7b: Depth range area change (km² and %) by trawlability status
ggplot(
  trawl_depth_comparison |>
    mutate(
      DEPTH_RANGE = fct_rev(DEPTH_RANGE),
      CHANGE_LABEL = sprintf("%+.0f km² (%+.1f%%)", DIFF_KM2, PCT_CHANGE_KM2)
    ),
  aes(x = DIFF_KM2, y = DEPTH_RANGE)
) +
  geom_col() +
  geom_text(
    aes(
      label = CHANGE_LABEL,
      hjust = if_else(DIFF_KM2 >= 0, -0.06, 1.06)
    ),
    size = 5.0
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(expand = expansion(mult = c(0.60, 0.60))) +
  facet_wrap(~ TRAWLABLE_STATUS, scales = "free_x", ncol = 1) +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = "Depth range",
    title = "Footprint Change by Trawlability Status and Depth Range"
  ) +
  theme_bw(base_size = 16)

## Individual stratum trawlability comparison
trawl_stratum_comparison <- expand_grid(
  STRATUM = unique(strata_comparison$STRATUM),
  TRAWLABLE = c("Y", "N", "UNK")
) |>
  left_join(
    strata_comparison |> select(STRATUM, AREA_NAME, SUBAREA, DEPTH_RANGE),
    by = "STRATUM"
  ) |>
  left_join(
    stns_2026_df |>
      group_by(STRATUM, TRAWLABLE) |>
      summarise(
        N_STATIONS_2026 = n(),
        AREA_KM2_2026 = sum(AREA_KM2),
        .groups = "drop"
      ),
    by = c("STRATUM", "TRAWLABLE")
  ) |>
  left_join(
    stns_2030_df |>
      group_by(STRATUM, TRAWLABLE) |>
      summarise(
        N_STATIONS_2030 = n(),
        AREA_KM2_2030 = sum(AREA_KM2),
        .groups = "drop"
      ),
    by = c("STRATUM", "TRAWLABLE")
  ) |>
  mutate(
    across(c(N_STATIONS_2026, N_STATIONS_2030), ~ replace_na(.x, 0L)),
    across(c(AREA_KM2_2026, AREA_KM2_2030), ~ replace_na(.x, 0)),
    TRAWLABLE_STATUS = factor(
      recode(
        TRAWLABLE,
        "Y" = "Trawlable (Y)",
        "N" = "Untrawlable (N)",
        "UNK" = "Unknown (UNK)"
      ),
      levels = c("Trawlable (Y)", "Untrawlable (N)", "Unknown (UNK)")
    ),
    DIFF_STATIONS = N_STATIONS_2030 - N_STATIONS_2026,
    PCT_CHANGE_STATIONS = if_else(
      N_STATIONS_2026 > 0,
      100 * DIFF_STATIONS / N_STATIONS_2026,
      NA_real_
    ),
    DIFF_KM2 = AREA_KM2_2030 - AREA_KM2_2026,
    PCT_CHANGE_KM2 = if_else(
      AREA_KM2_2026 > 0,
      100 * DIFF_KM2 / AREA_KM2_2026,
      NA_real_
    )
  )

## Plot 7c: Stratum-level area change (km²) broken down by trawlability status
ggplot(
  trawl_stratum_comparison |>
    group_by(STRATUM) |>
    mutate(NET_DIFF_KM2 = sum(DIFF_KM2)) |>
    ungroup() |>
    mutate(STRATUM = fct_reorder(as.character(STRATUM), NET_DIFF_KM2)),
  aes(x = DIFF_KM2, y = STRATUM, fill = TRAWLABLE_STATUS)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ DEPTH_RANGE, scales = "free_y", ncol = 2) +
  labs(
    x = "Area change (2026 to 2030, km²)",
    y = "Stratum ID",
    fill = "Trawlability",
    title = "Stratum Footprint Change by Trawlability Status and Depth Range"
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

## Plot 7d: Stratum-level percentage change in Trawlable (Y) area colored by subarea
ggplot(
  trawl_stratum_comparison |>
    filter(TRAWLABLE == "Y") |>
    mutate(STRATUM = fct_reorder(as.character(STRATUM), PCT_CHANGE_KM2)),
  aes(x = PCT_CHANGE_KM2, y = STRATUM, fill = SUBAREA)
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ DEPTH_RANGE, scales = "free_y", ncol = 2) +
  labs(
    x = "Trawlable (Y) area change (%)",
    y = "Stratum ID",
    fill = "Subarea",
    title = "Stratum Percentage Change in Trawlable (Y) Area by Depth Range"
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

## -------------------------------------------------------------------------
## 6. Haul depth range, footprint, and performance code evaluation
## -------------------------------------------------------------------------

racebase_haul <- read.csv(here::here("data", "racebase_ai_haul.csv"))

stratum_depths <- ai_strata_2030 |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  select(STRATUM = AREA_ID, DEPTH_MIN_M, DEPTH_MAX_M)

hauls_coords <- racebase_haul |>
  as_tibble() |>
  mutate(
    YEAR = floor(CRUISE / 100),
    LON_START = coalesce(START_LONGITUDE, END_LONGITUDE),
    LAT_START = coalesce(START_LATITUDE, END_LATITUDE),
    HAS_COORDS = !is.na(LON_START) & !is.na(LAT_START)
  ) |>
  filter(HAS_COORDS) |>
  left_join(
    stratum_depths |>
      rename(ORIG_DEPTH_MIN = DEPTH_MIN_M, ORIG_DEPTH_MAX = DEPTH_MAX_M),
    by = "STRATUM"
  )

pts_start <- sf::st_as_sf(
  hauls_coords,
  coords = c("START_LONGITUDE", "START_LATITUDE"),
  crs = 4326
) |>
  sf::st_transform(sf::st_crs(ai_strata_2030_valid))

pts_end <- hauls_coords |>
  mutate(
    END_LON = coalesce(END_LONGITUDE, START_LONGITUDE),
    END_LAT = coalesce(END_LATITUDE, START_LATITUDE)
  ) |>
  sf::st_as_sf(coords = c("END_LON", "END_LAT"), crs = 4326) |>
  sf::st_transform(sf::st_crs(ai_strata_2030_valid))

coords_mid_mat <- (sf::st_coordinates(pts_start) + sf::st_coordinates(pts_end)) / 2

pts_mid <- hauls_coords |>
  mutate(X_MID = coords_mid_mat[, 1], Y_MID = coords_mid_mat[, 2]) |>
  sf::st_as_sf(
    coords = c("X_MID", "Y_MID"),
    crs = sf::st_crs(ai_strata_2030_valid)
  )

join_2026_mid <- sf::st_join(
  pts_mid |> select(HAULJOIN),
  ai_strata_2026 |> select(STRATUM_2026_MID = STRATUM)
) |>
  sf::st_drop_geometry() |>
  distinct(HAULJOIN, .keep_all = TRUE)

join_2030_mid <- sf::st_join(
  pts_mid |> select(HAULJOIN),
  ai_strata_2030_valid |>
    select(
      STRATUM_2030_MID = STRATUM,
      NEW_MIN_MID = DEPTH_MIN_M,
      NEW_MAX_MID = DEPTH_MAX_M
    )
) |>
  sf::st_drop_geometry() |>
  distinct(HAULJOIN, .keep_all = TRUE)

hauls_eval <- hauls_coords |>
  left_join(join_2026_mid, by = "HAULJOIN") |>
  left_join(join_2030_mid, by = "HAULJOIN") |>
  mutate(
    IN_ORIG_DEPTH_BOT = !is.na(BOTTOM_DEPTH) & !is.na(ORIG_DEPTH_MIN) &
      BOTTOM_DEPTH >= ORIG_DEPTH_MIN & BOTTOM_DEPTH <= ORIG_DEPTH_MAX,
    IN_NEW_DEPTH_BOT = !is.na(BOTTOM_DEPTH) & !is.na(NEW_MIN_MID) &
      BOTTOM_DEPTH >= NEW_MIN_MID & BOTTOM_DEPTH <= NEW_MAX_MID,
    IN_ORIG_DEPTH_GEAR = !is.na(GEAR_DEPTH) & !is.na(ORIG_DEPTH_MIN) &
      GEAR_DEPTH >= ORIG_DEPTH_MIN & GEAR_DEPTH <= ORIG_DEPTH_MAX,
    IN_NEW_DEPTH_GEAR = !is.na(GEAR_DEPTH) & !is.na(NEW_MIN_MID) &
      GEAR_DEPTH >= NEW_MIN_MID & GEAR_DEPTH <= NEW_MAX_MID
  )

hauls_abund <- hauls_eval |>
  filter(HAULJOIN %in% gapindex_data$haul$HAULJOIN)

## Classify flagged survey tows from Questions 1-4 (1991-2026 HAUL_TYPE == 3)
flagged_tows_sf <- pts_mid |>
  select(HAULJOIN) |>
  inner_join(
    hauls_eval |>
      filter(
        CRUISE >= 199100 & HAUL_TYPE == 3 & !is.na(STRATUM)
      ) |>
      mutate(
        TOW_CATEGORY = case_when(
          PERFORMANCE %in% c(-6.12) & IN_NEW_DEPTH_BOT ~
            "Q4: Neg. depth performance, in 2030 depth range",
          is.na(STRATUM_2030_MID) ~
            "Q3: Still outside 2030 footprint",
          is.na(STRATUM_2026_MID) & !is.na(STRATUM_2030_MID) ~
            "Q2: Outside 2026 footprint, now in 2030 footprint",
          IN_ORIG_DEPTH_BOT & !is.na(STRATUM_2030_MID) & !IN_NEW_DEPTH_BOT ~
            "Q1: In assigned depth range, outside 2030 depth range",
          TRUE ~ NA_character_
        ),
        ANY_STRATUM = coalesce(STRATUM_2030_MID, STRATUM_2026_MID, STRATUM)
      ) |>
      filter(!is.na(TOW_CATEGORY)) |>
      left_join(
        strata_comparison |> select(ANY_STRATUM = STRATUM, SUBAREA),
        by = "ANY_STRATUM"
      ),
    by = "HAULJOIN"
  ) |>
  mutate(
    TOW_CATEGORY = factor(
      TOW_CATEGORY,
      levels = c(
        "Q1: In assigned depth range, outside 2030 depth range",
        "Q2: Outside 2026 footprint, now in 2030 footprint",
        "Q3: Still outside 2030 footprint",
        "Q4: Neg. depth performance, in 2030 depth range"
      )
    )
  )

## Helper to map flagged tows on individual INPFC subareas
plot_subarea_flagged_tows <- function(subarea_name) {
  subarea_poly <- ai_strata_2030_valid |>
    left_join(stratum_inpfc, by = "STRATUM") |>
    filter(INPFC_AREA == subarea_name)

  subarea_tows <- flagged_tows_sf |>
    filter(SUBAREA == subarea_name)

  bbox <- sf::st_bbox(bind_rows(
    subarea_poly |> select(geometry = geom),
    subarea_tows |> select(geometry)
  ))

  ggplot(subarea_poly) +
    geom_sf(
      data = ai_base_2026$akland,
      fill = "grey35",
      color = NA
    ) +
    geom_sf(
      fill = "grey85",
      color = "grey60",
      linewidth = 0.2
    ) +
    geom_sf(
      data = subarea_tows,
      aes(color = TOW_CATEGORY),
      size = 2.6
    ) +
    scale_color_discrete(drop = FALSE) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    guides(color = guide_legend(ncol = 1, title.position = "top")) +
    labs(
      color = "Flagged tow category",
      title = paste0(subarea_name, ": Flagged Tows (1991-2026 Survey Hauls)"),
      subtitle = sprintf("Total flagged tows in subarea: %d", nrow(subarea_tows))
    ) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom")
}

## Plot 8: Subarea maps of flagged tows from Questions 1-4
plot_subarea_flagged_tows("Western Aleutians")
plot_subarea_flagged_tows("Central Aleutians")
plot_subarea_flagged_tows("Eastern Aleutians")
plot_subarea_flagged_tows("Southern Bering Sea")










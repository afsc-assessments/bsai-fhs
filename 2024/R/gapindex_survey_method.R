# https://github.com/afsc-gap-products/gapindex/blob/main/vignettes/ex_agecomps_ebs_standard.Rmd

sql_channel <- gapindex::get_connected()
spp_start_year <-
  RODBC::sqlQuery(channel = sql_channel, 
                  query = "SELECT * FROM GAP_PRODUCTS.SPECIES_YEAR")

production_data <- gapindex::get_data(
  year_set = 1982:2023,
  survey_set = "EBS",
  spp_codes = NULL,
  pull_lengths = TRUE, 
  haul_type = 3, 
  abundance_haul = "Y",
  sql_channel = sql_channel)

## Remove hauls and data associated with hauls in strata 82 and 90
ebs_standard_hauls <- 
  with(production_data$haul, HAULJOIN[!(STRATUM %in% c(82, 90))])

production_data$haul <- 
  subset(x = production_data$haul, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$catch <- 
  subset(x = production_data$catch, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$size <- 
  subset(x = production_data$size, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$specimen <- 
  subset(x = production_data$specimen, 
         subset = HAULJOIN %in% ebs_standard_hauls)
production_data$strata <- 
  subset(x = production_data$strata, 
         subset = !(STRATUM %in% c(82, 90)))

## Remove subareas associated with the EBS + NW region
production_data$subarea <- 
  subset(x = production_data$subarea, 
         subset = !(AREA_ID %in% c(7, 8, 9, 100, 200, 300, 99900)))

## Calculate and zero-fill CPUE
production_cpue <-
  gapindex::calc_cpue(racebase_tables = production_data)

## Calculate biomass/abundance (w/variance), mean/variance CPUE across strata
production_biomass_stratum <-
  gapindex::calc_biomass_stratum(
    racebase_tables = production_data,
    cpue = production_cpue)

## Calculate size composition by stratum. Since the two regions have
## different functions, sizecomp_fn toggles which function to use
## and then it is called in the do.call function.
production_sizecomp_stratum <- 
  gapindex::calc_sizecomp_stratum(
    racebase_tables = production_data,
    racebase_cpue = production_cpue,
    racebase_stratum_popn = production_biomass_stratum,
    spatial_level = "stratum",
    fill_NA_method = "BS")

# Calculate regional ALK only including hauls in the EBS Standard Region
production_alk <- 
  subset(x = gapindex::calc_alk(
    racebase_tables = production_data,
    unsex = c("all", "unsex")[1], 
    global = FALSE),
    subset = AGE_FRAC > 0)

## Calculate age composition by stratum
production_agecomp_stratum <- 
  gapindex::calc_agecomp_stratum(
    racebase_tables = production_data,
    alk = production_alk,
    size_comp = production_sizecomp_stratum)

## Aggregate `production_agecomp_stratum` to subareas and regions
production_agecomp_region <- 
  gapindex::calc_agecomp_region(
    racebase_tables = production_data,
    age_comps_stratum = production_agecomp_stratum)

# Change "STRATUM" field name to "AREA_ID"
names(x = production_agecomp_stratum$age_comp)[
  names(x = production_agecomp_stratum$age_comp) == "STRATUM"] <- "AREA_ID"

production_agecomp <- 
  rbind(production_agecomp_region,
        production_agecomp_stratum$age_comp[, names(x = production_agecomp_region)])

production_agecomp %>% 
  filter(SPECIES_CODE == 10130 & AGE > 0 & !is.na(LENGTH_MM_MEAN)) %>%
  mutate(
    # sex = ifelse(sex == 1,2,1),  ## SS sex is inverse of this dataset
    length_grp = cut(round(LENGTH_MM_MEAN/10,0) ,
                     breaks = seq(0,60,2),
                     right = FALSE,
                     labels = FALSE),
    length_bin_use = seq(0,60,2)[length_grp]) %>%
  tidytable::mutate(tot = tidytable::n(), .by = YEAR) %>%
  # tidytable::filter(tot>49) %>%
  tidytable::mutate(n_h = length(unique(na.omit(haul_join))) +
                      length(unique(na.omit(port_join))),
                    .by = year) %>%
  tidytable::summarise(n_s = mean(tot),
                       n_h = mean(n_h),
                       age_tot = tidytable::n(),
                       .by = c(year, age))
  

production_data$specimen %>% filter(SPECIES_CODE == 10130 & !is.na(AGE) & !is.na(LENGTH) & SEX !=3) %>%
  group_by(YEAR) %>%
  summarise(n_h = length(unique(HAUL)))




## For the SPECIES_CODE values in spp_start_year, constrain the agecomp tables 
## to only the years when we feel confident about their taxonomic accuracy, 
## e.g., remove northern rock sole values prior to 1996.
for (irow in 1:nrow(x = spp_start_year)) { ## Loop over species -- start
  production_agecomp <- subset(
    x = production_agecomp,
    subset = !(SPECIES_CODE == spp_start_year$SPECIES_CODE[irow] & 
                 YEAR < spp_start_year$YEAR_STARTED[irow]))
} ## Loop over species -- end
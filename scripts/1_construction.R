## =============================================================================
## 01_data_construction.R
## -----------------------------------------------------------------------------
## Dissertation: "When grievances become formal claims: Explaining variation in
##   complaint filing before subnational human rights institutions in Mexico"
## MY499 Dissertation — MSc Social Research Methods, LSE
##
## PURPOSE: Build the analytical panel (32 states x 6 years, 2019-2024) from
##          raw sources. Produces: data/processed/panel_mexico_shri_2019_2024.csv
##
## RAW SOURCES (in data/raw/):
##   cndhe/     - INEGI CNDHE censuses (complaints, staff, budget, offices)
##   sat/       - SAT Donatarias Autorizadas (NGO density, independent variable)
##   sesnsp/    - SESNSP homicides (control)
##   conapo/    - CONAPO population (per-capita denominator)
##   pibe/      - INEGI state GDP (control)
##   red_tdt/   - Red TDT directory (cross-validation)
##   gobernadores/ - governor party (political alignment)
##
## NOTE ON REPRODUCIBILITY: This script documents the construction logic in R.
##   Several raw INEGI/SAT files require manual sheet/column identification
##   (documented inline). Paths assume the working directory is the repo root.
## =============================================================================

## ---- 0. Setup --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)

# Working directory = repository root
# setwd("path/to/my499-dissertation")

# GDP deflators, base 2020 = 100 (from INEGI)
deflators <- tibble(
  year = 2019:2024,
  deflator = c(95.612, 100.0, 104.507, 111.460, 116.654, 122.375)
)

# State code catalogue (INEGI CVE_ENT)
state_catalogue <- tibble(
  cve_ent = sprintf("%02d", 1:32),
  state = c("Aguascalientes","Baja California","Baja California Sur","Campeche",
            "Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México",
            "Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México",
            "Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
            "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
            "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
)

## ---- 1. Dependent variable: complaints (CNDHE M2) --------------------------
## The CNDHE is published annually; each edition covers the prior year.
## Complaints ("solicitudes de queja recibidas") are on the sheet indicated
## below for each edition. Both total and accepted complaints are extracted.
##
##   Data year | File (data/raw/cndhe/) | Sheet | Total col | Accepted col
##   2019      | CNDHE2020_M2.xlsx      | 6     | D         | E
##   2020      | cndhe2021_M2.xlsx      | 7     | D         | E
##   2021      | cndhe2022_M2.xlsx      | 8     | D         | E
##   2022      | cndhe2023_M2.xlsx      | 9     | D         | E
##   2023      | cndhe2024_M2.xlsx      | 7     | D         | E
##   2024      | cndhe2025_M2.xlsx      | 9     | E         | F

read_complaints <- function(file, sheet, year, col_total, col_accepted,
                             col_cve) {
  raw <- read_excel(file.path("data/raw/cndhe", file), sheet = sheet,
                    col_names = FALSE)
  # Keep only state-level rows (level == "Estatal"); parse manually because
  # INEGI headers span several rows. Users should verify row offsets per file.
  raw %>%
    filter(.[[1]] == "Estatal") %>%
    transmute(
      cve_ent = sprintf("%02d", as.integer(.[[col_cve]])),
      year = year,
      complaints_total = as.numeric(.[[col_total]]),
      complaints_accepted = as.numeric(.[[col_accepted]])
    )
}

complaints <- bind_rows(
  read_complaints("CNDHE2020_M2.xlsx", "6", 2019, 4, 5, 2),
  read_complaints("cndhe2021_M2.xlsx", "7", 2020, 4, 5, 2),
  read_complaints("cndhe2022_M2.xlsx", "8", 2021, 4, 5, 2),
  read_complaints("cndhe2023_M2.xlsx", "9", 2022, 4, 5, 2),
  read_complaints("cndhe2024_M2.xlsx", "7", 2023, 4, 5, 2),
  read_complaints("cndhe2025_M2.xlsx", "9", 2024, 5, 6, 3)
)

## ---- 2. Institutional capacity: staff, budget, offices (CNDHE M1) ----------
## Staff (total and visitadurías), budget (exercised, in pesos), and number of
## offices come from the M1 module. Sheet numbers vary by edition and must be
## verified against each file's index. Budget is later deflated to 2020 pesos.

# read_m1() analogous to read_complaints(); omitted here for brevity.
# Produces: institutional <- (cve_ent, year, staff_total, staff_visitadurias,
#                             budget_nominal, offices_total)
# See repository notes for exact sheet mappings per edition.

## ---- 3. Independent variable: NGO density (SAT) ----------------------------
## For each fiscal year 2018-2023, read the SAT Donatarias directory and
## classify organisations as human-rights NGOs using a keyword filter on the
## registered name and social purpose (see Appendix B for the keyword list).
## Count active organisations per state, then lag one year (t-1 predicts t).

hr_keywords_name <- c("derechos humanos","derechos civiles","justicia y derechos",
                      "defensa de derechos","derechos y justicia")

hr_keywords_purpose <- c(
  "derechos humanos","defensa de derechos humanos",
  "promoción de los derechos humanos","protección de derechos humanos",
  "violaciones a derechos humanos","violaciones de derechos humanos",
  "acceso a la justicia","asistencia jurídica gratuita","defensa jurídica",
  "representación legal gratuita","litigio estratégico","acompañamiento jurídico",
  "víctimas de violaciones","defensores de derechos humanos",
  "tortura y tratos crueles","desaparición forzada",
  "perspectiva de derechos humanos","exigibilidad de derechos","justiciabilidad",
  "mecanismos de protección de derechos",
  "incidencia en política pública de derechos"
)

classify_hr_ngo <- function(name, purpose) {
  name <- tolower(iconv(name, to = "ASCII//TRANSLIT"))
  purpose <- tolower(iconv(purpose, to = "ASCII//TRANSLIT"))
  kw_name <- tolower(iconv(hr_keywords_name, to = "ASCII//TRANSLIT"))
  kw_purp <- tolower(iconv(hr_keywords_purpose, to = "ASCII//TRANSLIT"))
  hit_name <- str_detect(name, str_c(kw_name, collapse = "|"))
  hit_purp <- str_detect(purpose, str_c(kw_purp, collapse = "|"))
  hit_name | hit_purp
}

read_sat_year <- function(file, fiscal_year) {
  sat <- read_excel(file.path("data/raw/sat", file))
  names(sat) <- toupper(names(sat))
  sat %>%
    mutate(is_hr = classify_hr_ngo(.data[["DENOMINACION"]],
                                   .data[["OBJETO SOCIAL"]])) %>%
    filter(is_hr) %>%
    mutate(cve_ent = str_pad(match(str_to_title(ENTIDAD),
                                   state_catalogue$state), 2, pad = "0")) %>%
    count(cve_ent, name = "cso_broad") %>%
    mutate(fiscal_year = fiscal_year)
}

# sat_counts <- bind_rows(lapply 2018:2023) ; then lag:
# cso_broad_lag[year] = cso_broad[year - 1]

## ---- 4. Controls: homicides (SESNSP) --------------------------------------
## Filter subtype == "Homicidio doloso", sum the twelve monthly columns,
## aggregate by state-year. Homicide rate = homicides / population * 100000.

# homicides <- read_csv("data/raw/sesnsp/Estatal-Delitos-...csv") %>% ...

## ---- 5. Population (CONAPO) ------------------------------------------------
## Sum population across age and sex by state-year (2019-2024). Used as the
## per-capita denominator and as the model offset.

# population <- read_csv(unz("data/raw/conapo/poblacion.zip", "...")) %>% ...

## ---- 6. State GDP per capita (INEGI PIBE) ----------------------------------
## Extract "Millones de pesos a precios de 2018 | B.1bP" per state-year;
## divide by population for GDP per capita.

## ---- 7. Political alignment (governors) ------------------------------------
alignment <- read_csv("data/raw/gobernadores/governors_political_alignment_2019_2024.csv") %>%
  transmute(cve_ent, year,
            governor, governor_party,
            political_alignment = political_alignment_morena)

## ---- 8. Cross-validation source: Red TDT -----------------------------------
red_tdt <- read_csv("data/raw/red_tdt/red_tdt_organizations_by_state.csv") %>%
  transmute(cve_ent, red_tdt_n = red_tdt_organizations,
            red_tdt_dummy = as.integer(red_tdt_organizations > 0))

## ---- 8b. Educational outreach events (CNDHE M2/M1) -------------------------
## Number of human rights education/outreach events per state-year, used in the
## outreach robustness check (Appendix G). Extracted from the CNDHE events
## sheets (M2 for 2019-2024; the 2018 figure comes from CNDHE2019_M1 sheet 1.13
## to build the t-1 lag for 2019). Only the event count is complete across all
## years; attendance breakdowns are not. The variable enters the model lagged.
##
##   Data year | File                | Module/Sheet
##   2018      | CNDHE2019_M1.xlsx   | 1.13 (events only)
##   2019      | CNDHE2020_M2.xlsx   | events sheet
##   ...       | ...                 | ...
##   2024      | cndhe2025_M2.xlsx   | events sheet
##
# events <- (cve_ent, year, n_eventos) ; then lag one year:
# n_eventos_lag[year] = n_eventos[year - 1]; log_eventos_lag = log(n_eventos_lag)

## ---- 9. Assemble the panel -------------------------------------------------
## Merge all components on (cve_ent, year); compute per-capita rates, logs,
## and the deflated budget. Zeros in NGO counts become NA under the log
## transform (documented in Appendix B; see also the log(x+1) robustness check).

panel <- state_catalogue %>%
  tidyr::crossing(year = 2019:2024) %>%
  left_join(complaints, by = c("cve_ent","year")) %>%
  # left_join(institutional, ...) %>%
  # left_join(ngo_lagged, ...) %>%
  # left_join(homicides, ...) %>%
  # left_join(population, ...) %>%
  # left_join(gdp, ...) %>%
  left_join(alignment, by = c("cve_ent","year")) %>%
  left_join(red_tdt, by = "cve_ent") %>%
  left_join(deflators, by = "year")

panel <- panel %>%
  mutate(
    complaints_per100k          = complaints_total / population * 1e5,
    complaints_accepted_per100k = complaints_accepted / population * 1e5,
    budget_real2020             = budget_nominal / (deflator / 100),
    log_complaints_per100k      = log(complaints_per100k),
    log_complaints_accepted_per100k = log(complaints_accepted_per100k),
    log_cso_broad_lag           = ifelse(cso_broad_lag > 0, log(cso_broad_lag), NA),
    log_staff                   = log(staff_total),
    log_budget_real             = log(budget_real2020),
    log_homicide_rate           = log(homicide_rate),
    log_gdp_percapita           = log(gdp_percapita_2018),
    log_eventos_lag             = ifelse(n_eventos_lag > 0, log(n_eventos_lag), NA)
  )

## ---- 10. Export ------------------------------------------------------------
## Rename the state key to `state_id` for consistency with 02_analysis.R,
## which expects columns `state_id` (2-digit code) and `state` (name).
panel <- panel %>% rename(state_id = cve_ent)

write_csv(panel, "data/processed/panel_mexico_shri_2019_2024.csv")
message("Panel written: ", nrow(panel), " rows x ", ncol(panel), " cols")

## =============================================================================
## END 01_data_construction.R
## =============================================================================

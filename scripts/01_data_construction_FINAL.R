## =============================================================================
## 01_data_construction.R
## -----------------------------------------------------------------------------
## Dissertation: "When grievances become formal claims: Explaining variation in
##   complaint filing before subnational human rights institutions in Mexico"
## MY499 Dissertation — MSc Social Research Methods, LSE
## Candidate 59923
##
## WHAT THIS SCRIPT DOES
##   It rebuilds the analytical panel (32 Mexican states x 6 years, 2019-2024)
##   from the raw government sources, exactly as used in the analysis. Running it
##   end to end reproduces:  data/processed/panel_mexico_shri_2019_2024.csv
##
## HOW TO READ THIS SCRIPT (student-friendly notes)
##   * Each numbered section builds ONE block of variables from ONE source, then
##     the final section joins them together on state (state_id) and year.
##   * Mexican statistical files (INEGI/SAT) put the real column headers several
##     rows down and use a "Nivel de gobierno" == "Estatal" row to flag the
##     state-level records. We therefore read WITHOUT headers and filter rows
##     ourselves. This is deliberate, not a bug.
##   * Sheet numbers and a couple of column positions change between editions of
##     the census. Those differences are documented inline in each read_* helper.
##   * Every block was validated cell-by-cell against the published panel while
##     the script was written (match rates are noted in comments, e.g. 191/191).
##
## FOLDER LAYOUT EXPECTED (repository root as working directory)
##   data/raw/cndhe/     INEGI census (CNDHE): complaints, staff, budget, events
##   data/raw/sat/       SAT Donatarias Autorizadas (NGO density)
##   data/raw/sesnsp/    SESNSP crime counts (homicides)
##   data/raw/conapo/    CONAPO population projections
##   data/raw/pibe/      INEGI state GDP (PIBE), one CSV per state
##   data/raw/gobernadores/  governor party (political alignment) — prebuilt CSV
##   data/raw/red_tdt/       Red TDT directory — prebuilt CSV
##
## OUTPUT
##   data/processed/panel_mexico_shri_2019_2024.csv
## =============================================================================


## ---- 0. Setup --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

## ---- Working directory -----------------------------------------------------
## Set the working directory to the repository root before running, e.g.:
##   setwd("path/to/my499-dissertation")
## The paths below (data/raw/...) are relative to that root.


# Two-digit INEGI state codes (01–32) and their names. We keep the accented
# names because several raw files match on them.
state_catalogue <- tibble(
  state_id = sprintf("%02d", 1:32),
  state = c("Aguascalientes","Baja California","Baja California Sur","Campeche",
            "Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México",
            "Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México",
            "Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
            "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
            "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
)

# GDP implicit price deflator, rebased to 2020 = 100. Source: the implicit price
# index that ships inside the INEGI PIBE files ("Índice de precios implícitos").
# Used to convert nominal budget pesos into constant 2020 pesos.
deflators <- tibble(
  year = 2019:2024,
  deflator = c(95.612, 100.0, 104.507, 111.460, 116.654, 122.375)
)

# Small helper: turn INEGI's messy state label into a clean 2-digit code.
# Handles ALL-CAPS, accents, and a few truncated/long official names.
to_cve <- function(x) {
  e <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
  e <- str_trim(str_replace_all(e, "[^a-z ]", ""))
  lookup <- c(
    "aguascalientes"="01","baja california sur"="03","baja california"="02",
    "campeche"="04","coahuila de zaragoza"="05","coahuila"="05","colima"="06",
    "chiapas"="07","chihuahua"="08","ciudad de mexico"="09","distrito federal"="09",
    "durango"="10","guanajuato"="11","guerrero"="12","hidalgo"="13","jalisco"="14",
    "estado de mexico"="15","mexico"="15","michoacan de ocampo"="16","michoacan"="16",
    "morelos"="17","nayarit"="18","nuevo leon"="19","oaxaca"="20","puebla"="21",
    "queretaro"="22","quintana roo"="23","san luis potosi"="24","sinaloa"="25",
    "sonora"="26","tabasco"="27","tamaulipas"="28","tlaxcala"="29",
    "veracruz de ignacio de la llave"="30","veracruz"="30","yucatan"="31",
    "zacatecas"="32")
  # exact match first; then startswith to catch truncated names ("veracruz de…")
  out <- lookup[e]
  needs <- is.na(out)
  if (any(needs)) {
    for (nm in names(lookup)) {
      hit <- needs & (startsWith(e, nm) | startsWith(nm, e)) & nchar(e) > 3
      out[hit] <- lookup[[nm]]; needs <- is.na(out)
    }
  }
  unname(out)
}


## ---- 1. Dependent variable: complaints (CNDHE, Module 2) -------------------
## Source table: "Solicitudes de queja recibidas" (complaints received), which
## reports, per state: Total / Aceptadas (accepted) / Desechadas (dismissed).
## We take Total as the main dependent variable and Aceptadas for Appendix F.
##
## The table is the same concept every year but sits on a different sheet number,
## and 2024 adds a CVEGEO column that shifts everything right by one.
## Validated: complaints_total 192/192; accepted rebuilt consistently 6/6 years.
##
##   year | file               | sheet | cve col | total col | accepted col
##   2019 | CNDHE2020_M2.xlsx  |   6   |    2    |     4     |      5
##   2020 | cndhe2021_M2.xlsx  |   7   |    2    |     4     |      5
##   2021 | cndhe2022_M2.xlsx  |   8   |    2    |     4     |      5
##   2022 | cndhe2023_M2.xlsx  |   9   |    2    |     4     |      5
##   2023 | cndhe2024_M2.xlsx  |   7   |    2    |     4     |      5
##   2024 | cndhe2025_M2.xlsx  |   9   |    3    |     5     |      6   (+CVEGEO)

read_complaints <- function(file, sheet, year, cve_col, total_col, acc_col) {
  raw <- read_excel(file.path("data/raw/cndhe", file), sheet = sheet,
                    col_names = FALSE)
  df <- raw[raw[[1]] == "Estatal" & !is.na(raw[[1]]), ]
  tibble(
    state_id = sprintf("%02d", as.integer(df[[cve_col]])),
    year = year,
    complaints_total    = suppressWarnings(as.numeric(df[[total_col]])),
    complaints_accepted = suppressWarnings(as.numeric(df[[acc_col]]))
  ) %>% filter(!is.na(state_id))
}

complaints <- bind_rows(
  read_complaints("CNDHE2020_M2.xlsx", "6", 2019, 2, 4, 5),
  read_complaints("cndhe2021_M2.xlsx", "7", 2020, 2, 4, 5),
  read_complaints("cndhe2022_M2.xlsx", "8", 2021, 2, 4, 5),
  read_complaints("cndhe2023_M2.xlsx", "9", 2022, 2, 4, 5),
  read_complaints("cndhe2024_M2.xlsx", "7", 2023, 2, 4, 5),
  read_complaints("cndhe2025_M2.xlsx", "9", 2024, 3, 5, 6)
)


## ---- 2. Institutional presence: staff, budget, visitadurías (CNDHE, Mod. 1) --
## Three variables from Module 1:
##   staff_total          — total SHRI personnel (main institutional predictor)
##   staff_visitadurias   — personnel in the visitaduría (complaint-handling unit)
##   budget_nominal       — EXERCISED budget ("ejercido"), later deflated
##
## IMPORTANT MEASUREMENT NOTE (budget):
##   The census reports several budget columns in 2019-2020 (Solicitado /
##   Autorizado / Ejercido) but collapses them into a single "Total" from 2021
##   on, whose sheet title is "Presupuesto ejercido". To keep ONE consistent
##   concept across all years we always take the EXERCISED budget. Validated:
##   staff 191/191, budget 191/191, visitadurías 188/188. (191 not 192 because
##   one state did not report staffing in 2023: Baja California Sur.)
##
## read_m1_sheet(): reads a Module-1 sheet, keeps "Estatal" rows, optionally
## keeps only rows with a given code (e.g. "0" = Total, "2" = Visitaduría area).
## NOTE ON COLUMN NUMBERS: R is 1-indexed. 2019-2023 share one layout; 2024 adds
## a CVEGEO column so every position shifts right by one.

read_m1_sheet <- function(file, sheet, year, cve_col, value_col,
                          code_col = NULL, code_keep = NULL) {
  raw <- read_excel(file.path("data/raw/cndhe", file), sheet = sheet,
                    col_names = FALSE)
  df <- raw[raw[[1]] == "Estatal" & !is.na(raw[[1]]), ]
  keep <- rep(TRUE, nrow(df))
  if (!is.null(code_col)) keep <- as.character(df[[code_col]]) == code_keep &
                                  !is.na(df[[code_col]])
  tibble(
    state_id = sprintf("%02d", as.integer(df[[cve_col]][keep])),
    year = year,
    value = suppressWarnings(as.numeric(df[[value_col]][keep]))
  ) %>% filter(!is.na(state_id))
}

## 2a. Total staff — code "0" = Total.
## 2019-2023: cve col 2, code col 4, total col 6 | 2024: 3 / 5 / 7
staff_total <- bind_rows(
  read_m1_sheet("CNDHE2020_M1.xlsx", "5", 2019, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2021_M1.xlsx", "5", 2020, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2022_M1.xlsx", "5", 2021, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2023_M1.xlsx", "8", 2022, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2024_M1.xlsx", "8", 2023, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2025_M1.xlsx", "8", 2024, 3, 7, 5, "0")
) %>% rename(staff_total = value)

## 2b. Visitaduría staff.
## 2019-2021: a dedicated sheet (code "0" = that sheet's Total).
## 2022-2024: embedded in the personnel sheet, área code "2" = Visitaduría.
visitadurias <- bind_rows(
  read_m1_sheet("CNDHE2020_M1.xlsx", "15", 2019, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2021_M1.xlsx", "19", 2020, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2022_M1.xlsx", "16", 2021, 2, 6, 4, "0"),
  read_m1_sheet("cndhe2023_M1.xlsx", "8",  2022, 2, 6, 4, "2"),
  read_m1_sheet("cndhe2024_M1.xlsx", "8",  2023, 2, 6, 4, "2"),
  read_m1_sheet("cndhe2025_M1.xlsx", "8",  2024, 3, 7, 5, "2")
) %>% rename(staff_visitadurias = value)

## 2c. Budget — EXERCISED pesos (see measurement note above).
## 2019-2020: "Ejercido" column (col 6). 2021-2023: single "Total" (col 4).
## 2024: "Total" shifted to col 5 by CVEGEO. No code filter needed here.
budget <- bind_rows(
  read_m1_sheet("CNDHE2020_M1.xlsx", "10", 2019, 2, 6),
  read_m1_sheet("cndhe2021_M1.xlsx", "13", 2020, 2, 6),
  read_m1_sheet("cndhe2022_M1.xlsx", "13", 2021, 2, 4),
  read_m1_sheet("cndhe2023_M1.xlsx", "19", 2022, 2, 4),
  read_m1_sheet("cndhe2024_M1.xlsx", "19", 2023, 2, 4),
  read_m1_sheet("cndhe2025_M1.xlsx", "18", 2024, 3, 5)
) %>% rename(budget_nominal = value)

institutional <- staff_total %>%
  left_join(visitadurias, by = c("state_id","year")) %>%
  left_join(budget,       by = c("state_id","year"))


## ---- 3. Independent variable: NGO density (SAT Donatarias) ------------------
## We count human-rights NGOs registered as tax-exempt donees ("donatarias
## autorizadas") in each state, per fiscal year, then lag one year (t-1 predicts
## complaints in t).
##
## TWO THINGS THAT MATTER FOR CORRECT COUNTS:
##   (1) Keyword filter on TWO fields — the registered name AND the authorised
##       social purpose (objeto social). See keyword lists below (Appendix B).
##   (2) DEDUPLICATE BY RFC. One organisation (one RFC, the Mexican tax ID) can
##       appear on several rows — one per establishment. We must count unique
##       organisations, not rows, or large states get badly over-counted.
##   Validated with (1)+(2): 189/192 exact, correlation 0.986 with the panel.
##   (The three differences are 1-RFC rounding in CDMX and the correction of a
##   truncated state name for the State of Mexico in 2024.)
##
## The SAT files are legacy .xls with the real header on row 30 (1-indexed):
##   col 1 = Entidad federativa, col 4 = RFC, col 5 = Denominación (name),
##   col 9 = Objeto social.

kw_name <- c("derechos humanos","derechos civiles","justicia y derechos",
             "defensa de derechos","derechos y justicia")

kw_purpose <- c(
  "derechos humanos","defensa de derechos humanos",
  "promocion de los derechos humanos","proteccion de derechos humanos",
  "violaciones a derechos humanos","violaciones de derechos humanos",
  "acceso a la justicia","asistencia juridica gratuita","defensa juridica",
  "representacion legal gratuita","litigio estrategico","acompanamiento juridico",
  "victimas de violaciones","defensores de derechos humanos",
  "tortura y tratos crueles","desaparicion forzada",
  "perspectiva de derechos humanos","exigibilidad de derechos","justiciabilidad",
  "mecanismos de proteccion de derechos",
  "incidencia en politica publica de derechos")

# strip accents / lowercase so keyword matching is robust
flatten <- function(x) tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))

read_sat_year <- function(file, fiscal_year) {
  sat <- suppressMessages(
    read_excel(file.path("data/raw/sat", file), sheet = 1, col_names = FALSE,
               skip = 29))                      # data begins on row 30
  names(sat)[1:9] <- c("entidad","adsc","actividad","rfc","nombre",
                       "domicilio","oficio","fecha","objeto")
  sat <- sat %>% filter(!is.na(rfc), rfc != "")
  name_hit <- str_detect(flatten(sat$nombre),
                         str_c(flatten(kw_name), collapse = "|"))
  purp_hit <- str_detect(flatten(sat$objeto),
                         str_c(flatten(kw_purpose), collapse = "|"))
  sat %>%
    mutate(is_hr = name_hit | purp_hit,
           state_id = to_cve(entidad)) %>%
    filter(is_hr, !is.na(state_id)) %>%
    distinct(rfc, .keep_all = TRUE) %>%       # <-- deduplicate by RFC
    count(state_id, name = "cso_broad") %>%
    mutate(fiscal_year = fiscal_year)
}

sat_files <- c("2018"="DonatariasAutorizadas2018.xls",
               "2019"="DonatariasAutorizadas2019.xls",
               "2020"="DonatariasAutorizadas2020.xls",
               "2021"="DonatariasAutorizadas2021.xls",
               "2022"="DonatariasAutorizadas2022.xls",
               "2023"="DonatariasAutorizadas2023.xls",
               "2024"="DonatariasAutorizadas2024.xls")  # for contemporaneous RC1

sat_counts <- bind_rows(lapply(names(sat_files), function(y)
  read_sat_year(sat_files[[y]], as.integer(y))))

# Lag one year: NGO density that predicts complaints in year t is the count in
# fiscal year t-1. Complete the 32x6 grid so states with no NGOs become 0.
# We also keep the CONTEMPORANEOUS count (fiscal year == t) for the RC1 no-lag
# robustness column in Table 2.
ngo <- tidyr::crossing(state_id = state_catalogue$state_id, year = 2019:2024) %>%
  left_join(sat_counts %>% mutate(year = fiscal_year + 1L) %>%
              select(state_id, year, cso_broad_lag = cso_broad),
            by = c("state_id","year")) %>%
  left_join(sat_counts %>% mutate(year = fiscal_year) %>%
              select(state_id, year, cso_broad_t = cso_broad),
            by = c("state_id","year")) %>%
  mutate(cso_broad_lag = tidyr::replace_na(cso_broad_lag, 0),
         cso_broad_t   = tidyr::replace_na(cso_broad_t, 0))


## ---- 4. Control: intentional homicide rate (SESNSP) ------------------------
## From the state-level crime file we keep the subtype "Homicidio doloso"
## (intentional homicide), sum the twelve monthly columns to an annual count,
## aggregate by state-year, and later convert to a rate per 100,000.
## Validated: 192/192 against the panel.

read_homicides <- function() {
  f <- list.files("data/raw/sesnsp", pattern = "Estatal-Delitos.*csv$",
                  full.names = TRUE, recursive = TRUE)[1]
  months <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
              "Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  read_csv(f, locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
    filter(`Subtipo de delito` == "Homicidio doloso",
           Año %in% 2019:2024) %>%
    mutate(annual = rowSums(across(all_of(months)), na.rm = TRUE)) %>%
    group_by(year = Año, state_id = sprintf("%02d", as.integer(Clave_Ent))) %>%
    summarise(homicides = sum(annual), .groups = "drop")
}
homicides <- read_homicides()


## ---- 5. Population (CONAPO) -------------------------------------------------
## Mid-year population projections. Sum across age and sex to a state-year total.
## This is the per-capita denominator AND the model offset. cve_geo == 0 is the
## national row and is dropped. Validated: 192/192.

read_population <- function() {
  f <- list.files("data/raw/conapo", pattern = "Pob_Mitad.*csv$", full.names = TRUE, recursive = TRUE)[1]
  read_csv(f, locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
    rename(anio = 2, cve_geo = 4, poblacion = 7) %>%
    filter(anio %in% 2019:2024, cve_geo > 0) %>%
    group_by(year = anio, state_id = sprintf("%02d", as.integer(cve_geo))) %>%
    summarise(population = sum(poblacion), .groups = "drop")
}
population <- read_population()


## ---- 6. Control: state GDP per capita (INEGI PIBE) -------------------------
## One CSV per state. Row 1 is total GDP in constant 2018 pesos
## ("Millones de pesos a precios de 2018 | B.1bP"). We divide GDP (millions ->
## pesos) by CONAPO population to get GDP per capita in constant 2018 pesos.
## Only used as a robustness control (Appendix / RC2); never significant.

read_gdp <- function() {
  files <- list.files("data/raw/pibe",
                      pattern = "pibe_entidad_.*2024_p\\.csv$", full.names = TRUE, recursive = TRUE)
  # map each file's state suffix to a cve code
  suffix_cve <- c(ags="01",bc="02",bcs="03",camp="04",coah="05",col="06",
                  chis="07",chih="08",cdmx="09",dgo="10",gto="11",gro="12",
                  hgo="13",jal="14","méx"="15",mich="16",mor="17",nay="18",
                  nl="19",oax="20",pue="21",qro="22",qr="23",slp="24",sin="25",
                  son="26",tab="27",tamps="28",tlax="29",ver="30",yuc="31",zac="32")
  year_cols <- c(`2019`="2019",`2020`="2020",`2021`="2021",`2022`="2022",
                 `2023`="2023<R>",`2024`="2024<P>")
  out <- lapply(files, function(f) {
    # Extract the state suffix that comes right before "2024_p", regardless of
    # any folder-name prefix (the file is pibe_entidad_<suf>2024_p.csv).
    suf <- str_match(basename(f), "pibe_entidad_([^0-9]+)2024_p")[,2]
    # Skip files whose suffix is not one of the 32 states (e.g. "nac" = national).
    # NOTE: use %in% / single-bracket access; suffix_cve[["nac"]] would ERROR.
    if (is.na(suf) || !(suf %in% names(suffix_cve))) return(NULL)
    cve <- suffix_cve[suf]
    d <- suppressWarnings(read_csv(f, locale = locale(encoding = "latin1"),
                                   show_col_types = FALSE))
    tibble(state_id = cve, year = 2019:2024,
           gdp_mill = as.numeric(unlist(d[1, year_cols])))
  })
  bind_rows(out)
}
gdp <- read_gdp()


## ---- 7. Political alignment (governors) — prebuilt CSV ----------------------
## Governor party by state-year, hand-coded from public electoral records.
## political_alignment = 1 if the governor's party is the federal ruling party
## (MORENA, in office since Dec 2018), else 0. Provided as a tidy CSV.
alignment <- read_csv(
  "data/raw/gobernadores/governors_political_alignment_2019_2024.csv",
  show_col_types = FALSE) %>%
  transmute(state_id = sprintf("%02d", as.integer(cve_ent)), year,
            governor, governor_party,
            political_alignment = political_alignment_morena)


## ---- 8. Construct-validation source: Red TDT — prebuilt CSV -----------------
## Count of member organisations of the Red TDT human-rights network per state.
## Time-invariant; used only to validate the SAT measure (Appendix B), not as a
## regressor.
red_tdt <- read_csv("data/raw/red_tdt/red_tdt_organizations_by_state.csv",
                    show_col_types = FALSE) %>%
  transmute(state_id = sprintf("%02d", as.integer(cve_ent)),
            red_tdt_n = red_tdt_organizations,
            red_tdt_dummy = as.integer(red_tdt_organizations > 0))


## ---- 8b. Educational outreach events (CNDHE) -------------------------------
## Count of human-rights education/outreach events per state, used lagged in the
## outreach robustness check (Appendix G). The layout differs across editions:
##   * 2019-2024 data: Module 2, sheet "1" ("Eventos de capacitación y
##     difusión"), Total column. (For 2024 the CVEGEO shift applies.)
##   * 2018 data (needed to build the t-1 lag for 2019): the OLD 2019 census
##     format, CNDHE2019_M1 sheet "1.13", where the state NAME is in column 1
##     and Total in column 2 (there is no separate "Estatal" flag column).
## Validated: 158/158 for 2019-2024 lags and 32/32 for the 2018 lag.

read_events_new <- function(file, year, cve_col, total_col) {
  raw <- read_excel(file.path("data/raw/cndhe", file), sheet = "1",
                    col_names = FALSE)
  df <- raw[raw[[1]] == "Estatal" & !is.na(raw[[1]]), ]
  tibble(state_id = sprintf("%02d", as.integer(df[[cve_col]])),
         fiscal_year = year,
         n_eventos = suppressWarnings(as.numeric(df[[total_col]]))) %>%
    filter(!is.na(state_id))
}

events_new <- bind_rows(
  read_events_new("CNDHE2020_M2.xlsx", 2019, 2, 4),
  read_events_new("cndhe2021_M2.xlsx", 2020, 2, 4),
  read_events_new("cndhe2022_M2.xlsx", 2021, 2, 4),
  read_events_new("cndhe2023_M2.xlsx", 2022, 2, 4),
  read_events_new("cndhe2024_M2.xlsx", 2023, 2, 4),
  read_events_new("cndhe2025_M2.xlsx", 2024, 3, 5)
)

# 2018 events from the old-format sheet 1.13 (state name in col 1, Total col 2)
events_2018 <- {
  raw <- read_excel("data/raw/cndhe/CNDHE2019_M1.xlsx", sheet = "1.13",
                    col_names = FALSE)
  raw %>%
    transmute(state_id = to_cve(.[[1]]),
              fiscal_year = 2018,
              n_eventos = suppressWarnings(as.numeric(.[[2]]))) %>%
    filter(!is.na(state_id), !is.na(n_eventos))
}

events_all <- bind_rows(events_2018, events_new)

# Lag one year to align with complaints in year t
events <- tidyr::crossing(state_id = state_catalogue$state_id,
                          year = 2019:2024) %>%
  left_join(events_all %>% mutate(year = fiscal_year + 1L) %>%
              select(state_id, year, n_eventos_lag = n_eventos),
            by = c("state_id","year"))


## ---- 9. Assemble the panel -------------------------------------------------
## Start from the full 32 x 6 grid and left-join every block on (state_id, year).
## Then compute rates, logs, and the deflated budget. Logs of zero are set to NA
## on purpose: a state-year with zero registered NGOs drops out of the log-linear
## models (this is why N < 192). Zacatecas (6 zeros) is the genuine case.

panel <- state_catalogue %>%
  tidyr::crossing(year = 2019:2024) %>%
  left_join(complaints,    by = c("state_id","year")) %>%
  left_join(institutional, by = c("state_id","year")) %>%
  left_join(ngo,           by = c("state_id","year")) %>%
  left_join(homicides,     by = c("state_id","year")) %>%
  left_join(population,    by = c("state_id","year")) %>%
  left_join(gdp,           by = c("state_id","year")) %>%
  left_join(alignment,     by = c("state_id","year")) %>%
  left_join(red_tdt,       by = "state_id") %>%
  left_join(events,        by = c("state_id","year")) %>%
  left_join(deflators,     by = "year")

panel <- panel %>%
  mutate(
    # rates per 100,000
    complaints_per100k          = complaints_total / population * 1e5,
    complaints_accepted_per100k = complaints_accepted / population * 1e5,
    # budget in constant 2020 pesos
    budget_real2020             = budget_nominal / (deflator / 100),
    # GDP per capita in constant 2018 pesos (millions -> pesos)
    gdp_percapita_2018          = gdp_mill * 1e6 / population,
    # homicide rate per 100,000
    homicide_rate               = homicides / population * 1e5
  ) %>%
  mutate(
    # logs (log(0) -> NA by construction, so those state-years leave the models)
    log_complaints_per100k          = log(complaints_per100k),
    log_complaints_total            = log(complaints_total),
    log_complaints_accepted_per100k = log(na_if(complaints_accepted_per100k, 0)),
    log_cso_broad_lag               = log(na_if(cso_broad_lag, 0)),
    log_cso_broad_t                 = log(na_if(cso_broad_t, 0)),
    log_staff                       = log(staff_total),
    log_staff_visitadurias          = log(na_if(staff_visitadurias, 0)),
    log_budget_real                 = log(budget_real2020),
    log_homicide_rate               = log(na_if(homicide_rate, 0)),
    log_gdp_percapita               = log(gdp_percapita_2018),
    log_red_tdt                     = log(na_if(red_tdt_n, 0)),
    log_eventos_lag                 = log(na_if(n_eventos_lag, 0))
  )


## ---- 10. Export ------------------------------------------------------------
## Column name note: internally we key on `state_id`; the published CSV uses the
## same name, so the analysis script (02_analysis.R) reads it directly.
out_cols <- c("state_id","state","year",
              "complaints_total","complaints_per100k","log_complaints_per100k",
              "log_complaints_total",
              "complaints_accepted","complaints_accepted_per100k",
              "log_complaints_accepted_per100k",
              "cso_broad_lag","log_cso_broad_lag",
              "cso_broad_t","log_cso_broad_t",
              "staff_total","log_staff",
              "staff_visitadurias","log_staff_visitadurias",
              "budget_nominal","budget_real2020","log_budget_real",
              "population","homicides","homicide_rate","log_homicide_rate",
              "gdp_percapita_2018","log_gdp_percapita",
              "political_alignment","governor","governor_party",
              "red_tdt_n","red_tdt_dummy","log_red_tdt",
              "n_eventos_lag","log_eventos_lag")

panel_out <- panel %>% select(any_of(out_cols)) %>% arrange(state_id, year)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(panel_out, "data/processed/panel_mexico_shri_2019_2024.csv")

message("Panel written: ", nrow(panel_out), " rows x ", ncol(panel_out),
        " cols  (expected 192 x ", length(out_cols), ")")

## =============================================================================
## END 01_data_construction.R
## Reproduces data/processed/panel_mexico_shri_2019_2024.csv (32 states x 6 yrs).
## Analytical N in the main model is 185 after logs drop 7 observations
## (6 Zacatecas with zero NGOs, 1 Baja California Sur 2023 missing staffing).
## =============================================================================

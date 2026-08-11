## =============================================================================
## 02_analysis.R
## -----------------------------------------------------------------------------
## Dissertation: "When grievances become formal claims: Explaining variation in
##   complaint filing before subnational human rights institutions in Mexico"
## MY499 Dissertation — MSc Social Research Methods, LSE
## Candidate 59923
##
## WHAT THIS SCRIPT DOES
##   Reproduces every quantitative result in the dissertation from the processed
##   panel: descriptive statistics (Table 1), the main models (Table 2), all
##   robustness appendices (B–H), and every figure (Figures 1–2 and the
##   Appendix A maps).
##
## HOW TO READ THIS SCRIPT (student-friendly notes)
##   * The estimator is a negative binomial model with two-way fixed effects
##     (state and year) and log(population) as an OFFSET, so the outcome is
##     effectively a complaint RATE. OLS on the logged rate is shown as a
##     robustness check with standard errors clustered by state.
##   * "M2" is the preferred model: NGO density (t-1), political alignment,
##     staffing, homicide rate. Budget is deliberately excluded from M2 and
##     examined separately in Appendix E.
##   * Each section is labelled with the table/appendix it produces so you can
##     match output to the written document.
##
## INPUT   data/processed/panel_mexico_shri_2019_2024.csv   (from script 01)
## OUTPUT  console tables + files under output/figures/
## =============================================================================


## ---- 0. Setup --------------------------------------------------------------
library(fixest)     # fast fixed-effects negative binomial / Poisson / OLS
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(sf)         # maps
library(patchwork)  # arrange plots side by side

## ---- Working directory -----------------------------------------------------
## Set the working directory to the repository root before running, e.g.:
##   setwd("path/to/my499-dissertation")

panel <- read_csv("data/processed/panel_mexico_shri_2019_2024.csv",
                  show_col_types = FALSE) %>%
  mutate(state_id = sprintf("%02d", as.integer(state_id)))

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# Helper used in Appendix B: map a Mexican state NAME to its 2-digit code.
to_cve_name <- function(x) {
  lut <- setNames(sprintf("%02d", 1:32),
    c("Aguascalientes","Baja California","Baja California Sur","Campeche",
      "Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México","Durango",
      "Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos",
      "Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
      "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala",
      "Veracruz","Yucatán","Zacatecas"))
  unname(lut[x])
}

# Significance convention used throughout the dissertation tables:
#   † p < 0.10,  * p < 0.05,  ** p < 0.01,  *** p < 0.001
# (fixest's default etable stars differ; where we build tables by hand we apply
#  the convention above.)


## ---- 1. Table 1: descriptive statistics ------------------------------------
## Summary statistics for the variables entering the models (analytical scale).
desc_vars <- c("complaints_total","complaints_per100k","cso_broad_lag",
               "staff_total","staff_visitadurias","budget_real2020",
               "homicide_rate","population","gdp_percapita_2018",
               "political_alignment","complaints_accepted")

table1 <- panel %>%
  select(any_of(desc_vars)) %>%
  summarise(across(everything(),
    list(n    = ~sum(!is.na(.)),
         mean = ~mean(., na.rm = TRUE),
         sd   = ~sd(., na.rm = TRUE),
         min  = ~min(., na.rm = TRUE),
         max  = ~max(., na.rm = TRUE)),
    .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("variable",".value"), names_sep = "__")

cat("\n===== TABLE 1. Descriptive statistics =====\n")
print(as.data.frame(table1), digits = 3)


## ---- 2. Table 2: main specification (7 columns) ----------------------------
## The published Table 2 reports one negative binomial column plus an OLS main
## model and five robustness columns (RC1–RC5). Each column is a separate model;
## they are assembled side by side with etable(). Notes on each:
##   NegBin         negative binomial, staff + budget, IID SE (offset population)
##   OLS Main       OLS on log complaint rate, clustered SE
##   RC1 No lag     NGO measured contemporaneously (t) instead of lagged (t-1)
##   RC2 + GDP      adds log GDP per capita (drops 2024 -> smaller N)
##   RC3 DV levels  dependent variable in LEVELS (complaints per 100k, not log)
##   RC4 Visit.staff total staffing replaced by visitaduría staffing
##   RC5 No politics political alignment removed

## Column 1 — Negative binomial (staff + budget), the anchor model
t2_negbin <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_budget_real + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)

## Column 2 — OLS main (log rate), clustered SE
t2_ols <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_budget_real + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

## Column 3 — RC1: contemporaneous NGO density (no lag)
t2_rc1 <- feols(
  log_complaints_per100k ~ log_cso_broad_t + political_alignment +
    log_staff + log_budget_real + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

## Column 4 — RC2: add GDP per capita
t2_rc2 <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_budget_real + log_homicide_rate +
    log_gdp_percapita | state_id + year,
  data = panel, cluster = ~state_id)

## Column 5 — RC3: dependent variable in levels (rate per 100k, not logged)
t2_rc3 <- feols(
  complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_budget_real + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

## Column 6 — RC4: visitaduría staffing instead of total staffing
t2_rc4 <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff_visitadurias + log_budget_real + log_homicide_rate |
    state_id + year, data = panel, cluster = ~state_id)

## Column 7 — RC5: drop political alignment
t2_rc5 <- feols(
  log_complaints_per100k ~ log_cso_broad_lag +
    log_staff + log_budget_real + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

cat("\n===== TABLE 2. Main models (7 columns) =====\n")
etable(t2_negbin, t2_ols, t2_rc1, t2_rc2, t2_rc3, t2_rc4, t2_rc5,
       se.below = TRUE, digits = 3, fitstat = ~ n + r2 + wr2,
       headers = c("NegBin","OLS Main","RC1 No lag","RC2 +GDP",
                   "RC3 DV levels","RC4 Visit.staff","RC5 No politics"))

## The preferred negative binomial model (M2, no budget) used throughout the
## rest of the analysis and the appendices:
m1 <- t2_negbin                      # alias: full negative binomial (staff+budget)
m2 <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)
m2_cl <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel, vcov = ~state_id)


## ---- 3. Elasticities (reported in Section 4.2) -----------------------------
## Convert log-log coefficients into the % change in complaints for a 10% rise.
b_ngo   <- coef(m2)["log_cso_broad_lag"]
b_staff <- coef(m2)["log_staff"]
cat("\n===== Elasticities (from M2) =====\n")
cat(sprintf("NGO:      +10%% -> %+.1f%% complaints\n", (1.1^b_ngo   - 1)*100))
cat(sprintf("Staffing: +10%% -> %+.1f%% complaints\n", (1.1^b_staff - 1)*100))
cat(sprintf("Ratio staffing/NGO: %.1fx\n",
            (1.1^b_staff - 1)/(1.1^b_ngo - 1)))


## ---- 4. Appendix C: Poisson vs Negative Binomial ---------------------------
## Overdispersion check. A large LR statistic and lower AIC favour NegBin.
m_pois <- fepois(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)
lr_pois <- 2 * (as.numeric(logLik(m2)) - as.numeric(logLik(m_pois)))
cat("\n===== APPENDIX C. Poisson vs Negative Binomial =====\n")
cat(sprintf("LR (NegBin vs Poisson) = %.1f (p < 0.001)\n", lr_pois))
cat(sprintf("AIC NegBin = %.1f | AIC Poisson = %.1f | theta = %.3f\n",
            AIC(m2), AIC(m_pois), m2$theta))


## ---- 5. Appendix D: standard-error sensitivity (IID vs clustered) ----------
## Same M2 model; compare IID and state-clustered SE. NGO weakens under
## clustering; staffing stays significant.
cat("\n===== APPENDIX D. SE sensitivity (M2) =====\n")
cat("IID SE:\n");       print(coeftable(m2)[c("log_cso_broad_lag","log_staff"),])
cat("\nClustered SE:\n"); print(coeftable(m2_cl)[c("log_cso_broad_lag","log_staff"),])


## ---- 6. Appendix E: staffing versus budget ---------------------------------
## Three nested models and two likelihood-ratio tests establish that budget adds
## nothing once staffing is included, while staffing adds a great deal over
## budget. M3 = budget only. Budget is the EXERCISED budget (see script 01).
m3 <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_budget_real + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)

cat("\n===== APPENDIX E. Staffing vs budget (Table 6) =====\n")
etable(m1, m2, m3, se.below = TRUE, digits = 3, fitstat = ~ n + aic + ll,
       headers = c("M1 Both","M2 Staffing","M3 Budget"))

lr_budget <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m2)))  # budget adds?
lr_staff  <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m3)))  # staff adds?
cat(sprintf("LR M1 vs M2 (budget adds over staff): chi2 = %.3f, p = %.4f\n",
            lr_budget, pchisq(lr_budget, 1, lower.tail = FALSE)))
cat(sprintf("LR M1 vs M3 (staff adds over budget): chi2 = %.3f, p = %.4f\n",
            lr_staff,  pchisq(lr_staff,  1, lower.tail = FALSE)))


## ---- 7. Appendix F: accepted complaints as alternative outcome -------------
## Re-estimate the PREFERRED specification (M2, no budget) with accepted
## complaints as the outcome. Both columns share the same specification; only
## the dependent variable changes. Convergence corroborates the main finding.
p_acc <- panel %>% filter(complaints_accepted > 0)

m_acc <- fenegbin(
  complaints_accepted ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = p_acc)
m_acc_cl <- fenegbin(
  complaints_accepted ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = p_acc, vcov = ~state_id)

cat("\n===== APPENDIX F. Accepted complaints (Table 7) =====\n")
etable(m2, m_acc, se.below = TRUE, digits = 3, fitstat = ~ n + aic,
       headers = c("Total (M2)","Accepted (M2)"))
cat("Accepted — clustered SE (NGO, staff):\n")
print(coeftable(m_acc_cl)[c("log_cso_broad_lag","log_staff"),])


## ---- 8. Appendix G: educational outreach -----------------------------------
## Add lagged outreach events to M2, estimated on the common sample so the LR
## test is valid. Outreach is not significant; staffing stays strong.
p_common <- panel %>% filter(!is.na(log_eventos_lag))

m2_common <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = p_common)
m2_outreach <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate + log_eventos_lag +
    offset(log(population)) | state_id + year, data = p_common)

cat("\n===== APPENDIX G. Educational outreach (Table G1) =====\n")
etable(m2_common, m2_outreach, se.below = TRUE, digits = 3,
       fitstat = ~ n + aic, headers = c("M2 (common)","M2 + outreach"))
lr_out <- 2 * (as.numeric(logLik(m2_outreach)) - as.numeric(logLik(m2_common)))
cat(sprintf("LR (outreach adds): chi2 = %.3f, df = 1, p = %.4f  [N = %d]\n",
            lr_out, pchisq(lr_out, 1, lower.tail = FALSE), nobs(m2_outreach)))


## ---- 9. Appendix H: endogeneity checks for staffing ------------------------
## (i) lagged staffing; (ii) pseudo-Granger; (iii) within-state variation.
panel <- panel %>%
  arrange(state_id, year) %>%
  group_by(state_id) %>%
  mutate(log_staff_lag      = dplyr::lag(log_staff, 1),
         log_complaints_lag = dplyr::lag(log(complaints_total + 1), 1)) %>%
  ungroup()

## (i) lagged staffing (IID and clustered)
m_lag <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff_lag + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)
m_lag_cl <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff_lag + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel, vcov = ~state_id)

cat("\n===== APPENDIX H. Endogeneity =====\n")
cat("Lagged staffing — IID:\n")
print(coeftable(m_lag)[c("log_cso_broad_lag","log_staff_lag"),])
cat("Lagged staffing — clustered:\n")
print(coeftable(m_lag_cl)[c("log_cso_broad_lag","log_staff_lag"),])
cat(sprintf("N (lagged staffing) = %d\n", nobs(m_lag)))

## (ii) pseudo-Granger: do past complaints predict staffing?
m_granger <- feols(log_staff ~ log_complaints_lag | state_id + year,
                   data = panel, cluster = ~state_id)
cat("\nPseudo-Granger (lagged complaints -> staffing):\n")
print(coeftable(m_granger))

## (iii) within-state variation in log(staffing)
within_sd <- panel %>% group_by(state_id) %>%
  mutate(w = log_staff - mean(log_staff, na.rm = TRUE)) %>% ungroup() %>%
  summarise(sd_total  = sd(log_staff, na.rm = TRUE),
            sd_within = sd(w, na.rm = TRUE))
cat(sprintf("\nlog(staff): SD total = %.3f, SD within = %.3f (%.1f%% within)\n",
            within_sd$sd_total, within_sd$sd_within,
            100*within_sd$sd_within/within_sd$sd_total))


## ---- 10. Appendix B: construct validation of the NGO measure ---------------
## Compare the SAT-based NGO count with two external sources:
##   (a) Red TDT network membership (already in the panel);
##   (b) the CLUNI federal civil-society register (read here from raw).
## Report Pearson correlations at the state level.

## (a) SAT vs Red TDT — average SAT count per state vs Red TDT membership
val <- panel %>%
  group_by(state_id) %>%
  summarise(sat_avg = mean(cso_broad_lag, na.rm = TRUE),
            red_tdt = first(red_tdt_n), .groups = "drop")
cat("\n===== APPENDIX B. Construct validation =====\n")
cat(sprintf("SAT vs Red TDT:  r = %.3f\n",
            cor(val$sat_avg, val$red_tdt, use = "complete.obs")))

## (b) SAT vs CLUNI — count ACTIVE human-rights OSC per state from the register.
## Active = status ACTIVA or ACTIVA CONDICIONADA. Names use "Distrito Federal"
## and "Estado de México", mapped to codes 09 and 15.
cluni_path <- "data/raw/cluni/ANEXO 340025800049526.xlsx"
if (file.exists(cluni_path)) {
  cluni <- read_excel(cluni_path, sheet = "OSC")
  names(cluni)[3:5] <- c("figura","entidad","estatus")
  cluni_state <- cluni %>%
    mutate(active = toupper(estatus) %in% c("ACTIVA","ACTIVA CONDICIONADA"),
           state_id = recode(entidad, "Distrito Federal" = "Ciudad de México",
                                       "Estado de México" = "México")) %>%
    mutate(state_id = to_cve_name(state_id)) %>%
    filter(active, !is.na(state_id)) %>%
    count(state_id, name = "cluni_active")

  cmp <- val %>% left_join(cluni_state, by = "state_id") %>%
    mutate(cluni_active = tidyr::replace_na(cluni_active, 0))
  cat(sprintf("SAT vs CLUNI:    r = %.3f (levels), r = %.3f (logs)\n",
              cor(cmp$sat_avg, cmp$cluni_active, use = "complete.obs"),
              cor(log(cmp$sat_avg), log(cmp$cluni_active + 1e-6),
                  use = "complete.obs")))
} else {
  cat(sprintf("SAT vs CLUNI: CLUNI file not found — skipping (see Appendix B).\n"))
}


## ---- 11. Figure 1: temporal trends -----------------------------------------
trends <- panel %>% group_by(year) %>%
  summarise(complaints = mean(complaints_per100k, na.rm = TRUE),
            ngo   = mean(cso_broad_lag, na.rm = TRUE),
            staff = mean(staff_total,   na.rm = TRUE), .groups = "drop")

fa <- ggplot(trends, aes(year, complaints)) + geom_line() + geom_point() +
  labs(title = "(a) Complaint rate (per 100k)", x = NULL, y = NULL) + theme_minimal()
fb <- ggplot(trends, aes(year, ngo)) + geom_line() + geom_point() +
  labs(title = "(b) NGO density", x = NULL, y = NULL) + theme_minimal()
fc <- ggplot(trends, aes(year, staff)) + geom_line() + geom_point() +
  labs(title = "(c) SHRI staffing", x = NULL, y = NULL) + theme_minimal()
fig1 <- fa + fb + fc + plot_annotation(title = "Figure 1. Temporal trends, 2019–2024")
ggsave("output/figures/figure1_temporal_trends.pdf", fig1, width = 15, height = 5)


## ---- 12. Figure 2: cross-sectional distribution ----------------------------
cross <- panel %>% group_by(state_id) %>%
  summarise(complaints = mean(complaints_per100k, na.rm = TRUE),
            ngo   = mean(cso_broad_lag, na.rm = TRUE),
            staff = mean(staff_total,   na.rm = TRUE), .groups = "drop")

g1 <- ggplot(cross, aes(complaints)) + geom_histogram(bins = 12) +
  labs(title = "(a) Complaint rate", x = NULL, y = NULL) + theme_minimal()
g2 <- ggplot(cross, aes(ngo)) + geom_histogram(bins = 12) +
  labs(title = "(b) NGO density", x = NULL, y = NULL) + theme_minimal()
g3 <- ggplot(cross, aes(staff)) + geom_histogram(bins = 12) +
  labs(title = "(c) SHRI staffing", x = NULL, y = NULL) + theme_minimal()
fig2 <- g1 + g2 + g3 +
  plot_annotation(title = "Figure 2. Cross-sectional distribution, 2019–2024 averages")
ggsave("output/figures/figure2_cross_section.pdf", fig2, width = 15, height = 5)


## ---- 13. Appendix A: choropleth maps ---------------------------------------
## The 2025 Marco Geoestadístico shapefile has both CVEGEO and CVE_ENT; we use
## CVE_ENT (2-digit state code). Quintile shading of the three key variables.
mex <- st_read("data/geo/00ent.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(state_id = sprintf("%02d", as.integer(CVE_ENT)))

mapdat <- panel %>% group_by(state_id) %>%
  summarise(complaints = mean(complaints_per100k, na.rm = TRUE),
            ngo   = mean(cso_broad_lag, na.rm = TRUE),
            staff = mean(staff_total,   na.rm = TRUE), .groups = "drop")
mex <- left_join(mex, mapdat, by = "state_id")

make_map <- function(var, palette, title) {
  mex$q <- cut(mex[[var]], breaks = quantile(mex[[var]], 0:5/5, na.rm = TRUE),
               include.lowest = TRUE, labels = paste0("Q", 1:5))
  ggplot(mex) + geom_sf(aes(fill = q), colour = "white", size = 0.3) +
    scale_fill_brewer(palette = palette, name = "Quintile", na.value = "grey85") +
    labs(title = title) + theme_void() +
    theme(plot.title = element_text(size = 10, face = "bold"))
}
p1 <- make_map("complaints", "Blues",   "Complaint rate (per 100k)")
p2 <- make_map("ngo",        "Greens",  "NGO density")
p3 <- make_map("staff",      "Oranges", "SHRI staffing")
fig_maps <- p1 + p2 + p3 +
  plot_annotation(title = "Complaint filing, NGO density, and SHRI staffing across Mexican states, 2019–2024")
ggsave("output/figures/maps_three_variables.pdf", fig_maps, width = 18, height = 7, dpi = 300)
ggsave("output/figures/maps_three_variables.png", fig_maps, width = 18, height = 7, dpi = 200)

cat("\n===== DONE. Figures written to output/figures/ =====\n")

## =============================================================================
## END 02_analysis.R
## =============================================================================

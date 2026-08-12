## =============================================================================
## 02_analysis.R
## -----------------------------------------------------------------------------
## Dissertation: "When grievances become formal claims in non-binding contexts:
##   Explaining variation in complaint filing before subnational human rights
##   institutions in Mexico"
## MY499 Dissertation — MSc Social Research Methods, LSE
## Candidate 59923
##
## WHAT THIS SCRIPT DOES
##   Reproduces every quantitative result in the dissertation from the processed
##   panel: descriptive statistics (Table 1), the main models (Table 2), the
##   robustness appendices (A–H), and every figure (Figures 1–2 and the
##   Appendix A maps). Sections are ordered to follow the dissertation.
##
## KEY MODELLING NOTES
##   * Estimator: negative binomial with two-way (state, year) fixed effects and
##     log(population) as an OFFSET, so the outcome is effectively a rate. OLS on
##     the logged rate is reported as a robustness check with state-clustered SE.
##   * "M2" is the preferred model: NGO density (t-1) + political alignment +
##     staffing + homicide rate. Budget is deliberately EXCLUDED from M2 and is
##     examined separately as an alternative operationalisation in Appendix C.
##   * Significance convention in the written tables:
##       † p < 0.10,  * p < 0.05,  ** p < 0.01,  *** p < 0.001
##
## APPENDIX MAP (letters follow the dissertation's order of first mention)
##   A  Descriptive maps                 (Section 3.1)
##   B  Construct validation of NGO      (Section 3.3.2)
##   C  Staffing versus budget           (Section 3.3.2 / 3.4)
##   D  Poisson versus negative binomial (Section 3.4)
##   E  IID versus clustered SE + wild cluster bootstrap (Section 4.2)
##   F  Endogeneity of SHRI staffing     (Section 4.2)
##   G  Accepted complaints              (Section 4.3)
##   H  Educational outreach             (Section 4.3 / 4.4)
##
## INPUT   data/analysis/panel_mexico_shri_2019_2024_FINAL.csv   (from script 01)
## OUTPUT  console tables + files under output/figures/
##
## HOW TO RUN
##   Set the working directory to the repository root, e.g.
##     setwd("path/to/my499-dissertation")
##   then source this file. All paths below are relative to that root.
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

panel <- read_csv("data/analysis/panel_mexico_shri_2019_2024_FINAL.csv",
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


## ============================================================================
## SECTION 4.1 — TABLE 1: descriptive statistics
## ============================================================================
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


## ============================================================================
## SECTION 4.2 — TABLE 2: main specification (7 columns, budget EXCLUDED)
## ----------------------------------------------------------------------------
##   NegBin          negative binomial, staff only, IID SE (offset population)
##   OLS Main        OLS on log complaint rate, clustered SE
##   RC1 No lag      NGO measured contemporaneously (t) instead of lagged (t-1)
##   RC2 + GDP       adds log GDP per capita
##   RC3 DV levels   dependent variable in LEVELS (complaints per 100k)
##   RC4 Visit.staff total staffing replaced by visitaduría staffing
##   RC5 No politics political alignment removed
## ============================================================================

## Column 1 — Negative binomial (staff, NO budget) = M2 preferred
t2_negbin <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)

## Column 2 — OLS main (log rate), clustered SE
t2_ols <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

## Column 3 — RC1: contemporaneous NGO density (no lag)
t2_rc1 <- feols(
  log_complaints_per100k ~ log_cso_broad_t + political_alignment +
    log_staff + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

## Column 4 — RC2: add GDP per capita
t2_rc2 <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    log_gdp_percapita | state_id + year,
  data = panel, cluster = ~state_id)

## Column 5 — RC3: dependent variable in levels
t2_rc3 <- feols(
  complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

## Column 6 — RC4: visitaduría staffing instead of total staffing
t2_rc4 <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff_visitadurias + log_homicide_rate |
    state_id + year, data = panel, cluster = ~state_id)

## Column 7 — RC5: drop political alignment
t2_rc5 <- feols(
  log_complaints_per100k ~ log_cso_broad_lag +
    log_staff + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id)

cat("\n===== TABLE 2. Main models (7 columns, budget excluded) =====\n")
etable(t2_negbin, t2_ols, t2_rc1, t2_rc2, t2_rc3, t2_rc4, t2_rc5,
       se.below = TRUE, digits = 3, fitstat = ~ n + r2 + wr2,
       headers = c("NegBin","OLS Main","RC1 No lag","RC2 +GDP",
                   "RC3 DV levels","RC4 Visit.staff","RC5 No politics"))

## The preferred negative binomial model (M2), reused throughout the appendices.
## IID and state-clustered versions:
m2 <- t2_negbin
m2_cl <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel, vcov = ~state_id)


## ---- Elasticities (reported in Section 4.2) --------------------------------
## Convert log-log coefficients into the % change in complaints for a 10% rise.
b_ngo   <- coef(m2)["log_cso_broad_lag"]
b_staff <- coef(m2)["log_staff"]
cat("\n===== Elasticities (from M2) =====\n")
cat(sprintf("NGO:      +10%% -> %+.1f%% complaints\n", (1.1^b_ngo   - 1)*100))
cat(sprintf("Staffing: +10%% -> %+.1f%% complaints\n", (1.1^b_staff - 1)*100))
cat(sprintf("Ratio staffing/NGO: %.1fx\n",
            (1.1^b_staff - 1)/(1.1^b_ngo - 1)))


## ---- Interaction NGO x staffing (reported in Section 4.2) ------------------
## Tests whether the association of either predictor depends on the level of the
## other. The interaction is negative and statistically insignificant.
m_interact <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_cso_broad_lag:log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)
cat("\n===== Interaction NGO x staffing (Section 4.2) =====\n")
print(coeftable(m_interact)["log_cso_broad_lag:log_staff", ])
etable(m_interact, se.below = TRUE, digits = 3, fitstat = ~ n + aic)


## ============================================================================
## APPENDIX C — Staffing versus budget  (first mentioned in Section 3.3.2/3.4)
## ----------------------------------------------------------------------------
## Three nested negative binomial models and two likelihood-ratio tests show
## that budget adds little once staffing is included, while staffing adds a
## great deal over budget. Budget is the EXERCISED budget (see script 01).
##   M1 = both staffing and budget
##   M2 = staffing only (preferred)
##   M3 = budget only
## ============================================================================
m1 <- fenegbin(                       # both staffing and budget
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_budget_real + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)
m3 <- fenegbin(                       # budget only
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_budget_real + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)

cat("\n===== APPENDIX C. Staffing vs budget =====\n")
etable(m1, m2, m3, se.below = TRUE, digits = 3, fitstat = ~ n + aic + ll,
       headers = c("M1 Both","M2 Staffing","M3 Budget"))

lr_budget <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m2)))  # budget adds?
lr_staff  <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m3)))  # staff adds?
cat(sprintf("LR M1 vs M2 (budget adds over staff): chi2 = %.3f, p = %.4f\n",
            lr_budget, pchisq(lr_budget, 1, lower.tail = FALSE)))
cat(sprintf("LR M1 vs M3 (staff adds over budget): chi2 = %.3f, p = %.4f\n",
            lr_staff,  pchisq(lr_staff,  1, lower.tail = FALSE)))


## ============================================================================
## APPENDIX D — Poisson versus negative binomial  (Section 3.4)
## ----------------------------------------------------------------------------
## Overdispersion check. A large LR statistic and lower AIC/BIC favour NegBin.
## ============================================================================
m_pois <- fepois(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = panel)
lr_pois <- 2 * (as.numeric(logLik(m2)) - as.numeric(logLik(m_pois)))
cat("\n===== APPENDIX D. Poisson vs Negative Binomial =====\n")
cat(sprintf("LogLik NegBin = %.1f | LogLik Poisson = %.1f\n",
            as.numeric(logLik(m2)), as.numeric(logLik(m_pois))))
cat(sprintf("AIC NegBin = %.1f | AIC Poisson = %.1f\n", AIC(m2), AIC(m_pois)))
cat(sprintf("BIC NegBin = %.1f | BIC Poisson = %.1f\n", BIC(m2), BIC(m_pois)))
cat(sprintf("LR (NegBin vs Poisson) = %.1f (p < 0.001) | theta = %.3f\n",
            lr_pois, m2$theta))


## ============================================================================
## APPENDIX E — IID versus clustered SE, and wild cluster bootstrap (Sect. 4.2)
## ----------------------------------------------------------------------------
## (i) Same M2 model, IID vs state-clustered SE. NGO weakens under clustering;
##     staffing stays significant.
## (ii) Wild cluster bootstrap on the OLS specification as a further check on
##      inference with a small number of clusters (Cameron, Gelbach & Miller,
##      2008). Rademacher weights, 9,999 replications, null imposed.
## ============================================================================
cat("\n===== APPENDIX E. SE sensitivity (M2) =====\n")
cat("IID SE:\n");        print(coeftable(m2)[c("log_cso_broad_lag","log_staff"), ])
cat("\nClustered SE:\n"); print(coeftable(m2_cl)[c("log_cso_broad_lag","log_staff"), ])
etable(m2, m2_cl, se.below = TRUE, digits = 3)

## Wild cluster bootstrap (manual; Rademacher weights; null imposed) on t2_ols.
wild_boot <- function(var_test, B = 9999, seed = 12345) {
  set.seed(seed)
  vars_model <- c("log_complaints_per100k", "log_cso_broad_lag",
                  "political_alignment", "log_staff",
                  "log_homicide_rate", "state_id", "year")
  dat <- panel[complete.cases(panel[, vars_model]), ]

  # Full model -> observed clustered t
  m_full <- feols(
    log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
      log_staff + log_homicide_rate | state_id + year, data = dat)
  t_obs <- coeftable(m_full, cluster = ~state_id)[var_test, "t value"]

  # Restricted model (drop the tested variable -> impose H0)
  otras <- setdiff(c("log_cso_broad_lag","political_alignment",
                     "log_staff","log_homicide_rate"), var_test)
  fml_rest <- as.formula(paste0(
    "log_complaints_per100k ~ ", paste(otras, collapse = " + "),
    " | state_id + year"))
  m_rest    <- feols(fml_rest, data = dat)
  u_rest    <- resid(m_rest)
  yhat_rest <- fitted(m_rest)

  clusters <- dat$state_id
  uniq_cl  <- unique(clusters)
  G        <- length(uniq_cl)

  fml_full <- as.formula(paste0(
    "y_star ~ log_cso_broad_lag + political_alignment + ",
    "log_staff + log_homicide_rate | state_id + year"))

  t_boot <- numeric(B)
  for (b in 1:B) {
    w_cl <- sample(c(-1, 1), G, replace = TRUE)
    names(w_cl) <- as.character(uniq_cl)
    w_i <- w_cl[as.character(clusters)]
    dat_b <- dat
    dat_b$y_star <- yhat_rest + u_rest * w_i
    m_b <- feols(fml_full, data = dat_b)
    t_boot[b] <- coeftable(m_b, cluster = ~state_id)[var_test, "t value"]
  }
  p_boot <- mean(abs(t_boot) >= abs(t_obs))
  cat(sprintf("Wild cluster bootstrap: %-20s t_obs = %6.3f | p = %.4f | B = %d | clusters = %d\n",
              var_test, t_obs, p_boot, B, G))
  invisible(list(t_obs = t_obs, p = p_boot, G = G))
}

cat("\n----- Wild cluster bootstrap (OLS specification) -----\n")
res_ngo   <- wild_boot("log_cso_broad_lag")   # NGO   (H1): expected insignificant
res_staff <- wild_boot("log_staff")           # staff (H2): expected significant


## ============================================================================
## APPENDIX F — Endogeneity checks for staffing  (Section 4.2)
## ----------------------------------------------------------------------------
## (i) lagged staffing; (ii) pseudo-Granger; (iii) within-state variation.
## ============================================================================
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

cat("\n===== APPENDIX F. Endogeneity =====\n")
cat("Lagged staffing — IID:\n")
print(coeftable(m_lag)[c("log_cso_broad_lag","log_staff_lag"), ])
cat("Lagged staffing — clustered:\n")
print(coeftable(m_lag_cl)[c("log_cso_broad_lag","log_staff_lag"), ])
cat(sprintf("N (lagged staffing) = %d | theta = %.3f | AIC = %.1f\n",
            nobs(m_lag), m_lag$theta, AIC(m_lag)))
cat("Other coefficients (political, homicide):\n")
print(coeftable(m_lag)[c("political_alignment","log_homicide_rate"), ])
etable(m_lag, se.below = TRUE, digits = 3, fitstat = ~ n + aic)

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


## ============================================================================
## APPENDIX G — Accepted complaints as alternative outcome  (Section 4.3)
## ----------------------------------------------------------------------------
## Re-estimate the preferred specification (M2, no budget) with accepted
## complaints as the outcome. Convergence corroborates the main finding.
## ============================================================================
p_acc <- panel %>% filter(complaints_accepted > 0)

m_acc <- fenegbin(
  complaints_accepted ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = p_acc)
m_acc_cl <- fenegbin(
  complaints_accepted ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = p_acc, vcov = ~state_id)

cat("\n===== APPENDIX G. Accepted complaints =====\n")
etable(m2, m_acc, se.below = TRUE, digits = 3, fitstat = ~ n + aic,
       headers = c("Total (M2)","Accepted (M2)"))
cat(sprintf("Accepted — theta = %.3f\n", m_acc$theta))
cat("Accepted — IID SE (NGO, staff):\n")
print(coeftable(m_acc)[c("log_cso_broad_lag","log_staff"), ])
cat("Accepted — clustered SE (NGO, staff):\n")
print(coeftable(m_acc_cl)[c("log_cso_broad_lag","log_staff"), ])


## ============================================================================
## APPENDIX H — Educational outreach  (Section 4.3 / 4.4)
## ----------------------------------------------------------------------------
## Add lagged outreach events to M2, estimated on the common sample so the LR
## test is valid. Outreach is not significant; staffing stays strong.
## ============================================================================
p_common <- panel %>% filter(!is.na(log_eventos_lag))

m2_common <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year, data = p_common)
m2_outreach <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate + log_eventos_lag +
    offset(log(population)) | state_id + year, data = p_common)

cat("\n===== APPENDIX H. Educational outreach =====\n")
etable(m2_common, m2_outreach, se.below = TRUE, digits = 3,
       fitstat = ~ n + aic, headers = c("M2 (common)","M2 + outreach"))
cat(sprintf("theta: M2 common = %.3f | M2 outreach = %.3f\n",
            m2_common$theta, m2_outreach$theta))
lr_out <- 2 * (as.numeric(logLik(m2_outreach)) - as.numeric(logLik(m2_common)))
cat(sprintf("LR (outreach adds): chi2 = %.3f, df = 1, p = %.4f  [N = %d]\n",
            lr_out, pchisq(lr_out, 1, lower.tail = FALSE), nobs(m2_outreach)))
cat("Outreach coefficient:\n")
print(coeftable(m2_outreach)["log_eventos_lag", ])


## ============================================================================
## APPENDIX B — Construct validation of the NGO measure  (Section 3.3.2)
## ----------------------------------------------------------------------------
## Compare the SAT-based NGO count with two external sources:
##   (a) Red TDT network membership (already in the panel);
##   (b) the CLUNI federal civil-society register (read here from raw).
## Report Pearson correlations at the state level.
## ============================================================================
val <- panel %>%
  group_by(state_id) %>%
  summarise(sat_avg = mean(cso_broad_lag, na.rm = TRUE),
            red_tdt = first(red_tdt_n), .groups = "drop")
cat("\n===== APPENDIX B. Construct validation =====\n")
cat(sprintf("SAT vs Red TDT:  r = %.3f\n",
            cor(val$sat_avg, val$red_tdt, use = "complete.obs")))

## (b) SAT vs CLUNI — active human-rights OSC per state from the register.
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
  cat("SAT vs CLUNI: CLUNI file not found — skipping (see Appendix B).\n")
}


## ============================================================================
## FIGURE 1 — temporal trends  (Section 4.1)
## ============================================================================
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


## ============================================================================
## FIGURE 2 — cross-sectional distribution  (Section 4.1)
## ============================================================================
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


## ============================================================================
## APPENDIX A — choropleth maps  (Section 3.1)
## ----------------------------------------------------------------------------
## The 2025 Marco Geoestadístico shapefile has CVE_ENT (2-digit state code).
## Quintile shading of the three key variables.
## ============================================================================
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

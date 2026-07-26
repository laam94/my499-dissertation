## =============================================================================
## 02_analysis.R
## -----------------------------------------------------------------------------
## Dissertation: "When grievances become formal claims: Explaining variation in
##   complaint filing before subnational human rights institutions in Mexico"
## MY499 Dissertation — MSc Social Research Methods, LSE
##
## PURPOSE: Reproduce all models, tables, figures, and robustness checks from
##          the constructed panel (data/processed/panel_mexico_shri_2019_2024.csv)
##
## MODEL: Negative binomial with two-way fixed effects (state + year) and
##        population offset. OLS with clustered SE as robustness.
##
## Preferred specification (M2): NGO density (t-1), political alignment,
##   staff, homicide rate. Budget omitted (see Appendix E: LR test p = 0.305).
## =============================================================================

## ---- 0. Setup --------------------------------------------------------------
library(fixest)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)

panel <- read_csv("data/processed/panel_mexico_shri_2019_2024.csv") %>%
  mutate(state_id = sprintf("%02d", as.integer(state_id)))

## ---- 1. Main specification (Table 2) ---------------------------------------
## M1: full model (staff + budget) — reference
m1 <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_budget_real + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = panel
)

## M2: preferred model (staff, no budget)
m2 <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = panel
)

## M2 with clustered SE
m2_cl <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = panel, vcov = ~state_id
)

## OLS (robustness), clustered SE
m_ols <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate | state_id + year,
  data = panel, cluster = ~state_id
)

## OLS excluding Mexico City
m_ols_nocdmx <- feols(
  log_complaints_per100k ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate | state_id + year,
  data = panel %>% filter(state_id != "09"), cluster = ~state_id
)

etable(m1, m2, m2_cl, m_ols, m_ols_nocdmx,
       se.below = TRUE, digits = 3, fitstat = ~ n + aic,
       headers = c("M1 NegBin","M2 NegBin","M2 Clustered",
                   "OLS","OLS no CDMX"))

## ---- 2. Elasticities -------------------------------------------------------
b_ngo   <- coef(m2)["log_cso_broad_lag"]
b_staff <- coef(m2)["log_staff"]
cat(sprintf("NGO:      10%% rise -> %.1f%% more complaints\n",
            (exp(b_ngo * log(1.1)) - 1) * 100))
cat(sprintf("Staffing: 10%% rise -> %.1f%% more complaints\n",
            (exp(b_staff * log(1.1)) - 1) * 100))
cat(sprintf("Ratio staffing/NGO: %.1fx\n",
            (exp(b_staff*log(1.1))-1)/(exp(b_ngo*log(1.1))-1)))

## ---- 3. Diagnostics: Poisson vs Negative Binomial (Appendix C) ------------
m_pois <- fepois(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = panel
)
lr <- 2 * (as.numeric(logLik(m2)) - as.numeric(logLik(m_pois)))
cat(sprintf("LR test (NegBin vs Poisson): chi2 = %.1f, p < 0.001\n", lr))
cat(sprintf("AIC NegBin = %.1f | AIC Poisson = %.1f | theta = %.3f\n",
            AIC(m2), AIC(m_pois), m2$theta))

## ---- 4. Staffing vs budget (Appendix E) ------------------------------------
m3 <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_budget_real + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = panel
)
lr_budget  <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m2)))  # budget adds?
lr_staff   <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m3)))  # staff adds?
cat(sprintf("LR M1 vs M2 (budget adds over staff): chi2 = %.3f, p = %.4f\n",
            lr_budget, pchisq(lr_budget, 1, lower.tail = FALSE)))
cat(sprintf("LR M1 vs M3 (staff adds over budget): chi2 = %.3f, p = %.4f\n",
            lr_staff, pchisq(lr_staff, 1, lower.tail = FALSE)))

## ---- 5. Endogeneity checks (Appendix H) ------------------------------------
panel <- panel %>%
  arrange(state_id, year) %>%
  group_by(state_id) %>%
  mutate(log_staff_lag = dplyr::lag(log_staff, 1),
         log_complaints_lag = dplyr::lag(log(complaints_total + 1), 1)) %>%
  ungroup()

# Lagged staffing
m2_stafflag <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff_lag + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = panel
)
print(coeftable(m2_stafflag)[c("log_cso_broad_lag","log_staff_lag"),])

# Pseudo-Granger: do past complaints predict staffing?
m_granger <- feols(log_staff ~ log_complaints_lag | state_id + year,
                   data = panel, cluster = ~state_id)
print(coeftable(m_granger))

## ---- 5b. Education and outreach (Appendix G) -------------------------------
## Does the staffing association reflect outreach activities specifically, or
## institutional presence more broadly? Augment the preferred model (M2) with
## the lagged number of human rights education/outreach events. `log_eventos_lag`
## is already constructed in the panel (see 01_data_construction.R). Estimated
## on the common sample (N = 182) so the likelihood-ratio test is valid.

p_common <- panel %>% filter(!is.na(log_eventos_lag))

# M2 re-estimated on the common sample (baseline for the LR test)
m2_common <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = p_common
)

# M2 + outreach
m2_outreach <- fenegbin(
  complaints_total ~ log_cso_broad_lag + political_alignment +
    log_staff + log_homicide_rate + log_eventos_lag +
    offset(log(population)) | state_id + year,
  data = p_common
)

etable(m2_common, m2_outreach, se.below = TRUE, digits = 3,
       fitstat = ~ n + aic, headers = c("M2 (common)", "M2 + outreach"))

# LR test: does outreach improve fit? (valid: same sample, N = 182)
# Expected: outreach beta ~ -0.070 (p ~ 0.118, n.s.); LR chi2 ~ 3.163, p ~ 0.075
lr_outreach <- 2 * (as.numeric(logLik(m2_outreach)) - as.numeric(logLik(m2_common)))
cat(sprintf("LR test (outreach adds): chi2 = %.3f, df = 1, p = %.4f\n",
            lr_outreach, pchisq(lr_outreach, 1, lower.tail = FALSE)))

## ---- 6. Accepted complaints (Appendix F) -----------------------------------
p_acc <- panel %>% filter(complaints_accepted > 0)
m_acc <- fenegbin(
  complaints_accepted ~ log_cso_broad_lag + political_alignment +
    log_staff + log_budget_real + log_homicide_rate +
    offset(log(population)) | state_id + year,
  data = p_acc
)
print(coeftable(m_acc))

## ---- 7. Cross-validation of NGO measure (Appendix B) -----------------------
# SAT vs Red TDT
sat_avg <- tapply(panel$cso_broad_lag, panel$state_id, mean, na.rm = TRUE)
tdt     <- tapply(panel$red_tdt_n,     panel$state_id, mean, na.rm = TRUE)
cat(sprintf("SAT vs Red TDT: r = %.3f\n", cor(sat_avg, tdt, use = "complete.obs")))

# SAT vs CLUNI (levels and logs) — CLUNI counts from data/raw/cluni/
# See appendix; r = 0.987 (levels), 0.895 (logs).

## ---- 8a. Figure 1: temporal trends (body) ----------------------------------
## Panel (a) complaints per 100k, (b) NGO density, (c) staffing — annual means.
trends <- panel %>%
  group_by(year) %>%
  summarise(complaints = mean(complaints_per100k, na.rm = TRUE),
            ngo        = mean(cso_broad_lag,       na.rm = TRUE),
            staff      = mean(staff_total,         na.rm = TRUE))

fa <- ggplot(trends, aes(year, complaints)) + geom_line() + geom_point() +
  labs(title = "(a) Complaint rate (per 100k)", x = NULL, y = NULL) + theme_minimal()
fb <- ggplot(trends, aes(year, ngo)) + geom_line() + geom_point() +
  labs(title = "(b) NGO density", x = NULL, y = NULL) + theme_minimal()
fc <- ggplot(trends, aes(year, staff)) + geom_line() + geom_point() +
  labs(title = "(c) SHRI staffing", x = NULL, y = NULL) + theme_minimal()

fig1 <- fa + fb + fc +
  plot_annotation(title = "Figure 1. Temporal trends, 2019-2024")
ggsave("output/figures/figure1_temporal_trends.pdf", fig1, width = 15, height = 5)

## ---- 8b. Figure 2: cross-sectional distribution (body) ---------------------
## Histograms/densities of the three variables averaged over 2019-2024.
cross <- panel %>%
  group_by(state_id) %>%
  summarise(complaints = mean(complaints_per100k, na.rm = TRUE),
            ngo        = mean(cso_broad_lag,       na.rm = TRUE),
            staff      = mean(staff_total,         na.rm = TRUE))

g1 <- ggplot(cross, aes(complaints)) + geom_histogram(bins = 12) +
  labs(title = "(a) Complaint rate", x = NULL, y = NULL) + theme_minimal()
g2 <- ggplot(cross, aes(ngo)) + geom_histogram(bins = 12) +
  labs(title = "(b) NGO density", x = NULL, y = NULL) + theme_minimal()
g3 <- ggplot(cross, aes(staff)) + geom_histogram(bins = 12) +
  labs(title = "(c) SHRI staffing", x = NULL, y = NULL) + theme_minimal()

fig2 <- g1 + g2 + g3 +
  plot_annotation(title = "Figure 2. Cross-sectional distribution, 2019-2024 averages")
ggsave("output/figures/figure2_cross_section.pdf", fig2, width = 15, height = 5)

## ---- 8c. Choropleth maps: cross-state variation (Appendix A) ----------------
mex <- st_read("data/geo/00ent.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(cve_ent = sprintf("%02d", as.integer(CVE_ENT)))

avg <- panel %>%
  group_by(state_id) %>%
  summarise(complaints = mean(complaints_per100k, na.rm = TRUE),
            ngo        = mean(cso_broad_lag,       na.rm = TRUE),
            staff      = mean(staff_total,         na.rm = TRUE)) %>%
  rename(cve_ent = state_id)

mex <- left_join(mex, avg, by = "cve_ent")

make_map <- function(var, palette, title) {
  mex$q <- cut(mex[[var]],
               breaks = quantile(mex[[var]], 0:5/5, na.rm = TRUE),
               include.lowest = TRUE, labels = paste0("Q", 1:5))
  ggplot(mex) +
    geom_sf(aes(fill = q), colour = "white", size = 0.3) +
    scale_fill_brewer(palette = palette, name = "Quintile", na.value = "grey85") +
    labs(title = title) + theme_void() +
    theme(plot.title = element_text(size = 10, face = "bold"))
}

p1 <- make_map("complaints", "Blues",   "Complaint Rate (per 100k)")
p2 <- make_map("ngo",        "Greens",  "NGO Density")
p3 <- make_map("staff",      "Oranges", "SHRI Staffing")

fig_maps <- p1 + p2 + p3 +
  plot_annotation(title = "Complaint Filing, NGO Density, and SHRI Staffing across Mexican States, 2019-2024")

ggsave("output/figures/maps_three_variables.pdf", fig_maps,
       width = 18, height = 7, dpi = 300)
ggsave("output/figures/maps_three_variables.png", fig_maps,
       width = 18, height = 7, dpi = 200)

## =============================================================================
## END 02_analysis.R
## =============================================================================

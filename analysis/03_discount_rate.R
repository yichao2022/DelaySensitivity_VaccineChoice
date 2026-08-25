# --- Setup ---------------------------------------------------------------
# If you already have the data.frame loaded as `df`, skip the read.csv line.
df <- data_indexed # set path if needed

# Mixed logit coefficient on standardized wait time
beta_wait_std <- -0.43

# Delays in months (your design)
t <- c(0, 1, 3, 6)

# --- Unstandardize the time coefficient ---------------------------------
# WaitTime_std = (WaitTime - mean)/sd  =>  beta_unstd = beta_std / sd(WaitTime)
sd_wait <- sd(df$WaitTime, na.rm = TRUE)
beta_wait_unstd <- beta_wait_std / sd_wait

# --- Build observed discount points (from exponential form) --------------
# Interpret decline in utility per month as log discount: D_obs(t) = exp(beta_unstd * t)
D_obs <- exp(beta_wait_unstd * t)

# --- Fit hyperbolic discount: D(t) = 1 / (1 + k t) ----------------------
D_hyper <- function(t, k) 1 / (1 + k * t)

# SSE to minimize; we optimize over log(k) so k > 0 is guaranteed
sse_obj <- function(theta) {
  k <- exp(theta)
  D_pred <- D_hyper(t, k)
  sum((D_obs - D_pred)^2)
}

# Nelder–Mead
res <- optim(par = log(0.1), fn = sse_obj, method = "Nelder-Mead")
k_hat <- exp(res$par)
sse   <- res$value

cat(sprintf("Best-fit hyperbolic k: %.6f\nSSE: %.6f\n", k_hat, sse))

# --- Compare observed vs fitted -----------------------------------------
comp <- data.frame(
  t_months = t,
  D_obs = D_obs,
  D_pred = D_hyper(t, k_hat)
)
print(comp)

# --- (Optional) Plot -----------------------------------------------------
 install.packages("ggplot2") # if needed
library(ggplot2)

ggplot(comp, aes(x = t_months)) +
  geom_point(aes(y = D_obs), size = 3) +
  geom_line(aes(y = D_pred)) +
  labs(
    title = sprintf("Hyperbolic fit to observed discount points (k = %.3f)", k_hat),
    x = "Delay (months)", y = "Discount factor"
  ) +
  theme_minimal()



beta_wait_std <- -0.43                 # mixed logit coef on WaitTime_std
t <- c(0, 1, 3, 6)                     # delays in months

# --- Back out unstandardized time slope ---------------------------------
sd_wait <- sd(df$WaitTime, na.rm = TRUE)
beta_wait_unstd <- beta_wait_std / sd_wait

# Observed discount points from slope (exponential mapping)
D_obs <- exp(beta_wait_unstd * t)

# --- Fit EXponential: D(t) = exp(-delta * t) ----------------------------
D_exp <- function(t, delta) exp(-delta * t)

# SSE in levels; optimize over log(delta) so delta > 0
sse_exp <- function(theta) {
  delta <- exp(theta)
  D_pred <- D_exp(t, delta)
  sum((D_obs - D_pred)^2)
}

fit_exp <- optim(par = log(0.1), fn = sse_exp, method = "Nelder-Mead")
delta_hat <- exp(fit_exp$par)
sse_exp_val <- fit_exp$value

cat(sprintf("Exponential fit: delta = %.6f,  SSE = %.6f\n", delta_hat, sse_exp_val))

# Comparison table
comp_exp <- data.frame(
  t_months = t,
  D_obs = D_obs,
  D_pred_exp = D_exp(t, delta_hat)
)
print(comp_exp)

# --- Plot (optional) -----------------------------------------------------
# install.packages("ggplot2") # if needed
library(ggplot2)
ggplot(comp_exp, aes(x = t_months)) +
  geom_point(aes(y = D_obs), size = 3) +
  geom_line(aes(y = D_pred_exp)) +
  labs(
    title = sprintf("Exponential fit: delta = %.3f", delta_hat),
    x = "Delay (months)", y = "Discount factor"
  ) +
  theme_minimal()



library(mlogit)
library(dplyr)

df <- read.csv("data_indexed.csv")

# Make sure key fields are the right types
df <- df %>%
  mutate(
    Choice   = as.integer(Choice),          # 0/1
    idx.chid = as.factor(idx.chid),
    idx.alt  = as.factor(idx.alt),
    Gender   = relevel(factor(Gender), ref = "F")  # Female baseline
  )

# Build mlogit data (NOTE: no 'opposite' arg)
mlogit_data <- mlogit.data(
  df,
  choice   = "Choice",
  shape    = "long",
  chid.var = "idx.chid",
  alt.var  = "idx.alt",
  id.var   = "RespondentID"
)

base_mod <- mlogit(
  Choice ~ WaitTime_std + VaccineEfficacy_std + SideEffects_std +
    CashIncentives_std + VaccineOrigin + ASC_optout | 0,
  data = mlogit_data,
  rpar  = c(
    WaitTime_std        = "n",
    VaccineEfficacy_std = "n",
    SideEffects_std     = "n",
    CashIncentives_std  = "n"
  ),
  panel = TRUE,
  R     = 200,
  halton = NA        # <- not TRUE/FALSE
)

mod_gender <- mlogit(
  Choice ~ WaitTime_std + VaccineEfficacy_std + SideEffects_std +
    CashIncentives_std + VaccineOrigin + ASC_optout +
    WaitTime_std:Gender | 0,
  data  = mlogit_data,
  rpar  = c(
    WaitTime_std        = "n",
    VaccineEfficacy_std = "n",
    SideEffects_std     = "n",
    CashIncentives_std  = "n"
  ),
  panel = TRUE,
  R     = 200,
  halton = N
)
 summary(mod_gender)


 co <- coef(mod_gender)
 beta_wait_F_std <- unname(co["WaitTime_std"])
 # Interaction will be named like "WaitTime_std:GenderM"
 beta_wait_M_std <- beta_wait_F_std + ifelse("WaitTime_std:GenderM" %in% names(co), co["WaitTime_std:GenderM"], 0)
 
 sd_wait <- sd(df$WaitTime, na.rm = TRUE)
 beta_unstd <- c(F = beta_wait_F_std/sd_wait, M = beta_wait_M_std/sd_wait)
 
 t <- c(0,1,3,6)
 D_obs <- lapply(beta_unstd, function(b) exp(b * t))
 
 D_hyper <- function(t,k) 1/(1+k*t)
 D_exp   <- function(t,delta) exp(-delta*t)
 
 fit_one <- function(D) {
   sse_k <- function(theta){ k<-exp(theta); sum((D - D_hyper(t,k))^2) }
   sse_d <- function(theta){ d<-exp(theta); sum((D - D_exp(t,d))^2) }
   ok <- optim(log(0.1), sse_k, method="Nelder-Mead")
   od <- optim(log(0.1), sse_d, method="Nelder-Mead")
   c(k = exp(ok$par), SSE_h = ok$value, delta = exp(od$par), SSE_e = od$value)
 }
 res_gender <- sapply(D_obs, fit_one)
 print(res_gender)
 

 
 library(dplyr)
 library(tidyr)
 library(purrr)

 # --- 1. Extract group-specific standardized coefficients
 co <- coef(mod_gender)
 
 beta_wait_F_std <- unname(co["WaitTime_std"])  # Female baseline
 beta_wait_M_std <- beta_wait_F_std + ifelse("WaitTime_std:GenderM" %in% names(co),
                                             co["WaitTime_std:GenderM"], 0)
 
 # --- 2. Convert to unstandardized (per month)
 sd_wait <- sd(df$WaitTime, na.rm = TRUE)
 beta_wait_F_unstd <- beta_wait_F_std / sd_wait
 beta_wait_M_unstd <- beta_wait_M_std / sd_wait
 
 # --- 3. Function to fit hyperbolic k and exponential delta
 t_vals <- c(0, 1, 3, 6)
 
 D_hyper <- function(t, k) 1 / (1 + k * t)
 D_exp   <- function(t, delta) exp(-delta * t)
 
 fit_k_delta <- function(beta_unstd) {
   D_obs <- exp(beta_unstd * t_vals)
   
   sse_k <- function(theta) {
     k <- exp(theta)
     sum((D_obs - D_hyper(t_vals, k))^2)
   }
   sse_d <- function(theta) {
     delta <- exp(theta)
     sum((D_obs - D_exp(t_vals, delta))^2)
   }
   
   ok <- optim(par = log(0.1), fn = sse_k, method = "Nelder-Mead")
   od <- optim(par = log(0.1), fn = sse_d, method = "Nelder-Mead")
   
   tibble(
     k_hat     = exp(ok$par),
     SSE_hyper = ok$value,
     delta_hat = exp(od$par),
     SSE_exp   = od$value
   )
 }
 
 # --- 4. Build results table
 results_tbl <- tibble(
   Gender           = c("Female", "Male"),
   beta_wait_std    = c(beta_wait_F_std, beta_wait_M_std),
   beta_wait_unstd  = c(beta_wait_F_unstd, beta_wait_M_unstd)
 ) %>%
   mutate(fits = map(beta_wait_unstd, fit_k_delta)) %>%
   unnest(fits)
 
 print(results_tbl)
 
 
 library(ggplot2)
 library(dplyr)
 library(purrr)
 library(tidyr)
 
 # --- Build observed points ---
 t_vals <- c(0, 1, 3, 6)
 
 obs_df <- results_tbl %>%
   mutate(D_obs = map(beta_wait_unstd, ~ exp(.x * t_vals))) %>%
   select(Gender, D_obs) %>%
   unnest(D_obs) %>%
   mutate(t = rep(t_vals, times = n_distinct(Gender)))
 
 # --- Build fitted curves (smooth) ---
 t_grid <- seq(0, 6, length.out = 200)
 
 hyper_df <- results_tbl %>%
   mutate(D_pred = map(k_hat, ~ 1 / (1 + .x * t_grid))) %>%
   select(Gender, D_pred) %>%
   unnest(D_pred) %>%
   mutate(t = rep(t_grid, times = n_distinct(Gender)),
          Model = "Hyperbolic")
 
 exp_df <- results_tbl %>%
   mutate(D_pred = map(delta_hat, ~ exp(-.x * t_grid))) %>%
   select(Gender, D_pred) %>%
   unnest(D_pred) %>%
   mutate(t = rep(t_grid, times = n_distinct(Gender)),
          Model = "Exponential")
 
 curve_df <- bind_rows(hyper_df, exp_df)
 
 # --- Plot ---
 ggplot() +
   geom_point(data = obs_df, aes(x = t, y = D_obs), size = 2) +
   geom_line(data = curve_df, aes(x = t, y = D_pred, linetype = Model), size = 1) +
   facet_wrap(~ Gender) +
   labs(
     title = "Discounting Curves by Gender",
     x = "Delay (months)",
     y = "Discount Factor",
     linetype = "Model"
   ) +
   theme_minimal(base_size = 14) +
   theme(
     panel.grid.minor = element_blank(),
     legend.position = "bottom"
   )
 


 library(mlogit)
 library(dplyr)
 library(purrr)
 library(tidyr)
 library(ggplot2)

 df<-data_indexed
 

 
 library(dplyr)
 
 summary(data$Education)
 
 data<-dce_data_indexed_with_3_alts
 
 # Replace in-place
 data$Gender[data$Gender == "F"] <- "Female"
 data$Gender[data$Gender == "M"] <- "Male"
 
 # Verify the exact column name
 names(data)
 
 # Recode to a factor with labels
 data$UrbanRural <- factor(data$UrbanRural,
                           levels = c(0, 1),
                           labels = c("Rural", "Urban"))
 
 
 # … "SmokingStatus" …
 
 # Recode into an ordered factor
 data$SmokingStatus <- factor(data$SmokingStatus,
                              levels = c(0, 1, 2),
                              labels = c("Never", "Sometimes", "Often"))
 # Directly to an ordered factor
 data$PhysicalActivities <- factor(data$PhysicalActivities,
                                   levels = c(0, 1, 2),
                                   labels = c("Rare", "Sometimes", "Quite often"))
 
 # Directly to an ordered factor
 data$MedicalConditions <- factor(data$MedicalConditions,
                                  levels = c(0, 1),
                                  labels = c("No", "Yes"))
 
 
 # First, add “Prefer not to say” as an allowed level
 data$MedicalConditions <- addNA(data$MedicalConditions)
 levels(data$MedicalConditions)[is.na(levels(data$MedicalConditions))] <- "Prefer not to say"
 
 # Then replace actual NAs in the data with that new level
 data$MedicalConditions[is.na(data$MedicalConditions)] <- "Prefer not to say"
 
 # Turn 1–5 into descriptive, ordered levels
 data$HealthDescription <- factor(
   data$HealthDescription,
   levels = 1:5,
   labels = c("Poor", "Fair", "Good", "Very good", "Excellent"),
   ordered = TRUE
 )
 
 # If you only want to replace 0 → "No" in-place:
 data$SideEffectsCov[data$SideEffectsCov == 0] <- "No"
 
 # Recode into an ordered factor in one step
 data$GoverTrust <- factor(
   data$GoverTrust,
   levels = c(-2, -1, 0, 1, 2),
   labels = c(
     "Strongly distrust",
     "Somewhat distrust",
     "Neutral",
     "Somewhat trust",
     "Strongly trust"
   ),
   ordered = TRUE
 )
 
 # First, ensure "Prefer not to say" is one of the factor levels
 data$GoverTrust <- addNA(data$GoverTrust)
 # Rename the NA level
 levels(data$GoverTrust)[is.na(levels(data$GoverTrust))] <- "Prefer not to say"
 
 # Then replace actual NAs in the data with that level
 data$GoverTrust[is.na(data$GoverTrust)] <- "Prefer not to say"
 
 
 
 
 
 
 
 library(dplyr)
 
 # 1) Make Choice strictly logical TRUE/FALSE
 df <- df %>%
   mutate(
     Choice = case_when(
       Choice %in% c(1, "1", TRUE,  "TRUE", "T")  ~ TRUE,
       Choice %in% c(0, "0", FALSE, "FALSE", "F") ~ FALSE,
       TRUE ~ NA
     )
   )
 
 # 2) Quick diagnostics
 cat("Choice table (after coercion):\n")
 print(table(df$Choice, useNA = "ifany"))
 
 # 3) Ensure one chosen alt per choice set
 by_chid <- df %>%
   group_by(idx.chid) %>%
   summarise(n_true = sum(Choice, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
 
 problem_sets <- by_chid %>% filter(n_true != 1)
 if (nrow(problem_sets) > 0) {
   cat("⚠️ These choice sets do not have exactly one chosen alternative:\n")
   print(head(problem_sets, 20))
   # If you want to drop problematic sets (optional, be cautious):
   # bad_ids <- problem_sets$idx.chid
   # df <- df %>% filter(!idx.chid %in% bad_ids)
 }
 
 # 4) Types for IDs/alternatives (safe)
 df <- df %>%
   mutate(
     idx.chid = as.factor(idx.chid),
     idx.alt  = as.factor(idx.alt)
   )
 
 

 
 
 


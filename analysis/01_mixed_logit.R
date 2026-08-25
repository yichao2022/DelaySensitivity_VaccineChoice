
data<-df

# Install packages if not already installed
install.packages(c("mlogit", "data.table"))

install.packages("dfidx")
library(dfidx)
library(mlogit)
library(data.table)
# Ensure VaccineOrigin is a factor (categorical variable)
data$VaccineOrigin <- as.factor(data$VaccineOrigin)


# Convert data to mlogit format
mlogit_data <- mlogit.data(
  data,
  choice = "Choice",
  shape = "long",
  chid.var = "chid",
  alt.var = "alt",
  id.var = "RespondentID"
)


# Estimate Mixed Logit Model with VaccineOrigin included
model <- mlogit(
  Choice ~ WaitTime_std + VaccineEfficacy_std + SideEffects_std + CashIncentives_std +
    VaccineOrigin + ASC_optout | 0,
  data = mlogit_data,
  rpar = c(
    WaitTime_std = "n",
    VaccineEfficacy_std = "n",
    SideEffects_std = "n",
    CashIncentives_std = "n"
    # VaccineOrigin is categorical, typically treated as fixed effects
  ),
  panel = TRUE,
  R = 50,    # Number of simulated draws (adjust as needed)
  halton = NA
)

# View results
summary(model)

# Conditional Logit (fixed effects)
cond_model <- mlogit(
  Choice ~ WaitTime_std + VaccineEfficacy_std + SideEffects_std +
    CashIncentives_std + VaccineOrigin + ASC_optout | 0,
  data = mlogit_data
)

summary(cond_model)

# Multinomial Logit (use same call, just no panel structure)
mnl_model <- mlogit(
  Choice ~ WaitTime_std + VaccineEfficacy_std + SideEffects_std +
    CashIncentives_std + VaccineOrigin + ASC_optout | 0,
  data = mlogit_data
)

summary(mul_model)


logLik(cond_model)
logLik(mnl_model)
logLik(mixed_model)

AIC(cond_model, mnl_model, model)
BIC(cond_model, mnl_model, model)

library(texreg)

# Convert manually if needed (see earlier message)
convert_mlogit <- function(model, name) {
  coefs <- model$coefficients
  se <- sqrt(diag(vcov(model)))
  pval <- 2 * (1 - pnorm(abs(coefs / se)))
  createTexreg(coef.names = names(coefs), coef = coefs, se = se, pvalues = pval, model.name = name)
}

tr_cond  <- convert_mlogit(cond_model, "Conditional Logit")
tr_mnl   <- convert_mlogit(mnl_model,  "Multinomial Logit")
tr_mixed <- convert_mlogit(model,"Mixed Logit")

# Side-by-side comparison in console
screenreg(list(tr_cond, tr_mnl, tr_mixed),
          custom.note = "Standard errors in parentheses. *p<0.1; **p<0.05; ***p<0.01")


# Or export to Word
htmlreg(list(tr_cond, tr_mnl, tr_mixed),
        file = "logit_model_comparison.doc",
        caption = "Comparison of Logit Models")






# --- Packages
# install.packages(c("mlogit","htmltools","knitr"))
library(mlogit)
library(htmltools)
library(knitr)

# =========================
# 0) DATA & TYPE HYGIENE
# =========================
dat <- data          # you set: data <- data2_Copy
dat$alt           <- factor(dat$alt)
dat$VaccineOrigin <- factor(dat$VaccineOrigin)
dat$Choice        <- as.integer(dat$Choice)
dat$chid          <- as.character(dat$chid)
# (Optional:) ensure grouping vars are factors
dat$Gender     <- factor(dat$Gender)
dat$UrbanRural <- factor(dat$UrbanRural)  # "0","1" as levels is fine
dat$Age        <- factor(dat$Age)
dat$Education  <- factor(dat$Education)

# ======================================
# 1) HELPERS (partitioned slopes + mixed)
# ======================================
make_pslopes <- function(dat, var) {
  d <- dat
  d[[var]] <- factor(d[[var]])
  lvls <- levels(d[[var]])
  for (lv in lvls) {
    nm <- paste0("WT_", var, "_", gsub("[^A-Za-z0-9]+","_", lv))
    d[[nm]] <- d$WaitTime_std * as.numeric(d[[var]] == lv)
  }
  wt_terms <- paste0("WT_", var, "_", gsub("[^A-Za-z0-9]+","_", lvls), collapse = " + ")
  fml <- as.formula(
    paste0("Choice ~ ", wt_terms,
           " + VaccineEfficacy_std + SideEffects_std + CashIncentives_std + VaccineOrigin + ASC_optout | 0")
  )
  list(data = d, lvls = lvls, fml = fml, var = var)
}

# Use pseudo-random draws (no halton arg -> avoids the halton$prime error)
fit_mixed_and_extract <- function(d, fml, var, lvls, R = 400, correlated = FALSE) {
  mld <- mlogit.data(d, choice = "Choice", shape = "long",
                     chid.var = "chid", alt.var = "alt", id.var = "RespondentID")
  rpar_names <- paste0("WT_", var, "_", gsub("[^A-Za-z0-9]+","_", lvls))
  rpar_list  <- setNames(rep("n", length(rpar_names)), rpar_names)
  
  set.seed(123)
  m <- mlogit(
    fml, data = mld,
    rpar = rpar_list,
    panel = TRUE,
    R = R,
    correlation = correlated
  )
  
  cf <- coef(m); vc <- vcov(m)
  rows <- lapply(lvls, function(lv){
    base <- paste0("WT_", var, "_", gsub("[^A-Za-z0-9]+","_", lv))
    mu   <- unname(cf[[base]])
    se   <- sqrt(unname(vc[base, base]))
    sd_name <- paste0("sd.", base)
    sd_val  <- if (sd_name %in% names(cf)) unname(cf[[sd_name]]) else NA_real_
    sd_se   <- if (sd_name %in% rownames(vc)) sqrt(unname(vc[sd_name, sd_name])) else NA_real_
    data.frame(
      Variable  = var,
      Level     = as.character(lv),
      Mean      = mu,
      SE_mean   = se,
      z_mean    = mu / se,
      CI_low    = mu - 1.96 * se,
      CI_high   = mu + 1.96 * se,
      SD_random = sd_val,
      SE_SD     = sd_se,
      check.names = FALSE
    )
  })
  list(table = do.call(rbind, rows), model = m)
}

# ===================================
# 2) OVERALL MIXED LOGIT (for row 1)
# ===================================
mld_all <- mlogit.data(dat, choice = "Choice", shape = "long",
                       chid.var = "chid", alt.var = "alt", id.var = "RespondentID")

set.seed(123)
m_overall <- mlogit(
  Choice ~ WaitTime_std + VaccineEfficacy_std + SideEffects_std +
    CashIncentives_std + VaccineOrigin + ASC_optout | 0,
  data   = mld_all,
  rpar   = c(
    WaitTime_std        = "n",
    VaccineEfficacy_std = "n",
    SideEffects_std     = "n",
    CashIncentives_std  = "n"
  ),
  panel  = TRUE,
  R      = 400
  # if you insist on Halton: add halton = list(normal=TRUE, randomized=TRUE, seed=123)
)

cf_all <- coef(m_overall); vc_all <- vcov(m_overall)
mu_all <- unname(cf_all["WaitTime_std"])
se_all <- sqrt(unname(vc_all["WaitTime_std","WaitTime_std"]))
sd_all <- if ("sd.WaitTime_std" %in% names(cf_all)) unname(cf_all["sd.WaitTime_std"]) else NA_real_
sdse_all <- if ("sd.WaitTime_std" %in% rownames(vc_all)) sqrt(unname(vc_all["sd.WaitTime_std","sd.WaitTime_std"])) else NA_real_

overall_row <- data.frame(
  Variable  = "Overall",
  Level     = "All",
  Mean      = mu_all,
  SE_mean   = se_all,
  z_mean    = mu_all / se_all,
  CI_low    = mu_all - 1.96 * se_all,
  CI_high   = mu_all + 1.96 * se_all,
  SD_random = sd_all,
  SE_SD     = sdse_all,
  check.names = FALSE
)

# =========================================
# 3) SUBGROUP MIXED LOGITS (Gender/Urban/Age/Edu)
# =========================================
G <- make_pslopes(dat, "Gender");      res_G <- fit_mixed_and_extract(G$data, G$fml, "Gender",      G$lvls, R = 400)
U <- make_pslopes(dat, "UrbanRural");  res_U <- fit_mixed_and_extract(U$data, U$fml, "UrbanRural",  U$lvls, R = 400)
A <- make_pslopes(dat, "Age");         res_A <- fit_mixed_and_extract(A$data, A$fml, "Age",         A$lvls, R = 350)
E <- make_pslopes(dat, "Education");   res_E <- fit_mixed_and_extract(E$data, E$fml, "Education",   E$lvls, R = 400)

# =========================================
# 4) COMBINE + VIEW + SAVE (HTML & CSV)
# =========================================
mixed_waittime_table <- rbind(
  overall_row,
  res_G$table,
  res_U$table,
  res_A$table,
  res_E$table
)

# Show in viewer
View(mixed_waittime_table, title = "Mixed Logit: WaitTime_std by Subgroup (mean & SD)")

# Save HTML "web" page
html_file <- "mixed_waittime_by_subgroup.html"
htmltools::save_html(
  knitr::kable(mixed_waittime_table, format = "html", digits = 3,
               caption = "Mixed Logit: WaitTime_std by Subgroup (Mean, SD, 95% CI)"),
  file = html_file
)
cat("Saved HTML to:", normalizePath(html_file), "\n")

# Also save CSV (nice for paper tables)
write.csv(mixed_waittime_table, "mixed_waittime_by_subgroup.csv", row.names = FALSE)


data<-dce_data


# Replace in-place
data$Gender[data$Gender == "F"] <- "Female"
data$Gender[data$Gender == "M"] <- "Male"


library(dplyr)

data <- data %>%
  mutate(Education = recode_factor(as.character(Education),
                                   `0` = "No formal education",
                                   `1` = "Primary school",
                                   `2` = "Middle school",
                                   `3` = "High school",
                                   `4` = "College or University",
                                   `5` = "Graduated"
  ))

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

# 1) Install (if needed) and load packages
# install.packages("gtsummary")
# install.packages("dplyr")
library(dplyr)
library(gtsummary)

# 2) (Re)factor your variables with proper labels
data <- data %>%
  mutate(
    Age = factor(Age,
                 levels = c("18-34", "35-44", "45-54", "55-65", "65 above")),
    Gender = factor(Gender, levels = c("Female", "Male")),
    Education = factor(Education,
                       levels = c("No formal education",
                                  "Primary school",
                                  "Middle school",
                                  "High school",
                                  "College or University",
                                  "Graduated")),
    UrbanRural = factor(UrbanRural, levels = c("Rural", "Urban")),
    SmokingStatus = factor(SmokingStatus,
                           levels = c("Never", "Sometimes", "Often")),
    PhysicalActivities = factor(PhysicalActivities,
                                levels = c("Rare", "Sometimes", "Quite often")),
    MedicalConditions = factor(MedicalConditions,
                               levels = c("No", "Yes", "Prefer not to say")),
    GoverTrust = factor(GoverTrust,
                        levels = c("Strongly distrust",
                                   "Somewhat distrust",
                                   "Neutral",
                                   "Somewhat trust",
                                   "Strongly trust",
                                   "Prefer not to say"))
  )

# 3) Build the descriptive table
tbl <- data %>%
  select(Age, Gender, Education, UrbanRural,
         SmokingStatus, PhysicalActivities,
         MedicalConditions, GoverTrust) %>%
  tbl_summary(
    by = NULL,                              # no grouping variable
    label = list(
      Age ~ "Age",
      Gender ~ "Gender",
      Education ~ "Education",
      UrbanRural ~ "Urban/Rural",
      SmokingStatus ~ "Smoking status",
      PhysicalActivities ~ "Physical activities",
      MedicalConditions ~ "Medical conditions",
      GoverTrust ~ "Government trust"
    ),
    statistic = all_categorical() ~ "{n} ({p}%)"
  ) %>%
  modify_header(label ~ "") %>%            # remove the default “Characteristic” header
  bold_labels()                            # bold the left-hand labels

# 4) Print it in your R Markdown or Viewer
tbl



#–– 1. Load required packages
library(dplyr)
library(knitr)

#–– 2. Reconstruct numeric versions of your four measures
df2 <- data %>%
  mutate(
    SmokingStatus_num = as.numeric(SmokingStatus) - 1,
    PhysicalActivities_num = as.numeric(PhysicalActivities) - 1,
    MedicalConditions_num = case_when(
      MedicalConditions == "No"               ~ 0,
      MedicalConditions == "Yes"              ~ 1,
      TRUE                                    ~ NA_real_
    ),
    GoverTrust_num = case_when(
      GoverTrust == "Strongly distrust"   ~ -2,
      GoverTrust == "Somewhat distrust"   ~ -1,
      GoverTrust == "Neutral"             ~  0,
      GoverTrust == "Somewhat trust"      ~  1,
      GoverTrust == "Strongly trust"      ~  2,
      TRUE                                 ~ NA_real_
    )
  )

#–– 3. Helper to print mean ± SD
fmt <- function(x) {
  sprintf("%.2f ± %.2f", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
}

#–– 4. Build one summary table per “row” variable, then stack
row_vars <- c("Age", "Gender", "Education", "UrbanRural")
summary_tables <- lapply(row_vars, function(rv) {
  df2 %>%
    group_by(across(all_of(rv))) %>%
    summarise(
      `SmokingStatus (mean ± SD)`      = fmt(SmokingStatus_num),
      `PhysicalActivities (mean ± SD)` = fmt(PhysicalActivities_num),
      `MedicalConditions (mean ± SD)`  = fmt(MedicalConditions_num),
      `GoverTrust (mean ± SD)`         = fmt(GoverTrust_num),
      .groups = "drop"
    ) %>%
    rename(Level = rv) %>%
    mutate(Variable = rv, .before = Level)
}) 
final_table <- bind_rows(summary_tables)

#–– 5. Render as a neat kable
kable(
  final_table,
  col.names = c("Variable", "Level",
                "SmokingStatus (mean ± SD)",
                "PhysicalActivities (mean ± SD)",
                "MedicalConditions (mean ± SD)",
                "GoverTrust (mean ± SD)"),
  align = c("l", "l", "c", "c", "c", "c"),
  caption = "Descriptive Statistics — Mean ± SD by Group"
)

# this will pop the raw final_table into the RStudio “Data Viewer” pane
View(final_table)

final_table

# install.packages(c("kableExtra","rstudioapi"))
library(kableExtra)
library(rstudioapi)

# 1. Turn your kable into a standalone HTML file
tbl_html <- final_table %>%
  knitr::kable(format = "html", 
               caption = "Descriptive Statistics — Mean ± SD by Group") %>%
  kable_styling(full_width = FALSE)

# 2. Save it to a temp file
tmp <- tempfile(fileext = ".html")
kableExtra::save_kable(tbl_html, tmp)

# 3. Display it in the RStudio Viewer
viewer(tmp)

# install.packages("DT")
library(DT)
datatable(final_table, 
          caption = "Descriptive Statistics — Mean ± SD by Group",
          options = list(pageLength = nrow(final_table), dom = 't'))










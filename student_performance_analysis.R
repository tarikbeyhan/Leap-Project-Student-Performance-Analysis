#' ---
#' title: "Understanding Student Performance: R Analysis"
#' subtitle: "Final Project, Scenario 2 | LEAP Data Analytics and Statistics with Applications"
#' author: "Tarık Beyhan"
#' date: "October 2026"
#' output:
#'   pdf_document:
#'     toc: true
#' ---

#+ knitr-setup, include = FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE,
                      fig.width = 7, fig.height = 4.2)

################################################################################
# FINAL PROJECT, SCENARIO 2: UNDERSTANDING STUDENT PERFORMANCE
# Analysis script (adapted from the supplied starter script)
#
# Author : Tarık Beyhan
# Course : LEAP | Data Analytics and Statistics with Applications
# Data   : UCI Student Performance, Mathematics course (Cortez, 2008)
#
# How to run
#   Put this file and student-mat.csv in the same folder, set that folder as the
#   working directory and run the script from top to bottom (or source() it).
#   Figures are written to ./figures and key result tables to ./results.
#
# Main changes compared with the starter script
#   1. Predictor set chosen from the exploratory analysis (adds mother's
#      education and going out with friends; see Question 3).
#   2. Investigation of the 38 students with a final grade of 0, all of whom
#      have zero recorded absences, and a sensitivity analysis without them.
#   3. Spearman correlations alongside Pearson, because most "numeric"
#      variables are ordered categories.
#   4. Manual VIF check and a simple classification table for the logistic
#      model.
#   5. Figures and tables are saved to disk for the report and presentation.
#
# G1 and G2 (earlier-period grades) are NOT used as predictors in any model.
################################################################################

#' ## 0. Setup

needed_packages <- c("ggplot2", "dplyr", "broom")

missing_packages <- needed_packages[!(needed_packages %in% rownames(installed.packages()))]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(ggplot2)
library(dplyr)
library(broom)

options(digits = 4)

# Output folders for figures and tables used in the report and slides.
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# Helper: save the most recent ggplot with consistent dimensions.
save_fig <- function(name, width = 6, height = 4) {
  ggsave(file.path("figures", paste0(name, ".png")),
         width = width, height = height, dpi = 200, bg = "white")
}

# A single, quiet theme for all figures.
theme_set(theme_minimal(base_size = 11))


#' ## Question 1. Understand and prepare the data

local_file <- "student-mat.csv"

# Download from UCI only if the local copy is missing.
if (!file.exists(local_file)) {
  zip_url <- "https://archive.ics.uci.edu/static/public/320/student+performance.zip"
  zip_file <- tempfile(fileext = ".zip")
  extract_dir <- tempfile("student_data_")
  dir.create(extract_dir)

  download.file(zip_url, zip_file, mode = "wb")

  zip_contents <- unzip(zip_file, list = TRUE)$Name
  math_name <- zip_contents[grepl("student-mat\\.csv$", zip_contents)]

  if (length(math_name) != 1) {
    stop("Could not identify student-mat.csv inside the downloaded ZIP file.")
  }

  unzip(zip_file, files = math_name, exdir = extract_dir)
  file.copy(file.path(extract_dir, math_name), local_file, overwrite = TRUE)
}

# The UCI file is semicolon separated.
students <- read.csv2(local_file, stringsAsFactors = FALSE)

head(students)
dim(students)          # 395 rows (students), 33 columns
names(students)
str(students)

# Data-quality checks.
colSums(is.na(students))
sum(is.na(students))       # 0 missing values
sum(duplicated(students))  # 0 duplicated rows
summary(students)

# Convert categorical (nominal and binary) variables to factors.
factor_variables <- c(
  "school", "sex", "address", "famsize", "Pstatus",
  "Mjob", "Fjob", "reason", "guardian", "schoolsup", "famsup",
  "paid", "activities", "nursery", "higher", "internet", "romantic"
)
students[factor_variables] <- lapply(students[factor_variables], factor)

# Ordered variables (Medu, Fedu, traveltime, studytime, famrel, freetime,
# goout, Dalc, Walc, health) are kept as integer codes so that they can enter
# correlations and regressions as approximately linear scores. This is a
# simplifying assumption that is discussed in the report.
# failures is a count that is top-coded: the value 3 means "3 or more".

# Binary outcome: Pass if G3 >= 10.
students <- students %>%
  mutate(pass = factor(ifelse(G3 >= 10, "Pass", "Fail"),
                       levels = c("Fail", "Pass")))
table(students$pass)

#' ### Problematic values: final grade of zero
# A Portuguese grade of 0 out of 20 is very unusual for a student who sat the
# exam. Check how many students have G3 == 0 and how they look on other
# variables.
zero_g3 <- students$G3 == 0
sum(zero_g3)                                   # 38 students
table(students$absences[zero_g3])              # all 38 have 0 absences
summary(students$G1[zero_g3])                  # earlier grades are non-zero
sum(students$G2[zero_g3] == 0)                 # 13 already had G2 == 0

# Interpretation: these are most plausibly students who did not sit the final
# assessment (dropped out or did not attend), with absences not recorded,
# rather than students who sat the exam and scored zero. We keep them in the
# main analysis, because they are real fails, but we test in Question 5
# whether the conclusions depend on them.

zero_summary <- students %>%
  mutate(group = ifelse(zero_g3, "G3 = 0", "G3 > 0")) %>%
  group_by(group) %>%
  summarise(n = n(),
            mean_absences = mean(absences),
            share_zero_absences = mean(absences == 0),
            mean_G1 = mean(G1),
            mean_failures = mean(failures))
print(zero_summary)
write.csv(zero_summary, "results/zero_grade_check.csv", row.names = FALSE)


#' ## Question 2. Explore the data

describe_numeric <- function(x) {
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q1 = unname(quantile(x, 0.25, na.rm = TRUE)),
    q3 = unname(quantile(x, 0.75, na.rm = TRUE)),
    max = max(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE)
  )
}

desc_vars <- c("G3", "age", "absences", "failures", "studytime", "Medu", "goout")
descriptives <- as.data.frame(t(sapply(students[desc_vars], describe_numeric)))
descriptives$variable <- rownames(descriptives)
print(round(descriptives[, -ncol(descriptives)], 2))
write.csv(descriptives, "results/descriptives.csv", row.names = FALSE)

# G3 without the zero group, for comparison.
describe_numeric(students$G3[!zero_g3])

# Frequency tables.
table(students$pass)
round(prop.table(table(students$pass)) * 100, 1)
table(students$sex)
table(students$higher)
table(students$failures)
table(students$schoolsup)

# Figure 1: distribution of the final grade, zero group highlighted.
ggplot(students, aes(x = G3, fill = G3 == 0)) +
  geom_histogram(binwidth = 1, boundary = -0.5, colour = "white") +
  geom_vline(xintercept = 9.5, linetype = 2) +
  annotate("text", x = 9.7, y = 54, label = "Pass threshold (10)",
           hjust = 0, size = 3.3) +
  scale_fill_manual(values = c(`FALSE` = "grey55", `TRUE` = "#C0392B"),
                    labels = c("G3 > 0", "G3 = 0 (38 students)"),
                    name = NULL) +
  labs(title = "Distribution of final Mathematics grade (G3)",
       x = "Final grade (0 to 20)", y = "Number of students") +
  theme(legend.position = "top")
save_fig("fig1_g3_distribution")

# Figure 2: final grade by number of previous class failures.
ggplot(students, aes(x = factor(failures), y = G3)) +
  geom_boxplot(fill = "grey85") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               colour = "#C0392B") +
  scale_x_discrete(labels = c("0", "1", "2", "3+")) +
  labs(title = "Final grade by previous class failures",
       subtitle = "Diamond = group mean",
       x = "Number of previous class failures", y = "Final grade (G3)")
save_fig("fig2_g3_by_failures", width = 5, height = 4)

# Figure 3: final grade by going out with friends.
ggplot(students, aes(x = factor(goout), y = G3)) +
  geom_boxplot(fill = "grey85") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               colour = "#C0392B") +
  labs(title = "Final grade by going out with friends",
       x = "Going out (1 = very low, 5 = very high)", y = "Final grade (G3)")
save_fig("fig3_g3_by_goout", width = 5, height = 4)

# Additional exploratory plots (from the starter script).
ggplot(students, aes(x = factor(studytime), y = G3)) +
  geom_boxplot(fill = "grey85") +
  labs(title = "Final grade by weekly study-time category",
       x = "Study time (1 = <2h, 2 = 2-5h, 3 = 5-10h, 4 = >10h)",
       y = "Final grade (G3)")
save_fig("figA_g3_by_studytime", width = 5, height = 4)

ggplot(students, aes(x = higher, y = G3)) +
  geom_boxplot(fill = "grey85") +
  labs(title = "Final grade by intention to continue to higher education",
       x = "Wants higher education", y = "Final grade (G3)")
save_fig("figA_g3_by_higher", width = 5, height = 4)

pass_by_sex <- students %>%
  count(sex, pass) %>%
  group_by(sex) %>%
  mutate(percent = 100 * n / sum(n))
print(pass_by_sex)


#' ## Question 3. Investigate relationships

cor_vars <- students %>%
  select(age, Medu, Fedu, traveltime, studytime, failures,
         famrel, freetime, goout, Dalc, Walc, health, absences, G3)

# Pearson treats codes as interval measurements; Spearman only uses ranks and
# is therefore more defensible for ordered categories. We report both.
cor_pearson  <- cor(cor_vars, method = "pearson")
cor_spearman <- cor(cor_vars, method = "spearman")

g3_cor <- data.frame(
  variable = rownames(cor_pearson),
  pearson  = cor_pearson[, "G3"],
  spearman = cor_spearman[, "G3"]
) %>%
  filter(variable != "G3") %>%
  arrange(pearson)
print(g3_cor, digits = 2)
write.csv(g3_cor, "results/correlations_with_G3.csv", row.names = FALSE)

# Notable correlations between predictors (|r| >= 0.4).
round(cor_pearson, 2)
# Medu-Fedu (about 0.62) and Dalc-Walc (about 0.65) overlap strongly, so only
# one of each pair is a candidate for the model.

# Figure 4: correlation heatmap (ggplot version, saved for the report).
cor_long <- as.data.frame(as.table(cor_pearson))
names(cor_long) <- c("var1", "var2", "r")
ggplot(cor_long, aes(var1, var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.3) +
  scale_fill_gradient2(low = "#2E86C1", mid = "white", high = "#C0392B",
                       limits = c(-1, 1), name = "r") +
  labs(title = "Pearson correlations (ordered codes treated as scores)",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig("fig4_correlation_heatmap", width = 7, height = 6)

#' ### Contingency tables with conditional (column) percentages

pass_table <- function(var) {
  tab <- table(students$pass, students[[var]])
  pct <- round(prop.table(tab, margin = 2) * 100, 1)
  print(tab)
  print(pct)
  invisible(pct)
}

pass_table("higher")      # wants higher education: yes vs no
pass_table("schoolsup")   # extra educational support
pass_table("sex")
pass_table("romantic")

# Previous failures grouped as any vs none.
students$any_failure <- factor(ifelse(students$failures > 0, "1 or more", "None"),
                               levels = c("None", "1 or more"))
tab_fail <- table(students$pass, students$any_failure)
print(tab_fail)
round(prop.table(tab_fail, margin = 2) * 100, 1)

# Chi-squared tests as a rough check of association (expected counts for
# 'higher = no' are small, so Fisher's exact test is used there).
chisq.test(tab_fail)
fisher.test(table(students$pass, students$higher))
chisq.test(table(students$pass, students$schoolsup))

# Figure 5: pass rate by key groups, for the report and presentation.
pass_rates <- bind_rows(
  students %>% group_by(level = any_failure) %>%
    summarise(rate = mean(pass == "Pass"), n = n()) %>%
    mutate(characteristic = "Previous failures"),
  students %>% group_by(level = higher) %>%
    summarise(rate = mean(pass == "Pass"), n = n()) %>%
    mutate(characteristic = "Wants higher education", level = as.character(level)),
  students %>% group_by(level = schoolsup) %>%
    summarise(rate = mean(pass == "Pass"), n = n()) %>%
    mutate(characteristic = "Extra school support", level = as.character(level))
) %>%
  mutate(level = factor(as.character(level),
                        levels = c("None", "1 or more", "no", "yes")))
print(pass_rates)
write.csv(pass_rates, "results/pass_rates_by_group.csv", row.names = FALSE)

ggplot(pass_rates, aes(x = level, y = rate * 100)) +
  geom_col(fill = "grey55", width = 0.6) +
  geom_text(aes(label = paste0(round(rate * 100), "%\n(n = ", n, ")")),
            vjust = -0.2, size = 3) +
  facet_wrap(~ characteristic, scales = "free_x") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Share of students passing (G3 >= 10) by group",
       x = NULL, y = "Pass rate (%)")
save_fig("fig5_pass_rates", width = 7, height = 3.6)

# Absences versus final grade, coloured by the zero group: shows why the
# overall correlation with absences is close to zero.
ggplot(students, aes(x = absences, y = G3, colour = G3 == 0)) +
  geom_jitter(alpha = 0.6, width = 0.3, height = 0.2) +
  geom_smooth(data = students[!zero_g3, ], aes(group = 1), method = "lm",
              se = TRUE, colour = "black") +
  scale_colour_manual(values = c(`FALSE` = "grey45", `TRUE` = "#C0392B"),
                      labels = c("G3 > 0", "G3 = 0"), name = NULL) +
  labs(title = "Final grade versus absences",
       subtitle = "Line fitted to students with G3 > 0 only",
       x = "Number of absences", y = "Final grade (G3)") +
  theme(legend.position = "top")
save_fig("fig6_absences_vs_g3", width = 6, height = 4)

# Choice of predictors for modelling (justified in the report):
#   failures  : strongest correlation with G3 (about -0.36)
#   Medu      : strongest positive correlation (about 0.22); chosen over Fedu
#   goout     : negative association (about -0.13); chosen over Dalc/Walc
#   studytime : expected to matter; weak but positive
#   absences  : behaviour of interest; distorted by the zero group
#   sex       : pass rates differ by sex
#   higher    : very large pass-rate gap, but only 20 students answer 'no'
# schoolsup is NOT included in the main model: extra support is given to
# weaker students, so its association with grades reflects selection, not
# the effect of support (reverse causation).


#' ## Question 4. Build and interpret statistical models

# Baseline: the starter-script model, kept for comparison.
baseline_model <- lm(G3 ~ studytime + failures + absences + sex + higher,
                     data = students)
summary(baseline_model)

#' ### Main multiple linear regression
# Reference categories: sex = F (female), higher = no.
linear_model <- lm(
  G3 ~ failures + Medu + goout + studytime + absences + sex + higher,
  data = students
)
summary(linear_model)

linear_coef <- tidy(linear_model, conf.int = TRUE)
print(linear_coef)
write.csv(linear_coef, "results/linear_model_coefficients.csv", row.names = FALSE)

model_fit <- bind_rows(
  glance(baseline_model) %>% mutate(model = "Baseline (starter)"),
  glance(linear_model)   %>% mutate(model = "Main model")
) %>% select(model, r.squared, adj.r.squared, sigma, AIC, nobs)
print(model_fit)
write.csv(model_fit, "results/linear_model_fit.csv", row.names = FALSE)

# Illustration: predicted G3 for a 'typical' student with 0 vs 2 failures,
# other predictors at their median / most common level.
typical <- data.frame(failures = c(0, 2), Medu = 3, goout = 3, studytime = 2,
                      absences = 4, sex = "F", higher = "yes")
predict(linear_model, newdata = typical, interval = "confidence")

#' ### Logistic regression for pass/fail
# glm() models the probability of the second level, Pass.
logistic_model <- glm(
  pass ~ failures + Medu + goout + studytime + absences + sex + higher,
  data = students, family = binomial
)
summary(logistic_model)

logistic_coef <- tidy(logistic_model, conf.int = TRUE)
logistic_or   <- tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE)
print(logistic_or)
write.csv(logistic_or, "results/logistic_odds_ratios.csv", row.names = FALSE)

# Predicted probabilities of passing for the same two 'typical' students.
# This translates odds ratios into probabilities for the presentation.
typical$p_pass <- predict(logistic_model, newdata = typical, type = "response")
print(typical)

# Predicted probability of passing across failures and going out.
prob_grid <- expand.grid(failures = 0:3, goout = c(1, 3, 5), Medu = 3,
                         studytime = 2, absences = 4, sex = "F", higher = "yes")
prob_grid$p_pass <- predict(logistic_model, newdata = prob_grid, type = "response")
print(prob_grid)
write.csv(prob_grid, "results/predicted_pass_probabilities.csv", row.names = FALSE)

# Figure 7: predicted pass probability by failures (for presentation).
ggplot(prob_grid, aes(x = failures, y = p_pass * 100,
                      colour = factor(goout), group = factor(goout))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("1" = "#2E86C1", "3" = "grey40", "5" = "#C0392B"),
                      labels = c("1 (very low)", "3 (average)", "5 (very high)"),
                      name = "Going out") +
  scale_x_continuous(breaks = 0:3, labels = c("0", "1", "2", "3+")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Model-predicted chance of passing",
       subtitle = "Female student, wants higher education; other predictors at typical values",
       x = "Previous class failures", y = "Predicted probability of passing (%)")
save_fig("fig7_predicted_pass_probability", width = 6, height = 4)

# Simple classification check at a 0.5 cut-off. Only 'Fail' predictions are
# useful for an early-warning reading, so sensitivity for Fail is reported.
students$p_pass <- predict(logistic_model, type = "response")
students$pred_class <- factor(ifelse(students$p_pass >= 0.5, "Pass", "Fail"),
                              levels = c("Fail", "Pass"))
conf_mat <- table(Observed = students$pass, Predicted = students$pred_class)
print(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
fail_detected <- conf_mat["Fail", "Fail"] / sum(conf_mat["Fail", ])
c(accuracy = accuracy, share_of_fails_detected = fail_detected,
  no_information_rate = max(prop.table(table(students$pass))))


#' ## Question 5. Evaluate the analysis

#' ### Residuals versus fitted
diagnostic_data <- augment(linear_model) %>%
  mutate(zero_grade = G3 == 0)

ggplot(diagnostic_data, aes(x = .fitted, y = .resid, colour = zero_grade)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(aes(group = 1), se = FALSE, colour = "black", method = "loess") +
  scale_colour_manual(values = c(`FALSE` = "grey45", `TRUE` = "#C0392B"),
                      labels = c("G3 > 0", "G3 = 0"), name = NULL) +
  labs(title = "Residuals versus fitted values (main linear model)",
       x = "Fitted G3", y = "Residual") +
  theme(legend.position = "top")
save_fig("fig8_residuals_vs_fitted", width = 6, height = 4)

# The G3 = 0 students form a diagonal band of large negative residuals
# (residual = -fitted). This is a clear violation of the linear-model
# assumptions for that group.

# Normal Q-Q plot of residuals (supplementary).
ggplot(diagnostic_data, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.6) + stat_qq_line() +
  labs(title = "Normal Q-Q plot of standardised residuals",
       x = "Theoretical quantiles", y = "Standardised residuals")
save_fig("figA_qq_plot", width = 5, height = 4)

#' ### Overlap between predictors
manual_vif <- function(model) {
  X <- model.matrix(model)[, -1, drop = FALSE]
  vif_values <- sapply(seq_len(ncol(X)), function(i) {
    r2 <- summary(lm(X[, i] ~ X[, -i, drop = FALSE]))$r.squared
    1 / (1 - r2)
  })
  setNames(vif_values, colnames(X))
}
vif_values <- manual_vif(linear_model)
round(vif_values, 2)   # all close to 1: little overlap among predictors

round(cor(students %>% select(failures, Medu, goout, studytime, absences)), 2)

#' ### Uncertainty
print(linear_coef)
print(logistic_or)

#' ### Sensitivity analysis: exclude the 38 students with G3 = 0
students_nz <- students %>% filter(G3 > 0)

linear_model_nz <- lm(
  G3 ~ failures + Medu + goout + studytime + absences + sex + higher,
  data = students_nz
)
summary(linear_model_nz)

logistic_model_nz <- glm(
  pass ~ failures + Medu + goout + studytime + absences + sex + higher,
  data = students_nz, family = binomial
)

sensitivity <- bind_rows(
  tidy(linear_model, conf.int = TRUE)    %>% mutate(model = "Linear, all 395"),
  tidy(linear_model_nz, conf.int = TRUE) %>% mutate(model = "Linear, G3 > 0 (357)"),
  tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(model = "Logistic OR, all 395"),
  tidy(logistic_model_nz, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(model = "Logistic OR, G3 > 0 (357)")
) %>% filter(term != "(Intercept)")
print(sensitivity, n = 40)
write.csv(sensitivity, "results/sensitivity_analysis.csv", row.names = FALSE)
glance(linear_model_nz)

# Residual plot without the zero group.
ggplot(augment(linear_model_nz), aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6, colour = "grey45") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, colour = "black", method = "loess") +
  labs(title = "Residuals versus fitted values (students with G3 > 0)",
       x = "Fitted G3", y = "Residual")
save_fig("fig9_residuals_nonzero", width = 6, height = 4)


#' ## Question 6. Draw appropriate conclusions

# No new code. Conclusions are discussed in the report. In short:
#  - Observational, cross-sectional, questionnaire-based data from two
#    Portuguese schools (2005-2006): no causal interpretation.
#  - Previous failures is the most robust and practically important
#    association across all models and both samples.
#  - Absences: the apparent positive or null association in the full data is
#    an artefact of the G3 = 0 group; among students with a final grade,
#    more absences are associated with lower grades.
#  - The models explain only around one fifth of the variation in G3.

# Save session information for reproducibility.
writeLines(capture.output(sessionInfo()), "results/session_info.txt")

################################################################################
# END OF SCRIPT
################################################################################

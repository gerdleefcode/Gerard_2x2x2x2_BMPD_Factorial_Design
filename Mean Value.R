sink("Gerard_BMPD_MEAN_N1468_260630.txt")
cat("\n-----------------------------------")
cat("\nPublisher: Lee Cheuk Man Gerard")
cat("\nPublication date: 30th June 2026")
cat("\n-----------------------------------\n")

# Required packages
if(!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# 1) Read data
df <- read.csv("N1468_01_Version.csv", stringsAsFactors = FALSE)

# 2) Rename long columns to short names and coerce DV columns to numeric (if needed)
df2 <- df %>%
  rename(
    Destination = DestinationTypeBMFunctional1PDHedonic0,
    IC = InformationCompletenessHigh1Low0,
    IS = InformationSourceHigh1Low0,
    SR = AISelfRatingHigh1Low0,
    PR = AIPublichRatingHigh1Low0,
    Pers = MEAN_PerceivedPersuasiveness,
    Info = MEAN_PerceivedInformativeness
  ) %>%
  # If Pers/Info read as character, force numeric:
  mutate(
    Pers = as.numeric(as.character(Pers)),
    Info = as.numeric(as.character(Info))
  )

# 3) Recode binary 0/1 into factors with labels Low/High and Destination into BM/PD
df2 <- df2 %>%
  mutate(
    Destination = factor(ifelse(as.numeric(as.character(Destination)) == 1, "BM", "PD")),
    IC = factor(as.numeric(as.character(IC)), levels = c(0,1), labels = c("Low","High")),
    IS = factor(as.numeric(as.character(IS)), levels = c(0,1), labels = c("Low","High")),
    PR = factor(as.numeric(as.character(PR)), levels = c(0,1), labels = c("Low","High")),
    SR = factor(as.numeric(as.character(SR)), levels = c(0,1), labels = c("Low","High"))
  )

# 4) Compute cell summaries (one row per combination of the 4 factors) for each Destination
cell_summary <- df2 %>%
  group_by(Destination, IC, IS, PR, SR) %>%
  summarise(
    n = n(),
    mean_Pers = mean(Pers, na.rm = TRUE),
    sd_Pers = sd(Pers, na.rm = TRUE),
    mean_Info = mean(Info, na.rm = TRUE),
    sd_Info = sd(Info, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # make factor columns character for consistent joining/presentation
  mutate(across(c(IC,IS,PR,SR,Destination), as.character))

# 5) Ensure full 16 combos per Destination are present (will show NA for missing combos)
all_combos <- expand.grid(
  Destination = c("BM","PD"),
  IC = c("Low","High"),
  IS = c("Low","High"),
  PR = c("Low","High"),
  SR = c("Low","High"),
  stringsAsFactors = FALSE
) %>% as_tibble()

cell_summary_full <- all_combos %>%
  left_join(cell_summary, by = c("Destination","IC","IS","PR","SR")) %>%
  arrange(Destination, IC, IS, PR, SR)

# 6) Split into the four ANOVA tables you listed:
# ANOVA Table 1: BM Pers
BM_Pers <- cell_summary_full %>% filter(Destination == "BM") %>%
  select(IC,IS,PR,SR, n, mean_Pers, sd_Pers)

# ANOVA Table 2: BM Info
BM_Info <- cell_summary_full %>% filter(Destination == "BM") %>%
  select(IC,IS,PR,SR, n, mean_Info, sd_Info)

# ANOVA Table 3: PD Pers
PD_Pers <- cell_summary_full %>% filter(Destination == "PD") %>%
  select(IC,IS,PR,SR, n, mean_Pers, sd_Pers)

# ANOVA Table 4: PD Info
PD_Info <- cell_summary_full %>% filter(Destination == "PD") %>%
  select(IC,IS,PR,SR, n, mean_Info, sd_Info)

# 7) Print results (or inspect them in View())
print("BM - Perceived Persuasiveness (16 cells)")
print(BM_Pers)
print("BM - Perceived Informativeness (16 cells)")
print(BM_Info)
print("PD - Perceived Persuasiveness (16 cells)")
print(PD_Pers)
print("PD - Perceived Informativeness (16 cells)")
print(PD_Info)

# Optional: write to CSV for external use
write.csv(cell_summary_full, "cell_means_4factors_by_destination.csv", row.names = FALSE)
write.csv(BM_Pers, "BM_Pers_16cells.csv", row.names = FALSE)
write.csv(BM_Info, "BM_Info_16cells.csv", row.names = FALSE)
write.csv(PD_Pers, "PD_Pers_16cells.csv", row.names = FALSE)
write.csv(PD_Info, "PD_Info_16cells.csv", row.names = FALSE)

# -------------------------------------------------------------------------
# Compute 2x2x2 summaries for four 3-factor groupings (per Destination & DV)
# -------------------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# 1) Read data (change filename/path if needed)
df <- read_csv("N1468_01_Version.csv", show_col_types = FALSE)

# 2) Rename columns and coerce types
df2 <- df %>%
  rename(
    Destination = DestinationTypeBMFunctional1PDHedonic0,
    IC = InformationCompletenessHigh1Low0,
    IS = InformationSourceHigh1Low0,
    SR = AISelfRatingHigh1Low0,
    PR = AIPublichRatingHigh1Low0,
    Pers = MEAN_PerceivedPersuasiveness,
    Info = MEAN_PerceivedInformativeness
  ) %>%
  mutate(
    Destination = if_else(as.numeric(Destination) == 1, "BM", "PD"),
    IC = factor(as.numeric(IC), levels = c(0,1), labels = c("Low","High")),
    IS = factor(as.numeric(IS), levels = c(0,1), labels = c("Low","High")),
    PR = factor(as.numeric(PR), levels = c(0,1), labels = c("Low","High")),
    SR = factor(as.numeric(SR), levels = c(0,1), labels = c("Low","High")),
    Pers = as.numeric(Pers),
    Info = as.numeric(Info)
  )

# 3) Helper: summarize a triple (collapsing across the fourth factor)
summarize_triple <- function(data, triple_vars) {
  # triple_vars: character vector of length 3, e.g. c("IC","IS","PR")
  grouping_vars <- c("Destination", triple_vars)
  summ <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n = n(),
      mean_Pers = mean(Pers, na.rm = TRUE),
      sd_Pers = sd(Pers, na.rm = TRUE),
      mean_Info = mean(Info, na.rm = TRUE),
      sd_Info = sd(Info, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(all_of(grouping_vars), as.character))
  
  # create all possible Destination x 2x2x2 combos (so missing combos appear as rows)
  combo_list <- c(list(Destination = c("BM", "PD")),
                  setNames(rep(list(c("Low","High")), length(triple_vars)), triple_vars))
  all_combos <- as_tibble(do.call(expand.grid, c(combo_list, stringsAsFactors = FALSE)))
  
  # left join to ensure full grid and set n = 0 for empty combos
  res <- all_combos %>%
    left_join(summ, by = grouping_vars) %>%
    arrange(Destination, across(all_of(triple_vars))) %>%
    mutate(n = replace_na(n, 0L))
  return(res)
}

# 4) Define triples and compute
triples <- list(
  IC_IS_PR = c("IC","IS","PR"),
  IC_PR_SR = c("IC","PR","SR"),
  IS_PR_SR = c("IS","PR","SR"),
  IC_IS_SR = c("IC","IS","SR")
)

results <- map(triples, ~ summarize_triple(df2, .x))

# 5) Print results to console
for (nm in names(results)) {
  cat("\n\n===== Triple:", nm, "=====\n")
  print(results[[nm]])
}

# 6) Write CSVs:
# - triplet_<name>_by_destination.csv  (contains Destination + 3 factor cols)
# - triplet_<name>_BM.csv and _PD.csv  (only the 3 factor cols for each destination)
walk2(results, names(results), function(tbl, nm) {
  write_csv(tbl, paste0("triplet_", nm, "_by_destination.csv"))
  write_csv(tbl %>% filter(Destination == "BM") %>% select(-Destination),
            paste0("triplet_", nm, "_BM.csv"))
  write_csv(tbl %>% filter(Destination == "PD") %>% select(-Destination),
            paste0("triplet_", nm, "_PD.csv"))
})

# -------------------------------------------------------------------------
# Optional: If you want the 3-factor tables collapsed ACROSS Destination
# (i.e., BM + PD combined), use the function below:
# -------------------------------------------------------------------------
summarize_triple_no_dest <- function(data, triple_vars) {
  grouping_vars <- triple_vars
  summ <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n = n(),
      mean_Pers = mean(Pers, na.rm = TRUE),
      sd_Pers = sd(Pers, na.rm = TRUE),
      mean_Info = mean(Info, na.rm = TRUE),
      sd_Info = sd(Info, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(all_of(grouping_vars), as.character))
  
  combo_list <- setNames(rep(list(c("Low","High")), length(triple_vars)), triple_vars)
  all_combos <- as_tibble(do.call(expand.grid, c(combo_list, stringsAsFactors = FALSE)))
  
  all_combos %>% left_join(summ, by = grouping_vars) %>% arrange(across(all_of(triple_vars)))
}

# Example (collapsed across Destination):
# collapsed_IC_IS_PR <- summarize_triple_no_dest(df2, c("IC","IS","PR"))
# print(collapsed_IC_IS_PR)
# write_csv(collapsed_IC_IS_PR, "triplet_IC_IS_PR_collapsedAcrossDestination.csv")

# 2-way (2x2) cell summaries for all factor pairs
# Requires tidyverse (readr, dplyr, purrr)
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# ---- 1) Read / rename / recode ----
df <- readr::read_csv("N1468_01_Version.csv", show_col_types = FALSE)

df2 <- df %>%
  rename(
    Destination = DestinationTypeBMFunctional1PDHedonic0,
    IC = InformationCompletenessHigh1Low0,
    IS = InformationSourceHigh1Low0,
    SR = AISelfRatingHigh1Low0,
    PR = AIPublichRatingHigh1Low0,
    Pers = MEAN_PerceivedPersuasiveness,
    Info = MEAN_PerceivedInformativeness
  ) %>%
  mutate(
    # Destination: 1 -> BM, 0 -> PD (based on your header)
    Destination = if_else(as.numeric(Destination) == 1, "BM", "PD"),
    # factors: 0 -> Low, 1 -> High
    IC = factor(as.numeric(IC), levels = c(0,1), labels = c("Low","High")),
    IS = factor(as.numeric(IS), levels = c(0,1), labels = c("Low","High")),
    PR = factor(as.numeric(PR), levels = c(0,1), labels = c("Low","High")),
    SR = factor(as.numeric(SR), levels = c(0,1), labels = c("Low","High")),
    # ensure DVs numeric
    Pers = as.numeric(Pers),
    Info = as.numeric(Info)
  )

# ---- 2) Helper to summarize a pair (Destination x factor1 x factor2) ----
summarize_pair <- function(data, pair_vars) {
  grouping_vars <- c("Destination", pair_vars)
  summ <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n = n(),
      mean_Pers = mean(Pers, na.rm = TRUE),
      sd_Pers   = sd(Pers,   na.rm = TRUE),
      mean_Info = mean(Info, na.rm = TRUE),
      sd_Info   = sd(Info,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(all_of(grouping_vars), as.character))
  
  # Build full grid (Destination x Low/High x Low/High) so missing combos appear
  combo_list <- c(list(Destination = c("BM","PD")),
                  setNames(rep(list(c("Low","High")), length(pair_vars)), pair_vars))
  all_combos <- as_tibble(do.call(expand.grid, c(combo_list, stringsAsFactors = FALSE)))
  
  # Left join so absent combos are present with n = 0 and NA means/SDs
  res <- all_combos %>%
    left_join(summ, by = grouping_vars) %>%
    arrange(Destination, across(all_of(pair_vars))) %>%
    mutate(n = replace_na(n, 0L))
  
  return(res)
}

# ---- 3) Define the 6 pairs and compute ----
pairs <- list(
  IC_IS = c("IC","IS"),
  IC_PR = c("IC","PR"),
  IC_SR = c("IC","SR"),
  IS_PR = c("IS","PR"),
  IS_SR = c("IS","SR"),
  PR_SR = c("PR","SR")
)

results <- purrr::map(pairs, ~ summarize_pair(df2, .x))

# ---- 4) Print to console (brief) ----
for (nm in names(results)) {
  cat("\n\n===== Pair:", nm, "=====\n")
  print(results[[nm]])
}

# ---- 5) Write CSVs: one combined (by destination) and separate BM/PD files ----
purrr::walk2(results, names(results), function(tbl, nm) {
  readr::write_csv(tbl, paste0("pair_", nm, "_by_destination.csv"))
  # BM only (drop Destination column)
  readr::write_csv(tbl %>% filter(Destination == "BM") %>% select(-Destination),
                   paste0("pair_", nm, "_BM.csv"))
  # PD only (drop Destination column)
  readr::write_csv(tbl %>% filter(Destination == "PD") %>% select(-Destination),
                   paste0("pair_", nm, "_PD.csv"))
})

# ---- 6) Optional: pair summaries collapsed ACROSS Destination ----
summarize_pair_no_dest <- function(data, pair_vars) {
  grouping_vars <- pair_vars
  summ <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n = n(),
      mean_Pers = mean(Pers, na.rm = TRUE),
      sd_Pers   = sd(Pers,   na.rm = TRUE),
      mean_Info = mean(Info, na.rm = TRUE),
      sd_Info   = sd(Info,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(all_of(grouping_vars), as.character))
  
  combo_list <- setNames(rep(list(c("Low","High")), length(pair_vars)), pair_vars)
  all_combos <- as_tibble(do.call(expand.grid, c(combo_list, stringsAsFactors = FALSE)))
  all_combos %>% left_join(summ, by = grouping_vars) %>% arrange(across(all_of(pair_vars)))
}

# Example: combined across destination for IC x IS
# combined_IC_IS <- summarize_pair_no_dest(df2, c("IC","IS"))
# print(combined_IC_IS)
# readr::write_csv(combined_IC_IS, "pair_IC_IS_collapsedAcrossDestination.csv")

sink()
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

sink()
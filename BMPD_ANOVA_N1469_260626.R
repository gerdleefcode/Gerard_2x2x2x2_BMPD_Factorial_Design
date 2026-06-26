sink("Gerard_BMPD_ANOVA_N1469_260626.txt")
cat("\n-----------------------------------")
cat("\nPublisher: Lee Cheuk Man Gerard")
cat("\nPublication date: 26th June 2026")
cat("\n-----------------------------------\n")

# Required packages (install if missing)
packages <- c("dplyr", "readr", "car", "effectsize", "emmeans")
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)

library(dplyr)
library(readr)
library(car)         # for Type III ANOVA and leveneTest
library(effectsize)  # for eta squared (optional)
library(emmeans)     # for marginal means follow-ups (optional)

# ---- 1) Read data ----
file <- "260626_AI website_BM_PD_Cleaned data_N=1469.csv"
df <- read.csv(file, stringsAsFactors = FALSE)

# ---- 2) Standardize / rename columns ----
# If your CSV has exactly the column names you posted, try to rename them by name;
# otherwise we fallback to positional renaming (first 9 columns).
expected <- c("NoPerEachScenario","Scenario","DestinationTypeBMFunctional1PDHedonic0",
              "InformationCompletenessHigh1Low0","InformationSourceHigh1Low0",
              "AISelfRatingHigh1Low0","AIPublichRatingHigh1Low0",
              "MEAN_PerceivedPersuasiveness","MEAN_PerceivedInformativeness")

if(all(expected %in% names(df))) {
  names(df)[names(df) == "DestinationTypeBMFunctional1PDHedonic0"] <- "Destination"
  names(df)[names(df) == "InformationCompletenessHigh1Low0"] <- "IC"
  names(df)[names(df) == "InformationSourceHigh1Low0"] <- "IS"
  names(df)[names(df) == "AISelfRatingHigh1Low0"] <- "SR"   # Self Rating
  names(df)[names(df) == "AIPublichRatingHigh1Low0"] <- "PR" # Public Rating (note spelling in header)
  names(df)[names(df) == "MEAN_PerceivedPersuasiveness"] <- "Pers"
  names(df)[names(df) == "MEAN_PerceivedInformativeness"] <- "Info"
} else {
  # fallback: positional rename (assumes the same 9 columns in order)
  names(df)[1:9] <- c("NoPerEachScenario","Scenario","Destination","IC","IS","SR","PR","Pers","Info")
}

# ---- 3) Convert variables, set contrasts for Type-III ----
# Destination: 1 = BM, 0 = PD (based on the data you showed)
df$Destination <- factor(as.integer(df$Destination), levels = c(0,1), labels = c("PD","BM"))

# Convert the 0/1 manipulations into factors Low/High
df <- df %>%
  mutate(
    IC = factor(as.integer(IC), levels = c(0,1), labels = c("Low","High")),
    IS = factor(as.integer(IS), levels = c(0,1), labels = c("Low","High")),
    PR = factor(as.integer(PR), levels = c(0,1), labels = c("Low","High")),
    SR = factor(as.integer(SR), levels = c(0,1), labels = c("Low","High")),
    Pers = as.numeric(Pers),
    Info = as.numeric(Info)
  )

# Use sum-to-zero contrasts so Type III tests are meaningful
options(contrasts = c("contr.sum", "contr.poly"))

# Quick checks
table(df$Destination)
table(df$IC, df$IS)   # example cross-tab
summary(df$Pers); summary(df$Info)

# ---- 4) Helper function to run Type III ANOVA and outputs ----
run_4way_anova <- function(data, dv, label = "") {
  # dv: string name of dependent variable column (e.g. "Pers" or "Info")
  form <- as.formula(paste0(dv, " ~ IC * IS * PR * SR"))
  model <- aov(form, data = data)
  
  cat("\n\n=========================================\n")
  cat("Destination:", label, "  DV:", dv, "\n")
  cat("Formula:", deparse(form), "\n\n")
  
  # Type III ANOVA table
  anova3 <- car::Anova(model, type = "III")
  print(anova3)
  
  # Convert Anova output to data.frame and compute partial eta-squared
  anova_df <- as.data.frame(anova3)
  anova_df$Effect <- rownames(anova_df)
  # residual sum of squares (SSE)
  ss_resid <- sum(resid(model)^2)
  anova_df$PartialEtaSq <- anova_df$`Sum Sq` / (anova_df$`Sum Sq` + ss_resid)
  # display partial eta-squared column
  cat("\nPartial eta-squared (calculated from Type-III SS):\n")
  print(anova_df[, c("Effect","Df","Sum Sq","Mean Sq","F value","Pr(>F)","PartialEtaSq")], digits = 4)
  
  # Levene test for homogeneity (use dv by the 4-way combination)
  cat("\nLevene's test for homogeneity (grouping by IC:IS:PR:SR):\n")
  print(car::leveneTest(form, data = data))
  
  # Residual diagnostics plots
  op <- par(no.readonly = TRUE)
  par(mfrow = c(2,2))
  plot(model, main = paste(label, dv))
  par(op)
  
  # Return model + ANOVA table (invisibly)
  invisible(list(model = model, anova3 = anova3, anova_df = anova_df))
}

# ---- 5) Run the four ANOVAs ----
df_BM <- filter(df, Destination == "BM")
df_PD <- filter(df, Destination == "PD")

res_BM_Pers <- run_4way_anova(df_BM, "Pers", label = "BM")
res_BM_Info <- run_4way_anova(df_BM, "Info", label = "BM")
res_PD_Pers <- run_4way_anova(df_PD, "Pers", label = "PD")
res_PD_Info <- run_4way_anova(df_PD, "Info", label = "PD")

# ---- 6) (Optional) Follow-up: estimated marginal means for a significant effect / interaction ----
# Example: if you want the marginal means for IC x PR in the BM Pers model:
# library(emmeans)  # already loaded above
# em <- emmeans(res_BM_Pers$model, ~ IC * PR)
# print(em)
# pairs(em, adjust = "tukey")

# ---- 7) (Optional) Save ANOVA tables to CSV ----
write.csv(res_BM_Pers$anova_df, file = "ANOVA_BM_Pers_TypeIII.csv", row.names = FALSE)
write.csv(res_BM_Info$anova_df, file = "ANOVA_BM_Info_TypeIII.csv", row.names = FALSE)
write.csv(res_PD_Pers$anova_df, file = "ANOVA_PD_Pers_TypeIII.csv", row.names = FALSE)
write.csv(res_PD_Info$anova_df, file = "ANOVA_PD_Info_TypeIII.csv", row.names = FALSE)

cat("\nAll analyses finished. ANOVA CSVs written to working directory.\n")

sink()
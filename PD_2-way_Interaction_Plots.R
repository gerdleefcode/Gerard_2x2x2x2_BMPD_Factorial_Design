# Read the data
df <- read.csv("Subset.csv", stringsAsFactors = FALSE)

# Recode predictors as Low / High factors
df$InformationCompleteness <- factor(
  df$InformationCompletenessHigh1Low0,
  levels = c(0, 1),
  labels = c("Low", "High")
)

df$AISelfRating <- factor(
  df$AISelfRatingHigh1Low0,
  levels = c(0, 1),
  labels = c("Low", "High")
)

# -------------------------------
# Plot 1: Perceived Persuasiveness
# -------------------------------
png("Perceived_Persuasiveness_Interaction.png", width = 1200, height = 900, res = 150)

interaction.plot(
  x.factor = df$InformationCompleteness,
  trace.factor = df$AISelfRating,
  response = df$MEAN_PerceivedPersuasiveness,
  fun = mean,
  type = "b",
  pch = c(16, 17),
  lwd = 2,
  col = c("#1f77b4", "#d62728"),
  xlab = "Information Completeness",
  ylab = "Mean Perceived Persuasiveness",
  trace.label = "AI Self-Rating",
  main = "Interaction Plot:\nPerceived Persuasiveness"
)

dev.off()

# --------------------------------
# Plot 2: Perceived Informativeness
# --------------------------------
png("Perceived_Informativeness_Interaction.png", width = 1200, height = 900, res = 150)

interaction.plot(
  x.factor = df$InformationCompleteness,
  trace.factor = df$AISelfRating,
  response = df$MEAN_PerceivedInformativeness,
  fun = mean,
  type = "b",
  pch = c(16, 17),
  lwd = 2,
  col = c("#1f77b4", "#d62728"),
  xlab = "Information Completeness",
  ylab = "Mean Perceived Informativeness",
  trace.label = "AI Self-Rating",
  main = "Interaction Plot:\nPerceived Informativeness"
)

dev.off()
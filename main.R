# =============================================================================
# ELECTÓMETRO PERÚ — Survey Response Analysis
# =============================================================================
# Transforms raw survey responses into net consensus scores per topic,
# then produces demographic breakdowns by age group, gender, and macroregion.
#
# Scale convention (set by graphic designer): -1 to +1
#   -1 = Strongly Opposed | 0 = Neutral | +1 = Strongly In Favor
#
# Output: all plots saved to plots/ as 300 dpi PNG files.
#   Naming: q{number}_{breakdown}.png
#   e.g.  q01_age.png | q02_gender.png | q02_gender_x_age.png
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

dir.create("plots", showWarnings = FALSE)

# =============================================================================
# 1. DATA LOADING & CLEANING
# =============================================================================
df <- read.csv2("dataset.csv", sep = ",")

# --- Core demographic types ---
target_columns <- c("gender", "region", "education")
df[target_columns] <- lapply(df[target_columns], as.factor)
df$age <- as.numeric(df$age)

# --- Predicted demographic types ---
target_columns <- c("prediction_gender", "prediction_education")
df[target_columns] <- lapply(df[target_columns], as.factor)
df$prediction_age <- as.numeric(df$prediction_age)
df$prediction_age <- round(df$prediction_age, 0)

# --- Force all response columns (cols 5–44) to numeric ---
df[5:44] <- lapply(df[5:44], as.numeric)

# --- Fill core demographic NAs with predicted values where available ---
df$gender    <- as.factor(ifelse(is.na(as.character(df$gender)),
                                 as.character(df$prediction_gender),
                                 as.character(df$gender)))
df$education <- as.factor(ifelse(is.na(as.character(df$education)),
                                 as.character(df$prediction_education),
                                 as.character(df$education)))
df$age       <- ifelse(is.na(df$age), df$prediction_age, df$age)

# Re-cast after ifelse (ifelse strips factor levels)
df$gender    <- as.factor(df$gender)
df$education <- as.factor(df$education)
df$age       <- as.numeric(df$age)

original_length <- nrow(df)
cat("Number of rows:", nrow(df))

# --- Drop rows where ALL four demographic fields are simultaneously NA ---
demo_cols <- c("gender", "region", "education", "age")
df <- df[rowSums(is.na(df[demo_cols])) < length(demo_cols), ]

# --- Remove predicted/imputed columns ---
df <- df[, !colnames(df) %in% c(
  "age_imputed", "education_imputed", "gender_imputed",
  "prediction_age", "prediction_education", "prediction_gender"
)]

cat("Number of rows:", nrow(df), nrow(df)/original_length)

# =============================================================================
# 2. CONSENSUS SCORE CALCULATION
# =============================================================================
# Each topic has two raw response columns:
#   responses_tX_1 → stance:  0 (against) | 0.5 (neutral) | 1 (in favor)
#   responses_tX_2 → weight:  importance score (unused)
#
# Transformation applied: (stance × 2) − 1
#   Maps 0 → -1 | 0.5 → 0 | 1 → +1
#

for (i in 1:20) {
  pos_col <- paste0("responses_t", i, "_1")
  new_col <- paste0("topic_", i, "_weighted")
  df[[new_col]] <- (df[[pos_col]] * 2) - 1
}

# =============================================================================
# 3. DEMOGRAPHIC FEATURE ENGINEERING
# =============================================================================
# --- Age groups (ordered factor) ---
df <- df %>%
  mutate(age_group = case_when(
    age < 18              ~ "Under 18",
    age >= 18 & age <= 25 ~ "18-25",
    age >= 26 & age <= 35 ~ "26-35",
    age >= 36 & age <= 50 ~ "36-50",
    age >= 51 & age <= 65 ~ "51-65",
    age > 65              ~ "Over 65",
    TRUE                  ~ NA_character_
  )) %>%
  mutate(age_group = factor(age_group,
                            levels  = c("Under 18", "18-25", "26-35", "36-50", "51-65", "Over 65"),
                            ordered = TRUE
  ))

# --- Macroregions (ordered factor) ---
df <- df %>%
  mutate(macroregion = case_when(
    region %in% c("Lima", "Callao", "Lima Metropolitana") ~ "Lima Metropolitana",
    region %in% c("Lima Provincias") ~ "Lima Provincias",
    region %in% c("Extranjero") ~ "Extranjero",
    region %in% c("Amazonas", "Áncash", "Cajamarca", "La Libertad", "Lambayeque", "Piura", "San Martín", "Tumbes") ~ "Norte",
    region %in% c("Pasco", "Junín", "Huancavelica", "Ayacucho", "Ica", "Huánuco") ~ "Centro",
    region %in% c("Arequipa", "Apurímac", "Cusco", "Ica", "Moquegua", "Tacna", "Madre de Dios", "Puno") ~ "Sur",
    region %in% c("Loreto", "Ucayali", "Amazonas") ~ "Oriente",
    TRUE ~ NA_character_
  )) %>%
  mutate(
    macroregion = factor(
      macroregion,
      levels = c("Lima Metropolitana", "Lima Provincias", "Norte", "Centro", "Extranjero", "Sur", "Oriente"),
      ordered = TRUE
    )
  )

# =============================================================================
# 4. VERIFICATION
# =============================================================================

str(df)
summary(df)
head(df)
table(df$gender,   useNA = "ifany")
table(df$age_group,   useNA = "ifany")
table(df$macroregion, useNA = "ifany")

# -----------------------------------------------------------------------------
# OVERALL SUMMARY — Selected topics: Q3, Q5, Q7, Q16, Q18
#
# Computes the overall mean consensus score (across all respondents). 
# Printed to console for quick
# reference and exported to CSV for sharing or further use.
# -----------------------------------------------------------------------------

question_labels <- c(
  "topic_1_weighted"  = "Q1: Castigo de criminales",
  "topic_2_weighted"  = "Q2: Educación sexual integral",
  "topic_3_weighted"  = "Q3: Veto de por vida por corrupción",
  "topic_4_weighted"  = "Q4: Mecanismos para regularizar mineros informales",
  "topic_5_weighted"  = "Q5: Aborto en caso de violación",
  "topic_6_weighted"  = "Q6: Sistema tributario progresivo",
  "topic_7_weighted"  = "Q7: Pena de muerte",
  "topic_8_weighted"  = "Q8: Mayor presencia militar",
  "topic_9_weighted"  = "Q9: Fomento de salud privada",
  "topic_10_weighted" = "Q10: Deportación de personas con situación migratoria irregular",
  "topic_11_weighted" = "Q11: Veto de pueblos indigenas",
  "topic_12_weighted" = "Q12: Cambio climático como principal obligación",
  "topic_13_weighted" = "Q13: Inversion extranjera",
  "topic_14_weighted" = "Q14: Reducir gasto estatal",
  "topic_15_weighted" = "Q15: Fomento de las AFP",
  "topic_16_weighted" = "Q16: Reconocer autoridad de la CIDH",
  "topic_17_weighted" = "Q17: Reducir impuestos para reducir informalidad",
  "topic_18_weighted" = "Q18: Se necesita una constituyente",
  "topic_19_weighted" = "Q19: Reconocer matrimonio igualitario",
  "topic_20_weighted" = "Q20: Prohibición de la reelección parlamentaria"
)

# Compute overall means, pivot to long format, and attach labels
df_overall_summary <- df %>%
  summarize(
    topic_1_weighted  = mean(topic_1_weighted,  na.rm = TRUE),
    topic_2_weighted  = mean(topic_2_weighted,  na.rm = TRUE),
    topic_3_weighted  = mean(topic_3_weighted,  na.rm = TRUE),
    topic_5_weighted  = mean(topic_5_weighted,  na.rm = TRUE),
    topic_6_weighted  = mean(topic_6_weighted,  na.rm = TRUE),
    topic_7_weighted  = mean(topic_7_weighted,  na.rm = TRUE),
    topic_9_weighted  = mean(topic_9_weighted,  na.rm = TRUE),
    topic_10_weighted = mean(topic_10_weighted, na.rm = TRUE),
    topic_11_weighted = mean(topic_11_weighted, na.rm = TRUE),
    topic_13_weighted = mean(topic_13_weighted, na.rm = TRUE),
    topic_14_weighted = mean(topic_14_weighted, na.rm = TRUE),
    topic_15_weighted = mean(topic_15_weighted, na.rm = TRUE),
    topic_16_weighted = mean(topic_16_weighted, na.rm = TRUE),
    topic_17_weighted = mean(topic_17_weighted, na.rm = TRUE),
    topic_18_weighted = mean(topic_18_weighted, na.rm = TRUE),
    topic_19_weighted = mean(topic_19_weighted, na.rm = TRUE),
    topic_20_weighted = mean(topic_20_weighted, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question_ID",
    values_to = "mean_consensus"
  ) %>%
  mutate(
    Question_Text = unname(question_labels[Question_ID])
  )

# Print to console for quick inspection
print(df_overall_summary)

# Export to CSV for sharing or use in external tools
write.csv(df_overall_summary, "plots/overall_summary.csv",
          row.names = FALSE)

# =============================================================================
# 5. PLOT FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# Core builder — single breakdown
# -----------------------------------------------------------------------------
plot_single <- function(data, topic_col, group_col, title, subtitle) {
  df_summary <- data %>%
    filter(!is.na(.data[[group_col]]), !is.na(.data[[topic_col]])) %>%
    group_by(.data[[group_col]]) %>%
    summarize(
      mean_consensus = mean(.data[[topic_col]]),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(mean_consensus)
  
  ggplot(df_summary, aes(
    x    = reorder(.data[[group_col]], mean_consensus),
    y    = mean_consensus,
    fill = mean_consensus > 0
  )) +
    geom_bar(stat = "identity", width = 0.65, color = "white") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_text(aes(
      label = sprintf("%.2f (n=%d)", mean_consensus, n),
      hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
    ), fontface = "bold", size = 4, color = "black") +
    scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
    scale_y_continuous(limits = c(-1.20, 1.20)) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      axis.text.y      = element_text(face = "bold", color = "black", size = 10),
      axis.title.x     = element_text(margin = margin(t = 10))
    ) +
    labs(
      title    = title,
      subtitle = paste0(subtitle, "\nN total = ", nrow(data %>% filter(!is.na(.data[[group_col]]), !is.na(.data[[topic_col]])))),
      caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
      x        = NULL,
      y        = "Puntuación de Consenso Neto"
    )
}

# -----------------------------------------------------------------------------
# Core builder — faceted intersection (two demographic dimensions)
# -----------------------------------------------------------------------------
plot_faceted <- function(data, topic_col, x_col, facet_col, title, subtitle) {
  df_filtered <- data %>%
    filter(
      !is.na(.data[[x_col]]),
      !is.na(.data[[facet_col]]),
      !is.na(.data[[topic_col]])
    )
  
  df_summary <- df_filtered %>%
    group_by(.data[[facet_col]], .data[[x_col]]) %>%
    summarize(
      mean_consensus = mean(.data[[topic_col]]),
      n = n(),
      .groups = "drop"
    )
  
  ggplot(df_summary, aes(
    x    = .data[[x_col]],
    y    = mean_consensus,
    fill = mean_consensus > 0
  )) +
    geom_bar(stat = "identity", width = 0.7, color = "white") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    geom_text(aes(
      label = sprintf("%.2f (n=%d)", mean_consensus, n),
      hjust = ifelse(mean_consensus >= 0, -0.15, 1.15)
    ), fontface = "bold", size = 3.5) +
    scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
    scale_y_continuous(limits = c(-1.30, 1.30)) +
    facet_wrap(~ .data[[facet_col]], ncol = 2) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    theme(
      strip.text       = element_text(face = "bold", size = 11),
      panel.spacing    = unit(1.5, "lines"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13)
    ) +
    labs(
      title    = title,
      subtitle = paste0(subtitle, "\nN total = ", nrow(df_filtered)),
      caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
      x        = NULL,
      y        = "Puntuación de Consenso Neto"
    )
}

# =============================================================================
# 6. PLOT GENERATION — all questions × all breakdowns
# =============================================================================

questions <- list(
  "01" = list(col = "topic_1_weighted",  text = "Q1: El estado debería priorizar el castigo de criminales en vez de su reintegración en la sociedad."),
  "02" = list(col = "topic_2_weighted",  text = "Q2: La educación sexual integral con enfoque de género debería ser una parte obligatoria del currículo escolar."),
  "03" = list(col = "topic_3_weighted",  text = "Q3: Los funcionarios condenados por corrupción deberían recibir un veto de por vida para ejercer cargos públicos."),
  "04" = list(col = "topic_4_weighted",  text = "Q4: El Estado debería desarrollar mecanismos más activos para regularizar a mineros informales."),
  "05" = list(col = "topic_5_weighted",  text = "Q5: El aborto debería estar permitido en casos de violación."),
  "06" = list(col = "topic_6_weighted",  text = "Q6: Las personas con más dinero deberían pagar un mayor porcentaje de impuestos sobre sus ingresos."),
  "07" = list(col = "topic_7_weighted",  text = "Q7: La pena de muerte debería aplicarse en el Perú."),
  "08" = list(col = "topic_8_weighted",  text = "Q8: El Estado debería aumentar la presencia militar en zonas de conflicto interno."),
  "09" = list(col = "topic_9_weighted",  text = "Q9: El Estado debería fomentar la salud privada en vez de priorizar la pública."),
  "10" = list(col = "topic_10_weighted", text = "Q10: Las personas con situación migratoria irregular deberían ser deportadas."),
  "11" = list(col = "topic_11_weighted", text = "Q11: Los pueblos indígenas deberían poder vetar proyectos extractivos en sus tierras."),
  "12" = list(col = "topic_12_weighted", text = "Q12: Tomar medidas contra el cambio climático debe ser una obligación principal del Estado."),
  "13" = list(col = "topic_13_weighted", text = "Q13: La inversión extranjera es necesaria para el desarrollo económico del Perú."),
  "14" = list(col = "topic_14_weighted", text = "Q14: El Estado debería reducir el gasto público para equilibrar el presupuesto nacional."),
  "15" = list(col = "topic_15_weighted", text = "Q15: El Estado debería fomentar el sistema de pensiones privadas en vez de priorizar las públicas."),
  "16" = list(col = "topic_16_weighted", text = "Q16: El Perú debería reconocer la autoridad de la Corte Interamericana de Derechos Humanos (CIDH)."),
  "17" = list(col = "topic_17_weighted", text = "Q17: Reducir impuestos a las empresas es la mejor forma de reducir la informalidad laboral."),
  "18" = list(col = "topic_18_weighted", text = "Q18: Debería convocarse a una Asamblea Constituyente para cambiar la Constitución peruana."),
  "19" = list(col = "topic_19_weighted", text = "Q19: A las parejas homosexuales se les debería reconocer el derecho al matrimonio igualitario."),
  "20" = list(col = "topic_20_weighted", text = "Q20: Debería prohibirse la reelección de congresistas.")
)

# Single breakdown specs: suffix → column name
single_breakdowns <- list(
  age        = "age_group",
  gender     = "gender",
  education  = "education",
  macroregion = "macroregion"
)

# Faceted breakdown specs: suffix → c(x_col, facet_col)
# Convention: x_col goes on the y-axis (after coord_flip), facet_col panels
faceted_breakdowns <- list(
  gender_x_age        = c(x_col = "gender",    facet_col = "age_group"),
  gender_x_macroregion = c(x_col = "gender",   facet_col = "macroregion"),
  gender_x_education  = c(x_col = "gender",    facet_col = "education"),
  age_x_macroregion   = c(x_col = "age_group", facet_col = "macroregion"),
  age_x_education     = c(x_col = "age_group", facet_col = "education"),
  education_x_macroregion = c(x_col = "education", facet_col = "macroregion")
)

# --- Generate and save all plots ---
for (q_num in names(questions)) {
  q      <- questions[[q_num]]
  col    <- q$col
  q_text <- q$text
  
  # Single breakdowns
  for (suffix in names(single_breakdowns)) {
    group_col <- single_breakdowns[[suffix]]
    p <- plot_single(
      data      = df,
      topic_col = col,
      group_col = group_col,
      title     = paste0("Q", q_num, ": Consenso Neto por ", group_col),
      subtitle  = q_text
    )
    ggsave(
      paste0("plots/q", q_num, "_", suffix, ".png"),
      plot = p, width = 10, height = 6, dpi = 300, bg = "white"
    )
  }
  
  # Faceted intersections
  for (suffix in names(faceted_breakdowns)) {
    dims <- faceted_breakdowns[[suffix]]
    p <- plot_faceted(
      data      = df,
      topic_col = col,
      x_col     = dims["x_col"],
      facet_col = dims["facet_col"],
      title     = paste0("Q", q_num, ": Intersección ", suffix),
      subtitle  = q_text
    )
    ggsave(
      paste0("plots/q", q_num, "_", suffix, ".png"),
      plot = p, width = 10, height = 9, dpi = 300, bg = "white"
    )
  }
}

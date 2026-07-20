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

# --- Drop rows where ALL four demographic fields are simultaneously NA ---
demo_cols <- c("gender", "region", "education", "age")
df <- df[rowSums(is.na(df[demo_cols])) < length(demo_cols), ]

# --- Force all response columns (cols 5–44) to numeric ---
df[5:44] <- lapply(df[5:44], as.numeric)

# --- Remove imputed/prediction columns added upstream ---
df <- df[, !colnames(df) %in% c(
  "prediction_age",       "age_imputed",
  "prediction_education", "education_imputed",
  "prediction_gender",    "gender_imputed"
)]

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
    region %in% c("Lima", "Callao") ~ "Lima Metropolitana",
    region %in% c("Extranjero") ~ "Extranjero",
    region %in% c("Amazonas", "Áncash", "Cajamarca", "La Libertad",
                  "Lambayeque", "Piura", "San Martín", "Tumbes") ~ "Norte",
    region %in% c("Pasco", "Junín", "Huancavelica", "Ayacucho", "Ica") ~ "Centro",
    region %in% c("Arequipa", "Apurímac", "Cusco", "Ica", "Moquegua",
                  "Tacna", "Madre de Dios", "Puno") ~ "Sur",
    region %in% c("Loreto", "Ucayali", "Amazonas") ~ "Oriente",
    TRUE ~ NA_character_
  )) %>%
  mutate(
    macroregion = factor(
      macroregion,
      levels = c("Lima Metropolitana", "Norte", "Centro",
                 "Extranjero", "Sur", "Oriente"),
      ordered = TRUE
    )
  )
# --- Remove respondents based outside Peru ---
# Any region value that didn't match a known Peruvian region is NA after the
# case_when above. Dropping those NA macroregion rows excludes foreign entries.
df <- df %>% filter(!is.na(macroregion))
print("N = 14426 (from 24851, 58,05%)")

# =============================================================================
# 4. VERIFICATION
# =============================================================================

str(df)
summary(df)
head(df)
table(df$gender,   useNA = "ifany")
table(df$age_group,   useNA = "ifany")
table(df$macroregion, useNA = "ifany")

# =============================================================================
# 5. GRAPHS
# =============================================================================
# Each plot is assigned to a named object, then saved immediately via ggsave().
# Export settings: 300 dpi PNG, 10 × 6 in (standard single breakdown).
#                  10 × 9 in for faceted Gender × Age Group plots.
# =============================================================================

# -----------------------------------------------------------------------------
# Q1 — El estado debería priorizar el castigo de criminales en vez de su
#        reintegración en la sociedad.
# Breakdown: Age Group
# -----------------------------------------------------------------------------

df_q1_age <- df %>%
  filter(!is.na(age_group), !is.na(topic_1_weighted)) %>%
  group_by(age_group) %>%
  summarize(mean_consensus = mean(topic_1_weighted))

p_q01_age <- ggplot(df_q1_age, aes(x = age_group, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
  ), color = "black", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
                    labels = c("TRUE" = "A Favor", "FALSE" = "En Contra")) +
  scale_y_continuous(limits = c(-1.15, 1.15)) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 16),
    axis.title.y     = element_text(margin = margin(r = 10))
  ) +
  labs(
    title    = "Q1: Consenso Neto por Grupo de Edad",
    subtitle = "El estado debería priorizar el castigo de criminales en vez de su reintegración en la sociedad.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Grupo de Edad",
    y        = "Puntuación de Consenso Neto",
    fill     = "Dirección de Postura"
  )

# p_q01_age
ggsave("plots/q01_age.png", plot = p_q01_age,
       width = 10, height = 6, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# Q2 — La educación sexual integral con enfoque de género debería ser una
#        parte obligatoria del currículo escolar.
# Breakdowns: (a) Gender only | (b) Gender × Age Group
# -----------------------------------------------------------------------------

# (a) By gender
df_q2_gender <- df %>%
  filter(!is.na(gender), !is.na(topic_2_weighted)) %>%
  group_by(gender) %>%
  summarize(mean_consensus = mean(topic_2_weighted))

p_q02_gender <- ggplot(df_q2_gender, aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
  ), fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.15, 1.15)) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Q2: Consenso Neto por Identidad de Género",
    subtitle = "La educación sexual integral con enfoque de género debería ser una parte obligatoria del currículo escolar.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q02_gender
ggsave("plots/q02_gender.png", plot = p_q02_gender,
       width = 10, height = 6, dpi = 300, bg = "white")

# (b) Gender × Age Group intersection
df_q2_combo <- df %>%
  filter(!is.na(gender), !is.na(age_group), !is.na(topic_2_weighted)) %>%
  group_by(age_group, gender) %>%
  summarize(mean_consensus = mean(topic_2_weighted), .groups = "drop")

p_q02_gender_x_age <- ggplot(df_q2_combo, aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.15, 1.15)
  ), fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.30, 1.30)) +
  facet_wrap(~age_group, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    panel.spacing    = unit(1.5, "lines"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Q2: Intersección Género × Grupo de Edad",
    subtitle = "La educación sexual integral con enfoque de género debería ser una parte obligatoria del currículo escolar.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q02_gender_x_age
ggsave("plots/q02_gender_x_age.png", plot = p_q02_gender_x_age,
       width = 10, height = 9, dpi = 300, bg = "white")

# (c) Gender × Macroregion intersection
df_q2_combo <- df %>%
  filter(!is.na(gender), !is.na(macroregion), !is.na(topic_2_weighted)) %>%
  group_by(macroregion, gender) %>%
  summarize(mean_consensus = mean(topic_2_weighted), .groups = "drop")

p_q02_gender_x_macroregion <- ggplot(
  df_q2_combo,
  aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)
) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.15, 1.15)
  ),
  fontface = "bold",
  size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(-1.30, 1.30)) +
  facet_wrap(~macroregion, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    panel.spacing    = unit(1.5, "lines"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Q2: Intersección Género × Macroregión",
    subtitle = "La educación sexual integral con enfoque de género debería ser una parte obligatoria del currículo escolar.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )
# p_q02_gender_x_macroregion
ggsave(
  "plots/q02_gender_x_macroregion.png",
  plot = p_q02_gender_x_macroregion,
  width = 10,
  height = 9,
  dpi = 300,
  bg = "white"
)

# -----------------------------------------------------------------------------
# OVERALL SUMMARY — Selected topics: Q3, Q5, Q7, Q16, Q18
#
# Computes the overall mean consensus score (across all respondents) for a
# curated set of politically salient questions. Printed to console for quick
# reference and exported to CSV for sharing or further use.
#
#   Q3:  Veto de por vida por corrupción
#   Q5:  Aborto en caso de violación
#   Q7:  Pena de muerte en el Perú
#   Q16: Reconocer autoridad de la CIDH
#   Q18: Se necesita una constituyente
# -----------------------------------------------------------------------------

# Human-readable labels keyed by column name
question_labels <- c(
  "topic_3_weighted"  = "Q3: Veto de por vida por corrupción",
  "topic_5_weighted"  = "Q5: Aborto en caso de violación",
  "topic_7_weighted"  = "Q7: Pena de muerte en el Perú",
  "topic_10_weighted" = "Q10: Deportación de personas con situación migratoria irregular",
  "topic_16_weighted" = "Q16: Reconocer autoridad de la CIDH",
  "topic_18_weighted" = "Q18: Se necesita una constituyente"
)

# Compute overall means, pivot to long format, and attach labels
df_overall_summary <- df %>%
  summarize(
    topic_3_weighted  = mean(topic_3_weighted,  na.rm = TRUE),
    topic_5_weighted  = mean(topic_5_weighted,  na.rm = TRUE),
    topic_7_weighted  = mean(topic_7_weighted,  na.rm = TRUE),
    topic_10_weighted = mean(topic_10_weighted, na.rm = TRUE),
    topic_16_weighted = mean(topic_16_weighted, na.rm = TRUE),
    topic_18_weighted = mean(topic_18_weighted, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Question_ID", values_to = "mean_consensus") %>%
  mutate(Question_Text = question_labels[Question_ID])

# Print to console for quick inspection
print(df_overall_summary)

# Export to CSV for sharing or use in external tools
write.csv(df_overall_summary, "plots/overall_summary_q3_q5_q7_q10_q16_q18.csv",
          row.names = FALSE)


# -----------------------------------------------------------------------------
# Q4 — El Estado debería desarrollar mecanismos más activos para regularizar
#        a mineros informales.
# Breakdown: Macroregion
# -----------------------------------------------------------------------------

df_q4_macro <- df %>%
  filter(!is.na(macroregion), !is.na(topic_4_weighted)) %>%
  group_by(macroregion) %>%
  summarize(mean_consensus = mean(topic_4_weighted)) %>%
  arrange(mean_consensus)

p_q04_macroregion <- ggplot(df_q4_macro, aes(x = reorder(macroregion, mean_consensus), y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.65, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
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
    title    = "Q4: Consenso Neto por Macroregión",
    subtitle = "El Estado debería desarrollar mecanismos más activos para regularizar a mineros informales.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Macroregión",
    y        = "Puntuación de Consenso Neto"
  )

# p_q04_macroregion
ggsave("plots/q04_macroregion.png", plot = p_q04_macroregion,
       width = 10, height = 6, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# Q5 — El aborto debería estar permitido en casos de violación.
# Breakdowns: (a) Gender only | (b) Gender × Age Group
# -----------------------------------------------------------------------------

# (a) By gender
df_q5_gender <- df %>%
  filter(!is.na(gender), !is.na(topic_5_weighted)) %>%
  group_by(gender) %>%
  summarize(mean_consensus = mean(topic_5_weighted))

p_q05_gender <- ggplot(df_q5_gender, aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
  ), fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.15, 1.15)) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Q5: El aborto debería estar permitido en casos de violación",
    subtitle = "Consenso Neto por Género (Escala: -1 a +1)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q05_gender
ggsave("plots/q05_gender.png", plot = p_q05_gender,
       width = 10, height = 6, dpi = 300, bg = "white")

# (b) Gender × Age Group intersection
df_q5_combo <- df %>%
  filter(!is.na(gender), !is.na(age_group), !is.na(topic_5_weighted)) %>%
  group_by(age_group, gender) %>%
  summarize(mean_consensus = mean(topic_5_weighted), .groups = "drop")

p_q05_gender_x_age <- ggplot(df_q5_combo, aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.15, 1.15)
  ), fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.30, 1.30)) +
  facet_wrap(~age_group, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    panel.spacing    = unit(1.5, "lines"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Q5: El aborto debería estar permitido en casos de violación",
    subtitle = "Intersección: Género × Grupo de Edad (Escala: -1 a +1)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q05_gender_x_age
ggsave("plots/q05_gender_x_age.png", plot = p_q05_gender_x_age,
       width = 10, height = 9, dpi = 300, bg = "white")

# (c) Gender × Macroregion intersection
df_q5_combo <- df %>%
  filter(!is.na(gender), !is.na(macroregion), !is.na(topic_5_weighted)) %>%
  group_by(macroregion, gender) %>%
  summarize(mean_consensus = mean(topic_5_weighted), .groups = "drop")

p_q05_gender_x_macroregion <- ggplot(
  df_q5_combo,
  aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)
) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.15, 1.15)
  ),
  fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.30, 1.30)) +
  facet_wrap(~macroregion, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    panel.spacing    = unit(1.5, "lines"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Q5: El aborto debería estar permitido en casos de violación",
    subtitle = "Intersección: Género × Macroregión (Escala: -1 a +1)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q05_gender_x_macroregion

ggsave(
  "plots/q05_gender_x_macroregion.png",
  plot = p_q05_gender_x_macroregion,
  width = 10, height = 9, dpi = 300, bg = "white"
)

# -----------------------------------------------------------------------------
# Q6 — Las personas con más dinero deberían pagar un mayor porcentaje de
#        impuestos sobre sus ingresos.
# Breakdown: Macroregion
# -----------------------------------------------------------------------------

df_q6_macro <- df %>%
  filter(!is.na(macroregion), !is.na(topic_6_weighted)) %>%
  group_by(macroregion) %>%
  summarize(mean_consensus = mean(topic_6_weighted)) %>%
  arrange(mean_consensus)

p_q06_macroregion <- ggplot(df_q6_macro, aes(x = reorder(macroregion, mean_consensus), y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.65, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
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
    title    = "Q6: Las personas con más dinero deberían pagar un mayor porcentaje de impuestos",
    subtitle = "Consenso Neto por Macroregión (Escala: -1 a +1)",
    x        = "Macroregión",
    y        = "Puntuación de Consenso Neto"
  )

# p_q06_macroregion
ggsave("plots/q06_macroregion.png", plot = p_q06_macroregion,
       width = 10, height = 6, dpi = 300, bg = "white")

# (b) By education level
df_q6_education <- df %>%
  filter(!is.na(education), !is.na(topic_6_weighted)) %>%
  group_by(education) %>%
  summarize(mean_consensus = mean(topic_6_weighted)) %>%
  arrange(mean_consensus)

p_q06_education <- ggplot(df_q6_education, aes(x = reorder(education, mean_consensus), y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.65, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
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
    title    = "Q6: Consenso Neto por Nivel Educativo",
    subtitle = "Las personas con más dinero deberían pagar un mayor porcentaje de impuestos sobre sus ingresos.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Nivel Educativo",
    y        = "Puntuación de Consenso Neto"
  )

# p_q06_education
ggsave("plots/q06_education.png", plot = p_q06_education,
       width = 10, height = 6, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# Q11, Q12, Q13, Q18 — Batch macroregion plots (loop)
#
#   Q11: Los pueblos indígenas deberían poder vetar proyectos extractivos en sus tierras
#   Q12: Tomar medidas contra el cambio climático debe ser una obligación principal
#   Q13: La inversión extranjera es necesaria para el desarrollo económico
#   Q18: Debería convocarse a una Asamblea Constituyente para cambiar la Constitución
# -----------------------------------------------------------------------------

target_batch <- list(
  "topic_11_weighted" = "Q11: Los pueblos indígenas deberían poder vetar proyectos extractivos en sus tierras",
  "topic_12_weighted" = "Q12: Tomar medidas contra el cambio climático debe ser una obligación principal",
  "topic_13_weighted" = "Q13: La inversión extranjera es necesaria para el desarrollo económico",
  "topic_18_weighted" = "Q18: Debería convocarse a una Asamblea Constituyente para cambiar la Constitución"
)

my_plots <- list()

for (col_name in names(target_batch)) {
  
  df_loop_summary <- df %>%
    filter(!is.na(macroregion), !is.na(.data[[col_name]])) %>%
    group_by(macroregion) %>%
    summarize(mean_consensus = mean(.data[[col_name]])) %>%
    arrange(mean_consensus)
  
  p <- ggplot(df_loop_summary, aes(x = reorder(macroregion, mean_consensus), y = mean_consensus, fill = mean_consensus > 0)) +
    geom_bar(stat = "identity", width = 0.65, color = "white") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_text(aes(
      label = sprintf("%.2f", mean_consensus),
      hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
    ), fontface = "bold", size = 4, color = "black") +
    scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
    scale_y_continuous(limits = c(-1.20, 1.20)) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 12),
      axis.text.y      = element_text(face = "bold", color = "black", size = 10),
      axis.title.x     = element_text(margin = margin(t = 10))
    ) +
    labs(
      title    = target_batch[[col_name]],
      subtitle = "Consenso Neto por Macroregión (Escala: -1 a +1)",
      x        = "Macroregión",
      y        = "Puntuación de Consenso Neto"
    )
  
  # Store with clean key name, e.g. "topic_11_weighted" → "q11"
  clean_name <- gsub("topic_([0-9]+)_weighted", "q\\1", col_name)
  my_plots[[clean_name]] <- p
  
  # Save immediately after building
  ggsave(paste0("plots/", clean_name, "_macroregion.png"), plot = p,
         width = 10, height = 6, dpi = 300, bg = "white")
}

# Render batch plots
# my_plots$q11
# my_plots$q12
# my_plots$q13
# my_plots$q18

# -----------------------------------------------------------------------------
# Q3 + Q11 — Additional education breakdowns
#
#   Q3:  Veto de por vida a funcionarios condenados por corrupción
#   Q11: Los pueblos indígenas deberían poder vetar proyectos extractivos en sus tierras
# -----------------------------------------------------------------------------

# Reusable helper to build an education breakdown plot
make_education_plot <- function(data, col, title_text, subtitle_text) {
  df_edu <- data %>%
    filter(!is.na(education), !is.na(.data[[col]])) %>%
    group_by(education) %>%
    summarize(mean_consensus = mean(.data[[col]])) %>%
    arrange(mean_consensus)
  
  ggplot(df_edu, aes(x = reorder(education, mean_consensus), y = mean_consensus, fill = mean_consensus > 0)) +
    geom_bar(stat = "identity", width = 0.65, color = "white") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_text(aes(
      label = sprintf("%.2f", mean_consensus),
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
      title    = title_text,
      subtitle = subtitle_text,
      caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
      x        = "Nivel Educativo",
      y        = "Puntuación de Consenso Neto"
    )
}

p_q03_education <- make_education_plot(
  df,
  col           = "topic_3_weighted",
  title_text    = "Q3: Consenso Neto por Nivel Educativo",
  subtitle_text = "Los funcionarios condenados por corrupción deberían recibir un veto de por vida para ejercer cargos públicos."
)

# p_q03_education
ggsave("plots/q03_education.png", plot = p_q03_education,
       width = 10, height = 6, dpi = 300, bg = "white")

p_q11_education <- make_education_plot(
  df,
  col           = "topic_11_weighted",
  title_text    = "Q11: Consenso Neto por Nivel Educativo",
  subtitle_text = "Los pueblos indígenas deberían poder vetar proyectos extractivos en sus tierras."
)

# p_q11_education
ggsave("plots/q11_education.png", plot = p_q11_education,
       width = 10, height = 6, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# Q15 — El Estado debería fomentar el sistema de pensiones privadas en vez
#         de priorizar las públicas.
# Breakdown: Age Group
# -----------------------------------------------------------------------------

df_q15_age <- df %>%
  filter(!is.na(age_group), !is.na(topic_15_weighted)) %>%
  group_by(age_group) %>%
  summarize(mean_consensus = mean(topic_15_weighted))

p_q15_age <- ggplot(df_q15_age, aes(x = age_group, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
  ), color = "black", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.15, 1.15)) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    axis.text.y      = element_text(face = "bold", color = "black", size = 11),
    axis.title.x     = element_text(margin = margin(t = 10))
  ) +
  labs(
    title    = "Q15: El Estado debería fomentar el sistema de pensiones privadas",
    subtitle = "Consenso Neto por Grupo de Edad (Escala: -1 a +1)",
    x        = "Grupo de Edad",
    y        = "Puntuación de Consenso Neto"
  )

# p_q15_age
ggsave("plots/q15_age.png", plot = p_q15_age,
       width = 10, height = 6, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# Q19 — Así como a las parejas heterosexuales, a las parejas homosexuales se
#         les debería reconocer el derecho al matrimonio igualitario.
# Breakdowns: (a) Gender | (b) Gender × Age Group | (c) Education
# -----------------------------------------------------------------------------

# (a) By gender
df_q19_gender <- df %>%
  filter(!is.na(gender), !is.na(topic_19_weighted)) %>%
  group_by(gender) %>%
  summarize(mean_consensus = mean(topic_19_weighted))

p_q19_gender <- ggplot(df_q19_gender, aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
  ), fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.15, 1.15)) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    axis.text.y      = element_text(face = "bold", color = "black", size = 11),
    axis.title.x     = element_text(margin = margin(t = 10))
  ) +
  labs(
    title    = "Q19: Consenso Neto por Identidad de Género",
    subtitle = "A las parejas homosexuales se les debería reconocer el derecho al matrimonio igualitario.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q19_gender
ggsave("plots/q19_gender.png", plot = p_q19_gender,
       width = 10, height = 6, dpi = 300, bg = "white")

# (b) Gender × Age Group intersection
df_q19_gender_x_age <- df %>%
  filter(!is.na(gender), !is.na(age_group), !is.na(topic_19_weighted)) %>%
  group_by(age_group, gender) %>%
  summarize(mean_consensus = mean(topic_19_weighted), .groups = "drop")

p_q19_gender_x_age <- ggplot(df_q19_gender_x_age, aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.15, 1.15)
  ), fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), guide = "none") +
  scale_y_continuous(limits = c(-1.30, 1.30)) +
  facet_wrap(~age_group, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    panel.spacing    = unit(1.5, "lines"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Q19: Intersección Género × Grupo de Edad",
    subtitle = "A las parejas homosexuales se les debería reconocer el derecho al matrimonio igualitario.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q19_gender_x_age
ggsave("plots/q19_gender_x_age.png", plot = p_q19_gender_x_age,
       width = 10, height = 9, dpi = 300, bg = "white")

# (c) By education level
df_q19_education <- df %>%
  filter(!is.na(education), !is.na(topic_19_weighted)) %>%
  group_by(education) %>%
  summarize(mean_consensus = mean(topic_19_weighted)) %>%
  arrange(mean_consensus)

p_q19_education <- ggplot(df_q19_education, aes(x = reorder(education, mean_consensus), y = mean_consensus, fill = mean_consensus > 0)) +
  geom_bar(stat = "identity", width = 0.65, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
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
    title    = "Q19: Consenso Neto por Nivel Educativo",
    subtitle = "A las parejas homosexuales se les debería reconocer el derecho al matrimonio igualitario.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Nivel Educativo",
    y        = "Puntuación de Consenso Neto"
  )

# p_q19_education
ggsave("plots/q19_education.png", plot = p_q19_education,
       width = 10, height = 6, dpi = 300, bg = "white")

# (d) By macroregion
df_q19_macroregion <- df %>%
  filter(!is.na(macroregion), !is.na(topic_19_weighted)) %>%
  group_by(macroregion) %>%
  summarize(mean_consensus = mean(topic_19_weighted), .groups = "drop")

p_q19_macroregion <- ggplot(
  df_q19_macroregion,
  aes(x = macroregion, y = mean_consensus, fill = mean_consensus > 0)
) +
  geom_bar(stat = "identity", width = 0.65, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.2, 1.2)
  ),
  fontface = "bold",
  size = 4,
  color = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    guide = "none"
  ) +
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
    title    = "Q19: Consenso Neto por Macroregión",
    subtitle = "A las parejas homosexuales se les debería reconocer el derecho al matrimonio igualitario.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Macroregión",
    y        = "Puntuación de Consenso Neto"
  )

# p_q19_macroregion

ggsave(
  "plots/q19_macroregion.png",
  plot = p_q19_macroregion,
  width = 10, height = 6, dpi = 300, bg = "white"
)

# (e) Gender × Macroregion intersection
df_q19_gender_x_macroregion <- df %>%
  filter(!is.na(gender), !is.na(macroregion), !is.na(topic_19_weighted)) %>%
  group_by(macroregion, gender) %>%
  summarize(mean_consensus = mean(topic_19_weighted), .groups = "drop")

p_q19_gender_x_macroregion <- ggplot(
  df_q19_gender_x_macroregion,
  aes(x = gender, y = mean_consensus, fill = mean_consensus > 0)
) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_text(aes(
    label = sprintf("%.2f", mean_consensus),
    hjust = ifelse(mean_consensus >= 0, -0.15, 1.15)
  ),
  fontface = "bold",
  size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(-1.30, 1.30)) +
  facet_wrap(~macroregion, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    panel.spacing    = unit(1.5, "lines"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Q19: Intersección Género × Macroregión",
    subtitle = "A las parejas homosexuales se les debería reconocer el derecho al matrimonio igualitario.",
    caption  = "Escala: -1 (Fuertemente en Contra) a +1 (Fuertemente a Favor)",
    x        = "Identidad de Género",
    y        = "Puntuación de Consenso Neto"
  )

# p_q19_gender_x_macroregion

ggsave(
  "plots/q19_gender_x_macroregion.png",
  plot = p_q19_gender_x_macroregion,
  width = 10, height = 9, dpi = 300, bg = "white"
)


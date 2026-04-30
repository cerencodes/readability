# *****************************************************************************
# LOAD PACKAGES  ----
# *****************************************************************************
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(stringi)
library(data.table)
library(textstem)
library(visreg)
library(modelsummary)
library(flextable)
library(officer)

# Prefer a dedicated data folder when present, but keep root-level fallback
# so the script remains portable across old and new repo layouts.
data_dir <- if (dir.exists("data")) "data" else "."
data_file <- function(filename) file.path(data_dir, filename)
# *****************************************************************************
# IMPORT DATA: Chatbot Responses ----
# *****************************************************************************

#Import dataset containing questions
conversations <- read.csv(data_file("sample_qa.csv"))

#Import Prompt datasets
df_baseline_p0p1 <- read_csv(data_file("20260324_temp0_topk5_all_p0_p1.csv"))
df_baseline_p0p1$prompt_id[df_baseline_p0p1$prompt_id == "A"] <- "0"
df_baseline_p0p1$prompt_id[df_baseline_p0p1$prompt_id == "B"] <- "1"

df_baseline_p2p3 <- read_csv(data_file("20260324_temp0_topk5_all_p2_p3.csv"))
df_baseline_p2p3$prompt_id[df_baseline_p2p3$prompt_id == "A"] <- "2"
df_baseline_p2p3$prompt_id[df_baseline_p2p3$prompt_id == "B"] <- "3"


#Combine the datasets
df_1 <- bind_rows(
  df_baseline_p0p1,
  df_baseline_p2p3
)

#Join sample questions to the datasets 
df_1 <- df_1 %>%
  left_join(
    conversations %>% select(question_id, question),
    by = c("conversation_id" = "question_id")
  ) %>%
  rename(question_id = conversation_id) %>%
  rename(context = question) %>%
  relocate(context, .before = 1) %>%
  mutate(row_id = row_number(), .before = 1) 


#Filter final dataset for only chatbot agent responses
df_assistant <- df_1 |>
  filter(role == "assistant") |>
  transmute(row_id = as.factor(row_id), 
            question_id = as.factor(question_id), 
            prompt_id = as.factor(prompt_id), 
            llm = as.factor(llm), 
            context, 
            content)
# *****************************************************************************
# DATA CLEANING ----
# *****************************************************************************

# Create Helper: clean text for sentence counting + word counting
clean_for_metrics <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_character_)
  
  x <- text
  
  # Remove markdown bold markers
  x <- stringr::str_replace_all(x, stringr::fixed("**"), "")
  
  # Normalize newlines
  x <- stringr::str_replace_all(x, "\r\n?", "\n")
  
  # Protect decimals/version dots (V2.1, 33.5)
  x <- stringr::str_replace_all(x, "(?<=\\d)\\.(?=\\d)", "<DOT>")
  
  # --- KEY FIX ---
  # If "Recommended Product: <product> Reason:" occurs on one line,
  # force a sentence break after the product name (but keep the colon).
  x <- stringr::str_replace(
    x,
    stringr::regex(
      "(Recommended\\s+Product:\\s*.+?)(?=\\s+Reason:\\s*)",
      ignore_case = TRUE
    ),
    "\\1."
  )
  
  # Bullets as sentence-like units
  x <- stringr::str_replace_all(x, "(^|\\n)\\s*-\\s+", ". ")
  
  # Restore protected decimal/version dots before readability analysis.
  x <- stringr::str_replace_all(x, "<DOT>", ".")
  
  # Squish whitespace
  x <- stringr::str_squish(x)
  
  x
}

# Count sentences from cleaned text using ICU sentence boundaries.
count_sentences_manual <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_integer_)
  
  sentences <- stringi::stri_split_boundaries(text, type = "sentence")[[1]]
  sentences <- stringr::str_squish(sentences)
  sentences <- sentences[sentences != ""]
  
  length(sentences)
}

# Count words using the same tokenization rules applied to the cleaned text.
count_words_manual <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_integer_)
  
  toks <- quanteda::tokens(
    text,
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE
  )[[1]]
  
  length(toks)
}

#use clean_for_metrics function to standardize text formating for calculations
df_assistant_clean <- df_assistant %>%
  mutate(
    content_clean = map_chr(content, clean_for_metrics)
  ) 


# Lemmatize cleaned text
df_assistant_clean <- df_assistant_clean %>%
  mutate(content_lemma = textstem::lemmatize_strings(content_clean))

# *****************************************************************************
# PART 1 - Readability Scores ----
# *****************************************************************************

# *****************************************************************************
# DATA ANALYSIS ----
# Calculate readability scores
# *****************************************************************************

#Step 1: Manual Readability Calculations ----

# One document per row from cleaned text
corp <- corpus(df_assistant_clean, text_field = "content_clean")

# word tokens (for word + syllable counts)
toks_word <- quanteda::tokens(
  corp,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_separators = TRUE
)

# Lowercase for wordlist matching
toks_word <- tokens_tolower(toks_word)

# Load External Dale-Chall familiar-word list normalized for inflectional matching----
dc_familiar <- readLines(data_file("dalechall_3000.txt"), warn = FALSE) %>%
  tolower() %>%
  textstem::lemmatize_words() %>%
  unique()


# Get intermediate counts (includes words, sentences, syllables per document)
intermediate <- textstat_readability(
  corp,
  measure = "Flesch", # measure can be anything here; we only use intermediate counts
  intermediate = TRUE,
  remove_hyphens = FALSE #do not split compound words like 'multi-layered'
  
)

# Add metrics back to the original data
df_metrics <- df_assistant_clean %>%
  mutate(
    word_count = map_int(content_clean, count_words_manual),
    sentence_count = map_int(content_clean, count_sentences_manual),
    syllable_count = intermediate$Sy
  )

#perform manual fkgl calculation----
df_metrics <- df_metrics %>%
  mutate(
    fk_grade = 0.39 * (word_count / sentence_count) +
      11.8 * (syllable_count / word_count) -
      15.59
  )

n_familiar <- vapply(
  as.list(toks_word),
  function(x) sum(textstem::lemmatize_words(x) %in% dc_familiar),
  numeric(1)
)

n_difficult <- vapply(
  as.list(toks_word),
  function(x) sum(!textstem::lemmatize_words(x) %in% dc_familiar),
  numeric(1)
)

#perform manual dale-chall calculation----
df_metrics <- df_metrics %>%
  mutate(
    n_familiar = n_familiar,
    n_difficult = n_difficult,
    dale_chall =
      64 - (0.95 * (n_difficult / word_count * 100)) -
      (0.69 * word_count / sentence_count)
  )

df_metrics <- df_metrics %>%
  mutate(
    pct_difficult = (n_difficult / word_count) * 100
  )

df_metrics <- df_metrics %>%
  mutate(doc_id = docnames(corp)) %>%
  relocate(sentence_count, .after = sentence_count) %>%
  relocate(pct_difficult, .after = n_difficult) %>%
  relocate(dale_chall, .after = pct_difficult)


#Step 2: Sanity Check ----
i <- which(docnames(corp) == "text270")[1]

manual_familiar_text270 <- tibble::tibble(
  token = as.character(toks_word[[i]])
) %>%
  mutate(token_lemma =
           textstem::lemmatize_words(token)) %>%
  count(token, token_lemma, name = "n") %>%
  filter(token_lemma %in% dc_familiar) %>%
  arrange(desc(n), token)

manual_familiar_text270

toks_word[[i]]


# *****************************************************************************
# DATA EXPLORATION ----
# *****************************************************************************

#Descriptive stats for text----
summary_metrics <- df_metrics %>%
  group_by(llm, prompt_id) %>%
  summarise(mean_word = round(mean(word_count), 0),
            mean_sentence = round(mean(sentence_count), 0),
            average_words_per_sent = round(sum(word_count)/sum(sentence_count), 0),
            mean_difficult = round(mean(n_difficult), 0),
            difficult_percentage = round(sum(n_difficult)/sum(word_count), 2)
  )

#Descriptive stats for readability----
summary_readability <- df_metrics %>%
  group_by(llm, prompt_id) %>%
  summarise(mean_fkgl = round(mean(fk_grade), 0),
            mean_dale_chall = round(mean(dale_chall), 0)
)
# *****************************************************************************
# REGRESSION MODEL: LLM + Prompt → Readability ----
# Purpose:
#   Estimate how chatbot readability varies across LLMs and prompt templates,
#   while accounting for repeated observations within the same question.
#
# Unit of analysis:
#   One chatbot response (a question-response pair).
#
# Dependent variables (DVs):
#   fk_grade     = manually computed Flesch-Kincaid Grade Level
#   dale_chall   = manually computed Dale-Chall readability score
#
# Key independent variables (IVs):
#   llm          = chatbot model
#   prompt_id    = prompt template / style instruction
#   llm*prompt_id = interaction allowing prompt effects to differ by model
#
# Random effect:
#   (1 | cluster_id) = random intercept for question_id
#                      accounts for within-question dependence across responses
# *****************************************************************************

#Linear Mixed-effect Model ----
library(lme4)
library(lmerTest)  # adds p-values for fixed effects
library(emmeans)

# Prep ----
df_model <- df_metrics %>%
  mutate(
    llm = factor(llm),
    prompt_id = factor(prompt_id),          
    cluster_id = factor(question_id)   #repeated-measures / item ID
  ) %>%
  filter(!is.na(fk_grade), !is.na(dale_chall))

# FKGL mixed model ----
m_fkgl <- lmer(
  fk_grade ~ llm * prompt_id + (1 | cluster_id),
  data = df_model,
  REML = FALSE
)

# Model results ----
summary(m_fkgl)
anova(m_fkgl)


# Dale–Chall mixed model----
m_dc <- lmer(
  dale_chall ~ llm * prompt_id + (1 | cluster_id),
  data = df_model,
  REML = FALSE
)

# Model results ----
summary(m_dc)
anova(m_dc)

# *****************************************************************************
# DATA VISUALIZATION ----
# *****************************************************************************


# FKGL: estimated marginal means ----
emm_fkgl <- emmeans(m_fkgl, ~ llm * prompt_id)
fkgl_plot_df <- as.data.frame(emm_fkgl)


# Dale–Chall: estimated marginal means----
emm_dc <- emmeans(m_dc, ~ llm * prompt_id) 
dc_plot_df <- as.data.frame(emm_dc)

# If prompt_id is numeric, convert to character first

#LLM + Prompt on FKGL Regression---- 

fkgl_plot_df <- fkgl_plot_df %>%
  mutate(
    llm = case_when(
      str_detect(str_to_lower(as.character(llm)), "claude") ~ "Claude",
      str_detect(str_to_lower(as.character(llm)), "gemini") ~ "Gemini",
      str_detect(str_to_lower(as.character(llm)), "chatgpt|gpt") ~ "ChatGPT",
      TRUE ~ as.character(llm)
    ),
    llm = factor(llm, levels = c("Claude", "Gemini", "ChatGPT")),
    prompt_id = as.character(prompt_id),
    prompt_label = case_when(
      prompt_id == "0" ~ "Baseline", 
      prompt_id == "1" ~ "Short Sentences",
      prompt_id == "2" ~ "Short Paragraph",
      prompt_id == "3" ~ "8th Grade Level",
      TRUE ~ prompt_id
    ),
    prompt_label = factor(
      prompt_label,
      levels = c(
        "Baseline",
        "Short Sentences",
        "Short Paragraph",
        "8th Grade Level"
      )
    )
  )
# Facet by LLM only; keep prompt names on the x-axis to avoid duplicate labels
ggplot(fkgl_plot_df, aes(x = prompt_label, y = emmean, color = llm)) +
  geom_hline(yintercept = 8, linetype = "dashed", color = "gray") +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  scale_color_manual(values = c(
    "Claude" = "#E69F00",
    "ChatGPT" = "#56B4E9",
    'Gemini' = "purple"
  ))   +
  scale_y_continuous(breaks = scales::breaks_width(2)) +
  facet_grid(llm ~ .) +
  labs(x = "Style Prompt", 
       y = "FK Grade Level", 
       title = "Structural Difficulty Across LLM and Prompts",
       subtitle = "The level of education needed to read the text.",
       caption = "*Dashed line marks the maximum recommended level (~8th Grade)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) 


#LLM + Prompt on Dale-Chall Regression---- 

dc_plot_df <- dc_plot_df %>%
  mutate(
    llm = case_when(
      str_detect(str_to_lower(as.character(llm)), "claude") ~ "Claude",
      str_detect(str_to_lower(as.character(llm)), "gemini") ~ "Gemini",
      str_detect(str_to_lower(as.character(llm)), "chatgpt|gpt") ~ "ChatGPT",
      TRUE ~ as.character(llm)
    ),
    llm = factor(llm, levels = c("Claude", "Gemini", "ChatGPT")),
    prompt_id = as.character(prompt_id),
    prompt_label = case_when(
      prompt_id == "0" ~ "Baseline", 
      prompt_id == "1" ~ "Short Sentences",
      prompt_id == "2" ~ "Short Paragraph",
      prompt_id == "3" ~ "8th Grade Level",
      TRUE ~ prompt_id
    ),
    prompt_label = factor(
      prompt_label,
      levels = c(
        "Baseline",
        "Short Sentences",
        "Short Paragraph",
        "8th Grade Level"
      )
    )
  )

ggplot(dc_plot_df, aes(x = prompt_label, y = emmean, color = llm)) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "gray") +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  scale_color_manual(values = c(
    "Claude" = "#E69F00",
    "ChatGPT" = "#56B4E9",
    'Gemini' = "purple"
  )) +
  facet_grid(llm ~ .) +
  labs(x = "Style Prompt", 
       y = "Dale-Chall Score", 
       title = "Word Familiarity Across LLM and Prompts",
       subtitle = "The reading ease of text based on familiar vocabulary.",
       caption = "*Dashed line marks the minimum recommended score (40 and above)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) 




# *****************************************************************************
# DATA VISUALIZATION (For Client) ----
# *****************************************************************************

# FKGL client plots are generated later in the script so the readability range
# can be tied to the main empathy regression rather than a hard-coded cutoff.



dc_client_plot_df <- dc_plot_df %>%
  mutate(threshold_flag = if_else(emmean >= 40, ">= 40", "< 40"))

ggplot(dc_client_plot_df, aes(x = prompt_label, y = emmean, color = threshold_flag)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "gray") +
  facet_grid(llm ~ .) +
  scale_color_manual(values = c(">= 40" = "#0072B2", "< 40" = "gray55")) +
  labs(
    x = "Style Prompt",
    y = "Familiarity Level",
    color = "Threshold",
    title = "Vocabulary Familiarity by Prompt"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# *****************************************************************************
# PART 2 - Service Quality Scores ----
# *****************************************************************************

# *****************************************************************************
# IMPORT DATA: Empathy Scores ----
# *****************************************************************************

empathy <- read_csv(data_file("customerbot-results.csv"))

df_empathy <- df_metrics %>%
  mutate(row_id = as.numeric(as.character(row_id))) %>%
  left_join(
    empathy %>%
      select(
        row_id,
        `openai_gpt_5_4_mini__average_empathy_score`,
        `google_gemini_2_5_flash__average_empathy_score`
      ),
    by = "row_id"
  ) %>%
  rename(
    chatgpt_empathy = `openai_gpt_5_4_mini__average_empathy_score`,
    gemini_empathy = `google_gemini_2_5_flash__average_empathy_score`
  ) %>%
  mutate(
    average_empathy = rowMeans(
      pick(chatgpt_empathy, gemini_empathy),
      na.rm = TRUE
    )
  )

# *****************************************************************************
# REGRESSION MODEL: FKGL → Empathy (Fixed-Effects OLS) ----
# Purpose:
#   Estimate how response readability (FKGL) relates to persona-rated empathy,
#   while controlling for model choice, prompt template, and question context.
#
# Unit of analysis:
#   One chatbot response (a question–response pair).
#
# Dependent variable (DV):
#   average_empathy (1–5 Likert) from LLM-powered customer persona rubric ratings.
#
# Key independent variables (IVs):
#   fk_grade          = Flesch–Kincaid Grade Level (higher = harder to read)
#   I(fk_grade^2)     = quadratic term to allow a nonlinear "sweet spot"
#                               (e.g., empathy rises with FKGL up to a point, then falls)
#
# Controls / Fixed effects:
#   factor(llm)               = controls for systematic differences across LLMs
#                               (e.g., some models are more empathic regardless of readability)
#   factor(prompt_id)         = controls for systematic differences across prompt templates
#                               (style constraints, tone, brevity instructions, etc.)
#   factor(question_id)   = question fixed effects (holds user intent/context constant)
#                               so FKGL effects are identified from within-question variation
# *****************************************************************************

# Fit fixed-effects OLS model with quadratic FKGL term
fit_fk_fe_emp <- lm(
  average_empathy ~ fk_grade + I(fk_grade^2) +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)

# Inference with heteroskedasticity-robust standard errors (HC3)
# Rationale:
#   Empathy is bounded (1–5) and residual variance often changes across fitted values.
#   HC3 makes coefficient uncertainty estimates more reliable under heteroskedasticity.
library(lmtest)
library(sandwich)
coeftest(fit_fk_fe_emp, vcov = vcovHC(fit_fk_fe_emp, type = "HC3"))

# Standard model summary (R², coefficients, residual diagnostics info)
# Note:
#   The default summary uses classical SEs; use coeftest(HC3) for final inference.
summary(fit_fk_fe_emp)


fit_llm_prompt_fe_emp <- lm(
  average_empathy ~ 
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)
summary(fit_llm_prompt_fe_emp)

# *****************************************************************************
# CHECK REGRESSION ASSUMPTIONS: FKGL → EMPATHY ----
# Model context:
#   DV: average_empathy (1–5 Likert)
#   Key IVs: fk_grade and fk_grade^2 (quadratic specification)
#   Controls: llm, prompt_id, and question_id fixed effects
# *****************************************************************************

# 0) Load packages used in diagnostics
library(car)       # crPlot(), influencePlot(), vif()
library(lmtest)    # coeftest(), bptest()
library(sandwich)  # vcovHC(), vcovCL()
library(splines)   # ns()

# *****************************************************************************
# A) Functional Form: Is the FKGL→empathy relationship nonlinear? ----
# What this checks:
#   Whether a simple linear term is enough, or whether a nonlinear shape is supported.
# How to interpret:
#   - crPlot: visual "partial relationship" after controlling for other predictors.
#   - anova(linear vs spline): tests whether allowing curvature improves fit.
# *****************************************************************************

# A1) Visual partial residual plot for FKGL (after controls) ----
crPlot(fit_fk_fe_emp, variable = "fk_grade")

# A2) Fit a spline model (non-parametric curvature) and compare to linear FKGL model ----
# NOTE: fit_fk_fe_emp_lin should be your linear-only FKGL FE model:
fit_fk_fe_emp_lin <- lm(average_empathy ~ fk_grade +
                          factor(llm) + factor(prompt_id) + factor(question_id),
                        data = df_empathy)

fit_fk_spline <- lm(
  average_empathy ~ ns(fk_grade, df = 3) +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)

# Does the spline (curvature) fit better than a straight line?
anova(fit_fk_fe_emp_lin, fit_fk_spline)

# *****************************************************************************
# B) Collinearity: Are FKGL and FKGL^2 too correlated to estimate reliably? ----
# What this checks:
#   Quadratic models often create high correlation between X and X^2.
# Why centering:
#   Centering FKGL reduces correlation between fk_c and fk_c^2, stabilizing estimates.
# How to interpret:
#   - VIF values near 1–2 indicate low collinearity concern.
# *****************************************************************************

# B1) Center FKGL (subtract mean; do NOT scale/standardize) ----
df_empathy$fk_c <- scale(df_empathy$fk_grade, center = TRUE, scale = FALSE)

# B2) Refit quadratic model using centered FKGL ----
fit_fk_fe_emp_c <- lm(
  average_empathy ~ fk_c + I(fk_c^2) +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)

# B3) Report coefficients with heteroskedasticity-robust SEs (HC3) ----
coeftest(fit_fk_fe_emp_c, vcov = vcovHC(fit_fk_fe_emp_c, type = "HC3"))

# B4) Check collinearity (use the adjusted GVIF column for factor variables) ----
vif(fit_fk_fe_emp_c)

# *****************************************************************************
# C) Heteroskedasticity: Is error variance non-constant? ----
# What this checks:
#   Whether residual spread changes across observations (common with 1–5 outcomes).
# How to interpret:
#   - bptest significant → heteroskedasticity likely.
# What to do:
#   - Use robust SEs (HC3) for inference (already done above).
# *****************************************************************************

bptest(fit_fk_fe_emp)

# *****************************************************************************
# D) Within-question Dependence: Are errors correlated within question_id? ----
# What this does (NOT a "test"):
#   Uses cluster-robust SEs so inference remains valid if within-question correlation exists.
# How to interpret:
#   If FKGL terms remain significant with clustered SEs, results are robust to clustering.
# *****************************************************************************

vcov_fk_cl <- vcovCL(fit_fk_fe_emp, cluster = df_empathy$question_id)
coeftest(fit_fk_fe_emp, vcov = vcov_fk_cl)

# Optional diagnostic (quick check): residual correlation with within-question mean residual ----
# Interpretation:
#   Near 0 → little detectable within-question residual correlation.
res <- resid(fit_fk_fe_emp)
icc_like <- cor(res, ave(res, df_empathy$question_id), use = "complete.obs")
icc_like

# *****************************************************************************
# E) Influence / Outliers: Are a few observations driving the FKGL result? ----
# What this checks:
#   Whether any points have high leverage / large residuals / high Cook’s distance.
# How to interpret:
#   Large bubbles / labeled IDs suggest influential points worth sensitivity-testing.
# *****************************************************************************

influencePlot(fit_fk_fe_emp, id.method = "identify")

# E1) Sensitivity analysis: refit after removing influential rows found above
# NOTE: Update drop_ids based on what influencePlot identifies for YOUR model.
drop_ids <- c(29, 107, 123, 247)

df_sub <- df_empathy[-drop_ids, ]

fit_fk_sub <- lm(
  average_empathy ~ fk_grade + I(fk_grade^2) +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_sub
)

# Robust SEs for the sensitivity model
coeftest(fit_fk_sub, vcov = vcovHC(fit_fk_sub, type = "HC3"))
summary(fit_fk_sub)

# *****************************************************************************
# F) Normality of Residuals (LOW PRIORITY WITH ROBUST SEs) ----
# What this checks:
#   Whether residuals follow a normal distribution (often rejected with larger n).
# How to interpret:
#   Minor deviations are not usually a problem when using robust/clustered SEs.
# *****************************************************************************

res <- resid(fit_fk_fe_emp)

# F1) Visual check ----
qqnorm(res); qqline(res)

# F2) Formal test (often rejects even for small deviations) ----
shapiro.test(res)

# *****************************************************************************
# G) Subgroup Robustness: Does the FKGL effect differ by model (LLM)? ----
# What this checks:
#   Whether the FKGL→empathy relationship is consistent across LLMs.
# How to interpret:
#   Differences suggest the readability/empathy link is model-dependent.
# *****************************************************************************

by(df_empathy, df_empathy$llm, function(d)
  summary(lm(
    average_empathy ~ fk_grade + I(fk_grade^2) +
      factor(prompt_id) + factor(question_id),
    data = d
  ))
)



# *****************************************************************************
# H) Supplementary Analysis: FKGL Sweet Spot by LLM ----
# Purpose:
#   Show whether the empathy-maximizing FKGL range differs by model.
# Note:
#   This is a supplementary deployment-oriented analysis and does not replace
#   the primary pooled fixed-effects FKGL model above.
# *****************************************************************************

fit_fk_llm_sup <- lm(
  average_empathy ~ (fk_grade + I(fk_grade^2)) * factor(llm) +
    factor(prompt_id) + factor(question_id),
  data = df_empathy
)

# Build a prediction grid over the observed FKGL range and average predictions
# across prompts and questions to avoid dependence on one reference category.
fk_grid_sup <- expand.grid(
  fk_grade = seq(
    floor(min(df_empathy$fk_grade, na.rm = TRUE)),
    ceiling(max(df_empathy$fk_grade, na.rm = TRUE)),
    by = 0.1
  ),
  llm = levels(df_empathy$llm),
  prompt_id = levels(df_empathy$prompt_id),
  question_id = levels(df_empathy$question_id)
)

fk_grid_sup$pred_empathy <- predict(fit_fk_llm_sup, newdata = fk_grid_sup)

fk_llm_curve_df <- fk_grid_sup %>%
  group_by(llm, fk_grade) %>%
  summarise(pred_empathy = mean(pred_empathy), .groups = "drop")

fk_llm_opt_table <- fk_llm_curve_df %>%
  group_by(llm) %>%
  slice_max(pred_empathy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    optimal_fkgl = round(fk_grade, 1),
    predicted_empathy = round(pred_empathy, 2)
  ) %>%
  select(llm, optimal_fkgl, predicted_empathy)

fk_llm_opt_table

ggplot(fk_llm_curve_df, aes(x = fk_grade, y = pred_empathy, color = llm)) +
  geom_line(linewidth = 1.1) +
  geom_point(
    data = fk_llm_opt_table,
    aes(x = optimal_fkgl, y = predicted_empathy, color = llm),
    size = 2.5,
    inherit.aes = FALSE
  ) +
  labs(
    x = "FKGL Score",
    y = "Predicted Empathy Score",
    title = "Supplementary: FKGL-Empathy Relationship by LLM",
    subtitle = "Predictions from an FKGL x LLM interaction model, averaged across prompts and questions"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(1, 5)) +
  scale_y_continuous(breaks = 1:5)


# *****************************************************************************
# REGRESSION MODEL: Dale-Chall → Empathy (Fixed-Effects OLS) ----
# Purpose:
#   Estimate how response readability (Dale-Chall) relates to persona-rated empathy,
#   while controlling for model choice, prompt template, and question context.
#
# Unit of analysis:
#   One chatbot response (a question–response pair).
#
# Dependent variable (DV):
#   average_empathy (1–5 Likert) from LLM-powered customer persona rubric ratings.
#
# Key independent variables (IVs):
#   dale_chall          = Flesch–Kincaid Grade Level (higher = harder to read)
#   I(dale_chall^2)     = quadratic term to allow a nonlinear "sweet spot"
#                               (e.g., empathy rises with FKGL up to a point, then falls)
#
# Controls / Fixed effects:
#   factor(llm)               = controls for systematic differences across LLMs
#                               (e.g., some models are more empathic regardless of readability)
#   factor(prompt_id)         = controls for systematic differences across prompt templates
#                               (style constraints, tone, brevity instructions, etc.)
#   factor(question_id)   = question fixed effects (holds user intent/context constant)
#                               so FKGL effects are identified from within-question variation
# *****************************************************************************


# 1A) Linear Dale–Chall FE model (needed for model comparison) ----
fit_dc_fe_emp_lin <- lm(
  average_empathy ~ dale_chall +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)
summary(fit_dc_fe_emp_lin)

# 1B) Quadratic Dale–Chall FE model (your main spec) ----
fit_dc_fe_emp <- lm(
  average_empathy ~ dale_chall + I(dale_chall^2) +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)
summary(fit_dc_fe_emp)

# Robust inference (HC3) for the quadratic model ----
coeftest(fit_dc_fe_emp, vcov = vcovHC(fit_dc_fe_emp, type = "HC3"))



# *****************************************************************************
# Check Regression Assumptions / Robustness: Dale-Chall -> Empathy ----
# Model context:
#   DV: average_empathy (1–5 Likert)
#   Key IVs: dale_chall and dale_chall^2 (quadratic specification)
#   Controls: llm, prompt_id, and question_id fixed effects
# *****************************************************************************

# 0) Packages used in diagnostics loaded in the previous code block for FKGL

# *****************************************************************************
# 1) Fit Base Models (Linear + Quadratic) for Dale-Chall ----
# *****************************************************************************

# 1A) Linear Dale–Chall FE model (needed for model comparison) ----

fit_dc_fe_emp_lin <- lm(
  average_empathy ~ dale_chall +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)
summary(fit_dc_fe_emp_lin)

# 1B) Quadratic Dale–Chall FE model (your main spec) ----
fit_dc_fe_emp <- lm(
  average_empathy ~ dale_chall + I(dale_chall^2) +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)
summary(fit_dc_fe_emp)  # descriptive; uses classical SEs

# Robust inference (HC3) for the quadratic model ----
coeftest(fit_dc_fe_emp, vcov = vcovHC(fit_dc_fe_emp, type = "HC3"))


# *****************************************************************************
# A) Functional Form (Shape): Is the Dale-Chall->empathy relationship nonlinear?
# *****************************************************************************

# A1) Visual partial residual plot for Dale–Chall (after controls) ----
crPlot(fit_dc_fe_emp, variable = "dale_chall")

# A2) Spline model (flexible curvature) and compare to linear model ----
fit_dc_spline <- lm(
  average_empathy ~ ns(dale_chall, df = 3) +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)

# Does the spline fit better than the straight line? ----
anova(fit_dc_fe_emp_lin, fit_dc_spline)

# *****************************************************************************
# B) Collinearity (Linear Spec): Is Dale-Chall too correlated with other predictors? ----
# Note:
#   Since the spline test suggested no meaningful nonlinearity, we use the LINEAR
#   Dale–Chall model going forward. With no squared term, centering is optional
#   and "Dale–Chall vs Dale–Chall^2 collinearity" is no longer relevant.
# What we check instead:
#   General multicollinearity via VIF/GVIF for the linear FE model.
# *****************************************************************************

# Main (linear) Dale–Chall FE model ----
fit_dc_fe_emp_lin <- lm(
  average_empathy ~ dale_chall +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_empathy
)

# VIF/GVIF collinearity check (use GVIF^(1/(2*Df)) column for factors) ----
vif(fit_dc_fe_emp_lin)

# *****************************************************************************
# C) Heteroskedasticity: Is residual variance non-constant? ----
# If significant:
#   Use robust SEs (HC3) for inference (recommended with 1–5 DV).
# *****************************************************************************

bptest(fit_dc_fe_emp_lin)

# Robust inference (HC3) for the linear model ----
summary(fit_dc_fe_emp_lin)  # descriptive (classical SEs)
coeftest(fit_dc_fe_emp_lin, vcov = vcovHC(fit_dc_fe_emp_lin, type = "HC3")) #robust




# *****************************************************************************
# D) Within-question Dependence: Cluster-robust SEs by question_id ----
# What this does:
#   Makes standard errors valid if residuals are correlated within question.
# *****************************************************************************

vcov_dc_cl <- vcovCL(fit_dc_fe_emp_lin, cluster = df_empathy$question_id)
coeftest(fit_dc_fe_emp_lin, vcov = vcov_dc_cl)

# Optional diagnostic: residual correlation with within-question mean residual ----
res_dc <- resid(fit_dc_fe_emp_lin)
icc_like_dc <- cor(res_dc, ave(res_dc, df_empathy$question_id), use = "complete.obs")
icc_like_dc

# *****************************************************************************
# E) Influence / Outliers: Are a few observations driving the Dale-Chall result? ----
# *****************************************************************************

library(car)
influencePlot(fit_dc_fe_emp_lin, id.method = "identify")

# E1) Sensitivity analysis: refit after removing influential rows flagged above ----
# NOTE: Replace IDs with the ones flagged for YOUR linear Dale–Chall model.
drop_ids_dc <- c(29, 107, 123, 247)  # <- update after viewing influencePlot

df_sub_dc <- df_empathy[-drop_ids_dc, ]

fit_dc_sub_lin <- lm(
  average_empathy ~ dale_chall +
    factor(llm) + factor(prompt_id) + factor(question_id),
  data = df_sub_dc
)

# Robust SEs for the sensitivity model ----
coeftest(fit_dc_sub_lin, vcov = vcovHC(fit_dc_sub_lin, type = "HC3"))
summary(fit_dc_sub_lin)

# *****************************************************************************
# F) Normality of Residuals (Low Priority with Robust/Clustered SEs) ----
# *****************************************************************************

res_dc <- resid(fit_dc_fe_emp_lin)
qqnorm(res_dc); qqline(res_dc)
shapiro.test(res_dc)

# *****************************************************************************
# G) Subgroup Robustness: Does the Dale-Chall effect differ by LLM? ----
# Note:
#   Keep the same linear specification within each LLM.
# *****************************************************************************

by(df_empathy, df_empathy$llm, function(d)
  summary(lm(
    average_empathy ~ dale_chall +
      factor(prompt_id) + factor(question_id),
    data = d
  ))
)

# *****************************************************************************
# DATA VISUALIZATION ----
# *****************************************************************************

#FKGL on Empathy Regression---- 
visreg(fit_fk_fe_emp, "fk_grade", gg = TRUE, partial = FALSE, rug = FALSE) +
  geom_vline(
    xintercept = 8,
    linetype = "dashed",
    color = "gray40",
    show.legend = FALSE
  ) +
  geom_segment(
    data = data.frame(
      x = 0, xend = 1, y = 1, yend = 1,
      line_key = "Regression fit"
    ),
    aes(x = x, xend = xend, y = y, yend = yend, color = line_key, linetype = line_key),
    inherit.aes = FALSE,
    alpha = 0,
    show.legend = TRUE
  ) +
  geom_segment(
    data = data.frame(
      x = 0, xend = 1, y = 1, yend = 1,
      line_key = "Grade 8"
    ),
    aes(x = x, xend = xend, y = y, yend = yend, color = line_key, linetype = line_key),
    inherit.aes = FALSE,
    alpha = 0,
    show.legend = TRUE
  ) +
  labs(
    x = "FKGL Score",
    y = "Predicted Empathy Score",
    title = "Increased Reading Difficulty Has Diminishing Returns",
    subtitle = "High-school reading level responses score as most empathetic.",
    caption = "*Dashed line marks the max recommended level (~8th Grade) for general public."
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Regression fit" = "#2C7FB8", "Grade 8" = "gray40"),
    breaks = c("Regression fit", "Grade 8")
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Regression fit" = "solid", "Grade 8" = "dashed"),
    breaks = c("Regression fit", "Grade 8")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(alpha = 1, linetype = c("solid", "dashed"))
    ),
    linetype = "none"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  scale_y_continuous(breaks = 1:5)


#
ggplot(df_empathy, aes(x = fk_grade, y = average_empathy, color = llm))  +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) +
  #facet_wrap(~ prompt_id) +
  labs(
    x = "FKGL Score",
    y = "Average Empathy",
    title = "Empathy Peaks at Moderate Structural Complexity"
  )


#Dale-Chall on Empathy Regression---- 
visreg(fit_dc_fe_emp_lin, "dale_chall", gg = TRUE, partial = FALSE, rug = FALSE) +
  geom_vline(
    xintercept = 40,
    linetype = "dashed",
    color = "gray40",
    show.legend = FALSE
  ) +
  geom_segment(
    data = data.frame(
      x = 0, xend = 1, y = 1, yend = 1,
      line_key = "Regression fit"
    ),
    aes(x = x, xend = xend, y = y, yend = yend, color = line_key, linetype = line_key),
    inherit.aes = FALSE,
    alpha = 0,
    show.legend = TRUE
  ) +
  geom_segment(
    data = data.frame(
      x = 0, xend = 1, y = 1, yend = 1,
      line_key = "Score 40"
    ),
    aes(x = x, xend = xend, y = y, yend = yend, color = line_key, linetype = line_key),
    inherit.aes = FALSE,
    alpha = 0,
    show.legend = TRUE
  ) +
  labs(
    x = "Dale-Chall Score",
    y = "Predicted Empathy Score",
    title = "Complex Vocabulary Hurts Empathy",
    subtitle = "Responses near the Score 10 score as most empathetic."
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Regression fit" = "#2C7FB8", "Score 40" = "gray40"),
    breaks = c("Regression fit", "Score 40")
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Regression fit" = "solid", "Score 40" = "dashed"),
    breaks = c("Regression fit", "Score 40")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(alpha = 1, linetype = c("solid", "dashed"))
    ),
    linetype = "none"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  scale_y_continuous(breaks = 1:5)




# *****************************************************************************

# write_csv(troubleshoot, "troubleshoot.csv")


# EXPORT
# Appendix B1
# Main FKGL model (HC3) and Main Dale–Chall model (HC3)
# Exports a clean Word (.docx) regression table

# Robust (HC3) variance-covariance matrices
vc_fk_hc3 <- vcovHC(fit_fk_fe_emp, type = "HC3")
vc_dc_hc3 <- vcovHC(fit_dc_fe_emp_lin, type = "HC3")

modelsummary(
  list(
    "FKGL FE (HC3)" = fit_fk_fe_emp,
    "Dale–Chall FE (HC3)" = fit_dc_fe_emp_lin
  ),
  vcov = list(vc_fk_hc3, vc_dc_hc3),
  coef_omit = "question_id",      # hide question fixed effects
  statistic = "({std.error})",        # show robust SEs in parentheses
  stars = TRUE,
  gof_omit = "Adj|F|Log|RMSE",        # optional: simplify GOF panel
  title = "Appendix B1. Fixed-Effects OLS Models Predicting Empathy (HC3 Robust SEs)",
  notes = "Question fixed effects included but not shown. Robust (HC3) standard errors in parentheses.",
  output = "Appendix_B1_Main_Regression_Results.docx"
)  

# Appendix B2
# Sensitivity: Drop Influential Points (HC3 Robust SEs)
# Exports Word (.docx) regression table

# Ensure these models already exist:
# fit_fk_sub       -> FKGL model after dropping influential rows
# fit_dc_sub_lin   -> Dale–Chall model after dropping influential rows

vc_fk_sub_hc3 <- vcovHC(fit_fk_sub, type = "HC3")
vc_dc_sub_hc3 <- vcovHC(fit_dc_sub_lin, type = "HC3")

modelsummary(
  list(
    "FKGL FE (Drop Influential, HC3)" = fit_fk_sub,
    "Dale–Chall FE (Drop Influential, HC3)" = fit_dc_sub_lin
  ),
  vcov = list(vc_fk_sub_hc3, vc_dc_sub_hc3),
  coef_omit = "question_id",   # hide question fixed effects
  statistic = "({std.error})",
  stars = TRUE,
  title = "Appendix B2. Sensitivity Analysis: Drop Influential Observations (HC3 Robust SEs)",
  notes = "Question fixed effects included but not shown. Robust (HC3) standard errors in parentheses.",
  output = "Appendix_B2_Sensitivity_Drop_Influential.docx"
)

# Appendix B3
# Cluster-robust SE (by question_id) regression table
# Exports Word (.docx) regression table


# Clustered variance-covariance matrices (clustered by question_id)
vc_fk_cluster <- vcovCL(fit_fk_fe_emp, cluster = df_empathy$question_id)
vc_dc_cluster <- vcovCL(fit_dc_fe_emp_lin, cluster = df_empathy$question_id)

modelsummary(
  list(
    "FKGL FE (Clustered by Question)" = fit_fk_fe_emp,
    "Dale–Chall FE (Clustered by Question)" = fit_dc_fe_emp_lin
  ),
  vcov = list(vc_fk_cluster, vc_dc_cluster),
  coef_omit = "question_id",   # hide question fixed effects
  statistic = "({std.error})",
  stars = TRUE,
  title = "Appendix B3. Cluster-Robust Standard Errors (Clustered by Question)",
  notes = "Question fixed effects included but not shown. Standard errors clustered by question_id.",
  output = "Appendix_B3_Clustered_SE.docx"
)


# Appendix D1
# Collinearity Diagnostics (VIF / GVIF)
# Exports a Word (.docx) table for both FKGL and Dale–Chall models


# Helper function to convert VIF/GVIF output into a clean data frame
vif_to_df <- function(model, model_name){
  v <- vif(model)
  
  if (is.matrix(v)) {
    df <- as.data.frame(v)
    df$Term <- rownames(df)
    rownames(df) <- NULL
    df <- df %>%
      select(Term, everything())
  } else {
    df <- data.frame(
      Term = names(v),
      VIF = as.numeric(v)
    )
  }
  
  df$Model <- model_name
  df
}

# FKGL model (quadratic specification)
vif_fk <- vif_to_df(fit_fk_fe_emp_c, "FKGL (centered quadratic)")

# Dale–Chall model (linear specification)
vif_dc <- vif_to_df(fit_dc_fe_emp_lin, "Dale–Chall (linear)")

# Combine
vif_df <- bind_rows(vif_fk, vif_dc)

# Create Word table
ft <- flextable(vif_df)
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(doc,
                    "Appendix D1. Collinearity Diagnostics (VIF / GVIF)",
                    style = "heading 1"
)
doc <- body_add_flextable(doc, ft)

#print(doc, target = "Appendix_D1_VIF_GVIF.docx")  

# Appendix D2
# Heteroskedasticity Diagnostics (Breusch–Pagan Test)
# Exports Word (.docx) table


# Run BP tests for both models
bp_fk <- bptest(fit_fk_fe_emp)
bp_dc <- bptest(fit_dc_fe_emp_lin)

# Convert to tidy format
bp_df <- bind_rows(
  tidy(bp_fk) %>% mutate(Model = "FKGL FE model"),
  tidy(bp_dc) %>% mutate(Model = "Dale–Chall FE model")
)

# Keep only relevant columns
bp_df <- bp_df %>%
  select(Model, statistic, parameter, p.value) %>%
  rename(
    `BP Statistic` = statistic,
    `Degrees of Freedom` = parameter,
    `p-value` = p.value
  )

# Create Word table
ft <- flextable(bp_df)
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(doc,
                    "Appendix D2. Heteroskedasticity Diagnostics (Breusch–Pagan Test)",
                    style = "heading 1"
)
doc <- body_add_flextable(doc, ft)

#print(doc, target = "Appendix_D2_Breusch_Pagan.docx")

# Appendix D4
# Residual Normality Diagnostics (Shapiro–Wilk Test)
# Exports Word (.docx) table

# Run Shapiro–Wilk tests
sh_fk <- shapiro.test(resid(fit_fk_fe_emp))
sh_dc <- shapiro.test(resid(fit_dc_fe_emp_lin))

# Convert to tidy data frame
sh_df <- bind_rows(
  tidy(sh_fk) %>% mutate(Model = "FKGL FE model"),
  tidy(sh_dc) %>% mutate(Model = "Dale–Chall FE model")
)

# Select and rename columns
sh_df <- sh_df %>%
  select(Model, statistic, p.value) %>%
  rename(
    `W Statistic` = statistic,
    `p-value` = p.value
  )

# Create Word table
ft <- flextable(sh_df)
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(doc,
                    "Appendix D4. Residual Normality Diagnostics (Shapiro–Wilk Test)",
                    style = "heading 1"
)
doc <- body_add_flextable(doc, ft)

#print(doc, target = "Appendix_D4_Residual_Normality.docx")


# Appendix E2
# Subgroup-by-LLM table: Dale–Chall term
# Exports Word (.docx) table


# Split data by LLM and fit the same linear model within each
models_dc_llm <- split(df_empathy, df_empathy$llm) %>%
  imap(~ lm(
    average_empathy ~ dale_chall +
      factor(prompt_id) + factor(question_id),
    data = .x
  ))

# Extract only the Dale–Chall coefficient from each model
dc_subgroup_table <- imap_dfr(models_dc_llm, ~ tidy(.x) %>%
                                filter(term == "dale_chall") %>%
                                mutate(LLM = .y)
)

# Clean table for presentation
dc_subgroup_table <- dc_subgroup_table %>%
  select(LLM, estimate, std.error, statistic, p.value) %>%
  rename(
    `LLM` = LLM,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `t value` = statistic,
    `p-value` = p.value
  )

# Create Word table
ft <- flextable(dc_subgroup_table)
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(doc,
                    "Appendix E2. Subgroup Results by LLM: Dale–Chall Model",
                    style = "heading 1"
)
doc <- body_add_flextable(doc, ft)

#print(doc, target = "Appendix_E2_DaleChall_Subgroup_by_LLM.docx")





# *****************************************************************************
# BUSINESS VISUALS: Readability + Empathy Decision Charts ----
# Purpose:
#   Summarize each LLM/prompt combination on readability and empathy together
#   for business-facing interpretation.
# *****************************************************************************

# Practical high-empathy FKGL range from the pooled quadratic empathy regression.
fk_seq_main <- seq(
  min(df_empathy$fk_grade, na.rm = TRUE),
  max(df_empathy$fk_grade, na.rm = TRUE),
  by = 0.1
)

fk_pred_main <- tibble(
  fk_grade = fk_seq_main,
  llm = levels(df_empathy$llm)[1],
  prompt_id = levels(df_empathy$prompt_id)[1],
  question_id = levels(df_empathy$question_id)[1]
) %>%
  mutate(pred_empathy = predict(fit_fk_fe_emp, newdata = .))

fk_max_main <- max(fk_pred_main$pred_empathy, na.rm = TRUE)

fk_range_main <- fk_pred_main %>%
  filter(pred_empathy >= fk_max_main - 0.05) %>%
  summarise(
    lower = min(fk_grade),
    upper = max(fk_grade)
  )

business_plot_df <- df_empathy %>%
  mutate(
    llm_label = case_when(
      str_detect(str_to_lower(as.character(llm)), "claude") ~ "Claude",
      str_detect(str_to_lower(as.character(llm)), "gemini") ~ "Gemini",
      str_detect(str_to_lower(as.character(llm)), "chatgpt|gpt") ~ "ChatGPT",
      TRUE ~ as.character(llm)
    ),
    llm_label = factor(llm_label, levels = c("Claude", "Gemini", "ChatGPT")),
    prompt_id = as.character(prompt_id),
    prompt_label = case_when(
      prompt_id == "0" ~ "Baseline",
      prompt_id == "1" ~ "Short Sentences",
      prompt_id == "2" ~ "Short Paragraph",
      prompt_id == "3" ~ "8th Grade Level",
      TRUE ~ prompt_id
    )
  ) %>%
  group_by(llm_label, prompt_label) %>%
  summarise(
    mean_fkgl = mean(fk_grade, na.rm = TRUE),
    mean_empathy = mean(average_empathy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    combo_label = paste(llm_label, prompt_label, sep = " - "),
    readability_zone = if_else(
      between(mean_fkgl, fk_range_main$lower, fk_range_main$upper),
      "In recommended range",
      "Outside range"
    )
  )


# 2) Ranked empathy plot with readability-zone coloring ----
business_rank_df <- business_plot_df %>%
  arrange(desc(mean_empathy)) %>%
  mutate(combo_label = factor(combo_label, levels = rev(combo_label)))

ggplot(business_rank_df, aes(x = mean_empathy, y = combo_label, color = readability_zone)) +
  geom_point(size = 4) +
  geom_segment(
    aes(x = 0, xend = mean_empathy, y = combo_label, yend = combo_label),
    color = "gray80"
  ) +
  scale_color_manual(values = c(
    "In recommended range" = "#0072B2",
    "Outside range" = "gray55"
  )) +
  labs(
    x = "Average Empathy Score",
    y = NULL,
    color = "Readability",
    title = "Highest-Empathy Combinations",
    subtitle = "Claude and ChatGPT achieve higher empathy with certain prompts."
  ) +
  theme_minimal()

# Updated business-facing versions: emphasize model choice over prompt details ----
business_model_df <- df_empathy %>%
  mutate(
    llm_label = case_when(
      str_detect(str_to_lower(as.character(llm)), "claude") ~ "Claude",
      str_detect(str_to_lower(as.character(llm)), "gemini") ~ "Gemini",
      str_detect(str_to_lower(as.character(llm)), "chatgpt|gpt") ~ "ChatGPT",
      TRUE ~ as.character(llm)
    ),
    llm_label = factor(llm_label, levels = c("Claude", "Gemini", "ChatGPT"))
  ) %>%
  group_by(llm_label) %>%
  summarise(
    mean_fkgl = mean(fk_grade, na.rm = TRUE),
    mean_empathy = mean(average_empathy, na.rm = TRUE),
    .groups = "drop"
  )

business_model_df <- business_model_df %>%
  mutate(
    readability_zone = if_else(
      between(mean_fkgl, fk_range_main$lower, fk_range_main$upper),
      "In recommended range",
      "Outside range"
    )
  )

# 3) Updated main chart: model-level readability-empathy matrix ----
ggplot(business_model_df, aes(x = mean_fkgl, y = mean_empathy, color = llm_label)) +
  annotate("rect", xmin = fk_range_main$lower, xmax = fk_range_main$upper,
           ymin = -Inf, ymax = Inf, fill = "#DCEAF7", alpha = 0.6) +
  geom_point(size = 5) +
  geom_text(aes(label = llm_label), nudge_y = 0.05, size = 3.8, show.legend = FALSE) +
  scale_color_manual(values = c(
    "Claude" = "#E69F00",
    "Gemini" = "#7B61A8",
    "ChatGPT" = "#56B4E9"
  )) +
  labs(
    x = "Average FKGL",
    y = "Average Empathy",
    color = "LLM",
    title = "Model Choice and Readability Jointly Shape Empathy",
    subtitle = "Claude appears highest on empathy and readability",
    caption = "*Blue area marks the practical high-empathy FKGL range implied by the main empathy regression."
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 4) Updated main chart: model empathy ranking with readability-zone flag ----
business_model_rank_df <- business_model_df %>%
  arrange(desc(mean_empathy)) %>%
  mutate(llm_label = factor(llm_label, levels = rev(llm_label)))

ggplot(business_model_rank_df, aes(x = mean_empathy, y = llm_label, color = readability_zone)) +
  geom_point(size = 5) +
  geom_segment(
    aes(x = 0, xend = mean_empathy, y = llm_label, yend = llm_label),
    color = "gray80"
  ) +
  scale_color_manual(values = c(
    "In recommended range" = "#0072B2",
    "Outside range" = "gray55"
  )) +
  labs(
    x = "Average Empathy",
    y = NULL,
    color = "FKGL Zone",
    title = "Empathy Ranking by Model",
    subtitle = "Overall, Claude produces more empathetic and readable responses"
  ) +
  theme_minimal()

# Updated FKGL client plots using the near-maximum range from the main model ----
fkgl_client_plot_df <- fkgl_plot_df %>%
  mutate(
    threshold_flag = if_else(
      between(emmean, fk_range_main$lower, fk_range_main$upper),
      "In recommended range",
      "Outside range"
    )
  )

ggplot(fkgl_client_plot_df, aes(x = prompt_label, y = emmean, color = threshold_flag)) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = fk_range_main$lower, ymax = fk_range_main$upper,
    fill = "#DCEAF7", alpha = 0.5
  ) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  facet_grid(llm ~ .) +
  scale_color_manual(values = c(
    "In recommended range" = "#0072B2",
    "Outside range" = "gray55"
  )) +
  scale_y_continuous(breaks = scales::breaks_width(2)) +
  labs(
    x = "Style Prompt",
    y = "Grade Level",
    color = "Readability Zone",
    title = "US Education Level Needed to Understand",
    subtitle = "Blue band marks the practical high-empathy FKGL range implied by the main model."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fkgl_client_plot_df_10 <- fkgl_plot_df %>%
  mutate(
    threshold_flag = if_else(
      between(emmean, fk_range_main$lower, fk_range_main$upper),
      "In recommended range",
      "Outside range"
    )
  )

ggplot(fkgl_client_plot_df_10, aes(x = prompt_label, y = emmean, color = threshold_flag)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  geom_hline(
    yintercept = c(fk_range_main$lower, fk_range_main$upper),
    linetype = "dashed",
    color = "gray40"
  ) +
  facet_grid(llm ~ .) +
  scale_color_manual(values = c(
    "In recommended range" = "#0072B2",
    "Outside range" = "gray55"
  )) +
  scale_y_continuous(breaks = scales::breaks_width(2)) +
  labs(
    x = "Style Prompt",
    y = "Grade Level",
    color = "Readability Zone",
    title = "Prompt-Level Readability Relative to the High-Empathy Range",
    subtitle = "Dashed lines mark the near-maximum FKGL range implied by the main empathy regression."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==============================================================================
# LOAD PACKAGES  ----
# ==============================================================================

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

# Resolve paths whether the script is run from the repo root or from script/.
project_dir <- if (dir.exists("data")) "." else if (dir.exists(file.path("..", "data"))) ".." else "."
data_dir <- if (dir.exists(file.path(project_dir, "data"))) file.path(project_dir, "data") else project_dir
results_dir <- file.path(project_dir, "results")
data_file <- function(filename) file.path(data_dir, filename)
results_file <- function(filename) file.path(results_dir, filename)

# ==============================================================================
# IMPORT DATA ----
# ==============================================================================

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
  rename(context = question) %>%
  relocate(context, .before = 1) %>%
  mutate(row_id = row_number(), .before = 1)
  

#Filter final dataset for only chatbot agent responses
df_assistant <- df_1 |>
  filter(role == "assistant") |>
  transmute(row_id = as.factor(row_id), 
            conversation_id = as.factor(conversation_id), 
            prompt_id = as.factor(prompt_id), 
            llm = as.factor(llm), 
            context, 
            content)

#Write to csv 
#write_csv(df_assistant, "df_assistant.csv")

# ==============================================================================
# DATA CLEANING ----
# ==============================================================================

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

#use clean_for_metrics function to standardize text formating for calculations
df_assistant_clean <- df_assistant %>%
  mutate(
    content_clean = map_chr(content, clean_for_metrics)
  ) 

#parse out the recommended product statement into new product_name column
df_assistant_clean <- df_assistant_clean %>%
  mutate(
    product_name = coalesce(
      str_match(
        content_clean,
        regex(
          "^\\s*Recommended\\s+Product\\s*:\\s*(.+?)(?=\\s*\\.?\\s*Reason\\s*:)",
          ignore_case = TRUE
        )
      )[, 2],
      str_match(
        content_clean,
        regex("^\\s*Recommended\\s+Product\\s*:\\s*(.+?)\\s*\\.(?=\\s|$)", ignore_case = TRUE)
      )[, 2]
    ),
    content_clean = str_replace(
      content_clean,
      regex(
        "^\\s*Recommended\\s+Product\\s*:\\s*.+?(?=\\s*\\.?\\s*Reason\\s*:|\\s*\\.(?=\\s|$))",
        ignore_case = TRUE
      ),
      ""
    ),
    content_clean = str_replace(
      content_clean,
      regex("^\\s*\\.?\\s*(?=Reason\\s*:)", ignore_case = TRUE),
      ""
    )
  )

# Lemmatize cleaned text so manual and quanteda computations run on the same text basis.
df_assistant_clean <- df_assistant_clean %>%
  mutate(content_lemma = textstem::lemmatize_strings(content_clean))

# ==============================================================================
# PART 1 - READABILITY SCORES ----
# ==============================================================================
# ==============================================================================
# DATA ANALYSIS ----
# Calculate readability scores
# ==============================================================================


#STEP 1: Manual Readability Calculations For Comparison ----

# One document per row from lemmatized text
corp <- corpus(df_assistant_clean, text_field = "content_lemma")

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

# Keep toks_norm alias for downstream QA/debug blocks in this script
toks_norm <- toks_word

# Dale-Chall familiar list normalized to match lemmatized tokens
dc_familiar <- unique(textstem::lemmatize_words(tolower(data_char_wordlists$dalechall)))

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
    word_count     = intermediate$W,
    sentence_count = intermediate$St,
    syllable_count = intermediate$Sy
  )

# Keep a token-count column aligned with the normalized token object used below.
df_metrics <- df_metrics %>%
  mutate(token_count = lengths(as.list(toks_norm)))

#perform manual fkgl calculation to validate package calculation
df_metrics <- df_metrics %>%
  mutate(
    fk_grade_manual = 0.39 * (word_count / sentence_count) +
      11.8 * (syllable_count / word_count) -
      15.59
  )

n_familiar <- vapply(
  as.list(toks_norm),
  function(x) sum(x %in% dc_familiar),
  numeric(1)
)

# difficult word count per document: tokens NOT in familiar list
n_difficult <- vapply(
  as.list(toks_norm),
  function(x) sum(!x %in% dc_familiar),
  numeric(1)
)

#perform manual new dale-chall calculation to validate package calculation
df_metrics <- df_metrics %>%
  mutate(
    n_familiar = n_familiar,
    n_difficult = n_difficult,
    dale_chall_manual_new =
      64 - (0.95 * (n_difficult / word_count * 100)) -
      (0.69 * word_count / sentence_count)
  )

#perform manual new dale-chall calculation to validate package calculation
df_metrics <- df_metrics %>%
  mutate(
    pct_difficult = (n_difficult / word_count) * 100,
    asl = word_count / sentence_count,
    dale_chall_manual = 0.1579 * pct_difficult + 0.0496 * asl +
      if_else(pct_difficult > 5, 3.6365, 0)
  )

#STEP 2: Sanity Check ----


# Built-in FKGL and Dale–Chall (same corpus, same doc order)
readability_q <- textstat_readability(
  corp,
  measure = c("Flesch.Kincaid", "Dale.Chall", "Dale.Chall.old"),
  remove_hyphens = FALSE
)

# Quanteda readability with intermediate components for BOTH FKGL + Dale–Chall
# Compare the package word, sentence, syllable etc counts to manual counts 
readability_int <- textstat_readability(
  corp,
  measure = c("Flesch.Kincaid", "Dale.Chall"),
  intermediate = TRUE,
  remove_hyphens = FALSE
)

# Join back to df_metrics (doc_id matches quanteda docnames)
df_metrics <- df_metrics %>%
  mutate(doc_id = docnames(corp)) %>%
  left_join(
    readability_q %>%
      mutate(doc_id = docnames(corp)) %>%
      select(doc_id, 
             fk_grade_quanteda = Flesch.Kincaid, 
             dale_chall_quanteda = Dale.Chall.old,
             dale_chall_quanteda_new = Dale.Chall),
    by = "doc_id"
  )

#Check the correlation between the manual and package calculation for Dale-Chall
cor.test(df_metrics$dale_chall_manual, df_metrics$dale_chall_quanteda)

# ==============================================================================
# DATA EXPLORATION ----
# ==============================================================================

summary_metrics <- df_metrics %>%
  group_by(llm, prompt_id) %>%
  summarise(mean_word = round(mean(word_count), 0),
            mean_sentence = round(mean(sentence_count), 0),
            average_words_per_sent = round(sum(word_count)/sum(sentence_count), 0),
            mean_difficult = round(mean(n_difficult), 0),
            difficult_percentage = round(sum(n_difficult)/sum(word_count), 2)
            )

summary_readability <- df_metrics %>%
  group_by(llm, prompt_id) %>%
  summarise(mean_fkgl = round(mean(fk_grade_quanteda), 0),
            mean_dale_chall = round(mean(dale_chall_quanteda), 0)
  )


# ==============================================================================
# MODELLING ----
# ==============================================================================

#Linear Mixed-effect Model ----
library(lme4)
library(lmerTest)  # adds p-values for fixed effects
library(emmeans)

# Prep ----
df_model <- df_metrics %>%
  mutate(
    llm = factor(llm),
    prompt_id = factor(prompt_id),          
    cluster_id = factor(conversation_id)   #repeated-measures / item ID
  ) %>%
  filter(!is.na(fk_grade_quanteda), !is.na(dale_chall_quanteda))

# FKGL mixed model ----
m_fkgl <- lmer(
  fk_grade_quanteda ~ llm * prompt_id + (1 | cluster_id),
  data = df_model,
  REML = FALSE
)

summary(m_fkgl)
anova(m_fkgl)



# Dale–Chall mixed model----
m_dc <- lmer(
  dale_chall_quanteda ~ llm * prompt_id + (1 | cluster_id),
  data = df_model,
  REML = FALSE
)

summary(m_dc)
anova(m_dc)


# ==============================================================================
# DATA VISUALIZATION ----
# ==============================================================================


# FKGL: estimated marginal means ----
emm_fkgl <- emmeans(m_fkgl, ~ llm * prompt_id)
fkgl_plot_df <- as.data.frame(emm_fkgl)


# Dale–Chall: estimated marginal means----
emm_dc <- emmeans(m_dc, ~ llm * prompt_id) 
dc_plot_df <- as.data.frame(emm_dc)

# If prompt_id is numeric, convert to character first

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
       y = "Grade Level", 
       title = "Education Level Needed to Read Chatbot Response",
       caption = "Dashed line marks the Grade 8 target.") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
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
       y = "Familiarity Level", 
       title = "Vocabulary Familiarity",
       caption = "Dashed line marks the Score 40 target.") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) 



# ==============================================================================
# DATA VISUALIZATION FOR CLIENT ----
# ==============================================================================

fkgl_client_plot_df <- fkgl_plot_df %>%
  mutate(threshold_flag = if_else(emmean <= 8, "<= 8", "> 8"))

ggplot(fkgl_client_plot_df, aes(x = prompt_label, y = emmean, color = threshold_flag)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  geom_hline(yintercept = 8, linetype = "dashed", color = "gray") +
  facet_grid(llm ~ .) +
  scale_color_manual(values = c("<= 8" = "#0072B2", "> 8" = "gray55"))  +
  scale_y_continuous(breaks = scales::breaks_width(2)) +
  labs(
    x = "Style Prompt",
    y = "Grade Level",
    color = "Threshold",
    title = "Education Level Needed to Read by Prompt"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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


# ==============================================================================
# PART 2 - SERVICE QUALITY SCORES ----
# ==============================================================================

# ==============================================================================
# IMPORT DATA: EMPATHY ----
# ==============================================================================
  
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
  
  # ==============================================================================
  # MODELLING: FKGL → Empathy (Fixed-Effects OLS) ----
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
  #   fk_grade_quanteda          = Flesch–Kincaid Grade Level (higher = harder to read)
  #   I(fk_grade_quanteda^2)     = quadratic term to allow a nonlinear "sweet spot"
  #                               (e.g., empathy rises with FKGL up to a point, then falls)
  #
  # Controls / Fixed effects:
  #   factor(llm)               = controls for systematic differences across LLMs
  #                               (e.g., some models are more empathic regardless of readability)
  #   factor(prompt_id)         = controls for systematic differences across prompt templates
  #                               (style constraints, tone, brevity instructions, etc.)
  #   factor(conversation_id)   = question fixed effects (holds user intent/context constant)
  #                               so FKGL effects are identified from within-question variation
  # ==============================================================================
  
  # Fit fixed-effects OLS model with quadratic FKGL term
  fit_fk_fe_emp <- lm(
    average_empathy ~ fk_grade_quanteda + I(fk_grade_quanteda^2) +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
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
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  summary(fit_llm_prompt_fe_emp)
  #------------------------------------------------------------
  # CHECK REGRESSION ASSUMPTIONS / ROBUSTNESS: FKGL → EMPATHY ----
  # Model context:
  #   DV: average_empathy (1–5 Likert)
  #   Key IVs: fk_grade_quanteda and fk_grade_quanteda^2 (quadratic specification)
  #   Controls: llm, prompt_id, and conversation_id fixed effects
  #------------------------------------------------------------
  
  # 0) Load packages used in diagnostics
  library(car)       # crPlot(), influencePlot(), vif()
  library(lmtest)    # coeftest(), bptest()
  library(sandwich)  # vcovHC(), vcovCL()
  library(splines)   # ns()
  
  #------------------------------------------------------------
  # A) FUNCTIONAL FORM (SHAPE): Is the FKGL→empathy relationship nonlinear? ----
  # What this checks:
  #   Whether a simple linear term is enough, or whether a nonlinear shape is supported.
  # How to interpret:
  #   - crPlot: visual "partial relationship" after controlling for other predictors.
  #   - anova(linear vs spline): tests whether allowing curvature improves fit.
  #------------------------------------------------------------
  
  # A1) Visual partial residual plot for FKGL (after controls) ----
  crPlot(fit_fk_fe_emp, variable = "fk_grade_quanteda")
  
  # A2) Fit a spline model (non-parametric curvature) and compare to linear FKGL model ----
  # NOTE: fit_fk_fe_emp_lin should be your linear-only FKGL FE model:
 fit_fk_fe_emp_lin <- lm(average_empathy ~ fk_grade_quanteda +
                         factor(llm) + factor(prompt_id) + factor(conversation_id),
                        data = df_empathy)
  
  fit_fk_spline <- lm(
    average_empathy ~ ns(fk_grade_quanteda, df = 3) +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  
  # Does the spline (curvature) fit better than a straight line?
  anova(fit_fk_fe_emp_lin, fit_fk_spline)
  
  #------------------------------------------------------------
  # B) COLLINEARITY: Are FKGL and FKGL^2 too correlated to estimate reliably? ----
  # What this checks:
  #   Quadratic models often create high correlation between X and X^2.
  # Why centering:
  #   Centering FKGL reduces correlation between fk_c and fk_c^2, stabilizing estimates.
  # How to interpret:
  #   - VIF values near 1–2 indicate low collinearity concern.
  #------------------------------------------------------------
  
  # B1) Center FKGL (subtract mean; do NOT scale/standardize) ----
  df_empathy$fk_c <- scale(df_empathy$fk_grade_quanteda, center = TRUE, scale = FALSE)
  
  # B2) Refit quadratic model using centered FKGL ----
  fit_fk_fe_emp_c <- lm(
    average_empathy ~ fk_c + I(fk_c^2) +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  
  # B3) Report coefficients with heteroskedasticity-robust SEs (HC3) ----
  coeftest(fit_fk_fe_emp_c, vcov = vcovHC(fit_fk_fe_emp_c, type = "HC3"))
  
  # B4) Check collinearity (use the adjusted GVIF column for factor variables) ----
  vif(fit_fk_fe_emp_c)
  
  #------------------------------------------------------------
  # C) HETEROSKEDASTICITY: Is error variance non-constant? ----
  # What this checks:
  #   Whether residual spread changes across observations (common with 1–5 outcomes).
  # How to interpret:
  #   - bptest significant → heteroskedasticity likely.
  # What to do:
  #   - Use robust SEs (HC3) for inference (already done above).
  #------------------------------------------------------------
  
  bptest(fit_fk_fe_emp)
  
  #------------------------------------------------------------
  # D) WITHIN-QUESTION DEPENDENCE: Are errors correlated within conversation_id? ----
  # What this does (NOT a "test"):
  #   Uses cluster-robust SEs so inference remains valid if within-question correlation exists.
  # How to interpret:
  #   If FKGL terms remain significant with clustered SEs, results are robust to clustering.
  #------------------------------------------------------------
  
  vcov_fk_cl <- vcovCL(fit_fk_fe_emp, cluster = df_empathy$conversation_id)
  coeftest(fit_fk_fe_emp, vcov = vcov_fk_cl)
  
  # Optional diagnostic (quick check): residual correlation with within-question mean residual ----
  # Interpretation:
  #   Near 0 → little detectable within-question residual correlation.
  res <- resid(fit_fk_fe_emp)
  icc_like <- cor(res, ave(res, df_empathy$conversation_id), use = "complete.obs")
  icc_like
  
  #------------------------------------------------------------
  # E) INFLUENCE / OUTLIERS: Are a few observations driving the FKGL result? ----
  # What this checks:
  #   Whether any points have high leverage / large residuals / high Cook’s distance.
  # How to interpret:
  #   Large bubbles / labeled IDs suggest influential points worth sensitivity-testing.
  #------------------------------------------------------------
  
  influencePlot(fit_fk_fe_emp, id.method = "identify")
  
  # E1) Sensitivity analysis: refit after removing influential rows found above
  # NOTE: Update drop_ids based on what influencePlot identifies for YOUR model.
  drop_ids <- c(29, 107, 123, 247)
  
  df_sub <- df_empathy[-drop_ids, ]
  
  fit_fk_sub <- lm(
    average_empathy ~ fk_grade_quanteda + I(fk_grade_quanteda^2) +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_sub
  )
  
  # Robust SEs for the sensitivity model
  coeftest(fit_fk_sub, vcov = vcovHC(fit_fk_sub, type = "HC3"))
  summary(fit_fk_sub)
  
  #------------------------------------------------------------
  # F) NORMALITY OF RESIDUALS (LOW PRIORITY WITH ROBUST SEs) ----
  # What this checks:
  #   Whether residuals follow a normal distribution (often rejected with larger n).
  # How to interpret:
  #   Minor deviations are not usually a problem when using robust/clustered SEs.
  #------------------------------------------------------------
  
  res <- resid(fit_fk_fe_emp)
  
  # F1) Visual check ----
  qqnorm(res); qqline(res)
  
  # F2) Formal test (often rejects even for small deviations) ----
  shapiro.test(res)
  
  #------------------------------------------------------------
  # G) SUBGROUP ROBUSTNESS: Does the FKGL effect differ by model (LLM)? ----
  # What this checks:
  #   Whether the FKGL→empathy relationship is consistent across LLMs.
  # How to interpret:
  #   Differences suggest the readability/empathy link is model-dependent.
  #------------------------------------------------------------
  
  by(df_empathy, df_empathy$llm, function(d)
    summary(lm(
      average_empathy ~ fk_grade_quanteda + I(fk_grade_quanteda^2) +
        factor(prompt_id) + factor(conversation_id),
      data = d
    ))
  )

  
  #------------------------------------------------------------
  # CHECK REGRESSION ASSUMPTIONS / ROBUSTNESS: DALE–CHALL → EMPATHY ----
  # Model context:
  #   DV: average_empathy (1–5 Likert)
  #   Key IVs: dale_chall_quanteda and dale_chall_quanteda^2 (quadratic specification)
  #   Controls: llm, prompt_id, and conversation_id fixed effects
  #------------------------------------------------------------
  
  # 0) Packages used in diagnostics loaded in the previous code block for FKGL
  
  #------------------------------------------------------------
  # 1) FIT BASE MODELS (LINEAR + QUADRATIC) FOR DALE–CHALL ----
  #------------------------------------------------------------
  
  # 1A) Linear Dale–Chall FE model (needed for model comparison) ----
  # MODELLING: Dale-Chall → Empathy (Fixed-Effects OLS) ----
  
  fit_dc_fe_emp_lin <- lm(
    average_empathy ~ dale_chall_quanteda +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  
  # 1B) Quadratic Dale–Chall FE model (your main spec) ----
  fit_dc_fe_emp <- lm(
    average_empathy ~ dale_chall_quanteda + I(dale_chall_quanteda^2) +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  
  # Robust inference (HC3) for the quadratic model ----
  coeftest(fit_dc_fe_emp, vcov = vcovHC(fit_dc_fe_emp, type = "HC3"))
  summary(fit_dc_fe_emp)  # descriptive; uses classical SEs
  
  #------------------------------------------------------------
  # A) FUNCTIONAL FORM (SHAPE): Is the Dale–Chall→empathy relationship nonlinear?
  #------------------------------------------------------------
  
  # A1) Visual partial residual plot for Dale–Chall (after controls) ----
  crPlot(fit_dc_fe_emp, variable = "dale_chall_quanteda")
  
  # A2) Spline model (flexible curvature) and compare to linear model ----
  fit_dc_spline <- lm(
    average_empathy ~ ns(dale_chall_quanteda, df = 3) +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  
  # Does the spline fit better than the straight line? ----
  anova(fit_dc_fe_emp_lin, fit_dc_spline)

  #------------------------------------------------------------
  # B) COLLINEARITY (LINEAR SPEC): Is Dale–Chall too correlated with other predictors? ----
  # Note:
  #   Since the spline test suggested no meaningful nonlinearity, we use the LINEAR
  #   Dale–Chall model going forward. With no squared term, centering is optional
  #   and "Dale–Chall vs Dale–Chall^2 collinearity" is no longer relevant.
  # What we check instead:
  #   General multicollinearity via VIF/GVIF for the linear FE model.
  #------------------------------------------------------------
  
  # Main (linear) Dale–Chall FE model ----
  fit_dc_fe_emp_lin <- lm(
    average_empathy ~ dale_chall_quanteda +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  
  # VIF/GVIF collinearity check (use GVIF^(1/(2*Df)) column for factors) ----
  vif(fit_dc_fe_emp_lin)
  
  #------------------------------------------------------------
  # C) HETEROSKEDASTICITY: Is residual variance non-constant? ----
  # If significant:
  #   Use robust SEs (HC3) for inference (recommended with 1–5 DV).
  #------------------------------------------------------------
  
  bptest(fit_dc_fe_emp_lin)
  
  # Robust inference (HC3) for the linear model ----
  summary(fit_dc_fe_emp_lin)  # descriptive (classical SEs)
  coeftest(fit_dc_fe_emp_lin, vcov = vcovHC(fit_dc_fe_emp_lin, type = "HC3")) #robust
  
  

  
  #------------------------------------------------------------
  # D) WITHIN-QUESTION DEPENDENCE: Cluster-robust SEs by conversation_id ----
  # What this does:
  #   Makes standard errors valid if residuals are correlated within question.
  #------------------------------------------------------------
  
  vcov_dc_cl <- vcovCL(fit_dc_fe_emp_lin, cluster = df_empathy$conversation_id)
  coeftest(fit_dc_fe_emp_lin, vcov = vcov_dc_cl)
  
  # Optional diagnostic: residual correlation with within-question mean residual ----
  res_dc <- resid(fit_dc_fe_emp_lin)
  icc_like_dc <- cor(res_dc, ave(res_dc, df_empathy$conversation_id), use = "complete.obs")
  icc_like_dc
  
  #------------------------------------------------------------
  # E) INFLUENCE / OUTLIERS: Are a few observations driving the Dale–Chall result? ----
  #------------------------------------------------------------
  
  library(car)
  influencePlot(fit_dc_fe_emp_lin, id.method = "identify")
  
  # E1) Sensitivity analysis: refit after removing influential rows flagged above ----
  # NOTE: Replace IDs with the ones flagged for YOUR linear Dale–Chall model.
  drop_ids_dc <- c(29, 107, 123, 247)  # <- update after viewing influencePlot
  
  df_sub_dc <- df_empathy[-drop_ids_dc, ]
  
  fit_dc_sub_lin <- lm(
    average_empathy ~ dale_chall_quanteda +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_sub_dc
  )
  
  # Robust SEs for the sensitivity model ----
  coeftest(fit_dc_sub_lin, vcov = vcovHC(fit_dc_sub_lin, type = "HC3"))
  summary(fit_dc_sub_lin)
  
  #------------------------------------------------------------
  # F) NORMALITY OF RESIDUALS (LOW PRIORITY WITH ROBUST/CLUSTERED SEs) ----
  #------------------------------------------------------------
  
  res_dc <- resid(fit_dc_fe_emp_lin)
  qqnorm(res_dc); qqline(res_dc)
  shapiro.test(res_dc)
  
  #------------------------------------------------------------
  # G) SUBGROUP ROBUSTNESS: Does the Dale–Chall effect differ by LLM? ----
  # Note:
  #   Keep the same linear specification within each LLM.
  #------------------------------------------------------------
  
  by(df_empathy, df_empathy$llm, function(d)
    summary(lm(
      average_empathy ~ dale_chall_quanteda +
        factor(prompt_id) + factor(conversation_id),
      data = d
    ))
  )
  
# ==============================================================================
# DATA VISUALIZATION ----
# ==============================================================================
  
  visreg(fit_fk_fe_emp, "fk_grade_quanteda", gg = TRUE, partial = FALSE, rug = FALSE) +
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
      subtitle = "High-school reading level responses score as most empathetic."
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

    
  #Dale-Chall only + fixed effects Visual  
  visreg(fit_dc_fe_emp_lin, "dale_chall_quanteda", gg = TRUE, partial = TRUE) +
    labs(
      x = "Dale-Chall score",
      y = "Empathy",
      title = "Complex Vocabulary Hurts Empathy",
      subtitle = "Chatbot is more empathetic when language is high school grade"
    ) +
    theme_minimal()  
  
  
  visreg(fit_dc_fe_emp_lin, "dale_chall_quanteda", gg = TRUE, partial = FALSE, rug = FALSE) +
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
      x = "New Dale-Chall Score",
      y = "Predicted Empathy Score",
      title = "Complex Vocabulary Hurts Empathy",
      subtitle = "High-school reading level responses score as most empathetic."
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
  
  

  
#------------------------------

  troubleshoot <- df_empathy %>%
    filter(as.numeric(as.character(row_id)) == 62) %>%
    select(-c(23, 24))
  
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
    coef_omit = "conversation_id",      # hide question fixed effects
    statistic = "({std.error})",        # show robust SEs in parentheses
    stars = TRUE,
    gof_omit = "Adj|F|Log|RMSE",        # optional: simplify GOF panel
    title = "Appendix B1. Fixed-Effects OLS Models Predicting Empathy (HC3 Robust SEs)",
    notes = "Question fixed effects included but not shown. Robust (HC3) standard errors in parentheses.",
    output = results_file("Appendix_B1_Main_Regression_Results.docx")
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
    coef_omit = "conversation_id",   # hide question fixed effects
    statistic = "({std.error})",
    stars = TRUE,
    title = "Appendix B2. Sensitivity Analysis: Drop Influential Observations (HC3 Robust SEs)",
    notes = "Question fixed effects included but not shown. Robust (HC3) standard errors in parentheses.",
    output = results_file("Appendix_B2_Sensitivity_Drop_Influential.docx")
  )

  # Appendix B3
  # Cluster-robust SE (by conversation_id) regression table
  # Exports Word (.docx) regression table
  
  
  # Clustered variance-covariance matrices (clustered by conversation_id)
  vc_fk_cluster <- vcovCL(fit_fk_fe_emp, cluster = df_empathy$conversation_id)
  vc_dc_cluster <- vcovCL(fit_dc_fe_emp_lin, cluster = df_empathy$conversation_id)
  
  modelsummary(
    list(
      "FKGL FE (Clustered by Question)" = fit_fk_fe_emp,
      "Dale–Chall FE (Clustered by Question)" = fit_dc_fe_emp_lin
    ),
    vcov = list(vc_fk_cluster, vc_dc_cluster),
    coef_omit = "conversation_id",   # hide question fixed effects
    statistic = "({std.error})",
    stars = TRUE,
    title = "Appendix B3. Cluster-Robust Standard Errors (Clustered by Question)",
    notes = "Question fixed effects included but not shown. Standard errors clustered by conversation_id.",
    output = results_file("Appendix_B3_Clustered_SE.docx")
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
  
  #print(doc, target = results_file("Appendix_D1_VIF_GVIF.docx"))  
  
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
  
  #print(doc, target = results_file("Appendix_D2_Breusch_Pagan.docx"))
  
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
  
  #print(doc, target = results_file("Appendix_D4_Residual_Normality.docx"))
 
  
# Appendix E2
# Subgroup-by-LLM table: Dale–Chall term
# Exports Word (.docx) table


# Split data by LLM and fit the same linear model within each
models_dc_llm <- split(df_empathy, df_empathy$llm) %>%
  imap(~ lm(
    average_empathy ~ dale_chall_quanteda +
      factor(prompt_id) + factor(conversation_id),
    data = .x
  ))

# Extract only the Dale–Chall coefficient from each model
dc_subgroup_table <- imap_dfr(models_dc_llm, ~ tidy(.x) %>%
  filter(term == "dale_chall_quanteda") %>%
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

#print(doc, target = results_file("Appendix_E2_DaleChall_Subgroup_by_LLM.docx"))

##############
  test_indiv <- test3_prompt2_newmodel %>%
  select(context, 
         content, 
         openai_gpt_5_4_mini__individual_attention_score, 
         openai_gpt_5_4_mini__individual_attention_reasoning, 
         google_gemini_2_5_flash__individual_attention_score,
         google_gemini_2_5_flash__individual_attention_reasoning
        ) %>%
  filter(openai_gpt_5_4_mini__individual_attention_score != google_gemini_2_5_flash__individual_attention_score)


test_care <- test3_prompt2_newmodel %>%
  select(context, 
         content, 
         openai_gpt_5_4_mini__caring_tone_score, 
         openai_gpt_5_4_mini__caring_tone_reasoning, 
         google_gemini_2_5_flash__caring_tone_score,
         google_gemini_2_5_flash__caring_tone_reasoning
  ) %>%
  filter(openai_gpt_5_4_mini__caring_tone_score != google_gemini_2_5_flash__caring_tone_score)


test_first <- test3_prompt2_newmodel %>%
  select(context, 
         content, 
         openai_gpt_5_4_mini__customer_first_orientation_score, 
         openai_gpt_5_4_mini__customer_first_orientation_reasoning, 
         google_gemini_2_5_flash__customer_first_orientation_score,
         google_gemini_2_5_flash__customer_first_orientation_reasoning
  ) %>%
  filter(openai_gpt_5_4_mini__customer_first_orientation_score != google_gemini_2_5_flash__customer_first_orientation_score)

test_need <- test3_prompt2_newmodel %>%
  select(context, 
         content, 
         openai_gpt_5_4_mini__need_understanding_score, 
         openai_gpt_5_4_mini__need_understanding_reasoning, 
         google_gemini_2_5_flash__need_understanding_score,
         google_gemini_2_5_flash__need_understanding_reasoning
  ) %>%
  filter(openai_gpt_5_4_mini__need_understanding_score != google_gemini_2_5_flash__need_understanding_score)

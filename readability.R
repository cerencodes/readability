library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(stringi)
library(data.table)
library(textstem)

# Prefer a dedicated data folder when present, but keep root-level fallback
# so the script remains portable across old and new repo layouts.
data_dir <- if (dir.exists("data")) "data" else "."
data_file <- function(filename) file.path(data_dir, filename)




# ==============================================================================
# IMPORT DATA 
# 
# ==============================================================================

#Import dataset containing questions
conversations <- read.csv(data_file("sample_qa.csv"))

#Import Baseline Prompt datasets
df_baseline_p0p1 <- read_csv(data_file("20260223_temp0_topk5_all_p0p1.csv"))
df_baseline_p0p1$prompt_id[df_baseline_p0p1$prompt_id == "p0"] <- "0"
df_baseline_p0p1$prompt_id[df_baseline_p0p1$prompt_id == "p1"] <- "1"

df_baseline_p2p3 <- read_csv(data_file("20260223_temp0_topk5_all_p2p3.csv"))
df_baseline_p2p3$prompt_id[df_baseline_p2p3$prompt_id == "p2"] <- "2"
df_baseline_p2p3$prompt_id[df_baseline_p2p3$prompt_id == "p3"] <- "3"

df_baseline_p4 <- read_csv(data_file("20260223_temp0_topk5_all_p4.csv"))
df_baseline_p4$prompt_id[df_baseline_p4$prompt_id == "p4"] <- "4"

#Import Readability Prompt datasets
df_readability_p01_p11 <- read_csv(data_file("20260224_temp0_topk5_all_p0.1p1.1.csv"))
df_readability_p01_p11$prompt_id[df_readability_p01_p11$prompt_id == "A"] <- "0.1"
df_readability_p01_p11$prompt_id[df_readability_p01_p11$prompt_id == "B"] <- "1.1"

df_readability_p02_p12 <- read_csv(data_file("20260303_temp0_topk5_all_p0.2p1.2.csv"))
df_readability_p02_p12$prompt_id[df_readability_p02_p12$prompt_id == "A"] <- "0.2"
df_readability_p02_p12$prompt_id[df_readability_p02_p12$prompt_id == "B"] <- "1.2"

df_readability_p21_p31 <- read_csv(data_file("20260224_temp0_topk5_all_p2.1p3.1.csv"))
df_readability_p21_p31 $prompt_id[df_readability_p21_p31$prompt_id == "A"] <- "2.1"
df_readability_p21_p31 $prompt_id[df_readability_p21_p31 $prompt_id == "B"] <- "3.1"


df_readability_p41 <- read_csv(data_file("20260224_temp0_topk5_all_p4.1.csv"))
df_readability_p41 <- df_readability_p41[df_readability_p41$prompt_id != "B", ]
df_readability_p41$prompt_id[df_readability_p41$prompt_id == "A"] <- "4.1"




#Combine all datasets
df_all <- bind_rows(
  df_baseline_p0p1,
  df_baseline_p2p3,
  df_baseline_p4,
  df_readability_p01_p11,
  df_readability_p02_p12,
  df_readability_p21_p31,
  df_readability_p41
)

#Combine the datasets that will be used in analysis (Prompt 0, Prompt 1 and variants)
df_1 <- bind_rows(
  df_baseline_p0p1,
  df_readability_p01_p11,
  df_readability_p02_p12
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
  transmute(row_id, context, conversation_id,llm, prompt_id, content = as.character(content))

#Write to csv 
#write_csv(df_assistant, "df_assistant.csv")

# ==============================================================================
# DATA CLEANING
# 
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
# PART 1 - READABILITY SCORES
# 
# ==============================================================================




# ==============================================================================
# DATA ANALYSIS
# Calculate readability scores
# ==============================================================================


#STEP 1: Manual Readability Calculations For Comparison

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

#perform manual dale-chall calculation to validate package calculation
df_metrics <- df_metrics %>%
  mutate(
    n_familiar = n_familiar,
    n_difficult = n_difficult,
    dale_chall_manual =
      64 - (0.95 * (n_difficult / word_count * 100)) -
      (0.69 * word_count / sentence_count)
  )


#STEP 2: Sanity Check


# Built-in FKGL and Dale–Chall (same corpus, same doc order)
readability_q <- textstat_readability(
  corp,
  measure = c("Flesch.Kincaid", "Dale.Chall"),
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
      select(doc_id, fk_grade_quanteda = Flesch.Kincaid, dale_chall_quanteda = Dale.Chall),
    by = "doc_id"
  )

# STEP 3: Additional readability metrics (MTLD + HD-D) using quanteda tokens

calc_mtld_direction <- function(tokens_vec, ttr_threshold = 0.72) {
  n <- length(tokens_vec)
  if (n == 0) return(NA_real_)
  
  token_count <- 0L
  type_count <- 0L
  factors <- 0
  seen <- character(0)
  
  for (tok in tokens_vec) {
    token_count <- token_count + 1L
    if (!(tok %in% seen)) {
      seen <- c(seen, tok)
      type_count <- type_count + 1L
    }
    
    ttr <- type_count / token_count
    if (ttr <= ttr_threshold) {
      factors <- factors + 1
      token_count <- 0L
      type_count <- 0L
      seen <- character(0)
    }
  }
  
  if (token_count > 0) {
    ttr <- type_count / token_count
    partial_factor <- (1 - ttr) / (1 - ttr_threshold)
    factors <- factors + partial_factor
  }
  
  if (factors == 0) return(NA_real_)
  n / factors
}

calc_mtld <- function(tokens_vec, ttr_threshold = 0.72) {
  fwd <- calc_mtld_direction(tokens_vec, ttr_threshold = ttr_threshold)
  rev <- calc_mtld_direction(rev(tokens_vec), ttr_threshold = ttr_threshold)
  mean(c(fwd, rev), na.rm = TRUE)
}

calc_hdd <- function(tokens_vec, sample_size = 42L) {
  n <- length(tokens_vec)
  if (n == 0) return(NA_real_)
  
  sample_n <- min(sample_size, n)
  freq <- table(tokens_vec)
  f <- as.numeric(freq)
  
  p_at_least_once <- 1 - exp(lchoose(n - f, sample_n) - lchoose(n, sample_n))
  sum(p_at_least_once) / sample_n
}

lexdiv_metrics <- tibble(
  doc_id = docnames(corp),
  token_count = quanteda::ntoken(toks_norm),
  mtld = vapply(as.list(toks_norm), calc_mtld, numeric(1)),
  hdd = vapply(as.list(toks_norm), calc_hdd, numeric(1))
)

df_metrics <- df_metrics %>%
  left_join(lexdiv_metrics, by = "doc_id")



# ==============================================================================
# QA Step
# Manual Scores vs Quanteda Calculated Scores using text295 as example
# ==============================================================================


tol <- 1e-8

#Find rows where manual dale-chall =/ package dale-chall value
df_mismatch <- df_metrics %>%
  mutate(diff = dale_chall_manual - dale_chall_quanteda) %>%
  filter(is.na(dale_chall_manual) != is.na(dale_chall_quanteda) |
           abs(diff) > tol) %>%
  arrange(desc(abs(diff)))

#join docid
df_mismatch <- df_mismatch %>%
  left_join(
    readability_int %>%
      mutate(doc_id = docnames(corp)) %>%
      select(doc_id, n_difficult_quanteda = W_wl.Dale.Chall),
    by = "doc_id"
  ) %>%
  relocate(n_difficult_quanteda, .after = n_difficult)

df_mismatch

 # Manual familiar words for text295 (uses your manual normalized pipeline)
doc_target <- "text295"
i <- which(docnames(corp) == doc_target)[1]

manual_familiar_text295 <- tibble::tibble(token = as.character(toks_norm[[i]])) %>%
  dplyr::count(token, name = "n") %>%
  dplyr::filter(token %in% dc_familiar) %>%
  dplyr::arrange(dplyr::desc(n), token)

manual_familiar_text295

# Quanteda familiar words for text295 (raw quanteda-style list match)
doc_target <- "text295"

tok_q <- quanteda::tokens(
  corp[doc_target],
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_separators = TRUE
) %>%
  quanteda::tokens_tolower()

dc_familiar_q <- tolower(data_char_wordlists$dalechall)

quanteda_familiar_text295 <- tibble::tibble(token = as.character(tok_q[[1]])) %>%
  dplyr::count(token, name = "n") %>%
  dplyr::filter(token %in% dc_familiar_q) %>%
  dplyr::arrange(dplyr::desc(n), token)

quanteda_familiar_text295



# ==============================================================================
# DATA EXPLORATION
# 
# ==============================================================================

summary_metrics <- df_metrics %>%
  group_by(llm, prompt_id) %>%
  summarise(mean_word = mean(word_count),
            mean_sentence = mean(sentence_count),
            average_words_per_sent = sum(word_count)/sum(sentence_count),
            mean_difficult = mean(n_difficult),
            difficult_percentage = sum(n_difficult)/sum(word_count)
            )

summary_readability <- df_metrics %>%
  group_by(llm, prompt_id) %>%
  summarise(mean_fkgl = round(mean(fk_grade_quanteda), 2),
            #mean_dale_chall = round(mean(dale_chall_quanteda), 2),
            #mean_mtld = mean(mtld, na.rm = TRUE),
            #mean_hdd = mean(hdd, na.rm = TRUE)
  )

# Token-count distribution across texts (for MTLD suitability checks)
token_count_distribution <- df_metrics %>%
  summarise(
    n_texts = n(),
    min_tokens = min(token_count, na.rm = TRUE),
    q1_tokens = quantile(token_count, 0.25, na.rm = TRUE),
    median_tokens = median(token_count, na.rm = TRUE),
    mean_tokens = mean(token_count, na.rm = TRUE),
    q3_tokens = quantile(token_count, 0.75, na.rm = TRUE),
    max_tokens = max(token_count, na.rm = TRUE)
  )

token_count_above_100 <- df_metrics %>%
  summarise(
    n_texts = n(),
    n_above_100 = sum(token_count > 100, na.rm = TRUE),
    pct_above_100 = 100 * n_above_100 / n_texts
  )

token_count_above_100_by_group <- df_metrics %>%
  group_by(llm, prompt_id) %>%
  summarise(
    n_texts = n(),
    n_above_100 = sum(token_count > 100, na.rm = TRUE),
    pct_above_100 = 100 * n_above_100 / n_texts,
    .groups = "drop"
  )

token_count_bins <- df_metrics %>%
  mutate(
    token_bin = cut(
      token_count,
      breaks = c(-Inf, 50, 100, 150, 200, Inf),
      labels = c("<=50", "51-100", "101-150", "151-200", "200+"),
      right = TRUE
    )
  ) %>%
  count(token_bin, name = "n_texts") %>%
  mutate(pct = 100 * n_texts / sum(n_texts))

# Optional quick plot of token-count distribution
ggplot(df_metrics, aes(x = token_count)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
  labs(x = "Token count per text", y = "Number of texts")


cor.test(df_metrics$dale_chall_manual, df_metrics$dale_chall_quanteda)


# ==============================================================================
# MODELLING
# 
# ==============================================================================

#Linear Mixed-effect Model
library(lme4)
library(lmerTest)  # adds p-values for fixed effects
library(emmeans)

# Prep 
df_model <- df_metrics %>%
  mutate(
    llm = factor(llm),
    prompt = factor(prompt_id),          
    cluster_id = factor(conversation_id)   #repeated-measures / item ID
  ) %>%
  filter(!is.na(fk_grade_quanteda), !is.na(dale_chall_quanteda))

# FKGL mixed model
m_fkgl <- lmer(
  fk_grade_quanteda ~ llm * prompt + (1 | cluster_id),
  data = df_model,
  REML = FALSE
)

summary(m_fkgl)
anova(m_fkgl)

# Dale–Chall mixed model
m_dc <- lmer(
  dale_chall_quanteda ~ llm * prompt + (1 | cluster_id),
  data = df_model,
  REML = FALSE
)

summary(m_dc)
anova(m_dc)

# MTLD mixed model
df_model_mtld <- df_model %>%
  filter(!is.na(mtld))

m_mtld <- lmer(
  mtld ~ llm * prompt + (1 | cluster_id),
  data = df_model_mtld,
  REML = FALSE
)

summary(m_mtld)
anova(m_mtld)

# HD-D mixed model
df_model_hdd <- df_model %>%
  filter(!is.na(hdd))

m_hdd <- lmer(
  hdd ~ llm * prompt + (1 | cluster_id),
  data = df_model_hdd,
  REML = FALSE
)

summary(m_hdd)
anova(m_hdd)

# Optional: pairwise comparisons
emmeans(m_fkgl, ~ llm | prompt) |> pairs(adjust = "tukey")
emmeans(m_dc,   ~ llm | prompt) |> pairs(adjust = "tukey")
emmeans(m_mtld, ~ llm | prompt) |> pairs(adjust = "tukey")
emmeans(m_hdd,  ~ llm | prompt) |> pairs(adjust = "tukey")





# ==============================================================================
# DATA VISUALIZATION
# 
# ==============================================================================


# FKGL: estimated marginal means
emm_fkgl <- emmeans(m_fkgl, ~ llm * prompt)
fkgl_plot_df <- as.data.frame(emm_fkgl)


# Dale–Chall: estimated marginal means
emm_dc <- emmeans(m_dc, ~ llm * prompt)
dc_plot_df <- as.data.frame(emm_dc)

# If prompt is numeric, convert to character first

fkgl_plot_df <- fkgl_plot_df %>%
  mutate(
    llm = case_when(
      str_detect(str_to_lower(as.character(llm)), "claude") ~ "Claude",
      str_detect(str_to_lower(as.character(llm)), "gemini") ~ "Gemini",
      str_detect(str_to_lower(as.character(llm)), "chatgpt|gpt") ~ "ChatGPT",
      TRUE ~ as.character(llm)
    ),
    llm = factor(llm, levels = c("Claude", "Gemini", "ChatGPT")),
    prompt = as.character(prompt),
    prompt_label = case_when(
      prompt == "0.2" ~ "Baseline", #no sentence limit
      prompt == "0.1" ~ "Base w/ readability",
      prompt == "0" ~ "Base w/ sent. limit",
      prompt == "1.2" ~ "Base w/ Key Spec", #no sentence limit
      prompt == "1.1" ~ "Key Sp w/ readability",
      prompt == "1" ~ "Key Sp w/ sent. limit",
      TRUE ~ prompt
    ),
    prompt_label = factor(
      prompt_label,
      levels = c(
        "Baseline",
        "Base w/ readability",
        "Base w/ sent. limit",
        "Base w/ Key Spec",
        "Key Sp w/ readability",
        "Key Sp w/ sent. limit"
      )
    ),
    prompt_group = stringr::str_replace(prompt, "\\.[12]$", ""),
    prompt_group = case_when(
      prompt_group == "0" ~ "Baseline",
      prompt_group == "1" ~ "Baseline w/ Key Spec",
      TRUE ~ paste0("Prompt ", prompt_group)
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
    prompt = as.character(prompt),
    prompt_label = case_when(
      prompt == "0.2" ~ "Baseline", #no sentence limit
      prompt == "0.1" ~ "Base w/ readability",
      prompt == "0" ~ "Base w/ sent. limit",
      prompt == "1.2" ~ "Base w/ Key Spec", #no sentence limit
      prompt == "1.1" ~ "Key Sp w/ readability",
      prompt == "1" ~ "Key Sp w/ sent. limit",
      TRUE ~ prompt
    ),
    prompt_label = factor(
      prompt_label,
      levels = c(
        "Baseline",
        "Base w/ readability",
        "Base w/ sent. limit",
        "Base w/ Key Spec",
        "Key Sp w/ readability",
        "Key Sp w/ sent. limit"
      )
    ),
    prompt_group = stringr::str_replace(prompt, "\\.[12]$", ""),
    prompt_group = case_when(
      prompt_group == "0" ~ "Baseline",
      prompt_group == "1" ~ "Baseline w/ Key Spec",
      TRUE ~ paste0("Prompt ", prompt_group)
    )
  )

# Facet by LLM and grouped prompts; color by prompt (keeps 0 vs 0.1 distinct)
ggplot(fkgl_plot_df, aes(x = prompt_label, y = emmean, color = prompt_group)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  scale_color_manual(values = c(
    "Baseline" = "#E69F00",
    "Baseline w/ Key Spec" = "#56B4E9"
  )) +
  facet_grid(llm ~ prompt_group, scales = "free_x", space = "free_x") +
  labs(x = "Prompt", 
       y = "Grade Level", 
       color = "Prompt Group", 
       title = "Education Level Needed to Read Chatbot Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(dc_plot_df, aes(x = prompt_label, y = emmean, color = prompt_group)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  scale_color_manual(values = c(
    "Baseline" = "#E69F00",
    "Baseline w/ Key Spec" = "#56B4E9"
  )) +
  facet_grid(llm ~ prompt_group, scales = "free_x", space = "free_x") +
  labs(x = "Prompt", 
       y = "Dale-Chall Index", 
       color = "Prompt Group", 
       title = "Education Level Needed to Read Chatbot Response") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




# ==============================================================================
# DATA VISUALIZATION FOR CLIENT
# 
# ==============================================================================

fkgl_client_plot_df <- fkgl_plot_df %>%
  mutate(threshold_flag = if_else(emmean <= 8, "<= 8", "> 8"))

ggplot(fkgl_client_plot_df, aes(x = prompt_label, y = emmean, color = threshold_flag)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  facet_wrap(~ llm) +
  scale_color_manual(values = c("<= 8" = "#0072B2", "> 8" = "gray55")) +
  labs(
    x = "Prompt",
    y = "Grade Level",
    color = "Threshold",
    title = "Readability by Model and Prompt"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dc_client_plot_df <- dc_plot_df %>%
  mutate(threshold_flag = if_else(emmean <= 40, "<= 40", "> 40"))

ggplot(dc_client_plot_df, aes(x = prompt_label, y = emmean, color = threshold_flag)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  facet_wrap(~ llm) +
  scale_color_manual(values = c("<= 40" = "#0072B2", "> 40" = "gray55")) +
  labs(
    x = "Prompt",
    y = "Dale-Chall Index",
    color = "Threshold",
    title = "Client View: Readability by Prompt"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# PART 2 - SERVICE QUALITY SCORES
# 
# ==============================================================================


# ==============================================================================
# IMPORT DATA: CLARITY 
# 
# ==============================================================================


#Import customerbot dataset for clarity
clarity <- read_csv(data_file("customerbot-results.csv"))

#Join clarity metrics to the main dataset
df_clarity <- df_metrics %>%
  left_join(
    clarity %>%
      select(
        row_id,
        `openai/gpt-4o-mini_score`,
        `anthropic/claude-3.5-haiku_score`
      ),
    by = "row_id"
  ) %>%
  rename(
    chatgpt_clarity = `openai/gpt-4o-mini_score`,
    claude_clarity = `anthropic/claude-3.5-haiku_score`
  ) %>%
  mutate(
    average_clarity = rowMeans(
      pick(chatgpt_clarity, claude_clarity),
      na.rm = TRUE
    )
  )


# ==============================================================================
# MODELLING
# 
# ==============================================================================



library(lmtest)
library(sandwich)

#FK only + fixed effects

fit_fk_fe <- lm(average_clarity ~ fk_grade_manual + I(fk_grade_manual^2) +
                  factor(llm) + factor(prompt_id) + factor(conversation_id),
                data=df_clarity)

coeftest(fit_fk_fe, vcov=vcovHC(fit_fk_fe, type="HC3"))


#Dale-Chall only + fixed effects
fit_dc_fe <- lm(
  average_clarity ~ dale_chall_quanteda + I(dale_chall_quanteda^2) +
    factor(llm) + factor(prompt_id) + factor(conversation_id),
  data = df_clarity
)

coeftest(fit_dc_fe, vcov = vcovHC(fit_dc_fe, type = "HC3"))


#H-DD only + fixed effects
fit_hdd_fe <- lm(
  average_clarity ~ hdd + I(hdd^2) +
    factor(llm) + factor(prompt_id) + factor(conversation_id),
  data = df_clarity
)

coeftest(fit_hdd_fe, vcov = vcovHC(fit_hdd_fe, type = "HC3"))


#MTLD only + fixed effects
fit_mtld_fe <- lm(
  average_clarity ~ mtld + I(mtld^2) +
    factor(llm) + factor(prompt_id) + factor(conversation_id),
  data = df_clarity
)

coeftest(fit_mtld_fe, vcov = vcovHC(fit_mtld_fe, type = "HC3"))

#Check for multicollienarity

#Highly correlated
cor(df_clarity$fk_grade_manual, df_clarity$dale_chall_quanteda, use="complete.obs")

fit_both <- lm(
  average_clarity ~
    fk_grade_manual + I(fk_grade_manual^2) +
    dale_chall_quanteda + I(dale_chall_quanteda^2) +
    factor(llm) + factor(prompt_id) + factor(conversation_id),
  data = df_clarity
)

#VIFs are high ( > 5)
library(car)
vif(fit_both)


# ==============================================================================
# DATA VISUALIZATION
# 
# ==============================================================================



library(visreg)
library(ggplot2)

visreg(fit_dc_fe, "dale_chall_quanteda", gg = TRUE, partial = TRUE) +
  labs(
    x = "Dale-Chall score",
    y = "Clarity of Text",
    title = "Simplified Vocabulary Has Diminishing Returns",
    subtitle = "Highest level of textual Clarity observed when text is at high school level"
  ) +
  theme_minimal()


  visreg(fit_dc_fe, "dale_chall_quanteda", gg = TRUE, partial = FALSE) +    labs(
    x = "Dale-Chall score",
    y = "Clarity of Text",
    title = "Simplifying vocabulary has diminishing returns"
  ) +
  theme_minimal()
  
  
# ==============================================================================
# IMPORT DATA: EMPATHY 
# 
# ==============================================================================
  
  empathy <- read_csv(data_file("customerbot-results-2.csv"))
  
  df_empathy <- df_metrics %>%
    left_join(
      empathy %>%
        select(
          row_id,
          `openai/gpt-4o-mini_score`,
          `google/gemini-2.0-flash-001_score`
        ),
      by = "row_id"
    ) %>%
    rename(
      chatgpt_empathy = `openai/gpt-4o-mini_score`,
      gemini_empathy = `google/gemini-2.0-flash-001_score`
    ) %>%
    mutate(
      average_empathy = rowMeans(
        pick(chatgpt_empathy, gemini_empathy),
        na.rm = TRUE
      )
    )
  
# ==============================================================================
# MODELLING
# 
# ==============================================================================
  
  
  
  #Dale-Chall only + fixed effects
  fit_dc_fe_emp <- lm(
    average_empathy ~ dale_chall_quanteda + I(dale_chall_quanteda^2) +
      factor(llm) + factor(prompt_id) + factor(conversation_id),
    data = df_empathy
  )
  
  coeftest(fit_dc_fe_emp, vcov = vcovHC(fit_dc_fe_emp, type = "HC3"))
  summary(fit_dc_fe_emp)

# ==============================================================================
# DATA VISUALIZATION
# 
# ==============================================================================
  
  
  #Dale-Chall only + fixed effects Visual  
  visreg(fit_dc_fe_emp, "dale_chall_quanteda", gg = TRUE, partial = TRUE) +
    labs(
      x = "Dale-Chall score",
      y = "Empathy",
      title = "Simplified Vocabulary Has Diminishing Returns",
      subtitle = "Chatbot is more empathetic when language is high school grade"
    ) +
    theme_minimal()  
  
  
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

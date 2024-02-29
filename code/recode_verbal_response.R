# ------------------------------------------------------------------------------
# Reads coded verbal response data and calculates some simple summary measures.
# Stores resulting data frame in `data/generated`. 
#
# See LICENSE file for license
# ------------------------------------------------------------------------------

suppressMessages(library(tidyverse))

vc <- read_csv(
	"data/external/verbal_responses_coded.csv", show_col_types = FALSE
) %>%
	select(-exp_pct_share)

df_vc <- vc %>%
  mutate(
    positive_reasons = rowSums(across(starts_with("pos_"))),
    negative_reasons = rowSums(across(starts_with("neg_"))),
    neutral_reasons = rowSums(across(starts_with("dep_"))),
    n_reasons = positive_reasons + negative_reasons + 
    	neutral_reasons,
    share_positive = positive_reasons/n_reasons,
    share_negative = negative_reasons/n_reasons,
    share_neutral = neutral_reasons/n_reasons
  ) %>%
  filter(n_reasons > 0)

names(df_vc)[3:ncol(df_vc)] <- paste0("verbal_", names(df_vc)[3:ncol(df_vc)])
saveRDS(df_vc, "data/generated/verbal_response_coded.rds")

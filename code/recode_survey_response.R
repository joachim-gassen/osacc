# ------------------------------------------------------------------------------
# Reads oTree data and codes variable based on data contained in
# data/external/variables.csv
#
# See LICENSE file for license
# ------------------------------------------------------------------------------

suppressMessages(library(tidyverse))

raw_df <- read_csv(
	"data/external/otree_output_wide_2022-12-14.csv", show_col_types = FALSE
)
vars <- read_csv("data/external/variables.csv", show_col_types = FALSE)

df <- raw_df %>% select(any_of(vars$otree_var)) %>% 
	filter(!is.na(participant.share)) 

names(df) <- vars$var[match(names(df), vars$otree_var)]

recode_var <- function(x) {
	if (!any(is.na(vars$resp_no[vars$var == names(df[x])]))) {
		ret_val <- factor(
			pull(df[x]), 
			labels = vars$resp_label[vars$var == names(df[x])]
		)
	} else ret_val <- pull(df[x])
	ret_val
}

recoded_df <- as.data.frame(lapply(seq_along(df), recode_var))
names(recoded_df) <- names(df)

saveRDS(recoded_df, "data/generated/survey_response.rds")

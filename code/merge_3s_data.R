# ------------------------------------------------------------------------------
# Reads Social Science Survey data (https://doi.org/10.1038/s41467-023-41111-1)
# and creates a merged sample for the overlapping questions
#
# See LICENSE file for license
# ------------------------------------------------------------------------------


suppressMessages(library(tidyverse))

sss_resp <- readRDS("data/generated/sss_response.rds")
resp <- readRDS("data/generated/survey_response.rds")

comp <- resp %>%
	mutate(
		survey = "acc",
		id = id,
		demog_published = TRUE,
		demog_discipline = "Accounting",
		demog_field = as.character(demog_field),
		demog_method = factor(case_when(
			demog_method == "Experimental" ~ "Experimental",
			demog_method == "Archival/Field" |
				demog_method == "Survey" ~ "Quantitative non-experimental",
			demog_method == "Theory" |
				demog_method == "Qualitative" ~ "Qualitative or Theoretical",
			!is.na(demog_method) ~ "Other"
		), level = c(
			"Experimental", "Quantitative non-experimental", 
			"Qualitative or Theoretical", "Other"
		)),
		sharing_heard = sharing_familiar != "Not at all familiar",
		sharing_important = as.numeric(sharing_important),
		sharing_use = !sharing_use == "Never",
		sharing_perc_other_share = sharing_perc_other_share,
		sharing_perc_other_prefer = sharing_perc_other_prefer,
		sharing_perc_subfield_prefer = sharing_perc_other_prefer,
		reprod = as.integer(reprod_acc),
		prereg_heard = prereg_familiar != "Not at all familiar",
		prereg_use_first_time = prereg_use_first_time,
		prereg_opinion = as.numeric(prereg_trust),
		.keep = "none"
	)
		
comp_sss <- sss_resp %>%
	mutate(
		survey = "sss",
		id = pid,
		demog_published = career_stage == "Published Author",
		demog_discipline = as.character(discipline),
		demog_field = field,
		demog_method = factor(case_when(
			method == "Experimental" ~ "Experimental",
			method == "Quantitative non-experimental" ~ "Quantitative non-experimental",
			method == "Qualitative or Theoretical" ~ "Qualitative or Theoretical",
			!is.na(method) ~ "Other"
		), level = c(
			"Experimental", "Quantitative non-experimental", 
			"Qualitative or Theoretical", "Other"
		)),
		sharing_heard = sharing_dc_ever | sharing_inst_ever,
		sharing_important = floor((sharing_dc_opinion + sharing_inst_opinion)/2),
		sharing_use = sharing_dc_how_many != "0" | sharing_inst_how_many != "0", 
		sharing_perc_other_share = sharing_dc_share,
		sharing_perc_other_prefer = sharing_dc_pref_other,
		sharing_perc_subfield_prefer = sharing_dc_pref_other_subfield,
		reprod = reprod, 
		prereg_heard = prereg_ever,
		prereg_use_first_time = prereg_first_year,
		prereg_opinion = prereg_opinion,
		.keep = "none"
	) %>%
	relocate(reprod, .after = sharing_perc_subfield_prefer) %>%
	relocate(prereg_opinion, .after = prereg_use_first_time)
	
merged_resp <- bind_rows(comp_sss, comp) %>%
	arrange(survey, id)
	
saveRDS(merged_resp, "data/generated/merged_response.rds")

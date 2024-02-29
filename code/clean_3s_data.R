# ------------------------------------------------------------------------------
# Reads Social Science Survey data (https://doi.org/10.1038/s41467-023-41111-1)
#
# See LICENSE file for license
# ------------------------------------------------------------------------------


suppressMessages(library(tidyverse))

response_overall <- read_csv(
	"data/external/sss_response_overall.csv", 
	guess_max = Inf, show_col_types = FALSE
)

sss_resp <- response_overall %>%
	select(
		pid, resp_type = Respondent_type, 
		discipline = Discipline, field = subfield,
		year_complete_phd, 
		career_stage = `Career stage`,
		method = `Research type`, 
		sharing_dc_ever = publicly_posting_have_you_ever,
		sharing_dc_how_many = publicly_posting_approximately_how_many,
		sharing_dc_first_year = publicly_posting_approximately_when_was,
		sharing_dc_opinion = publicly_posting_what_is_your_opinion,
		sharing_dc_share = publicly_posting_what_percentage_discipline,
		sharing_dc_share_subfield = publicly_posting_what_percentage_subfield,
		sharing_dc_pref_other_4 = publicly_posting_distribution_of_opinion_discipline_4,
		sharing_dc_pref_other_5 = publicly_posting_distribution_of_opinion_discipline_5,
		sharing_dc_pref_other_subfield_4 = publicly_posting_distribution_of_opinion_subfield_4,
		sharing_dc_pref_other_subfield_5 = publicly_posting_distribution_of_opinion_subfield_5,
		sharing_dc_attitude = publicly_posting_attitude,
		sharing_dc_behavior = publicly_posting_behavior,
		sharing_dc_desc_norm = publicly_posting_descriptive_norm,
		sharing_dc_presc_norm = publicly_posting_prescriptive_norm,
		sharing_inst_ever = posting_study_instruments_have_you_ever,
		sharing_inst_how_many = posting_study_instruments_approximately_how_many,
		sharing_inst_first_year = posting_study_instruments_approximately_when_was,
		sharing_inst_opinion = posting_study_instruments_what_is_your_opinion,
		sharing_inst_attitude = posting_study_instruments_attitude,
		sharing_inst_behavior = posting_study_instruments_behavior,
		reprod = trustworthiness_how_confident_are,
		prereg_ever = analysis_pre_registration_have_you_ever,
		prereg_how_many = analysis_pre_registration_approximately_how_many,
		prereg_first_year = analysis_pre_registration_approximately_when_was,
		prereg_opinion = analysis_pre_registration_what_is_your_opinion,
		prereg_share = analysis_pre_registration_what_percentage_discipline,
		prereg_attitude = analysis_pre_registration_attitude,
		prereg_behavior = analysis_pre_registration_behavior,
		prereg_desc_norm = analysis_pre_registration_descriptive_norm,
		prereg_presc_norm = analysis_pre_registration_prescriptive_norm,
	) %>% mutate(
		sharing_dc_pref_other = sharing_dc_pref_other_4 + sharing_dc_pref_other_5,
		sharing_dc_pref_other_subfield = sharing_dc_pref_other_subfield_4 +
			sharing_dc_pref_other_subfield_5
	) %>%
	relocate(
		sharing_dc_pref_other, .before = sharing_dc_pref_other_4
	) %>%
	relocate(
		sharing_dc_pref_other_subfield, .after = sharing_dc_pref_other
	) %>%
	select(
		-sharing_dc_pref_other_4, -sharing_dc_pref_other_5,
		-sharing_dc_pref_other_subfield_4, -sharing_dc_pref_other_subfield_5
	) 
	
saveRDS(sss_resp, "data/generated/sss_response.rds")

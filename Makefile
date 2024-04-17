# If you are new to Makefiles: https://makefiletutorial.com


# Commands

RSCRIPT := Rscript --encoding=UTF-8


# Main targets

PAPER := output/paper.pdf
PRESENTATION := output/presentation.pdf


# Data Targets

SURVEY_RESPONSE := data/generated/survey_response.rds
VERBAL_RESPONSE := data/generated/verbal_response_coded.rds
SSS_DATA := data/generated/sss_response.rds
MERGED_DATA := data/generated/merged_response.rds

ALL_TARGETS := $(PAPER) $(PRESENTATION) $(SURVEY_RESPONSE) $(VERBAL_RESPONSE) \
	$(SSS_DATA) $(MERGED_DATA)


# Materials needed for main targets

PRES_MATERIALS := docs/materials/ferguson_et_al_2023_uptake_over_time.png \
	docs/materials/fisar_et_al_2023_fields.jpeg \
	docs/materials/fisar_et_al_2023_methods.jpeg \
	docs/materials/gomes_et_al_2022_sharing_concerns.jpeg \
	docs/materials/beamer_theme_trr266_16x9.sty \
	docs/materials/trr266_logo.eps
	
PAPER_MATERIALS := docs/materials/scenario.pdf docs/materials/survey.pdf


# Phony targets

.phony: all clean

all: $(PAPER) $(PRESENTATION)

clean:
	rm -f $(ALL_TARGETS)


# Recipes

$(SURVEY_RESPONSE): data/external/otree_output_wide_2022-12-14.csv \
	data/external/variables.csv code/recode_survey_response.R
	$(RSCRIPT) code/recode_survey_response.R

$(VERBAL_RESPONSE): data/external/verbal_responses_coded.csv \
	code/recode_verbal_response.R
	$(RSCRIPT) code/recode_verbal_response.R

$(SSS_DATA): data/external/sss_response_overall.csv code/clean_3s_data.R
	$(RSCRIPT) code/clean_3s_data.R

$(MERGED_DATA): $(SURVEY_RESPONSE) $(SSS_DATA) code/merge_3s_data.R
	$(RSCRIPT) code/merge_3s_data.R

$(PRESENTATION): $(SURVEY_RESPONSE) $(VERBAL_RESPONSE) $(MERGED_DATA) \
	data/external/variables.csv data/external/invited_authors_anon.csv \
	data/external/obs_data_authors_anon.csv \
	data/external/obs_data_papers_anon.csv \
	code/table_functions.R code/figure_functions.R \
	code/obs_data_analyses.R \
	$(PRES_MATERIALS) docs/presentation.qmd
	quarto render docs/presentation.qmd --quiet
	rm -rf output/presentation_files

$(PAPER): $(SURVEY_RESPONSE) $(VERBAL_RESPONSE) $(MERGED_DATA) \
	data/external/variables.csv data/external/invited_authors_anon.csv \
	data/external/obs_data_authors_anon.csv \
	data/external/obs_data_papers_anon.csv \
	code/table_functions.R code/figure_functions.R \
	code/obs_data_analyses.R \
	$(PAPER_MATERIALS) docs/paper.qmd docs/references.bib
	quarto render docs/paper.qmd --quiet
	

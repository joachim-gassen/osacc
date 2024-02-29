### External data contained in this folder

| File | Description | Source |
|------|-------------|--------|
| `invited_authors_retratced.csv`| File resembling the original file containing the survey population data. To maintain the privacy of the invited authors only the affiliations' countries are not retracted.|Created by running essentially the code contained in `code/get_papers_scopus.py` and `code/get_top6_accounting_authors.R`. Additional cleaning steps explained in the paper. |
| `otree_output_wide_2022-12-14.csv`| Raw survey data as downloaded from oTree| Collected during survey. See [here](https://otree.readthedocs.io/) for general data format. The oTree code of our instruments is included in the folder `code/otree`|
| `sss_response_overall.csv`| Response data from Social Science Survey (merged Wave one and two) | Original file name `response_overall.csv` in [OSF repository](https://osf.io/zn8u2/) ([Study](https://doi.org/10.1038/s41467-023-41111-1)) |
| `top6_accounting_journals.csv` | Table containing the names and ISSNs of the "top six" accounting journals | Collected from Scopus by the authors. |
| `variables.csv` | Table to link survey questions, response labels and oTree variable names to variable names in recoded response data frame | Created by authors.|
| `verbal_responses_coded.csv` | Table containing the classification of the verbal responses provided by study participants about the rationales underlying their response to our experimental treatment question | Created by an RA of the author team that was uninformed about the experimental design and the treatment assignment.|

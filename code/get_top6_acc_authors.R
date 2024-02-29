# This code is not needed to reproduce the paper but together with
# the file get_papers_scopus.py it shows how we constructed the 
# population for the Survey from Scopus. The code below that
# we used to obtain the emails of authors is somewhat undocumented
# by Scopus so use at your own risk. It currently errors out in 
# our environment due to a Scopus security check. In any case,
# you will need a Scopus license and a Scopus API key to run the code.

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

scopus_key = MYKEYHERE

raw_papers <- read_csv(
	"data/generated/top6_acc_papers.csv", guess_max = Inf,
	show_col_types = FALSE
)

issn <- raw_papers %>%
  group_by(issn, publicationName) %>%
  mutate(npn = n()) %>%
  group_by(issn) %>%
  arrange(-npn) %>%
  summarise(
    journal = publicationName[1],
    npapers = n()
  )


acc_papers <- raw_papers %>%
  filter(!is.na(author_count)) %>%
  select(
    eid, issn, publicationName, coverDate, coverDisplayDate, volume, issueIdentifier, 
    subtypeDescription, pageRange, openaccess, freetoreadLabel,
    doi, title, description, authkeywords, citedby_count, fund_no,
    fund_sponsor, author_count
  ) %>%
  arrange(publicationName, volume, issueIdentifier, pageRange)

acc_authors <- raw_papers %>%
  filter(!is.na(author_count)) %>%
  # the below fixes data errors in paper 2-s2.0-84866769562 and
  # paper 2-s2.0-85106699685
  mutate(
    author_names = str_remove(author_names, fixed(";rregaard")),
    author_names = str_replace(
      author_names,
      fixed("C&cedil;o&die;teliog&caron;lu, Efe"),
      "Çötelioğlu, Efe"
    )
  ) %>%
  select(eid, author_ids, author_names, author_afids) %>%
  separate_rows(author_ids, author_names, author_afids, sep = ";") %>%
  rename(
    author_id = author_ids, author_name = author_names, affil_id = author_afids
  )
    
acc_affiliations <- raw_papers %>%
  filter(!is.na(author_count)) %>%
  mutate(
    affilname = str_replace_all(affilname, fixed("&amp;"), "&")
  ) %>%
  select(eid, afid, affilname, affiliation_city, affiliation_country) %>%
  separate_rows(
    afid, affilname, affiliation_city, affiliation_country, sep = ";"
  ) %>%
  rename(
    affil_id = afid, affil_name = affilname, 
    affil_city = affiliation_city, affil_country = affiliation_country
  )


acc_authors <- acc_papers %>%
  select(eid, coverDate) %>% 
  left_join(acc_authors, by = "eid") %>%
  mutate(affil_id = str_split_fixed(affil_id, fixed("-"), 2)[,1]) %>%
  left_join(acc_affiliations, by = c("eid", "affil_id")) %>%
  arrange(author_id, -as.numeric(coverDate)) %>%
  group_by(author_id) %>%
  filter(row_number() == 1) %>%
  select(
    author_id, author_name, affil_id, affil_name, affil_city, affil_country
  ) %>% 
  arrange(author_name)

get_author_email <- function(author_id) {
  url <- paste0("https://www.scopus.com/api/authors/", author_id)
  httpResponse <- GET(url, add_headers("X-ELS-APIKey" = scopus_key), accept_json())
  results <- fromJSON(content(httpResponse, "text"))
  results$emailAddress
}

# For what ever reason this fails on first try - second try works
# interesting API

acc_authors$email <- ""
for (i in 1:nrow(acc_authors)) {
  email <- get_author_email(acc_authors$author_id[i])
  if(!is.null(email)) acc_authors$email[i] <- email
  Sys.sleep(runif(1, 0.5, 1.5)) # not sure if needed - ran into errors wout it
  if (i %% 10 == 0) message(sprintf("Done parsing row %d", i))
}

write_csv(new_acc_authors, "data/generated/top6_accounting_authors_2016_2021.csv")

nrow(all_acc_authors) - sum(is.na(all_acc_authors$email) | all_acc_authors$email == "")

dup_names <- acc_authors %>% group_by(author_name) %>% filter(n() > 1) 

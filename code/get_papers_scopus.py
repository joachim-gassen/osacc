# This code is not needed to reproduce the paper but together with
# the file get_top6_acc_authors.R it shows how we constructed the 
# population for the Survey from Scopus. In any case,
# you will need a Scopus license and a Scopus API key to run the code.

from pybliometrics.scopus import ScopusSearch
import pandas as pd

START_YEAR=2016
END_YEAR=2021
journals = pd.read_csv('data/external/top6_acc_journals.csv', dtype=str)

for index, row in journals.iterrows():
    print(f"Reading {row['journal']} (ISSN {row['issn']})")
    q = f"ISSN({row['issn']}) AND PUBYEAR > {START_YEAR - 1} AND PUBYEAR < {END_YEAR + 1}"  
    s = ScopusSearch(q)  
    pubs = s.results  
    new_pubs = pd.DataFrame.from_dict(pubs)
    if index > 0:
        df_pubs = pd.concat([df_pubs, new_pubs], ignore_index = True, sort = False)
    else: df_pubs = new_pubs 

df_pubs.to_csv('data/generated/top6_acc_papers.csv', index=False)

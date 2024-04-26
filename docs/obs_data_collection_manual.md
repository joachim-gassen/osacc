## Observational Data Collection Manual

This describes the procedure that we will use to identify whether accounting researchers have pre-registered a study and/or publicly shared research materials (i.e., data, code, and research procedures) in the past. It is heavily inspired by the audit protocol that [Ferguson et al. (2023)](https://doi.org/10.1038/s41467-023-41111-1) use (see the Section 1.4.1 of their [Supplementary Information](https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-023-41111-1/MediaObjects/41467_2023_41111_MOESM1_ESM.pdf)).

You will receive a list of accounting authors (names, emails, and historic affiliation). All of these authors are in our population as they have published a study in the top six accounting journals over the period 2016-2021. We have drawn a random sample of 100 authors from a list of 2,542 authors.

For each author on the list, please follow the following protocol to collect the data:

1. Google the author name to identify their current affiliation (e.g., university) and a homepage. Ideally, this would be an affiliation page but it might also be a private homepage. It should have a research output list. Based on this, record the current main affiliation of the author (`curr_affil` (Char)), and their homepage URL (`hpage` (Char)) in the CSV file `data/external/audit_autors.csv`.

2. Using the information presented on the homepage (and sub-pages but no external links), verify whether there is a clear link to shared research materials or a pre-registration (`hpage_resmat` (Boolean), `hpage_prereg`, (Boolean))

3. Use [Scopus](https://www.scopus.com) (need to be on HU net to use Scopus) to find papers published by the authors. Record the Scopus ID (`scopus_id`), both in `data/external/obs_data_autors.csv` and in `data/external/obs_data_papers.csv`. For all studies published starting 2016 or later that have a DOI, use this DOI to check whether the webpage (not the paper itself), makes a clear reference to shared research materials or a pre-registration. Record the data for *each paper separately* in `data/external/obs_data_papers.csv` (`scopus_id` (char), `doi` (char), `resmat` (Boolean), `prereg` (Boolean)).

4. Some journals have dedicated data and code policies that require authors to provide data and/or code. For example, the Journal of Accounting Research or Management Science, but also other journals (Journal of Finance, Top 5 Econ Journals, etc.) have such policies in place. Please make sure to capture also these instances. Hint: The data/code contributions for JAR can be found [here](https://www.chicagobooth.edu/research/chookaszian/journal-of-accounting-research/online-supplements-and-datasheets)

5. Check the following web pages to see whether the author has a profile/data on them.

- [Github](https://github.com)
- [Open Science Foundation](https://osf.io)
- [Dataverse](https://dataverse.harvard.edu)
- [Zenodo](https://zenodo.org) 
- [AEA RCT Registry](https://www.socialscienceregistry.org)
- [AEA Repository on OPENIPCSR](https://www.openicpsr.org/openicpsr/search/aea/studies)

For the first four repositories, record the following variables (all Boolean):

- `{repo}_author` (if author present on repo)
- `{repo}_resmat` (if author has published research material on repo)
- `{repo}_prereg` (if author has pre-registered materials on repo)

The latter two repositories are dedicated for pre-registration and research material sharing, respectively. Thus, store your insights into Boolean variables `aearct_prereg` and `aearep_resmat`.

6. Finally, both data sheets have a `note` field that you can use to record any oddities that you encountered when collecting the data. In this case, please provides URLs etc. to explain your issue. 

If anything of the above is unclear, please yell on Slack. Thanks!

suppressMessages({
	library(tidyverse)
	library(kableExtra)
})	

# The code below relies on the dataframes papers and authors being
# available 

papers_by_author <- papers %>%
	mutate(
		mand_resmat = journal %in% 
			c("Journal of Accounting Research", "Management Science")
	) %>%	
	group_by(scopus_id) %>%
	summarise(
		papers_n = n(),
		papers_mn_resmat = mean(resmat),
		papers_mn_resmat_wo_mand = mean(resmat * (! mand_resmat)),
		papers_mn_prereg = mean(prereg)
	)

by_author <- authors %>%
	left_join(papers_by_author, by = "scopus_id") %>%
	transmute(
		papers_n = papers_n,
		papers_mn_resmat = papers_mn_resmat,
		papers_mn_resmat_wo_mand = papers_mn_resmat_wo_mand,
		has_made_resmat_available = hpage_resmat | github_resmat |
			osf_resmat | dverse_resmat | zenodo_resmat | aearep_resmat |
			papers_mn_resmat > 0,
		has_made_resmat_available_vol = hpage_resmat | github_resmat |
			osf_resmat | dverse_resmat | zenodo_resmat | aearep_resmat |
			papers_mn_resmat_wo_mand > 0,
		has_prereged = hpage_prereg | github_prereg |
			osf_prereg | dverse_prereg | zenodo_prereg | aearct_prereg |
			papers_mn_prereg > 0,
		uses_os_repos = github_author + osf_author + dverse_author + 
			zenodo_author + aearct_prereg + aearep_resmat > 0
	)

ci_has_made_resmat_available <- prop.test(
	sum(by_author$has_made_resmat_available),
	nrow(by_author)
)$conf.int
ci_has_made_resmat_available_vol <- prop.test(
	sum(by_author$has_made_resmat_available_vol),
	nrow(by_author)
)$conf.int
ci_uses_os_repos <- prop.test(
	sum(by_author$uses_os_repos),
	nrow(by_author)
)$conf.int

tab_by_author <- by_author %>%
	summarise(
		`... has made research material available` = 
			sprintf(
				"%.1f %% (%.1f %% - %.1f %%)", 
				100*mean(has_made_resmat_available),
				100*ci_has_made_resmat_available[1],
				100*ci_has_made_resmat_available[2]
			),
		`... has made research material available (without JAR/MS)` = 
			sprintf(
				"%.1f %% (%.1f %% - %.1f %%)", 
				100*mean(has_made_resmat_available_vol),
				100*ci_has_made_resmat_available_vol[1],
				100*ci_has_made_resmat_available_vol[2]
			),
		`... uses Open Science Repositories` = 
			sprintf(
				"%.1f %% (%.1f %% - %.1f %%)", 
				100*mean(uses_os_repos),
				100*ci_uses_os_repos[1],
				100*ci_uses_os_repos[2]
			)
	) %>% pivot_longer(everything(), names_to = "Author ... ", values_to = "%")


ci_papers_resmat <- 100*prop.test(
	sum(papers$resmat),
	nrow(papers)
)$conf.int

tab_by_journal <- papers %>%
	group_by(journal) %>%
	mutate(journal = ifelse(n() >= 20, journal, "Other Journal")) %>%
	group_by(journal) %>%
	summarise(
		papers = n(),
		papers_resmat = sum(resmat),
		ci_lb = 100*prop.test(papers_resmat, papers)$conf.int[1],
		ci_ub = 100*prop.test(papers_resmat, papers)$conf.int[2]
	) %>%
	transmute(
		Journal = journal,
		Papers = sprintf("%d out of %d", papers_resmat, papers),
		`%` = sprintf(
			"%.1f %% (%.1f %% - %.1f %%)", 
			100*(papers_resmat/papers), ci_lb, ci_ub
		)
	) %>%
	arrange(Journal == "Other Journal", Journal) %>%
	bind_rows(
		tibble(
			Journal = "Total",
			Papers = sprintf("%d out of %d", sum(papers$resmat), nrow(papers)),
			`%` = sprintf(
				"%.1f %% (%.1f %% - %.1f %%)", 
				100*mean(papers$resmat), ci_papers_resmat[1], ci_papers_resmat[2]
			)
		)
	)

papers_by_cyear <- papers %>%
	filter(!is.na(cyear)) %>%
	group_by(cyear) %>%
	filter(n() >= 50) %>%
	summarise(
		papers = n(),
		pct_resmat = mean(resmat),
		pct_resmat_lb = prop.test(sum(resmat), n())$conf.int[1],
		pct_resmat_ub = prop.test(sum(resmat), n())$conf.int[2]
	)

tab_by_cyear <- papers_by_cyear %>%
	transmute(
		`DOI Year` = cyear,
		Papers = papers,
		`Papers with Research Materials` = as.integer(pct_resmat*papers),
		`Percent` = sprintf(
			"%.1f %% (%.1f %% - %.1f %%)", 
			100*pct_resmat,
			100*pct_resmat_lb,
			100*pct_resmat_ub
		)
	) %>%
	arrange(`DOI Year`)

plot_resmat_sharing_over_cyear <- 
	ggplot(papers_by_cyear, aes(x = cyear)) +
		geom_point(aes(y = pct_resmat)) + 
		geom_errorbar(aes(ymin = pct_resmat_lb, ymax = pct_resmat_ub), width = 0) +
		scale_y_continuous(labels = scales::percent) +
		labs(x = "Year of DOI Creation", y = "") +
		theme_minimal() +
		theme(
			axis.title.x = element_text(size = 10),
			legend.text = element_text(size = 8),
			text=element_text(family="Times"),
			panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()
		)


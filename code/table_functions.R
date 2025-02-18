# ------------------------------------------------------------------------------
# Functions to create tables on the survey data.
# Example use cases are at the end of file.
#
# See LICENSE file for license
# ------------------------------------------------------------------------------

library(tidyverse)
library(kableExtra)
library(xml2)
library(countrycode)

GROUPS <- c(
	"region", "age", "gender", "active_res", "tenured", "fars", "archival", 
	"qualitative", "risk", "trust", "tment_ctrl", "tment_prvt"
)

myfmt <- function(x, ndigits = 3, p = FALSE) {
	rv <- format(round(x, ndigits), nsmall = ndigits)
	if (!p) return(rv)
	ifelse(x < 0.001, "< 0.001", paste("=", rv))
}

get_resp_label <- function(v) {
	# relies on vars being present in environment
	vars %>% filter(var == v) %>% pull(resp_label)
}

remove_quarto_formatting <- function(k) {
	k_xml <- kable_as_xml(k)
	xml_set_attr(k_xml, "data-quarto-disable-processing", "true")
	xml_as_kable(k_xml) 
}

starme <- function(v) {
	ifelse(v < 0.01, '***', ifelse(v < 0.05, '**', ifelse(v < 0.1, '*', '')))
}

create_split <- function(df, group) {
	if (!group %in% GROUPS) stop(
		sprintf(
			"Invalid group string. Valid strings are %s",
			paste0("'", paste0(GROUPS, collapse = "', '"), "'")
		)
	)
	case_when(
		group == "region" ~ 
			df %>% mutate(split_var = demog_region == "North America"),
		group == "age" ~
			df %>% mutate(split_var = as.numeric(demog_age) > 3),
		group == "gender" ~
			df %>% mutate(split_var = demog_gender == "Male"),
		group == "active_res" ~
			df %>% mutate(split_var = demog_active_res == "Yes"),
		group == "tenured" ~
			df %>% mutate(split_var = demog_job_cat == "Tenured faculty"),
		group == "fars" ~
			df %>% mutate(split_var = demog_field == "Financial accounting and reporting"),
		group == "archival" ~
			df %>% mutate(split_var = demog_method == "Archival/Field"),
		group == "qualitative" ~
			df %>% mutate(split_var = demog_method == "Qualitative"),
		group == "risk" ~
			df %>% mutate(split_var = demog_risk > median(df$demog_risk, na.rm = TRUE)),
		group == "trust" ~
			df %>% mutate(split_var = demog_trusting + demog_trustworthy >= median(df$demog_trusting + df$demog_trustworthy, na.rm = TRUE)),
		group == "tment_ctrl" ~
			df %>% mutate(split_var = exp_tment_control == 1),		
		group == "tment_prvt" ~
			df %>% mutate(split_var = exp_tment_private == 1),		
		
	)
}


test_group_differences <- function(df, var, group) {
	my_ttest <- function(df, var) {
		rv <- t.test(df[df$split_var,var], df[! df$split_var,var])
		list(
			statistic = rv$statistic,
			p.value = rv$p.value,
			conf.int = rv$conf.int,
			n = c(
				sum(!is.na(df[! df$split_var,var])),
				sum(!is.na(df[df$split_var,var]))
			),
			observed = c(
				mean(df[! df$split_var,var], na.rm = TRUE),
				mean(df[df$split_var,var], na.rm = TRUE)
			)
		)
	}
	
	my_chisq <- function(df, var) {
		rv <- chisq.test(df[, var], df$split_var, simulate.p.value = TRUE)
		list(
			statistic = rv$statistic,
			p.value = rv$p.value,
			conf.int = NA,
			n = colSums(rv$observed),
			observed = rv$observed
		)
	}
	
	test_func <- ifelse(is.numeric(df[, var]), my_ttest, my_chisq) 
	test_df <- create_split(df, group)
	test_func(test_df, var)
}

create_demog_table <- function(df, var, df_only = FALSE) {
	df$var <- df[, var]
	df <- df %>% filter(!is.na(var))
	tab <-  df %>%
		group_by(var) %>%
		summarise(
			n = as.character(n()),
			pct = sprintf("%.1f %%", 100 * n()/nrow(df))
		) %>%
		bind_rows(
			tibble(
				var = "Total",
				n = as.character(nrow(df)),
				pct = "100.0 %"
			)
		) 
	
	names(tab)[1] = str_remove(var, fixed("demog_"))
	if (df_only) return(tab)
	kable(
		tab, table.attr = 'data-quarto-disable-processing="true"',
		align = "lrr"
	) %>% 
		kable_classic(full_width = FALSE)
}


create_discrete_table <- function(df, var, groups = GROUPS) {
	tr <- table(df[,var])
	rt <- matrix("", 6 + nrow(tr), 2 + 2*length(groups))
	rt[1,2] <- "Full Sample"
	rt[3:(nrow(rt)-4), 1] <- rownames(tr)
	rt[nrow(rt)-3, 1] <- "N"
	rt[nrow(rt)-2, 1] <- "Chi-Sq (p-Value)"
	rt[nrow(rt)-1, 1] <- "Mean"
	rt[nrow(rt), 1] <- "t-Test (p-Value)"
	rt[3:(nrow(rt)-4), 2] <- tr
	rt[nrow(rt)-3, 2] <- sum(tr)
	rt[nrow(rt)-1, 2] <- myfmt(mean(as.numeric(df[, var]), na.rm = TRUE), 2)
	
	
	for (g in seq_along(groups)) {
		rt[1, 1+2*g] <- groups[g]
		rt[2, (1+2*g):(2+2*g)] <- c("No", "Yes")
		rv <- test_group_differences(df, var, groups[g])
		rt[3:(nrow(rt)-4), (1+2*g):(2+2*g)] <- rv$observed
		rt[3:(nrow(rt)-4), (1+2*g):(2+2*g)] <- rv$observed
		rt[nrow(rt)-3, (1+2*g):(2+2*g)] <- rv$n
		rt[nrow(rt)-2, (2+2*g)] <- sprintf(
			"%s (%s)", myfmt(rv$statistic, 2), myfmt(rv$p.value, 3)
		)
		df$numvar <- as.numeric(df[,var])
		rv <- test_group_differences(df, "numvar", groups[g])
		rt[nrow(rt)-1, (1+2*g):(2+2*g)] <- myfmt(rv$observed, 2)
		rt[nrow(rt), (2+2*g)] <- sprintf(
			"%s (%s)", myfmt(rv$statistic, 2), myfmt(rv$p.value, 3)
		)
	}
	
	tab <- rt[3:nrow(rt),2:ncol(rt)]
	rownames(tab) <- rt[3:nrow(rt),1]
	colnames(tab) <- rt[2,2:ncol(rt)]
	header = c(1, 1, rep(2, length(groups)))
	names(header) <- c(" ", rt[1,2], groups)
	
	kable(
		tab, table.attr = 'data-quarto-disable-processing="true"',
		align = "r"
	) %>% 
		kable_classic(full_width = FALSE) %>%
		add_header_above(header) 
}

create_continuous_table <- function(df, var, groups = GROUPS) {
	stats_by_group <- function(df, var, group = "all") {
		df$var <- df[, var]
		if (group == "all") df$split_var <- TRUE
		else df <- create_split(df, group)
		df %>%
			filter(!is.na(var), !is.na(split_var)) %>%
			group_by(split_var) %>%
			summarise(
				mean = mean(var),
				sd = sd(var),
				min = min(var),
				p25 = quantile(var, 0.25),
				median = median(var),
				p75 = quantile(var, 0.75),
				max = max(var),
				n = n()
			) %>% 
			pivot_longer(
				cols = everything() & !split_var, names_to = "stat", values_to = "val"
			) %>%
			pivot_wider(
				names_from = split_var, values_from = "val", 
				names_prefix = paste0(group, "_")
			)
	}
	tab <- purrr::reduce(
		lapply(c("all", GROUPS), stats_by_group, df = df, var =  var), 
		left_join, by = "stat"
	)
	
	strtab <- matrix(
		sprintf("%.1f", as.matrix(tab[,2:ncol(tab)])),
		nrow = nrow(tab), ncol = ncol(tab)-1
	)
	strtab[nrow(strtab),] <- as.integer(as.matrix(tab)[nrow(tab),2:ncol(tab)])
	colnames(strtab) <- c(" ", rep(c("No", "Yes"), length(GROUPS)))
	rownames(strtab) <- tab$stat
	
	testtab <- matrix("", nrow = 1, ncol = ncol(tab)-1)
	for (g in seq_along(groups)) {
		rv <- test_group_differences(df, var, groups[g])
		testtab[1, (1+2*g)] <- sprintf(
			"%s (%s)", myfmt(rv$statistic, 2), myfmt(rv$p.value, 3)
		)		
	}
	rownames(testtab) <- "t-Test"
	strtab <- rbind(strtab, testtab)
	
	header = c(1, 1, rep(2, length(GROUPS)))
	names(header) <- c(" ", "all", GROUPS)
	kable(
		strtab, table.attr = 'data-quarto-disable-processing="true"',
		align = "r"
	) %>% 
		kable_classic(full_width = FALSE) %>%
		add_header_above(header)
}

ttest_group_by_group <- function(df, var, groupvar, xg, yg) {
	x <- as.numeric(df[[var]][df[, groupvar] == xg]) %>% na.omit()
	y <- as.numeric(df[[var]][df[, groupvar] == yg]) %>% na.omit()
	if (length(x) <= 1) return(1)
	if (length(y) <= 1) return(1)
	rv <- t.test(x, y)
	rv$p.value
}

ttest_group_by_other_groups <- function(df, var, groupvar, xg) {
	groups = levels(as.factor(df[[groupvar]]))
	other_groups <- groups[groups != xg]
	sapply(other_groups, function(y) ttest_group_by_group(df, var, groupvar, xg, y))
}

create_cross_tab_table <- function(df, var, cross_var, df_only = FALSE) {
	tab <- table(df[, var], df[, cross_var])
	rt <- matrix("", nrow(tab) + 5, ncol(tab) + 2)
	rt[2:(nrow(tab) + 1),1] <- rownames(tab)
	rt[1,2:(ncol(tab) + 1)] <- colnames(tab)
	rt[nrow(tab) + 2, 1] <- "N"
	rt[1, ncol(tab) + 2] <- "N"
	rt[nrow(tab) + 3, 1] <- "Mean"
	rt[nrow(tab) + 4, 1] <- "Median"
	rt[nrow(tab) + 5, 1] <- "t-Test diff. from"
	rt[2:(nrow(tab) + 1), 2:(ncol(tab) + 1)] <- tab
	rt[nrow(tab) + 2,2:(ncol(tab) + 1)] <- colSums(tab)
	rt[2:(nrow(tab) + 1), ncol(tab) + 2] <- rowSums(tab)
	rt[(nrow(tab) + 2), (ncol(tab) + 2)] <- sum(sum(tab))
	df$var <- as.numeric(df[, var])
	df$cross_var <- df[, cross_var]
	rt[nrow(tab) + 3, 2:(ncol(tab) + 1)] <- myfmt(
		df %>% filter(!is.na(cross_var)) %>% group_by(cross_var) %>%
			summarise(mn = mean(var, na.rm = TRUE)) %>%
			pull(), 2
	)
	rt[nrow(tab) + 4, 2:(ncol(tab) + 1)] <- myfmt(
		df %>% filter(!is.na(cross_var)) %>% group_by(cross_var) %>%
			summarise(mn = median(var, na.rm = TRUE)) %>%
			pull(), 0
	)
	for (g in levels(df[, cross_var])) {
		pv <- ttest_group_by_other_groups(df, var, cross_var, g)
		rt[nrow(tab) + 5, which(levels(df[, cross_var]) == g) + 1] <-
			paste(names(pv[pv < 0.1]), collapse = ", ")
	}
	
	tab <- rt[2:nrow(rt),2:ncol(rt)]
	rownames(tab) <- rt[2:nrow(rt),1]
	colnames(tab) <- rt[1,2:ncol(rt)]
	if (df_only) {
		df <- as_tibble(tab)
		df <- df %>% mutate(
			Response = rownames(tab)
		) %>%
			select(Response, everything())
		return(df)
	}
	kable(
		tab, table.attr = 'data-quarto-disable-processing="true"',
		align = "r"
	) %>% 
		kable_classic(full_width = FALSE) 
}

create_pct_cross_tab_table <- function(
		df, var, cross_var, include_total = FALSE, resp_label = NULL, 
		ctests = FALSE, ctests_level = 0.1, drop_levels = "",
		add_num_labels = TRUE, df_only = FALSE
) {
	df$var <- as.numeric(df[[var]])
	df$cross_var <- as.factor(df[[cross_var]])
	if (include_total) full_df <- df
	if (drop_levels != "") {
		df <- df %>% filter(! as.character(cross_var) %in% drop_levels) 
	}
	if (is.factor(df$cross_var)) df$cross_var <- droplevels(df$cross_var)
	if (is.null(resp_label)) resp_label <- get_resp_label(var)
	if (is.numeric(df$cross_var)) df$cross_var <- as.factor(df$cross_var)
	
	tab_df <- data.frame(table(df$var, df$cross_var))
	if (include_total) {
		full_df$total <- "Total"
		total_df <- data.frame(table(full_df$var, full_df$total))
		tab_df <- bind_rows(total_df, tab_df)
	}
	
	freqs <- tab_df %>%
		pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq) %>%
		mutate(Var1 = if(add_num_labels) 
			sprintf("%s (%d)", resp_label, Var1) else
			sprintf("%s", resp_label)
		) %>%
		rename(Response = Var1)
	
	pct <- freqs %>%
		mutate(across(-1, ~sprintf("%d (%.1f %%)", .x, 100*(.x/sum(.x)))))
	
	stats <- df %>%
		group_by(cross_var) %>%
		select(cross_var, var) %>%
		na.omit() %>%
		summarise(
			n = sprintf("%d", n()),
			Mean = sprintf("%.2f", mean(var)),
			Median = sprintf("%.2f", median(var))
		) 
	
	if (include_total) {
		stats <- bind_rows(
			full_df %>%
				select(var) %>%
				na.omit() %>%
				summarise(
					cross_var = "Total",
					n = sprintf("%d", n()),
					Mean = sprintf("%.2f", mean(var)),
					Median = sprintf("%.2f", median(var))
				),
			stats
		)
	}
	
	if (ctests) {
		ctests <- NULL
		for (g in levels(df$cross_var)) {
			pv <- ttest_group_by_other_groups(df, "var", "cross_var", g)
			ctests <- c(
				ctests, 
				paste(substr(names(pv[pv < ctests_level]), 1, 2), collapse = ", ")
			)
		}
		if (include_total) ctests <- c("", ctests)
		stats <- stats %>% mutate(
			`Different from` = ctests
		) %>% t() %>% data.frame() 
	} else {
		stars <- starme(
			unname(ttest_group_by_other_groups(
				df, "var", "cross_var", levels(df$cross_var)[1]
			))
		)
		if (include_total) stars <- c("", stars)
		stats <- stats %>%
			mutate(
				Mean = paste0(Mean, c("", stars))
			) %>% t() %>% data.frame() 
	}
	
	names(stats) <- colnames(pct)[-1]
	stats <- rownames_to_column(stats[-1,], "Response")
	tab <- bind_rows(pct, stats)
	if (df_only) return(tab)
	kable(
		tab, table.attr = 'data-quarto-disable-processing="true"', 
		align = c('l', rep('r', ncol(tab) - 1))
	) %>% 
		kable_classic(full_width = FALSE)
}

create_cont_cross_tab_table <- function(
		df, var, cross_var, include_total = FALSE, drop_levels = "", df_only = FALSE
) {
	df$var <- as.numeric(df[[var]])
	df$cross_var <- df[[cross_var]]
	if (include_total) full_df <- df
	if (drop_levels != "") {
		df <- df %>% filter(! as.character(cross_var) %in% drop_levels) 
	}
	if (is.factor(df$cross_var)) df$cross_var <- droplevels(df$cross_var)
	if (is.numeric(df$cross_var)) df$cross_var <- as.factor(df$cross_var)

	tab <- df %>%
		filter(!is.na(var), !is.na(cross_var)) %>%
		group_by(cross_var) %>%
		summarise(
			`Mean` = mean(var),
			`Std. Dev.` = sd(var),
			`Minimum` = min(var),
			`25 %` = quantile(var, 0.25),
			`Median` = median(var),
			`75 %` = quantile(var, 0.75),
			`Maximum` = max(var),
			`N` = n()
		) %>% 
		pivot_longer(
			cols = everything() & !cross_var, names_to = "stat", values_to = "val"
		) %>%
		pivot_wider(
			names_from = cross_var, values_from = "val"
		)
	
	if (include_total) {
		total <- full_df %>%
			filter(!is.na(var), !is.na(cross_var)) %>%
			summarise(
				`Mean` = mean(var),
				`Std. Dev.` = sd(var),
				`Minimum` = min(var),
				`25 %` = quantile(var, 0.25),
				`Median` = median(var),
				`75 %` = quantile(var, 0.75),
				`Maximum` = max(var),
				`N` = n()
			) 
		tab <- tab %>%
			mutate(Total = t(total)[,1]) %>%
			select(stat, Total, everything())
	}
	
	strtab <- matrix(
		sprintf("%.1f", as.matrix(tab[,2:ncol(tab)])),
		nrow = nrow(tab), ncol = ncol(tab)-1
	)
	strtab[nrow(strtab),] <- as.integer(as.matrix(tab)[nrow(tab),2:ncol(tab)])
	colnames(strtab) <- colnames(tab)[2:ncol(tab)]
	rownames(strtab) <- tab$stat
	
	
	testtab <- matrix("", nrow = 1, ncol = ncol(tab)-1)
	for (g in colnames(strtab)) {
		if (g == "Total") break
		pv <- ttest_group_by_other_groups(df, var, cross_var, g)
		testtab[1, which(colnames(strtab) == g)] <-
			paste(names(pv[pv < 0.1]), collapse = ", ")
	}
	rownames(testtab) <- "t-Test diff. from"
	strtab <- rbind(strtab, testtab)
	if (df_only) return(strtab)
	kable(
		strtab, table.attr = 'data-quarto-disable-processing="true"',
		align = "r"
	) %>% 
		kable_classic(full_width = FALSE) 
}

ct_pvalue <- function(
		df, var, cross_var, group, 
		format = TRUE, exclude_other = TRUE
	) {
	pv <- ttest_group_by_other_groups(df, var, cross_var, group)
	if (exclude_other) pv <- pv[setdiff(names(pv), "Other")]
	if (!format) return(pv)
	rv <- myfmt(pv)
	ifelse(
		rv == "0.000", "two-sided p-value < 0.001", 
		paste("two-sided p-value =", rv)
	)
}

give_reasons_by_group <- function(df, group = "all") {
	if (group == "all") df$split_var <- TRUE
	else df <- create_split(df, group) 
	df %>%
		filter(sharing_use != "Always", !is.na(split_var)) %>%
		select(split_var, starts_with("sharing_reason")) %>% 
		select(-sharing_reason_other, -sharing_reason_other_text) %>%
		group_by(split_var) %>%
		mutate(sharing_reason_N = n()) %>%
		summarise_all(mean, na.rm = TRUE) %>%
		pivot_longer(
			cols = starts_with("sharing"), names_to = "Reason", values_to = group
		) %>%
		mutate(Reason = str_remove(Reason, fixed("sharing_reason_"))) %>%
		pivot_wider(names_from = split_var, values_from = 3, names_prefix = paste0(group, "_"))
}

give_reasons <- function(df) {
	tab <- purrr::reduce(
		lapply(c("all", GROUPS), give_reasons_by_group, df = df), 
		left_join, by = 'Reason'
	)
	
	strtab <- matrix(
		sprintf("%.1f %%", 100*as.matrix(tab[,2:ncol(tab)])),
		nrow = nrow(tab), ncol = ncol(tab)-1
	)
	strtab[nrow(strtab),] <- as.integer(as.matrix(tab)[nrow(tab),2:ncol(tab)])
	colnames(strtab) <- c(" ", rep(c("No", "Yes"), length(GROUPS)))
	rownames(strtab) <- tab$Reason
	header = c(1, 1, rep(2, length(GROUPS)))
	names(header) <- c(" ", "all", GROUPS)
	kable(
		strtab, 
		table.attr = 'data-quarto-disable-processing="true"', align = "r"
	) %>% kable_classic(full_width = FALSE) %>%
		add_header_above(header)
}

give_reasons_by_cross_var <- function(df, cross_var) {
	df$cross_var <- df[, cross_var]
	tab <- df %>%
		filter(sharing_use != "Always", !is.na(cross_var)) %>%
		select(cross_var, starts_with("sharing_reason")) %>% 
		select(-sharing_reason_other, -sharing_reason_other_text) %>%
		group_by(cross_var) %>%
		mutate(sharing_reason_N = n()) %>%
		summarise_all(mean, na.rm = TRUE) %>%
		pivot_longer(
			cols = starts_with("sharing"), names_to = "Reason", values_to = "vals"
		) %>%
		mutate(Reason = str_remove(Reason, fixed("sharing_reason_"))) %>%
		pivot_wider(names_from = cross_var, values_from = vals)
	
	strtab <- matrix(
		sprintf("%.1f %%", 100*as.matrix(tab[,2:ncol(tab)])),
		nrow = nrow(tab), ncol = ncol(tab)-1
	)
	strtab[nrow(strtab),] <- as.integer(as.matrix(tab)[nrow(tab),2:ncol(tab)])
	colnames(strtab) <- colnames(tab)[2:ncol(tab)]
	rownames(strtab) <- tab$Reason
	kable(
		strtab, table.attr = 'data-quarto-disable-processing="true"',
		align = "r"
	) %>% 
		kable_classic(full_width = FALSE) 
}

create_regions_rep_table <- function(pop, smp, shorten = FALSE) {
	pop_ctries <- pop %>% select(affil_country) %>%
		group_by(affil_country) %>%
		na.omit() %>%
		summarise(n = n()) %>%
		arrange(-n) %>%
		mutate(
			continent = countrycode(affil_country, "country.name", "continent"),
			region = countrycode(affil_country, "country.name", "region"),
			demog_region = case_when(
				region == "North America" ~ "North America",
				continent == "Europe" ~ "Europe",
				continent == "Asia" ~ "Asia (including the Middle East)",
				continent == "Oceania" ~ "Australia and Oceania",
				region == "Latin America & Caribbean" ~ "Central and South America (including the Caribbean)",
				continent == "Africa" ~ "Africa"
			)
		)
	
	pop_regions <- pop_ctries %>% 
		group_by(demog_region) %>%
		summarise(
			n_pop = sum(n)
		) %>%
		ungroup() 
	
	smp_regions <- smp %>% select(demog_region) %>%
		group_by(demog_region) %>%
		na.omit() %>%
		summarise(n_smp = n()) %>%
		ungroup() 
	
	part <- left_join(pop_regions, smp_regions, by = "demog_region") %>%
		arrange(-n_pop)
	
	# chisq.test(cbind(part$n_pop, part$n_smp), simulate.p.value = TRUE)
	
	part %>% 
		mutate(
			`Population n` = format(n_pop, big.mark = ","),
			`Population %` = sprintf("%.1f%%", 100*n_pop/sum(n_pop)),
			`Sample n` = format(n_smp, big.mark = ","),
			`Sample %` = sprintf("%.1f%%", 100*n_smp/sum(n_smp)),
			`Relative Resp. Rate` = sprintf("%.1f%%", 100*n_smp/n_pop)
		) %>%
		select(-n_pop, -n_smp) %>%
		rename(Region = demog_region) %>%
		mutate(
			Region = if (shorten) stringr::str_remove(Region, "\\(.*\\)") else Region
		) %>%
		rbind(
			list(
				"Total", 
				format(sum(part$n_pop), big.mark = ","),
				"100.0%",
				as.character(sum(part$n_smp)),
				"100.0%",
				sprintf("%.1f%%", 100*sum(part$n_smp)/sum(part$n_pop))
			)
		) 
}

if (FALSE) {
	# Some example code on how to use the functions in this file
	sr <- readRDS("data/generated/survey_response.rds")
	mr <- readRDS("data/generated/merged_response.rds")
	vars <- readxl::read_xlsx("data/external/variables.xlsx")

	create_demog_table(sr, "demog_field")
	create_discrete_table(sr, "reprod_acc")
	create_discrete_table(
		df, "reprod_acc", groups = c("region", "age", "tenured")
	)
	create_pct_cross_tab_table(
		mr, "reprod", "demog_discipline",
		resp_label =  get_resp_label("reprod_acc")
	)
	create_cross_tab_table(sr, "reprod_acc", "demog_method")
	create_pct_cross_tab_table(sr, "reprod_acc", "demog_method")
	ct_pvalue(sr, "reprod_acc", "demog_method", "Analytical")
	ct_pvalue(sr, "reprod_acc", "demog_method", "Analytical")["Archival/Field"]
	ct_pvalue(mr, "sharing_heard", "demog_discipline", "Accounting")
	create_continuous_table(sr, "sharing_perc_other_share")
	create_cont_cross_tab_table(sr, "sharing_perc_other_share", "demog_method")
	give_reasons(df)
	give_reasons_by_cross_var(df, "demog_method")
}
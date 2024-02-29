library(tidyverse)
library(ggbeeswarm)

# Inspiration for plot below is from 
# http://rnotr.com/likert/ggplot/barometer/likert-plots/

likert_plot <- function(
		data, lvar, gvar, title_str = "", 
		llevels = NULL, glevels = NULL, include_n = TRUE
	) {
	df <- data %>%
		transmute(gvar = .data[[gvar]], lvar = .data[[lvar]]) %>%
		filter(!is.na(lvar), !is.na(gvar))
	if (!is.factor(df$lvar)) df$lvar <- factor(df$lvar)
	if (!is.factor(df$gvar)) df$gvar <- factor(df$gvar)
	if (!is.null(llevels)) levels(df$lvar) <- llevels
	if (!is.null(glevels)) levels(df$gvar) <- glevels
	if (include_n) {
		ngrp <- as.vector(table(df$gvar))
		levels(df$gvar) <- sprintf("%s\n(n=%d)", levels(df$gvar), ngrp)
	}
	
	df <- df %>%
		group_by(gvar) %>%
		mutate(n = n()) %>%
		group_by(gvar, lvar) %>%
		arrange(gvar, lvar) 
	
	nl <- length(levels(df$lvar))
	sl <- ceiling(nl/2)
	
	pal <- RColorBrewer::brewer.pal(nl ,"RdBu")
	pal[sl] <- "#DFDFDF"
	likert_pal <- pal
	
	df_pct <- df %>%
		summarise(value = unique(n()/n), .groups = "drop") 
	
	df_low <- df_pct %>% 
		filter(lvar %in% levels(lvar)[1:sl]) %>%
		mutate(value = ifelse(lvar == levels(lvar)[sl], value/2, value))
	
	df_high <- df_pct %>% 
		filter(lvar %in% levels(lvar)[sl:nl]) %>%
		mutate(value = ifelse(lvar == levels(lvar)[sl], value/2, value))
	
	fill_scale <- likert_pal
	names(fill_scale) <- levels(df$lvar)
	
	ggplot() + 
		geom_bar(
			data=df_low, aes(x = gvar, y = -value, fill = lvar), 
			position = "stack", stat = "identity"
		) +
		geom_bar(
			data=df_high, aes(x = gvar, y = value, fill = lvar),  
			position = position_stack(reverse = TRUE), stat = "identity"
		) +
		geom_hline(yintercept = 0, color =c("white")) +
		scale_y_continuous(labels = scales::percent) + 
		scale_x_discrete(limits = rev) +
		scale_fill_manual("", values = fill_scale, drop = FALSE) + 
		labs(
			title = title_str, x = "", y = "Percentage response", fill = ""
		) + 
		theme_minimal() +
		coord_flip() +
		theme(legend.position = "bottom") +
		guides(fill = guide_legend(nrow = 1)) + 
		theme(
			panel.grid.major.y = element_blank(),
			panel.grid.minor = element_blank(),
			axis.text.y = element_text(size =  10),
			plot.title.position = "plot",
			title = element_text(size = 12),
			axis.title.x = element_text(size = 10),
			legend.text = element_text(size = 8),
			text=element_text(family="Times")
		)
}

pres_desc_plot <- function(df) {
	df <- df %>%
		select(
			demog_method, 
			sharing_perc_editor_prefer, sharing_perc_other_prefer,
			sharing_perc_other_share
		) %>%
		na.omit() %>%
		group_by(demog_method) %>% 
		pivot_longer(cols = 2:4, names_to = "var", values_to = "val") %>%
		mutate(val = val/100)
	
	
	df <- bind_rows(
		df, df %>%	mutate(demog_method = "Total Response")
	) %>% filter(demog_method != "Other") %>%
		mutate(demog_method = sprintf("%s\n(n=%d)", demog_method, n()/3))
	
	color_scale <- RColorBrewer::brewer.pal(3 ,"Set1")
	names(color_scale) <- unique(df$var)
	color_scale_labs <- c("Editors\nthat favor", "Researchers\nthat favor", "Researchers\nthat practice")
	
	ggplot(df, aes(
		x = demog_method, y = val, group = interaction(var, demog_method),
		color = var
	)) +
		geom_boxplot(outlier.shape = NA) +
		geom_quasirandom(
			size = 0.2, 
			dodge.width = 0.75
		) +	
		theme_minimal() +
		labs(
			x = "", y = "", color = "", 
			title = "What is the percentage that favor/practice research material sharing?"
		) +
		scale_y_continuous(labels = scales::percent) + 
		scale_color_manual("", values = color_scale, labels = color_scale_labs) +
		theme(
			legend.position = "bottom",
			panel.grid.major.x = element_blank(),
			panel.grid.minor = element_blank(),
			axis.text.y = element_text(size =  10),
			plot.title.position = "plot",
			title = element_text(size = 12),
			axis.title.x = element_text(size = 10),
			legend.text = element_text(size = 8),
			text=element_text(family="Times")
		) 
	
}


pre_reg_over_time_plot <- function(df) {
	color_scale <- RColorBrewer::brewer.pal(5 ,"Set1")
	
	df %>% select(demog_discipline, prereg_use_first_time) %>%
		group_by(demog_discipline) %>%
		mutate(ndisc = n()) %>%
		filter(!is.na(prereg_use_first_time)) %>%
		group_by(demog_discipline, prereg_use_first_time) %>%
		summarise(pct = unique(n()/ndisc), .groups = "drop_last") %>%
		rename(year = prereg_use_first_time) %>%
		arrange(demog_discipline, year) %>%
		mutate(share_prereg = cumsum(pct)) %>%
		filter(year >= 2010) %>%
		ggplot(., aes(x = year, y = share_prereg, color = demog_discipline)) +
		geom_line() + geom_point() +
		scale_color_manual("", values = color_scale) +
		scale_x_continuous("", breaks = seq(2010, 2022, by = 2)) +
		scale_y_continuous(labels = scales::percent) + theme_minimal() +
		labs(
			x = "", y = "", color = "", 
			title = "Approximately when was the first time you pre-registered\nhypotheses or analyses in advance of a study?"
		) +
		theme_minimal() +
		theme(
			legend.position = "bottom",
			panel.grid.major.x = element_blank(),
			panel.grid.minor = element_blank(),
			axis.text.y = element_text(size =  10),
			plot.title.position = "plot",
			title = element_text(size = 12),
			axis.title.x = element_text(size = 10),
			legend.text = element_text(size = 8),
			text=element_text(family="Times")
		) 
	
}

library(tidyverse)
library(ggbeeswarm)
library(readr)
library(readxl)
library(gghighlight)

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



byu_fields_comp <- function(byu_author_data, sd_data) {
  
  # Method and Research Area Mappings
  methodarea_map <- c('1' = 'method_archival', '2' = 'method_experimental', '3' = 'method_analytical', '4' = 'method_other')
  researcharea_map <- c('1' = 'area_financial', '2' = 'area_managerial', '3' = 'area_tax', '4' = 'area_audit', '5' = 'area_ais', '7' = 'area_other')
  
  byu_author_data <- byu_author_data %>%
    mutate(
      methodarea_cleaned = str_extract_all(methodarea, "\\d+"),
      researcharea_cleaned = str_extract_all(researcharea, "\\d+")
    )
  
  for (key in names(methodarea_map)) {
    byu_author_data[[methodarea_map[[key]]]] <- sapply(byu_author_data$methodarea_cleaned, function(x) key %in% x)
  }
  
  for (key in names(researcharea_map)) {
    byu_author_data[[researcharea_map[[key]]]] <- sapply(byu_author_data$researcharea_cleaned, function(x) key %in% x)
  }
  
  byu_author_data <- byu_author_data %>%
    select(-methodarea_cleaned, -researcharea_cleaned)
  
  # Prepare data for comparison
  field_order <- c('Financial accounting and reporting', 'Managerial accounting', 'Auditing', 'Taxation', 'Accounting information systems', 'Other')
  
  byu_to_sample_field_map <- c('area_financial' = 'Financial accounting and reporting', 'area_managerial' = 'Managerial accounting', 'area_audit' = 'Auditing', 'area_tax' = 'Taxation', 'area_ais' = 'Accounting information systems', 'area_other' = 'Other')
  
  byu_field_counts <- colSums(byu_author_data[, names(byu_to_sample_field_map)])
  names(byu_field_counts) <- byu_to_sample_field_map[names(byu_field_counts)]
  byu_field_counts <- byu_field_counts[field_order]
  
  our_field_counts <- table(factor(sd_data$demog_field, levels = field_order))
  
  # Calculate percentages
  byu_fields_percentage <- (byu_field_counts / sum(byu_field_counts)) * 100
  our_fields_percentage <- (our_field_counts / sum(our_field_counts)) * 100
  
  # Create the matrix for plotting
  fields_matrix <- rbind(byu_fields_percentage, our_fields_percentage)
  
  # Modify the label for plotting
  field_labels <- c('Financial accounting\nand reporting', 'Managerial\naccounting', 'Auditing', 'Taxation', 'AIS', 'Other')
  
  # Plotting the Fields/Areas comparison directly
  par(mgp = c(3, 1.5, 0))
  
  # Set axes = FALSE to prevent barplot from drawing axes
  bar_positions <- barplot(fields_matrix, beside = TRUE, col = c("skyblue", "lightgreen"), ylim = c(0, 100), 
                           names.arg = field_labels, las = 1, border = "black", 
                           cex.names = 1.1, axes = FALSE)
  
  # Redraw the X-axis line
  abline(h = 0, col = "black")
  
  # Add legend
  legend("topright", legend = c("BYU Sample", "Our Sample"), fill = c("skyblue", "lightgreen"), cex = 1.25)
  
  # Add percentages above the bars, rounded to whole digits and with "%" symbol
  text(bar_positions, fields_matrix + 2, labels = paste0(round(fields_matrix, 0), "%"), cex = 1.25, pos = 3)
  
  # Manually add y-axis labels only horizontally
  axis(2, cex.axis = 1.25, las = 1)  # las = 1 makes the labels horizontal
}


byu_methods_comp <- function(byu_author_data, sd_data) {
  
  # Method Area Mapping
  methodarea_map <- c('1' = 'method_archival', '2' = 'method_experimental', '3' = 'method_analytical', '4' = 'method_other')

  # Clean and split methodarea
  byu_author_data <- byu_author_data %>%
    mutate(methodarea_cleaned = str_extract_all(methodarea, "\\d+"))

  # Create binary columns for methodarea
  for (key in names(methodarea_map)) {
    byu_author_data[[methodarea_map[[key]]]] <- sapply(byu_author_data$methodarea_cleaned, function(x) key %in% x)
  }

  # Prepare data for comparison
  method_order <- c('Archival/Field', 'Experimental', 'Analytical', 'Other')
  
  byu_to_sample_method_map <- c('method_archival' = 'Archival/Field', 'method_experimental' = 'Experimental', 'method_analytical' = 'Analytical', 'method_other' = 'Other')
  
  # Ensure that the summation of counts considers all valid cases
  byu_method_counts <- sapply(names(byu_to_sample_method_map), function(col) {
    sum(byu_author_data[[col]], na.rm = TRUE)
  })
  
  names(byu_method_counts) <- byu_to_sample_method_map
  
  # Reorder according to method_order to ensure consistency
  byu_method_counts <- byu_method_counts[method_order]
  
  # Combine "Qualitative" and "Survey" into "Other" for demog_method in "Our sample"
  sd_data$demog_method <- factor(sd_data$demog_method)
  levels(sd_data$demog_method)[levels(sd_data$demog_method) %in% c('Qualitative', 'Survey')] <- 'Other'
  sd_data$demog_method <- droplevels(sd_data$demog_method)

  our_method_counts <- table(factor(sd_data$demog_method, levels = method_order))
  
  # Calculate percentages
  byu_methods_percentage <- (byu_method_counts / sum(byu_method_counts)) * 100
  our_methods_percentage <- (our_method_counts / sum(our_method_counts)) * 100
  
  # Create the matrix for plotting
  methods_matrix <- rbind(byu_methods_percentage, our_methods_percentage)
  
  # Modify the label for plotting
  method_labels <- c('Archival/Field', 'Experimental', 'Analytical', 'Other')
  
  # Plotting the Methods comparison directly
  par(mgp = c(3, 1.5, 0))
  
  # Set axes = FALSE to prevent barplot from drawing axes
  bar_positions <- barplot(methods_matrix, beside = TRUE, col = c("skyblue", "lightgreen"), ylim = c(0, 100), 
                           names.arg = method_labels, las = 1, border = "black", 
                           cex.names = 1.25, axes = FALSE)
  
  # Redraw the X-axis line
  abline(h = 0, col = "black")
  
  # Add legend
  legend("topright", legend = c("BYU Sample", "Our Sample"), fill = c("skyblue", "lightgreen"), cex = 1.25)
  
  # Add percentages above the bars, rounded to whole digits and with "%" symbol
  text(bar_positions, methods_matrix + 2, labels = paste0(round(methods_matrix, 0), "%"), cex = 1.25, pos = 3)
  
  # Manually add y-axis labels only horizontally
  axis(2, cex.axis = 1.25, las = 1)  # las = 1 makes the labels horizontal
}

post_hoc_power_figure <- function(power_res) {
	ggplot(
		power_res %>% filter(term == "collabTRUE"), 
		aes(x = teffect, y = power, group = n_bin)
	) +
		geom_line() + geom_point() +
		gghighlight(n_bin == 60, use_direct_label = F, use_group_by = F) + 
		geom_hline(yintercept = 0.8, lty = 2, color = "red") + 
		labs(x = "Main Effect (H1/H2)", y = "Power", color = "Bin size") +
		scale_y_continuous(labels = scales::percent) +
		scale_x_continuous(labels = function(x) paste(round(100*x), "PP")) +
		theme_classic(base_size = 18) +
		theme(
			text=element_text(family="Times")
		) 
}


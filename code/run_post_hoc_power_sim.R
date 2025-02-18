suppressMessages({
	library(tidyverse)
	library(broom)
	library(purrr)
})

set.seed(42)

N_BIN = 64
BLINE_PROB = 0.6 # Control/Public
BLINE_SD = 0.25

NRUNS <- 10000
FNAME_STUB <- "power_sim"

fname_sim_dta <- paste0("data/generated/", FNAME_STUB, "_", NRUNS, "_data.rds")

if (file.exists(fname_sim_dta)) {
	sim_dta <- readRDS(fname_sim_dta)
} else {
	sim_sample <- function(
		teffect_collab, teffect_private, teffect_iaction,
		n_bin = N_BIN, bline = BLINE_PROB, bline_sd = BLINE_SD
	) {
		tibble(
			collab = c(rep(F, 2*n_bin), rep(T, 2*n_bin)),
			private = c(rep(F, n_bin), rep(T, n_bin), rep(F, n_bin), rep(T, n_bin)),
			share_prob  = round(20*pmin(
				pmax(
					rep(0, 4*n_bin), 
					bline + teffect_collab*collab + teffect_private*private + 
						teffect_iaction*collab*private + rnorm(4*n_bin, 0, bline_sd)
				), 
				rep(1, 4*n_bin)
			))/20
		)
	}
	
	test <- function(df) {
		rv <- lm(share_prob ~ collab*private, data = df)
		tidy(rv, conf.int = TRUE)
	}
	
	do_run <- function(teffect, n_bin, run) {
		df <- sim_sample(teffect, teffect, teffect/2, n_bin)
		bind_cols(tibble(run = run, teffect = teffect, n_bin = n_bin, test(df)))
	}
	
	sim_grid <- expand_grid(
		n_bin = seq(40, 100, 10),
		teffect = seq(0.01, 0.15, 0.01),
		run = 1:NRUNS
	)
	
	sim_dta <- bind_rows(
		pmap(sim_grid, do_run, .progress = paste("Simulating", NRUNS, "runs"))
	)
	
	saveRDS(sim_dta, fname_sim_dta)
}

power_res <- sim_dta %>% 
	group_by(n_bin, teffect, term) %>%
	summarise(power = mean(conf.low > 0), .groups = "drop")

fname_sim_res <- paste0("data/static/", FNAME_STUB, "_", NRUNS, "_results.rds")

saveRDS(power_res, fname_sim_res)



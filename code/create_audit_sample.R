suppressMessages(library(tidyverse))

set.seed(42)

N_TO_SAMPLE <- 100

# To create the actual audit sample, you would need to have the 
# unrestricted authors file, containing their access information.
# If you feel that you need this file, please contact the 
# authors.

pop <- read_csv("data/external/invited_authors.csv")
table(pop$affil_country)

smp <- pop %>%
	group_by(affil_country) %>%
	sample_frac(N_TO_SAMPLE/nrow(pop))

smp <- smp[sample(nrow(smp)),]
table(smp$affil_country)

write_csv(smp, "data/generated/audit_sample.csv")

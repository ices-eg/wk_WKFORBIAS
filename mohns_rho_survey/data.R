## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)

mkdir("data")

# read in retro bias results
mohns_all <- read.taf("bootstrap/data/retrobias_survey/mohns_all.csv")

sd <- read.taf("bootstrap/data/sid_data/stockinfo.csv")

# combine
mohns_all <-
  mohns_all %>%
  right_join(sd, by = c("stock", "year")) %>%
  arrange(stock, year)

# save
write.taf(mohns_all, dir = "data", quote = TRUE)

# all stocks
mohns_all %>%
  select(stock, SpeciesCommonName, year, ExpertGroup, ssb_rho, rec_rho, fbar_rho) %>%
  arrange(stock, year)

# which stocks are not represented
missing_stocks <-
  mohns_all %>%
    filter(is.na(ssb_rho)) %>%
    select(stock, SpeciesCommonName, year, ExpertGroup, ssb_rho, rec_rho, fbar_rho) %>%
    arrange(stock, year)

write.taf(missing_stocks, dir = "data", quote = TRUE)

# which stocks *are* represented
provided_stocks <-
  mohns_all %>%
    filter(!is.na(ssb_rho)) %>%
    select(stock, SpeciesCommonName, year, ExpertGroup, ssb_rho, rec_rho, fbar_rho) %>%
    arrange(stock, year)

write.taf(provided_stocks, dir = "data", quote = TRUE)

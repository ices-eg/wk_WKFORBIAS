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
  select(year, stock, SpeciesCommonName, ExpertGroup, DataCategory, ssb_rho, rec_rho, fbar_rho) %>%
  arrange(year, stock)

# which stocks are not represented
missing_cat_12_stocks <-
  mohns_all %>%
    filter(is.na(ssb_rho) & DataCategory <= 2) %>%
    select(year, stock, SpeciesCommonName, ExpertGroup, DataCategory) %>%
    arrange(year, stock)

write.taf(missing_cat_12_stocks, dir = "data", quote = TRUE)

# which stocks *are* represented
provided_stocks <-
  mohns_all %>%
    filter(!is.na(ssb_rho)) %>%
    select(year, stock, SpeciesCommonName, ExpertGroup, DataCategory, ssb_rho, rec_rho, fbar_rho) %>%
    arrange(year, stock)

write.taf(provided_stocks, dir = "data", quote = TRUE)

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
  arrange(stock, year) %>%
  select(-GeneratedOn, -ModifiedDate)

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
    select(year, stock, SpeciesCommonName, ExpertGroup, DataCategory, AssessmentType) %>%
    arrange(year, stock)

# remove ones that are not relavent
missing_cat_12_stocks <-
  missing_cat_12_stocks %>%
  filter(!substring(stock, 1, 3) %in% c("nep", "sal", "cap")) %>%
  filter(!stock %in% c("her.27.30", "dgs.27.nea", "bli.27.5b67")) %>%
  filter(!grepl(".*SS3|Gadget|CBBM|Bayesian|Stock Synthesis 3|XSAM.*", AssessmentType)) %>%
  filter(!(stock %in% c("tur.27.4", "wit.27.3a47d") & year == 2018))

write.taf(missing_cat_12_stocks, dir = "data", quote = TRUE)

# which stocks *are* represented
provided_stocks <-
  mohns_all %>%
    filter(!is.na(ssb_rho)) %>%
    select(year, stock, SpeciesCommonName, ExpertGroup, DataCategory, ssb_rho, rec_rho, fbar_rho) %>%
    arrange(year, stock)



#mohns_all[mohns_all$year == 2019 &
#          mohns_all$stock == "nop.27.3a4",
#          c("ssb_rho", "rec_rho", "fbar_rho")]

# scale values provided in percent

filter(provided_stocks, abs(ssb_rho) > 10 | abs(fbar_rho) > 10 | abs(rec_rho) > 10)

perc_stocks <-
  c("nop.27.3a4",
    "her.27.3031", "her.27.3a47d", "her.27.6a7bc",
    "san.sa.3r",
    "sol.27.20-24")

provided_stocks <-
  provided_stocks %>%
  mutate(
    ssb_rho = ifelse(stock %in% perc_stocks, ssb_rho / 100, ssb_rho),
    fbar_rho = ifelse(stock %in% perc_stocks, fbar_rho / 100, fbar_rho),
    rec_rho = ifelse(stock %in% perc_stocks, rec_rho / 100, rec_rho)
    )

write.taf(provided_stocks, dir = "data", quote = TRUE)



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

# remove ones that are not relavent
missing_cat_12_stocks <-
  missing_cat_12_stocks %>%
  filter(!substring(stock, 1, 3) %in% c("nep", "sal")) %>%
  filter(!stock %in% c("her.27.30"))

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
# is 50% for SSB, -33% for Fbar, and 103% for R shown in Figure 12.3.8. 

# .  For Icelandic saithe retro of SSB is 0.08, F -0.07, Recruiment -0.46 and Reference biomass -0.01.  



# apply submissions not in DB


write.taf(provided_stocks, dir = "data", quote = TRUE)



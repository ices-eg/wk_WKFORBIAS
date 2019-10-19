
library(icesTAF)
taf.library(icesSD)
library(dplyr)

msg("getting auxilliary stock data from ICES SD database")
sd <- icesSD::getSD()

sd <-
  sd %>%
  select(StockKeyLabel, ActiveYear,
         ExpertGroup, DataCategory,
         AssessmentType, AssessmentKey,
         TrophicGuild, SizeGuild) %>%
  filter(ActiveYear %in% c(2018, 2019))
msg("getting stock coordinators")
stockinfo <- jsonlite::read_json("http://internalservices.ices.local:60/api/SDinternalDWs",
                         simplifyVector = TRUE)
stockinfo <-
  stockinfo %>%
  select(StockKeyLabel, ActiveYear,
         ExpertGroup, DataCategory,
         TrophicGuild, SizeGuild,
         StockCoordinator1, StockCoordinator2,
         AssessmentCoordinator1, AssessmentCoordinator2) %>%
  filter(ActiveYear %in% c(2018, 2019)) %>%
  rename(
    stock = StockKeyLabel,
    year = ActiveYear
  ) %>%
  left_join(sd)

# save
write.taf(stockinfo, quote = TRUE)

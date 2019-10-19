
library(icesTAF)
taf.library(icesSD)
library(dplyr)

msg("getting auxilliary stock data from ICES SD database")
stockinfo <- icesSD::getSD()

stockinfo <-
  stockinfo %>%
  filter(ActiveYear %in% c(2018, 2019)) %>%
  rename(
    stock = StockKeyLabel,
    year = ActiveYear
  )

# save
write.taf(stockinfo, quote = TRUE)

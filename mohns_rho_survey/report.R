## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(ggplot2)

mkdir("report")

# read data

mohns_raw <- read.csv("data/provided_stocks.csv")

filter(mohns_raw, abs(ssb_rho) > 10 | abs(fbar_rho) > 10 | abs(rec_rho) > 10)

perc_stocks <-
  c("nop.27.3a4", 
    "her.27.3031", "her.27.3a47d", "her.27.6a7bc",
    "san.sa.3r",
    "sol.27.20-24")

mohns <- 
  mohns_raw %>%
  mutate(
    ssb_rho = ifelse(stock %in% perc_stocks, ssb_rho / 100, ssb_rho),
    fbar_rho = ifelse(stock %in% perc_stocks, fbar_rho / 100, fbar_rho),
    rec_rho = ifelse(stock %in% perc_stocks, rec_rho / 100, rec_rho)
    )



# SSB rho by stocks
ssb_plot <- 
  ggplot() +
    geom_point(data = mohns, aes(x = stock, y = ssb_rho, col = abs(ssb_rho) > 0.2)) +
    geom_hline(yintercept = c(-1, 1) * 0.2, col = "darkgreen") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    facet_wrap( ~ year)

ggplot2::ggsave("SSB_rho_Figure1.png", path = "report", 
                width = 170, height = 200, 
                units = "mm", dpi = 300)



ggplot() +
  geom_point(data = mohns, aes(x = stock, y = fbar_rho))


ggplot() +
  geom_point(data = mohns, aes(x = stock, y = ssb_rho))

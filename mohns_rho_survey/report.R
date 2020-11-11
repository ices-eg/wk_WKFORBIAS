## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(ggplot2)
library(htmlwidgets)
library(glue)

mkdir("report")

# read data

mohns <- read.csv("data/provided_stocks.csv")





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


p <- plotly::ggplotly(ssb_plot)
saveWidget(p, "ssb_retro.html", selfcontained = TRUE)
cp("ssb_retro.html", "report", move = TRUE)


files <- c(
  dir("data", full = TRUE),
  dir("report", full = TRUE)
)

zip(
  file.path(
    "report",
    glue("mohns_rho_{Sys.Date()}.zip")
  ),
  files,
  extras = "-j"
)

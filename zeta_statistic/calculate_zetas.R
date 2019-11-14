
install_github("fishfollower/sam/stockassessment@df7252ec66d2")
library(stockassessment)

stocks <- list.dirs(recursive = FALSE, full.names = FALSE)

# get function
source("utilities.R")


for (stock in stocks) {
  #stock <- stocks[1]
  # load data
  (load(file.path(stock, "fit.RData")))

  # only use 5 peels
  RETRO <- RETRO[1:5]

  #unique(names(fit$sdrep$value))

  ssb_zeta <- calc_zetas("logssb", fit, RETRO)
  fbar_zeta <- calc_zetas("logfbar", fit, RETRO)
  R_zeta <- calc_zetas("logR", fit, RETRO)

  pssb <- 
    with(ssb_zeta,
      plot_zetas(all, retro, zetas, paste0(stock, ", ssb \n"))
    )

  pfbar <- 
    with(fbar_zeta,
      plot_zetas(all, retro, zetas, paste0(stock, ", fbar \n"))
    )

  pR <- 
    with(R_zeta,
      plot_zetas(all, retro, zetas, paste0(stock, ", R \n"))
    )
    
  png(paste0(stock, ".png"),
      width = 300,
      height = 100,
      units = "mm",
      res = 300)
  gridExtra::grid.arrange(pR, pssb, pfbar, ncol = 3)
  dev.off()

}


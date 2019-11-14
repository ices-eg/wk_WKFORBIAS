
require(ggplot2)

tableit2 <- function(fit, what = "logssb") {
  idx <- names(fit$sdrep$value) == what
  data.frame(
    year = fit$data$years,
    fit = fit$sdrep$value[idx],
    se.fit = fit$sdrep$sd[idx]
  )
}


calc_zetas <- function(what = "logssb", fit, RETRO) {
  # extract terminal year fits and se.fits from RETRO
  retro <- 
    do.call(
      rbind,
      lapply(
        lapply(RETRO, tableit2, what = what), 
      tail, 1)
    )

  # merge with final run fits and se.fits
  retro_merged <-
    merge(
      retro, 
      tableit2(fit, what = what), 
      by = "year", 
      suffixes = c(".peel",".final")
    )

  # compute various standardised rhos
  zetas <- 
    with(retro_merged,
      c(
        zeta_f = sqrt(length(fit.peel)) * mean((fit.peel - fit.final)/se.fit.final),
        zeta_p = sqrt(length(fit.peel)) * mean((fit.peel - fit.final)/se.fit.peel)
      )
    )

  i <<- 0
  all <- 
    do.call(
      rbind,
      lapply(
        c(list(fit), RETRO),
        function(x) {
          x <- cbind(tableit2(x, what = what), id = i <<- i + 1)
          x[x$year > 2004,]  
        }      
      ))

  list(all = all, retro = retro, zetas = zetas)
}


plot_zetas <- function(x_all, x_retro, zetas, main = "") {
  ggplot(data = x_all, 
         aes(x = year, y = exp(fit), group = factor(id))) +
    geom_line() +
    geom_ribbon(
      data = x_all[x_all$id == 1,],
      aes(x = year, 
          ymin = exp(fit - 2*se.fit),
          ymax = exp(fit + 2*se.fit)), 
      fill = grey(0.5, 0.5),
      col = "transparent") +
    geom_linerange(
      data = cbind(x_retro, id = 1:nrow(ssb_retro) + 1),
      aes(x = year, 
          ymin = exp(fit - 2*se.fit),
          ymax = exp(fit + 2*se.fit))
      ) +
#    annotate(
#      "text",
#      x = max(x_retro$year)-2,
#      y = max(exp(x_all$fit)),
#      label = paste(names(zetas), round(zetas, 3), collapse = "\n", sep = ": ")
 #   ) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(
      paste0(
        main, 
        paste(names(zetas), round(zetas, 3), collapse = "\n", sep = ": ")
      )
    ) +
    ylab("")
}

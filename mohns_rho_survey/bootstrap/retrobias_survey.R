

library(icesTAF)
taf.library(icesSharePoint)
library(dplyr)

getList <- function(lst) {
  msg("getting results from SP list: ", lst)
  uri <- sprintf("https://community.ices.dk/ExpertGroups/_api/web/lists/GetByTitle('%s')/Items", lst)
  res <- spget(uri)$d$results

  # return everything as a data.frame (but not meta entry tag [-1])
  out <-
    do.call(
      rbind.data.frame,
      lapply(res, function(x) {
        spget(x$FieldValuesAsText$`__deferred`$uri)$d[-1]
      })
    )
  out %>%
    rename(
      stock = Stock_x005f_x0020_x005f_code,
      terminal_catch_year = Terminal_x005f_x0020_x005f_year_x005f_x0020_x005f_of_x005f_x00,
      n_retros = Number_x005f_x0020_x005f_of_x005f_x0020_x005f_retrospect,
      fbar_rho = Fbar_x005f_x0020_x005f_rho_x005f_x0020_x005f_value,
      ssb_rho = SSB_x005f_x0020_x005f_rho_x005f_x0020_x005f_value,
      ssb_intermediate_year = SSB_x005f_x0020_x005f_rho_x005f_x003a_x005f__x005f_x0020_x005f_was_x005f_x,
      rec_rho = Recruitment_x005f_x0020_x005f_rho_x005f_x0020_x005f_valu,
      rec_intermediate_year = Recruitment_x005f_x0020_x005f_rho_x005f_x003a_x005f__x005f_x00,
      expert_opinion = Expert_x005f_x0020_x005f_opinion_x005f_x0020_x005f_on_x005f_x0,
      author = Author,
      editor = Editor,
      modified = Modified
    ) %>%
    select(
      stock, terminal_catch_year, n_retros,
      fbar_rho,
      ssb_rho, ssb_intermediate_year,
      rec_rho, rec_intermediate_year,
      expert_opinion,
      author, editor, modified
    )
}

# get data from sharepoint
mohns_2018 <- getList('Retro-bias-2018')
mohns_2019 <- getList('Retro-bias-2019')

# save
write.taf(mohns_2018, quote = TRUE)
write.taf(mohns_2019, quote = TRUE)

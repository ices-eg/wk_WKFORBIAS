

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
      select(
        matches("^Stock|^Terminal|^Number|^Fbar|^SSB|^Recruitment|^Expert"),
        Author, Editor, Modified
      ) %>%
      rename_all(
        funs(
            stringr::str_replace_all(., "_x[0-9]{3}[0-9a-f]", "") %>%
              stringr::str_replace_all(., "_x[0-9]*", "") %>%
              stringr::str_replace_all(., "_va?l?u?e?_?", "_value") %>%
              stringr::str_replace_all(., "_$|_of$|_+was$|_on$", "")
        )
      ) %>%

    rename(
      stock = Stock_code,
      terminal_catch_year = Terminal_year,
      n_retros = Number_of_retrospect,
      fbar_rho = Fbar_rho_value,
      ssb_rho = SSB_rho_value,
      ssb_intermediate_year = SSB_rho,
      rec_rho = Recruitment_rho_value,
      rec_intermediate_year = Recruitment_rho,
      expert_opinion = Expert_opinion,
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
mohns_2020 <- getList('Retro-bias-2020')

# combine
mohns_all <-
  rbind(cbind(mohns_2018, year = 2018),
        cbind(mohns_2019, year = 2019),
        cbind(mohns_2020, year = 2020))
rownames(mohns_all) <- NULL

# check which stock names were missing
mohns_all %>% filter(stock == "")

# remove names
mohns_all <-
  mohns_all %>%
  select(-author, -editor)

# save
write.taf(mohns_all, quote = TRUE)

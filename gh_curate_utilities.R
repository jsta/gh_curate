library(gh)
library(lq) # remotes::install_github("ropensci/lq")
library(jqr)
suppressMessages(library(dplyr))
suppressMessages(library(purrr))

unpack_gh <- function(x, key) {
  n_levels <- length(key)
  if (n_levels > 1) {
    res <- (tj(x) %>% index()) %>% fj
    res <- res[,key]
    res
  }else{
    if (length(grep("/", key)) > 0) {
      key1 <- regmatches(key, regexpr("^.*(?=\\/)", key, perl = TRUE))
      key2 <- regmatches(key, regexpr("(?<=\\/).*", key, perl = TRUE))
      as.character(flatten(x[key1])[key2])
    }else{
      as.character(flatten(x[key]))
    }
  }
}

pull_gh <- function(quantity = NA, user_name = NA, n_pages = NA, key,
                    query_string = NA){
  res      <- list()
  if (all(is.na(query_string))) {
    res_page <- gh(paste0("/users/:username/", quantity), username = user_name)
    res[[1]] <- res_page

    for (i in 2:n_pages) {
      tryCatch({
        print(i)
        res_page <- gh_next(res_page)
        res[[i]] <- unpack_gh(res_page, key)
      },
      error = function(e){
      }
      )
    }

    res[[1]] <- unpack_gh(res[[1]], key)
    res <- suppressWarnings(bind_rows(lapply(res, function(x) data.frame(id = x))))
  }else{
    for (i in seq_along(query_string)) {
      # i <- query_string[1]
      res_page <- gh(query_string[i])
      res[[i]] <- unpack_gh(res_page, key)
    }
    res <- as.character(res)
  }
  res
}

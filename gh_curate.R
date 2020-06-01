library(gh)
library(lq) # remotes::install_github("ropensci/lq")
library(jqr)
suppressMessages(library(dplyr))
suppressMessages(library(purrr))

total_stars     <- 1500
total_followers <- 120
total_following <- 400
user_name <- "jsta"

unpack_gh <- function(x, key) {
  n_levels <- length(key)
  if(n_levels > 1){
    as.character(unlist(flatten((tj(x) %>% index()) %>% fj)[key[2]]))
  }else{
    as.character(unlist(((tj(x) %>% index()) %>% fj)[key]))
  }
}

latest_gh <- function(user_name){
  print(user_name)
  quantity <- "events"
  res_page <- gh(paste0("/users/:username/", quantity), username = user_name)
  if(nchar(res_page[[1]]) > 0){
    res_page[[1]]$created_at
  }else{
    NA
  }
}

pull_gh <- function(quantity, user_name, n_pages, key){
  res      <- list()
  res_page <- gh(paste0("/users/:username/", quantity), username = user_name)
  res[[1]] <- res_page
  for(i in 2:n_pages){
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
  res
}

# get stars ####
stars <- pull_gh("starred", user_name,
                 n_pages = ceiling(total_stars) / 30,
                 key = c("owner", "login"))

group_by(stars, id) %>%
  dplyr::tally() %>%
  dplyr::arrange(desc(n))

# get followers ####
followers <- pull_gh("followers", user_name,
                     n_pages = ceiling(total_followers / 30),
                     key = "login")

# get following ####
following <- pull_gh("following", user_name,
                     n_pages = ceiling(total_following / 30),
                     key = "login")

# rm following not in followers or in stars
bad_following <- following[!(following$id %in% followers$id) &
                             !(following$id %in% stars$id), "id"]
paste0(length(test), " following with no stars who are not mutual followers")

# rm users active in the last 90 days
last_action_date <- lapply(bad_following, latest_gh)
last_action_date <-  unlist(last_action_date)
bad_following    <- bad_following[is.na(last_action_date)]

# rm users in common orgs
org_name <- "cont-limno"
res_page <- gh(paste0("/orgs/", org_name, "/members"))
org_members <- as.character(unlist(lapply(res_page, function(x) x["login"])))
bad_following <- bad_following[!(bad_following %in% org_members)]

org_name <- "GLEON"
res_page <- gh(paste0("/orgs/", org_name, "/members"))
org_members <- as.character(unlist(lapply(res_page, function(x) x["login"])))
bad_following <- bad_following[!(bad_following %in% org_members)]

org_name <- "USGS-R"
res_page <- gh(paste0("/orgs/", org_name, "/members"))
org_members <- as.character(unlist(lapply(res_page, function(x) x["login"])))
bad_following <- bad_following[!(bad_following %in% org_members)]

# rm users meeting "bad user" criteria
baduser_gh <- function(user_name){
  # user_name <- "DruidSmith"
  print(user_name)
  res_page <- gh(paste0("/users/", user_name))
  res <- tibble::enframe(unlist(res_page)) %>%
    tidyr::spread(name, value) %>%
    dplyr::filter(followers >= 3 &
                    public_repos > 0 &
                    updated_at > "2017-01-01")
  nrow(res) == 0
}

badusers <- unlist(lapply(bad_following, baduser_gh))
bad_following <- bad_following[badusers]

# make sure your token has unfollow permissions
sapply(bad_following, function(x) gh(paste0("DELETE /user/following/:username"),
                                     username = x))


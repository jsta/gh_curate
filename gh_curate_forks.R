# https://developer.github.com/v3/repos/#get-a-repository
source("gh_curate_utilities.R")

# pull jsta forks
user_name <- "jsta"
repos <- pull_gh("repos", user_name,
                 n_pages = 2,
                 key = c("full_name", "fork"))
forks <- dplyr::filter(repos, id.fork == TRUE)

# pull parent repo info
parents <- pull_gh(query_string =
                     paste0("/repos/", forks$id.full_name),
                   key = "parent/full_name")
forks$parents <- parents

updated_at <- pull_gh(query_string =
                     paste0("/repos/", forks$parents),
                   key = "pushed_at")
updated_at <- data.frame(parents = forks$parents, updated_at = updated_at)
forks <- dplyr::left_join(forks, updated_at, by = "parents")

# pull CRAN status
library(cranlogs) # see cranlogs package
library(pkgsearch)

test <- pkgsearch::ps("discretization")
test <- test[which.max(test$score),]
dplyr::filter(test, score == 100)

parent_repos <- regmatches(forks$parents,
                          regexpr("(?<=\\/).*", forks$parents, perl = TRUE))
forks$repo <- parent_repos
on_cran <- cranlogs::cran_downloads(parent_repos[!grepl("-", parent_repos)]) %>%
  dplyr::filter(count != 0) %>%
  pull(package)
forks$cran <- forks$repo %in% on_cran



forks <- forks[forks$repo %in% on_cran,]


# remove fork if the upstream repo was updated recently
# remove fork if on CRAN


cutoff_date <- Sys.Date() - 365
forks       <- dplyr::filter(forks, updated_at > cutoff_date)

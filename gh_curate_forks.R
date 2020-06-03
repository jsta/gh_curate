# https://developer.github.com/v3/repos/#get-a-repository
source("gh_curate_utilities.R")
library(pkgsearch)
library(cranlogs)

# protect specific repos
to_keep <- c("jsta/binb", "jsta/cdlTools", "jsta/lakemorpho", "jsta/rgrass7sf",
             "jsta/EML", "jsta/esri2sf", "jsta/laketemps", "jsta/msu-thesis",
             "jsta/OCNet", "jsta/organization-geospatial", "jsta/pinp",
             "jsta/r-raster-vector-geospatial", "jsta/ReScience-submission",
             "jsta/rethinking", "jsta/riverdist", "jsta/rloadest",
             "jsta/rnoaa", "jsta/smwrQW", "jsta/styles", "jsta/tgp",
             "jsta/tidybayes", "jsta/vapour", "jsta/workshop-template")

# pull jsta forks
user_name <- "jsta"
repos <- pull_gh("repos", user_name,
                 n_pages = 14,
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
parent_repos <- regmatches(forks$parents,
                          regexpr("(?<=\\/).*", forks$parents, perl = TRUE))
forks$repo <- parent_repos
on_cran <- cranlogs::cran_downloads(parent_repos[!grepl("-", parent_repos)]) %>%
  dplyr::filter(count != 0) %>%
  pull(package)
forks$cran <- forks$repo %in% on_cran

# remove fork if maintainer string is a fuzzy match to parent owner
# disambiguate non-cran repos with the same name
forks$owner <- regmatches(forks$parents,
                          regexpr("^.*(?=\\/)", forks$parents, perl = TRUE))
cran_match <- function(x){
  res <- pkgsearch::ps(x)
  res[which.max(res$score),][c("maintainer_name", "maintainer_email", "url")]
}
cran_matches <- lapply(forks$repo[forks$cran], cran_match)
cran_matches <- dplyr::bind_rows(cran_matches) %>%
  mutate(repo = forks$repo[forks$cran])
# forks <- dplyr::select(forks, -maintainer_name, -url)
forks <- left_join(forks, cran_matches, by = "repo")
forks$url[grep(",", forks$url)] <- regmatches(forks$url[grep(",", forks$url)],
                        regexpr("^.*(?=,)", forks$url[grep(",", forks$url)], perl = TRUE))
forks$github <- grepl("github", forks$url)
forks$cran_repo <- NA
forks$cran_repo[forks$github] <- basename(forks$url[forks$github])
forks$cran_parent <- NA
forks$cran_parent[forks$github] <- base_str_extract(forks$url[forks$github], "(?<=\\/github.com/).*")

# set cran to false if no fuzzy match between [owner, maintainer_name]
forks$cran <- forks$cran & (
  apply(forks, 1, function(x)
    agrepl(x["owner"], x["maintainer_name"],
           max.distance = 5, ignore.case = TRUE)) |
    forks$parents == forks$cran_parent)
forks$cran[is.na(forks$cran)] <- FALSE

forks$to_remove <- FALSE
# remove fork if the owner is cran
forks$to_remove[forks$cran] <- TRUE
# remove fork if the upstream repo was updated recently
cutoff_date <- "2020-03-13" # Sys.Date() - 365
forks$to_remove[forks$updated_at > cutoff_date] <- TRUE

# keep specific forks
forks$to_remove[forks$id.full_name %in% to_keep] <- FALSE

# DELETE!!!!
res <- list()
i   <- 1
res <- sapply(forks$id.full_name[forks$to_remove],
       function(x){
         # x <- forks$id.full_name[forks$to_remove][1]
         approval <- 3
         while (approval == 3) {
           approval <- menu(c("Yes", "No", "Browser"),
                            title = paste0("delete ", x, "?"))
            if (approval == 3) {
              system(paste0("firefox https://github.com/", x))
            }
            if (approval == 1) {
              gh(paste0("DELETE /repos/:repo"), repo = x)
            }
           if (approval == 2) {
             res[[i]] <- x
           }
         }
         i <- i + 1
        })

clipr::write_clip(names(res))

# datapasta::vector_paste()

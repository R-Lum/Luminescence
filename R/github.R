#  ------------------------------------------------------------------------
# Author: Christoph Burow <christoph.burow@uni-koeln.de>
# Affiliation: University of Cologne
# Date: 10/01/2017
# API version: v3
# Reference: https://developer.github.com/v3/
#  ------------------------------------------------------------------------

#' GitHub API
#' 
#' R Interface to the GitHub API v3.
#' 
#' These functions can be used to query a specific repository hosted on GitHub. \cr
#' 
#' 
#' @param user \code{\link{character}}: 
#' GitHub user name (defaults to 'r-lum').
#' 
#' @param repo \code{\link{character}}: 
#' name of a GitHub repository (defaults to 'luminescence').
#' 
#' @param branch \code{\link{character}}: 
#' branch of a GitHub repository (defaults to 'master').
#' 
#' @param n \code{\link{integer}}:
#' number of commits returned (defaults to 5).
#' 
#' @param verbose \code{\link{logical}}: 
#' print the output to the console (defaults to \code{TRUE}).
#' 
#' @author Christoph Burow, University of Cologne (Germany)
#' 
#' @section Function version: 0.1.0
#' 
#' @references 
#' 
#' GitHub Developer API v3. \url{https://developer.github.com/v3/}, last accessed: 10/01/2017.
#' 
#' @examples
#' 
#' \dontrun{
#' github_branches(user = "r-lum", repo = "luminescence")
#' github_issues(user = "r-lum", repo = "luminescence")
#' github_commits(user = "r-lum", repo = "luminescence", branch = "master", n = 10)
#' }
#' 
#' @name GitHub-API
NULL

# COMMITS -----------------------------------------------------------------
#' @rdname GitHub-API
#' 
#' @details 
#' \code{github_commits} lists the most recent \code{n} commits of a specific
#' branch of a repository.
#' 
#' @return 
#' \code{github_commits}: \code{\link{data.frame}} with columns:
#' \tabular{ll}{
#'  [ ,1] \tab SHA \cr
#'  [ ,2] \tab AUTHOR \cr
#'  [ ,3] \tab DATE \cr
#'  [ ,4] \tab MESSAGE \cr
#' }
#' 
#' @export
github_commits <- function(user = "r-lum", repo = "luminescence", 
                           branch = "master", n = 5) {
  
  # fetch available branches and check if provided branch exists
  branches <- github_branches(user, repo)
  if (!any(grepl(branch, branches$BRANCH)))
    stop("Branch ", branch, " does not exist.", call. = FALSE)
  
  # build URL and retrieve content
  sha <- branches$SHA[grep(paste0("^", branch, "$"), branches$BRANCH)]
  url <- paste0("https://api.github.com/repos/", user, "/", repo, "/commits?",
                "per_page=", n, "&sha=", sha)
  content <- github_getContent(url)
  
  # format output as data.frame
  output <- do.call(rbind, lapply(content, function(x) {
    data.frame(SHA = x$sha, 
               AUTHOR = x$commit$author$name, 
               DATE = x$commit$author$date, 
               MESSAGE = x$commit$message,
               stringsAsFactors = FALSE)
  }))
  
  return(output)
}


# BRANCHES ----------------------------------------------------------------
#' @rdname GitHub-API
#' 
#' @details 
#' \code{github_branches} can be used to list all current branches of a
#' repository and returns the corresponding SHA hash as well as an installation
#' command to install the branch in R via the 'devtools' package.
#' 
#' @return 
#' \code{github_branches}: \code{\link{data.frame}} with columns:
#' \tabular{ll}{
#'  [ ,1] \tab BRANCH \cr
#'  [ ,2] \tab SHA \cr
#'  [ ,3] \tab INSTALL \cr
#' }
#' 
#' @export
github_branches <- function(user = "r-lum", repo = "luminescence") {
  
  # build URL and retrieve content
  url <- paste0("https://api.github.com/repos/", user, "/", repo, "/branches")
  content <- github_getContent(url)
  
  # extract relevant information from server response
  branches <- sapply(content, function(x) x$name)
  sha <- sapply(content, function(x) x$commit$sha)
  
  # format output as data.frame
  output <- data.frame(
    BRANCH = branches,
    SHA = sha,
    INSTALL = paste0("devtools::install_github('r-lum/luminescence@", branches, "')"),
    stringsAsFactors = FALSE
  )
  
  return(output)
}


# ISSUES ------------------------------------------------------------------
#' @rdname GitHub-API
#' 
#' @details 
#' \code{github_issues} lists all open issues for a repository in valid YAML.
#' 
#' @return 
#' \code{github_commits}: Nested \code{\link{list}} with \code{n} elements.
#' Each commit element is a list with elements:
#' \tabular{ll}{
#'  [[1]] \tab NUMBER \cr
#'  [[2]] \tab TITLE \cr
#'  [[3]] \tab BODY \cr
#'  [[4]] \tab CREATED \cr
#'  [[5]] \tab UPDATED \cr
#'  [[6]] \tab CREATOR \cr
#'  [[7]] \tab URL \cr
#'  [[8]] \tab STATUS \cr
#' }
#' 
#' @export
github_issues <- function(user = "r-lum", repo = "luminescence", verbose = TRUE) {
  
  # build URL and retrieve content
  url <- paste0("https://api.github.com/repos/", user,"/", repo, "/issues")
  content <- github_getContent(url)
  
  # format output as nested list
  issues <- lapply(content, function(x) {
    list(
      NUMBER = x$number,
      TITLE = x$title,
      BODY = gsub("\n", "", x$body),
      CREATED = x$created_at,
      UPDATED = x$updated_at,
      CREATOR = x$user$login,
      URL = x$url,
      STATUS = x$state,
      MILESTONE = x$milestone$title)
  })
  
  # custom printing of the the issues-list as print.list produces unreadable
  # console output
  if (verbose) {
    tmp <- lapply(issues, function(x) {
      
      # limit width of description text
      DESCRIPTION <- ""
      for (i in seq_len(ceiling(nchar(x$BODY) / 100))) 
        DESCRIPTION <- paste(DESCRIPTION, "  ", 
                             substr(x$BODY, i*100-99, i*100), "\n")
      
      # print to console in valid YAML
      cat(paste0("---\n",
                 'title: "', x$TITLE, '"', "\n",
                 "number: ", x$NUMBER, "\n",
                 'url: "', x$URL, '"', "\n",
                 "created: ", x$CREATED, "\n",
                 "updated: ", x$UPDATED, "\n",
                 "creator: ", x$CREATOR, "\n",
                 "status: ", x$STATUS, "\n",
                 'milestone: "', x$MILESTONE, '"', "\n",
                 "description: >\n", DESCRIPTION, 
                 "\n\n\n"))
      
    })
  }
  # return invisible as we explicitly print the output
  invisible(issues)
}



# HELPER ------------------------------------------------------------------

# This function queries the URL, checks the server response and returns
# the content.
github_getContent <- function(url) {
  response <- GET(url, accept_json())
  if (status_code(response) != 200)
    stop("Contacting ", url, " had status code ", status_code(response), 
         call. = FALSE)
  content <- content(response)
  return(content)
}

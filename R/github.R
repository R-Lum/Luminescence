#  ------------------------------------------------------------------------




# BRANCHES ----------------------------------------------------------------
github_branches <- function(user = "r-lum", repo = "luminescence") {
  
  url <- paste0("https://api.github.com/repos/", user,"/", repo, "/branches")
  content <- github_getContent(url)
  branches <- sapply(content, function(x) x$name)
  
  output <- data.frame(
    BRANCH = branches,
    INSTALL = paste0("devtools::install_github('r-lum/luminescence@", branches, "')")
  )
  
  return(output)
}


# ISSUES ------------------------------------------------------------------
github_issues <- function(user = "r-lum", repo = "luminescence") {
  
  url <- paste0("https://api.github.com/repos/", user,"/", repo, "/issues")
  content <- github_getContent(url)
  
  issues <- lapply(content, function(x) {
    list(
      NUMBER = x$number,
      TITLE = x$title,
      BODY = gsub("\n", "", x$body),
      CREATED = x$created_at,
      UPDATED = x$updated_at,
      CREATOR = x$user$login,
      URL = x$url,
      STATUS = x$state)
  })
  
  tmp <- lapply(issues, function(x) {
    cat(paste("Issue #", x$NUMBER, "-", x$TITLE, "\n"))
    cat(paste(paste(rep("-", 103), collapse = ""), "\n"))
    for (i in seq_len(ceiling(nchar(x$BODY) / 100))) cat(paste("|", substr(x$BODY, i*100-99, i*100), "\n"))
    cat("-\n")
    cat(paste("| URL:", paste0("https://github.com/", user, "/", repo,"/issues/31"), "\n"))
    cat(paste("| Created by:", x$CREATOR, paste0("(", x$CREATED, ")"), "\n"))
    cat(paste("| Last update:", x$UPDATED, "\n"))
    cat(paste("| Status:", toupper(x$STATUS)))
    cat("\n\n\n")
  })
  
  invisible(issues)
}



# HELPER ------------------------------------------------------------------
github_getContent <- function(url) {
  response <- GET(url, accept_json())
  if (status_code(response) != 200)
    stop("Contacting ", url, " had status code ", status_code(response), call. = FALSE)
  content <- content(response)
  return(content)
}

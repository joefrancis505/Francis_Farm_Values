# =============================================================================
# download_utils.R — Shared helpers for the download scripts
# =============================================================================
# Sourced by 00_Download_NHGIS.R. Keeps API-key retrieval, network retry,
# and resumable curl download in one place.
# =============================================================================

load_dotenv <- function(path = NULL) {
  if (is.null(path)) {
    candidates <- c(".env", "../.env", "../../.env")
    hit <- candidates[file.exists(candidates)]
    if (!length(hit)) return(invisible(NULL))
    path <- hit[1]
  }
  if (!file.exists(path)) return(invisible(NULL))
  for (ln in readLines(path, warn = FALSE)) {
    ln <- trimws(ln)
    if (ln == "" || startsWith(ln, "#")) next
    eq <- regexpr("=", ln, fixed = TRUE)
    if (eq < 1) next
    k <- trimws(substr(ln, 1, eq - 1))
    v <- sub('^[\"\']|[\"\']$', "", trimws(substr(ln, eq + 1, nchar(ln))))
    if (!nzchar(Sys.getenv(k))) do.call(Sys.setenv, setNames(list(v), k))
  }
  invisible(path)
}

get_ipums_api_key <- function() {
  if (!nzchar(Sys.getenv("IPUMS_API_KEY")) && !nzchar(Sys.getenv("IPUMS_API"))) {
    loaded <- load_dotenv()
    if (!is.null(loaded)) cat(sprintf("Loaded %s\n", loaded))
  }
  key <- Sys.getenv("IPUMS_API_KEY")
  if (!nzchar(key)) key <- Sys.getenv("IPUMS_API")
  if (nzchar(key)) {
    cat("Using IPUMS API key from environment.\n")
    return(key)
  }
  key <- readline(prompt = "Enter your IPUMS API key: ")
  if (!nzchar(key)) stop("IPUMS API key is required.")
  key
}

with_retry <- function(expr, max_retries = 5, pause = 30) {
  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(return(eval(expr)), error = function(e) {
      cat(sprintf("\nNetwork error (attempt %d/%d): %s\n",
                  attempt, max_retries, conditionMessage(e)))
      if (attempt < max_retries) Sys.sleep(pause * attempt)
      NULL
    })
    if (!is.null(res)) return(res)
  }
  stop("Maximum retries reached.")
}

robust_ipums_download <- function(url, dest, api_key) {
  cat(sprintf("  Resumable download: %s\n", basename(dest)))
  cmd <- sprintf(
    "curl -L -C - -H \"Authorization: %s\" --retry 10 --retry-delay 5 --connect-timeout 30 --keepalive-time 60 -o %s %s",
    api_key, shQuote(dest), shQuote(url)
  )
  res <- system(cmd)
  return(res == 0)
}

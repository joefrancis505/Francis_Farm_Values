# Regenerate docs/sessionInfo.txt: attach the packages the analysis steps
# load, then dump sessionInfo() as the reference session for the release.
suppressMessages({
  library(sf); library(rd2d); library(dplyr); library(readr)
  library(tidyr); library(data.table); library(fixest)
})
out <- file.path("docs", "sessionInfo.txt")
con <- file(out, "w")
writeLines(c(
  "Reference session for Francis_Farm-Values v6.1",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  strrep("=", 70), ""
), con)
capture.output(sessionInfo(), file = con, append = TRUE)
close(con)
cat("Wrote", out, "\n")

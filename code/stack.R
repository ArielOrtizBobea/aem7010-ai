# stack.R
# Stack the five per-department placement CSVs into data/placements_all.csv
# with columns dept, name, year, placement, source_url (in that order).
# The source CSVs are produced by code/scrape_<dept>.R.

required_pkgs <- c("readr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readr)
})

depts <- c("dyson", "berkeley", "davis", "minnesota", "wisconsin")

parts <- vector("list", length(depts))
for (i in seq_along(depts)) {
  d    <- depts[[i]]
  path <- sprintf("data/placements_%s.csv", d)
  if (!file.exists(path)) stop(sprintf("Missing input: %s", path))
  df <- read_csv(path, show_col_types = FALSE,
                 col_types = cols(.default = col_character()))
  parts[[i]] <- data.frame(
    dept       = d,
    name       = df$name,
    year       = df$year,
    placement  = df$placement,
    source_url = df$source_url,
    stringsAsFactors = FALSE
  )
}

all <- do.call(rbind, parts)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
write_csv(all, "data/placements_all.csv")

message(sprintf("stack: wrote %d rows to data/placements_all.csv", nrow(all)))

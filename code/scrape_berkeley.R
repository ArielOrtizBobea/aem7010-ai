# scrape_berkeley.R
# Scrape the "ARE Doctoral Job Placements" table from the Berkeley ARE Past
# Placements page and write data/placements_berkeley.csv with columns
# name, year, placement, source_url (in that order).
#
# The dept page hosts the data inside an embedded Google Doc iframe. We pick
# the iframe by its src host (docs.google.com/document) so we don't depend on
# CSS classes, then parse all 4-column tables in the published doc. Year-only
# rows mark the start of each cohort; the first year-less table is the most
# recent cohort, whose year we extract from the doc's title text
# "ARE Doctoral Job Placements YYYY-YYYY".

required_pkgs <- c("rvest", "readr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(rvest)
  library(readr)
})

source_url <- "https://are.berkeley.edu/job-candidates/past-placements"

ua <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
             "AppleWebKit/537.36 (KHTML, like Gecko) ",
             "Chrome/124.0 Safari/537.36")

fetch_html <- function(url) {
  tmp <- tempfile(fileext = ".html")
  download.file(url, tmp, quiet = TRUE, method = "curl",
                extra = c("-fsSL", "-A", shQuote(ua)))
  read_html(tmp)
}

page <- fetch_html(source_url)

iframe <- html_element(
  page,
  xpath = "//iframe[contains(@src, 'docs.google.com/document')]"
)
if (inherits(iframe, "xml_missing")) {
  stop("Could not find Google Doc iframe on the Berkeley placements page.")
}
doc_url <- html_attr(iframe, "src")
doc_page <- fetch_html(doc_url)

# Pull the most-recent year from the doc title text "...YYYY-YYYY".
title_text <- html_text2(html_element(doc_page, xpath = "//table[1]"))
m <- regmatches(title_text,
                regexpr("\\b(19|20)\\d{2}\\s*[–—-]\\s*((19|20)\\d{2})\\b",
                        title_text, perl = TRUE))
latest_year <- if (length(m)) {
  sub(".*[–—-]\\s*", "", m)
} else {
  format(Sys.Date(), "%Y")
}

clean_cell <- function(x) {
  x <- as.character(x)
  x <- gsub(" ", " ", x, fixed = TRUE)        # nbsp -> space
  x <- gsub("Â", "", x, fixed = TRUE)         # stray Â from charset glitch
  x <- gsub("[\\s]+", " ", x, perl = TRUE)
  trimws(ifelse(is.na(x), "", x))
}

is_year <- function(x) grepl("^(19|20)\\d{2}$", x)

# Iterate every table; keep only the year-segmented 4-column ones.
tabs <- html_elements(doc_page, "table")
rows <- list()
current_year <- latest_year
year_seen_yet <- FALSE

for (i in seq_along(tabs)) {
  tbl <- tryCatch(html_table(tabs[[i]], trim = TRUE, fill = TRUE),
                  error = function(e) NULL)
  if (is.null(tbl) || ncol(tbl) != 4 || nrow(tbl) == 0) next

  cells <- lapply(tbl, clean_cell)
  c1 <- cells[[1]]; c2 <- cells[[2]]; c3 <- cells[[3]]; c4 <- cells[[4]]

  for (r in seq_along(c1)) {
    name  <- c1[[r]]
    title <- c2[[r]]
    inst  <- c4[[r]]

    # Year header row: all four cells the same 4-digit year.
    if (is_year(name) && c2[[r]] == name && c3[[r]] == name && c4[[r]] == name) {
      current_year <- name
      year_seen_yet <- TRUE
      next
    }

    # Skip rows that are all empty, blank-name continuation rows (those carry
    # an additional placement for the previous person), pure header chrome,
    # paragraph-style descriptions parked in column 1, or document-internal
    # placeholders (e.g. "###", "(_v250912jb)") that don't contain a name.
    if (name == "" && title == "" && inst == "") next
    if (name == "") next
    if (nchar(name) > 100) next
    if (grepl("^Fields listed", name)) next
    if (grepl("^Congratulations", name)) next
    if (!grepl("[A-Za-z]", name)) next
    if (grepl("###|^\\(?_v\\d", name, perl = TRUE)) next

    # Strip parenthesised field tags like "(DEV, ERE, POL)" off the name.
    name_clean <- trimws(sub("\\s*\\(.*?\\)\\s*$", "", name))

    placement <- if (title == "" && inst == "") {
      ""
    } else if (title == "") {
      inst
    } else if (inst == "") {
      title
    } else {
      paste(title, "at", inst)
    }

    rows[[length(rows) + 1L]] <- data.frame(
      name       = name_clean,
      year       = current_year,
      placement  = placement,
      source_url = source_url,
      stringsAsFactors = FALSE
    )
  }
}

out <- if (length(rows)) do.call(rbind, rows) else
  data.frame(name = character(), year = character(),
             placement = character(), source_url = character(),
             stringsAsFactors = FALSE)

# Drop rows where all four data fields collapsed to empty after cleaning.
all_empty <- out$name == "" & out$year == "" & out$placement == ""
out <- out[!all_empty, , drop = FALSE]

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
write_csv(out, "data/placements_berkeley.csv")

message(sprintf("berkeley: wrote %d rows to data/placements_berkeley.csv",
                nrow(out)))

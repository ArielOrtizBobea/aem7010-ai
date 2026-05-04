# scrape_davis.R
# Scrape the "Past Placements" table from the UC Davis ARE PhD page and write
# data/placements_davis.csv with columns name, year, placement, source_url
# (in that order).
#
# We anchor on the table's <caption> text "Placement of Past Students" so we
# don't depend on the table id or CSS classes. Each <tr> stores the student
# name and graduation year in the first cell (the year is in a span.year),
# the initial position cell holds title / department / institution as
# <br>-separated lines, and we use the first line as the title and the last
# line as the institution.

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

source_url <- "https://are.ucdavis.edu/phd/past-placements/"

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

table_node <- html_element(
  page,
  xpath = "//table[caption[normalize-space(.) = 'Placement of Past Students']]"
)
if (inherits(table_node, "xml_missing")) {
  stop("Could not find table with caption 'Placement of Past Students'.")
}

rows <- html_elements(table_node, "tbody > tr")

clean_text <- function(x) {
  if (length(x) == 0) return("")
  x <- gsub(" ", " ", x, fixed = TRUE)
  trimws(x)
}

split_lines <- function(text) {
  parts <- strsplit(text, "\n", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts[nchar(parts) > 0]
}

records <- vector("list", length(rows))

for (i in seq_along(rows)) {
  tr <- rows[[i]]
  tds <- html_elements(tr, "td")
  if (length(tds) < 2) next

  student_cell <- tds[[1]]
  initial_cell <- tds[[2]]

  # Name: take the visible text minus the year span content. The year span
  # is inside the same cell, so html_text2 will pick it up too — strip it.
  name_node <- html_element(student_cell, xpath = ".//a")
  name <- if (inherits(name_node, "xml_missing")) {
    clean_text(html_text2(student_cell))
  } else {
    clean_text(html_text2(name_node))
  }

  year_node <- html_element(student_cell, "span.year")
  year <- if (inherits(year_node, "xml_missing")) "" else clean_text(html_text2(year_node))

  pieces <- split_lines(html_text2(initial_cell))
  if (length(pieces) == 0) next
  title       <- pieces[[1]]
  institution <- pieces[[length(pieces)]]
  if (length(pieces) == 1) institution <- ""

  placement <- if (title == "" && institution == "") {
    ""
  } else if (title == "") {
    institution
  } else if (institution == "") {
    title
  } else {
    paste(title, "at", institution)
  }

  records[[i]] <- data.frame(
    name       = name,
    year       = year,
    placement  = placement,
    source_url = source_url,
    stringsAsFactors = FALSE
  )
}

records <- records[!vapply(records, is.null, logical(1))]
out <- if (length(records)) do.call(rbind, records) else
  data.frame(name = character(), year = character(),
             placement = character(), source_url = character(),
             stringsAsFactors = FALSE)

# Drop rows where all data cells came back empty.
all_empty <- out$name == "" & out$year == "" & out$placement == ""
out <- out[!all_empty, , drop = FALSE]

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
write_csv(out, "data/placements_davis.csv")

message(sprintf("davis: wrote %d rows to data/placements_davis.csv", nrow(out)))

# scrape_minnesota.R
# Scrape the "Placement of Recent Graduate Students" table from the UMN
# Department of Applied Economics page and write data/placements_minnesota.csv
# with columns name, year, placement, source_url (in that order).
#
# We anchor on the page heading "Placement of Recent Graduate Students" and
# take the year from the most-recent year heading (h2/h3 of the form YYYY)
# that precedes the table. The table has a "PhD" name column and a "Position"
# column whose values are formatted "<title>, <institution>"; we split on the
# first comma to recover the two pieces.

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

source_url <- "https://apec.umn.edu/graduate/placement-recent-graduates"

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

heading <- html_element(
  page,
  xpath = "//*[self::h1 or self::h2 or self::h3][normalize-space(.) = 'Placement of Recent Graduate Students']"
)
if (inherits(heading, "xml_missing")) {
  stop("Could not find heading 'Placement of Recent Graduate Students'.")
}

# Walk forward from the heading; track the most-recent year heading seen.
following <- html_elements(heading, xpath = "following::*[self::h2 or self::h3 or self::table]")

clean_text <- function(x) trimws(gsub(" ", " ", as.character(x), fixed = TRUE))
is_year <- function(x) grepl("^(19|20)\\d{2}$", x)

records <- list()
current_year <- ""

for (el in following) {
  nm  <- html_name(el)
  txt <- clean_text(html_text2(el))

  if (nm %in% c("h2", "h3") && is_year(txt)) {
    current_year <- txt
    next
  }
  if (nm != "table") next

  tbl <- tryCatch(html_table(el, trim = TRUE, fill = TRUE), error = function(e) NULL)
  if (is.null(tbl) || ncol(tbl) < 2 || nrow(tbl) == 0) next

  hdrs <- tolower(trimws(names(tbl)))
  i_name <- {
    j <- grep("phd|name|student|graduate", hdrs); if (length(j)) j[[1]] else 1L
  }
  i_pos <- {
    j <- grep("position|placement|title|job|employer", hdrs)
    if (length(j)) j[[1]] else min(2L, ncol(tbl))
  }
  if (i_name == i_pos && ncol(tbl) >= 2) i_pos <- if (i_name == 1L) 2L else 1L

  for (r in seq_len(nrow(tbl))) {
    name <- clean_text(tbl[[i_name]][[r]])
    pos  <- clean_text(tbl[[i_pos]][[r]])

    if (name == "" && pos == "") next

    title <- ""; inst <- ""
    if (grepl(",", pos, fixed = TRUE)) {
      idx <- regexpr(",", pos, fixed = TRUE)
      title <- trimws(substr(pos, 1, idx - 1))
      inst  <- trimws(substr(pos, idx + 1, nchar(pos)))
    } else {
      title <- pos
    }

    placement <- if (title == "" && inst == "") {
      ""
    } else if (title == "") {
      inst
    } else if (inst == "") {
      title
    } else {
      paste(title, "at", inst)
    }

    records[[length(records) + 1L]] <- data.frame(
      name       = name,
      year       = current_year,
      placement  = placement,
      source_url = source_url,
      stringsAsFactors = FALSE
    )
  }
}

out <- if (length(records)) do.call(rbind, records) else
  data.frame(name = character(), year = character(),
             placement = character(), source_url = character(),
             stringsAsFactors = FALSE)

all_empty <- out$name == "" & out$year == "" & out$placement == ""
out <- out[!all_empty, , drop = FALSE]

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
write_csv(out, "data/placements_minnesota.csv")

message(sprintf("minnesota: wrote %d rows to data/placements_minnesota.csv",
                nrow(out)))

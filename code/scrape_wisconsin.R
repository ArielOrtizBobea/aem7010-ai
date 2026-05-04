# scrape_wisconsin.R
# Scrape the Ph.D. cohort of the Wisconsin AAE "Graduate Student Placement"
# page and write data/placements_wisconsin.csv with columns name, year,
# placement, source_url (in that order).
#
# The page has three tabs (PhD + two MS variants). We anchor on the visible
# tab link text "Ph.D. Placement", read its href fragment, and parse only
# entries inside that container. Within the container, year <h2> headings
# group <ul><li>...</li></ul> entries; each <li> has the form
#   <strong>Name, Position</strong>
#   Institution. (optional annotations)

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

source_url <- "https://aae.wisc.edu/graduate-programs/placement/"

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

# Find the tab link whose text says "Ph.D. Placement" — its href fragment is
# the id of the container that holds the PhD cohort data.
tab_link <- html_element(
  page,
  xpath = "//a[normalize-space(.) = 'Ph.D. Placement']"
)
if (inherits(tab_link, "xml_missing")) {
  stop("Could not find 'Ph.D. Placement' tab link on the page.")
}
href <- html_attr(tab_link, "href")
panel_id <- sub("^#", "", href)
if (!nzchar(panel_id)) stop("PhD tab link has no fragment id.")

panel <- html_element(page, xpath = sprintf("//*[@id='%s']", panel_id))
if (inherits(panel, "xml_missing")) {
  stop(sprintf("Could not find PhD panel container with id '%s'.", panel_id))
}

clean_text <- function(x) {
  x <- gsub(" ", " ", as.character(x), fixed = TRUE)
  x <- gsub("[ \\t]+", " ", x, perl = TRUE)
  trimws(x)
}

is_year <- function(x) grepl("^(19|20)\\d{2}$", x)

# Walk the panel's children sequentially: track the latest year heading and
# parse every <li> inside <ul> blocks under it.
children <- html_elements(panel, xpath = "*")

records <- list()
current_year <- ""

for (el in children) {
  nm  <- html_name(el)
  txt <- clean_text(html_text2(el))

  if (nm == "h2" && is_year(txt)) {
    current_year <- txt
    next
  }
  if (nm != "ul" || current_year == "") next

  lis <- html_elements(el, xpath = "li")
  for (li in lis) {
    strong_node <- html_element(li, "strong")
    if (inherits(strong_node, "xml_missing")) next

    name_pos <- clean_text(html_text2(strong_node))
    full_text <- clean_text(html_text2(li))

    inst <- trimws(sub(name_pos, "", full_text, fixed = TRUE))
    # Drop trailing "20XX research project ... AAE faculty." annotations.
    inst <- sub("\\.?\\s*\\d{4} research project[^.]*(?:\\.[^.]*)?\\s*$", "",
                inst, perl = TRUE, ignore.case = TRUE)
    inst <- trimws(sub("\\.\\s*$", "", inst))

    # Split "Name, Position" on the first comma.
    name <- ""; title <- ""
    if (grepl(",", name_pos, fixed = TRUE)) {
      idx   <- regexpr(",", name_pos, fixed = TRUE)
      name  <- trimws(substr(name_pos, 1, idx - 1))
      title <- trimws(substr(name_pos, idx + 1, nchar(name_pos)))
    } else {
      name <- name_pos
    }

    if (name == "" && title == "" && inst == "") next

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
write_csv(out, "data/placements_wisconsin.csv")

message(sprintf("wisconsin: wrote %d rows to data/placements_wisconsin.csv",
                nrow(out)))

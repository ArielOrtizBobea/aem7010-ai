# scrape_dyson.R
# Scrape the "Recent PhD Job Placements" table from the Cornell Dyson School
# graduate placements page and write data/placements_dyson.csv with columns
# name, year, placement, source_url (in that order).

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

source_url <- "https://dyson.cornell.edu/programs/graduate/placements/"

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

# Anchor on the heading text. The page emits a non-breaking space (U+00A0)
# between "PhD" and "Job", so translate it to a regular space before matching.
nbsp <- " "
heading_xpath <- paste0(
  "//*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6]",
  "[normalize-space(translate(., '", nbsp, "', ' ')) = 'Recent PhD Job Placements']"
)
heading <- html_element(page, xpath = heading_xpath)
if (inherits(heading, "xml_missing")) {
  stop("Could not find heading 'Recent PhD Job Placements' on the page.")
}

table_node <- html_element(heading, xpath = "following::table[1]")
if (inherits(table_node, "xml_missing")) {
  stop("Could not find a <table> following the placements heading.")
}

raw <- html_table(table_node, trim = TRUE, fill = TRUE)

# Resolve columns by header keyword, falling back to position 1..4.
clean <- function(x) tolower(trimws(x))
hdrs  <- clean(names(raw))
pick <- function(patterns) {
  for (p in patterns) {
    i <- grep(p, hdrs); if (length(i)) return(i[[1]])
  }
  NA_integer_
}
i_name  <- pick("name|student|graduate")
i_year  <- pick("year|class")
i_title <- pick("title|position|job")
i_inst  <- pick("institution|employer|placement|organization|company|firm|school|university|agency")

n <- ncol(raw)
if (is.na(i_year)  && n >= 1) i_year  <- 1L
if (is.na(i_name)  && n >= 2) i_name  <- 2L
if (is.na(i_title) && n >= 3) i_title <- 3L
if (is.na(i_inst)  && n >= 4) i_inst  <- 4L

to_chr <- function(x) {
  x <- trimws(as.character(x))
  ifelse(is.na(x), "", x)
}

df <- data.frame(
  name        = to_chr(raw[[i_name]]),
  year        = to_chr(raw[[i_year]]),
  title       = to_chr(raw[[i_title]]),
  institution = to_chr(raw[[i_inst]]),
  stringsAsFactors = FALSE
)

# Drop rows where all four data cells are empty.
all_empty <- df$name == "" & df$year == "" & df$title == "" & df$institution == ""
df <- df[!all_empty, , drop = FALSE]

placement <- ifelse(
  df$title == "" & df$institution == "", "",
  ifelse(df$title == "",       df$institution,
  ifelse(df$institution == "", df$title,
         paste(df$title, "at", df$institution)))
)

out <- data.frame(
  name       = df$name,
  year       = df$year,
  placement  = placement,
  source_url = source_url,
  stringsAsFactors = FALSE
)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
write_csv(out, "data/placements_dyson.csv")

message(sprintf("dyson: wrote %d rows to data/placements_dyson.csv", nrow(out)))

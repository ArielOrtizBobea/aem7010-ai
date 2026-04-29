# scrape_dyson_cowork.R
# Scrape the "Recent PhD Job Placements" table from the Dyson School page
# and write a tidy CSV to data/placements_dyson.csv.
#
# Output columns (in order): name, year, placement, source_url
#   - placement = "<job title> at <institution>"
#   - source_url is the page URL, repeated on every row.
#
# The selector is anchored on the heading text "Recent PhD Job Placements"
# so it survives changes in CSS class names.
#
# Dependencies: rvest, readr. The script installs them automatically from
# the RStudio CRAN mirror if they are not already available, so it can be
# run from a fresh R session without prior setup.

required_pkgs <- c("rvest", "readr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs,
                                       requireNamespace,
                                       logical(1),
                                       quietly = TRUE)]
if (length(missing_pkgs)) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(rvest)
  library(readr)
})

source_url <- "https://dyson.cornell.edu/programs/graduate/placements/"

page <- read_html(source_url)

# Locate the heading by its text, then grab the first <table> that follows it
# in document order. This is robust to the heading being h1/h2/h3/... and to
# the table being nested inside wrapper divs. The page actually emits a
# non-breaking space (U+00A0) between "PhD" and "Job", so we translate U+00A0
# to a regular space before comparing.
nbsp <- "\u00a0"
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

# Identify columns by header keywords; fall back to positional order if the
# headers are missing or non-standard. The source table is expected to have
# four columns: name, year, job title, institution.
clean <- function(x) tolower(trimws(x))
hdrs  <- clean(names(raw))

pick <- function(patterns) {
  for (p in patterns) {
    i <- grep(p, hdrs)
    if (length(i)) return(i[[1]])
  }
  NA_integer_
}

i_name  <- pick("name|student|graduate")
i_year  <- pick("year|class")
i_title <- pick("title|position|job")
i_inst  <- pick("institution|employer|placement|organization|company|firm|school|university")

n <- ncol(raw)
if (is.na(i_name)  && n >= 1) i_name  <- 1L
if (is.na(i_year)  && n >= 2) i_year  <- 2L
if (is.na(i_title) && n >= 3) i_title <- 3L
if (is.na(i_inst)  && n >= 4) i_inst  <- 4L

trim_chr <- function(x) trimws(as.character(x))

df <- data.frame(
  name        = trim_chr(raw[[i_name]]),
  year        = trim_chr(raw[[i_year]]),
  title       = trim_chr(raw[[i_title]]),
  institution = trim_chr(raw[[i_inst]]),
  stringsAsFactors = FALSE
)

# Drop rows where all four source cells are empty.
all_empty <- df$name == "" & df$year == "" & df$title == "" & df$institution == ""
df <- df[!all_empty, , drop = FALSE]

# Build the placement column: "<title> at <institution>". Handle the cases
# where one side is missing so we never emit a stray "at".
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

message(sprintf("Wrote %d rows to data/placements_dyson.csv", nrow(out)))

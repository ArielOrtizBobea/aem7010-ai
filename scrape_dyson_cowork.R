# scrape_dyson_cowork.R
# Scrape the "Recent PhD Job Placements" table from the Dyson School page
# and write a tidy CSV to data/placements_dyson.csv.
#
# Output columns (in order): name, year, placement, source_url, is_postdoc, country
#   - placement  = "<job title> at <institution>"
#   - source_url is the page URL, repeated on every row.
#   - is_postdoc is TRUE when the job title looks like a postdoctoral role.
#   - country    is a heuristic best guess from the institution string; rows
#                that no pattern matches are returned as NA so they can be
#                inspected and the lookup table extended.
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

# Coerce every cell to a trimmed character string; html_table() type-converts
# numeric-looking columns so empty cells come back as NA, which we standardise
# to "" so the empty-row filter and placement builder behave predictably.
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

# is_postdoc: TRUE when the title contains "postdoc" / "post-doc" / "post doc"
# / "postdoctoral", case-insensitively.
is_postdoc <- grepl("\\bpost[-\\s]?doc(toral)?\\b", df$title,
                    ignore.case = TRUE, perl = TRUE)

# country: heuristic best guess from the institution string. Patterns are
# evaluated in order and the first match wins; United States is intentionally
# last because its patterns are the most permissive (state suffixes, federal
# agencies, R1 university names). Returns NA when nothing matches so the user
# can inspect the unmatched rows and extend the table below.
country_patterns <- list(
  c("International",    "World Bank|International Monetary Fund|InterAmerican Development Bank|United Nations|\\bIMF\\b|\\bIDB\\b"),
  c("Saudi Arabia",     "Riyadh|KAPSARC|Saudi"),
  c("South Korea",      "KDI School|\\bKorea\\b|Seoul"),
  c("Vietnam",          "VNUHCM|Vietnam|Ho Chi Minh"),
  c("Hong Kong",        "Hong Kong"),
  c("Singapore",        "Nanyang|National University of Singapore|\\bNUS\\b|Singapore"),
  c("China",            "Peking|Tsinghua|Beijing|Shanghai|Xi.an Jiaotong|Industrial and Commercial Bank of China|\\bChina\\b"),
  c("Germany",          "Munich|Berlin|Heidelberg|Ludwig-Maximilian|\\bGermany\\b"),
  c("Switzerland",      "\\bBern\\b|Zurich|Switzerland"),
  c("Australia",        "Monash|University of Sydney|Melbourne|\\bAustralia\\b"),
  c("Finland",          "Finland|Helsinki"),
  c("Zambia",           "Zambia"),
  c("India",            "Azim Premji|Indian Institute|\\bIIMA\\b|Krea University|Bangalore|Mumbai|\\bDelhi\\b|\\bIndia\\b"),
  c("Canada",           "\\bCanada\\b|Alberta|University of Toronto|McGill|British Columbia"),
  c("United Kingdom",   "United Kingdom|\\bUK\\b|\\bLondon\\b|Oxford|Cambridge, England"),
  c("United States",    paste(
    # Explicit US markers
    "United States", "\\bUSA\\b", "\\bU\\.S\\.",
    "Washington, ?D\\.?C\\.?", "USDA", "USAID", "\\bNBER\\b",
    "National Bureau of Economic Research",
    "U\\.S\\. Department", "U\\.S\\. Securities",
    # US universities that show up in the table
    "Cornell", "Stanford", "Harvard", "Yale", "Princeton", "\\bMIT\\b",
    "\\bBrown\\b", "Notre Dame", "Rutgers", "Temple", "Fordham",
    "Johns Hopkins", "Purdue", "Colgate", "Amherst", "Middlebury",
    "James Madison", "Arizona State", "Ohio State", "Oregon State",
    "Utah State", "California State", "\\bBerkeley\\b", "\\bUCLA\\b",
    "University of California", "University of Illinois",
    "University of Washington", "University of Delaware",
    "University of Maryland", "University of Georgia",
    "University of Evansville", "University of Rhode Island",
    "St\\. Mary.s College of Maryland", "Earth Institute",
    # US-headquartered firms / agencies
    "Pacific Gas", "Capital One", "Citibank", "Morgan Stanley", "StoneX",
    "Acadian", "Bates White", "Berkeley Research", "Edgeworth",
    "Analysis Group", "Dean & Company", "ISO New England", "Volpe",
    "Tata-Cornell", "UCLA Anderson", "South Coast Air Quality",
    "\\bUber\\b", "Freddie Mac", "Center for Governmental Research",
    "Holy Cross",
    # US state-suffix patterns ", XX" where XX is a two-letter state code
    ", (?:AL|AK|AZ|AR|CA|CO|CT|DE|FL|GA|HI|ID|IL|IN|IA|KS|KY|LA|ME|MD|MA|MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|OH|OK|OR|PA|RI|SC|SD|TN|TX|UT|VT|VA|WA|WV|WI|WY)\\b",
    sep = "|"))
)

guess_country <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  for (pair in country_patterns) {
    if (grepl(pair[[2]], text, ignore.case = TRUE, perl = TRUE)) {
      return(pair[[1]])
    }
  }
  NA_character_
}

country <- vapply(df$institution, guess_country, character(1),
                  USE.NAMES = FALSE)

out <- data.frame(
  name       = df$name,
  year       = df$year,
  placement  = placement,
  source_url = source_url,
  is_postdoc = is_postdoc,
  country    = country,
  stringsAsFactors = FALSE
)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
write_csv(out, "data/placements_dyson.csv")

message(sprintf("Wrote %d rows to data/placements_dyson.csv", nrow(out)))

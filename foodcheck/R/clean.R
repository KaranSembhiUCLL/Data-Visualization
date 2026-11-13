# ---------------------------------------------------------------------------
# clean.R - loading and cleaning both datasets (kept strictly separate)
# ---------------------------------------------------------------------------

# Some text was saved as UTF-8, read back as Windows-1252 and saved again,
# which turns "Nestl\u00e9" into "Nestl\u00c3\u00a9" and Cyrillic into "\u00d0\u0153\u00d0\u00b0...". We reverse
# that: map every character back to its Windows-1252 byte and re-read the
# bytes as UTF-8. Only strings that become valid UTF-8 are replaced.
.cp1252_extra <- c(
  "20AC" = 0x80, "201A" = 0x82, "0192" = 0x83, "201E" = 0x84, "2026" = 0x85,
  "2020" = 0x86, "2021" = 0x87, "02C6" = 0x88, "2030" = 0x89, "0160" = 0x8A,
  "2039" = 0x8B, "0152" = 0x8C, "017D" = 0x8E, "2018" = 0x91, "2019" = 0x92,
  "201C" = 0x93, "201D" = 0x94, "2022" = 0x95, "2013" = 0x96, "2014" = 0x97,
  "02DC" = 0x98, "2122" = 0x99, "0161" = 0x9A, "203A" = 0x9B, "0153" = 0x9C,
  "017E" = 0x9E, "0178" = 0x9F
)

fix_mojibake_one <- function(s) {
  if (is.na(s) || !grepl("[\u00c2-\u00f4][\u0080-\u00bf\u0152\u0153\u0160\u0161\u0178\u017d\u017e\u0192\u02c6\u02dc\u2013-\u203a\u20ac\u2122]", s)) {
    return(s)
  }
  cps <- utf8ToInt(s)
  bytes <- vapply(cps, function(cp) {
    if (cp < 256) return(as.integer(cp))
    hit <- .cp1252_extra[sprintf("%04X", cp)]
    if (is.na(hit)) -1L else as.integer(hit)
  }, integer(1))
  if (any(bytes < 0)) return(s)
  out <- rawToChar(as.raw(bytes))
  Encoding(out) <- "UTF-8"
  if (validUTF8(out)) out else s
}
fix_mojibake <- function(x) vapply(x, fix_mojibake_one, character(1), USE.NAMES = FALSE)

# Lower case, no accents, no punctuation, single spaces. Used only for matching.
normalise_text <- function(x) {
  x <- tolower(x)
  x <- chartr(
    "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f2\u00f3\u00f4\u00f5\u00f6\u00f9\u00fa\u00fb\u00fc\u00fd\u00ff\u00f1\u00e7\u0153\u00e6",
    "aaaaaaeeeeiiiiooooouuuuyyncoa", x
  )
  x <- gsub("\\((l|xl|s|m)\\)", " ", x)       # size / list markers like "(L)"
  x <- gsub("[^a-z0-9\u0400-\u04ff ]+", " ", x) # keep latin, digits, cyrillic
  x <- gsub("\\s+", " ", trimws(x))
  x[is.na(x) | x == ""] <- NA_character_
  x
}

# A cell can hold several barcodes ("5410...;0541..."). Split them and strip
# leading zeros so EAN-13 "05400141168051" and "5400141168051" are equal.
split_barcodes <- function(x) {
  lapply(x, function(b) {
    if (is.na(b)) return(character(0))
    parts <- trimws(unlist(strsplit(b, "[;,/ ]+")))
    parts <- parts[grepl("^[0-9]{6,}$", parts)]
    unique(sub("^0+", "", parts))
  })
}

split_categories <- function(x) {
  lapply(x, function(c) {
    if (is.na(c) || trimws(c) == "") return("Uncategorised")
    trimws(unlist(strsplit(c, ",")))
  })
}

load_text <- function(path) {
  d <- readxl::read_excel(path, col_types = "text")
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  for (col in c("name", "remarks", "brands", "bron", "categories")) d[[col]] <- fix_mojibake(d[[col]])
  d$active <- as.integer(d$active)
  d$id <- sprintf("T%04d", seq_len(nrow(d)))
  d$name_norm <- normalise_text(d$name)
  d$brand_norm <- normalise_text(d$brands)
  d$remarks_norm <- normalise_text(d$remarks)
  d$barcode_list <- split_barcodes(d$barcode)
  d$cat_list <- split_categories(d$categories)
  d$label <- ifelse(is.na(d$brands), d$name, paste0(d$name, " \u00b7 ", d$brands))
  d
}

NUTRIENTS <- c(
  energy = "Energy (kcal)", protein = "Protein", fat = "Fat",
  saturated_fatty_acid = "Saturated fat", carbohydrates = "Carbohydrates",
  sugar = "Sugar", starch = "Starch", dietary_fiber = "Fibre",
  salt = "Salt", sodium = "Sodium (mg)"
)

load_numeric <- function(path) {
  d <- readxl::read_excel(path)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  d$active <- as.integer(d$active)
  d$id <- sprintf("N%04d", seq_len(nrow(d)))
  # salt and sodium carry the same information (salt g = sodium mg / 400):
  # fill salt from sodium where only sodium is known, then use salt only.
  d$salt_filled <- ifelse(is.na(d$salt) & !is.na(d$sodium), d$sodium / 400, d$salt)
  d$cat_list <- split_categories(d$categories)
  cat1 <- vapply(d$cat_list, `[`, character(1), 1)
  d$label <- sprintf(
    "%s \u00b7 %s \u00b7 %s kcal",
    d$id, ifelse(cat1 == "Uncategorised", "no category", cat1),
    ifelse(is.na(d$energy), "?", format(round(d$energy), big.mark = ""))
  )
  d
}

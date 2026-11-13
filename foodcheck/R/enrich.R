# ---------------------------------------------------------------------------
# enrich.R - Q3: look up nutritional values on Open Food Facts
# Only possible for the TEXT dataset: the numeric file has no name or
# barcode to search with.
# ---------------------------------------------------------------------------

OFF_FIELDS <- c(
  energy = "energy-kcal_100g", protein = "proteins_100g", fat = "fat_100g",
  saturated_fatty_acid = "saturated-fat_100g", carbohydrates = "carbohydrates_100g",
  sugar = "sugars_100g", starch = "starch_100g", dietary_fiber = "fiber_100g",
  salt = "salt_100g", sodium = "sodium_100g"
)
OFF_UA <- "TeamF8-FoodCheck/1.0 (student project; data visualisation course)"

off_get <- function(url) {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, OFF_UA)
  req <- httr2::req_timeout(req, 10)
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

off_to_table <- function(product, how) {
  nut <- product$nutriments
  if (is.null(nut)) return(NULL)
  vals <- vapply(OFF_FIELDS, function(f) {
    v <- nut[[f]]; if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v))
  }, numeric(1))
  if ("sodium" %in% names(vals) && !is.na(vals["sodium"])) vals["sodium"] <- vals["sodium"] * 1000  # g -> mg
  if (all(is.na(vals))) return(NULL)
  code <- product$code %||% product$`_id` %||% ""
  list(
    product = paste0(product$product_name %||% "(no name)",
                     if (!is.null(product$brands) && nzchar(product$brands)) paste0(" \u00b7 ", product$brands) else ""),
    url = paste0("https://world.openfoodfacts.org/product/", code),
    how = how,
    values = data.frame(nutrient = unname(NUTRIENTS[names(vals)]), value = unname(vals),
                        stringsAsFactors = FALSE)
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Try every barcode first (exact), then a name + brand search (fuzzy).
lookup_nutrition <- function(item) {
  for (code in item$barcode_list[[1]]) {
    res <- tryCatch(off_get(sprintf(
      "https://world.openfoodfacts.org/api/v2/product/%s.json?fields=code,product_name,brands,nutriments", code)),
      error = function(e) NULL)
    if (!is.null(res) && identical(as.integer(res$status), 1L)) {
      tab <- off_to_table(res$product, paste("barcode", code))
      if (!is.null(tab)) return(tab)
    }
  }
  terms <- trimws(paste(item$name, ifelse(is.na(item$brands), "", item$brands)))
  res <- tryCatch(off_get(paste0(
    "https://world.openfoodfacts.org/cgi/search.pl?search_simple=1&json=1&page_size=5",
    "&fields=code,product_name,brands,nutriments&search_terms=", utils::URLencode(terms, reserved = TRUE))),
    error = function(e) structure(list(), error = conditionMessage(e)))
  if (!is.null(attr(res, "error"))) stop("Open Food Facts could not be reached: ", attr(res, "error"))
  for (prod in res$products) {
    tab <- off_to_table(prod, paste0("name search \u201c", terms, "\u201d"))
    if (!is.null(tab)) return(tab)
  }
  NULL
}

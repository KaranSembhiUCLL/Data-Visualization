# ---------------------------------------------------------------------------
# quality.R - Q7: missing or impossible nutritional values (numeric file)
# ---------------------------------------------------------------------------

# Nutrients a label must have (EU "big 7"). Starch, fibre and sodium are
# optional on labels, so a missing value there is not an error.
REQUIRED <- c("energy", "fat", "saturated_fatty_acid", "carbohydrates",
              "sugar", "protein", "salt")
GRAM_FIELDS <- c("protein", "fat", "saturated_fatty_acid", "carbohydrates",
                 "sugar", "starch", "dietary_fiber", "salt")

quality_issues <- function(d) {
  p <- d[d$active == 0, ]
  out <- list()
  add <- function(rows, nutrient, issue, severity, value = NA_real_) {
    if (!length(rows)) return()
    out[[length(out) + 1]] <<- data.frame(
      id = p$id[rows], nutrient = nutrient, value = value, issue = issue,
      severity = severity, stringsAsFactors = FALSE)
  }
  for (f in REQUIRED) {
    miss <- is.na(p[[f]])
    if (f == "salt") miss <- is.na(p$salt_filled)
    add(which(miss), f, "Missing (required on EU labels)", "Missing")
  }
  for (f in GRAM_FIELDS) {
    r <- which(p[[f]] > 100)
    add(r, f, "More than 100 g per 100 g", "Impossible", p[[f]][r])
  }
  r <- which(p$energy > 900)
  add(r, "energy", "Above 900 kcal (pure fat is ~900)", "Impossible", p$energy[r])
  r <- which(p$saturated_fatty_acid > p$fat + 0.1)
  add(r, "saturated_fatty_acid", "Saturated fat higher than total fat", "Impossible", p$saturated_fatty_acid[r])
  r <- which(p$sugar > p$carbohydrates + 0.1)
  add(r, "sugar", "Sugar higher than carbohydrates", "Impossible", p$sugar[r])
  macro <- rowSums(cbind(p$protein, p$fat, p$carbohydrates, p$dietary_fiber, p$salt_filled), na.rm = TRUE)
  r <- which(macro > 105)
  add(r, "carbohydrates", sprintf("Macros add up to %.0f g per 100 g", macro[r]), "Impossible", macro[r])
  # energy check: 4 kcal/g protein & carbs, 9 fat, 2 fibre
  calc <- 4 * p$protein + 9 * p$fat + 4 * p$carbohydrates + 2 * ifelse(is.na(p$dietary_fiber), 0, p$dietary_fiber)
  gap <- abs(calc - p$energy)
  r <- which(!is.na(gap) & gap > 60 & gap > 0.25 * pmax(p$energy, calc))
  add(r, "energy", sprintf("Energy %.0f kcal but macros give %.0f kcal", p$energy[r], calc[r]), "Suspicious", p$energy[r])
  if (!length(out)) {
    return(data.frame(id = character(), nutrient = character(), value = numeric(),
                      issue = character(), severity = character()))
  }
  res <- do.call(rbind, out)
  res$severity <- factor(res$severity, c("Impossible", "Suspicious", "Missing"))
  res[order(res$severity, res$id), ]
}

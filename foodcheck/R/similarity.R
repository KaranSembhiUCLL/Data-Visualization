# ---------------------------------------------------------------------------
# similarity.R - one definition of "similar" per dataset
# Both return a list with a pending x active score matrix (0-1) and parts.
# ---------------------------------------------------------------------------

# TEXT dataset -------------------------------------------------------------
# score = 1                                    if a barcode is shared
#       = 0.8 * name + 0.2 * brand             if both brands are known
#       = name                                 if a brand is missing
# name  = cosine similarity of character 3-grams (robust to word order and
#         small typos: "chips paprika" ~ "paprika chips"), best of the
#         active product's name and its French name (remarks)
# brand = Jaro-Winkler similarity (brands are short, typos sit at the end)
TEXT_WEIGHTS <- c(name = 0.8, brand = 0.2)

text_similarity <- function(d) {
  p <- d[d$active == 0, ]
  a <- d[d$active == 1, ]
  pn <- ifelse(is.na(p$name_norm), "", p$name_norm)
  an <- ifelse(is.na(a$name_norm), "", a$name_norm)
  # Active names are mostly Dutch with a French name in "remarks", while
  # scanned products often arrive in French: compare against both, keep max.
  ar <- ifelse(is.na(a$remarks_norm), "", a$remarks_norm)
  name_sim <- stringdist::stringsimmatrix(pn, an, method = "cosine", q = 3)
  rem_sim  <- stringdist::stringsimmatrix(pn, ar, method = "cosine", q = 3)
  name_sim[is.na(name_sim)] <- 0; rem_sim[is.na(rem_sim)] <- 0
  name_sim <- pmax(name_sim, rem_sim)

  # brand similarity on unique brands only (much cheaper), then expand
  ub_p <- unique(na.omit(p$brand_norm)); ub_a <- unique(na.omit(a$brand_norm))
  bsim_u <- stringdist::stringsimmatrix(ub_p, ub_a, method = "jw", p = 0.1)
  brand_sim <- matrix(NA_real_, nrow(p), nrow(a))
  ip <- match(p$brand_norm, ub_p); ia <- match(a$brand_norm, ub_a)
  okp <- which(!is.na(ip)); oka <- which(!is.na(ia))
  brand_sim[okp, oka] <- bsim_u[ip[okp], ia[oka], drop = FALSE]

  score <- ifelse(is.na(brand_sim), name_sim,
                  TEXT_WEIGHTS["name"] * name_sim + TEXT_WEIGHTS["brand"] * brand_sim)

  # shared barcode = same product, whatever the name says
  code_owner <- rep(seq_len(nrow(a)), lengths(a$barcode_list))
  codes_a <- unlist(a$barcode_list)
  barcode_hit <- matrix(FALSE, nrow(p), nrow(a))
  for (i in seq_len(nrow(p))) {
    hits <- code_owner[codes_a %in% p$barcode_list[[i]]]
    if (length(hits)) barcode_hit[i, hits] <- TRUE
  }
  score[barcode_hit] <- 1

  dimnames(score) <- list(p$id, a$id)
  list(score = score, name = name_sim, brand = brand_sim, barcode = barcode_hit,
       pending_ids = p$id, active_ids = a$id)
}

# NUMERIC dataset ----------------------------------------------------------
# Nutrients are skewed (most salt values < 1 g, a few at 40 g), so every
# nutrient is log1p-transformed and divided by its spread among active
# products. Distance = root-mean-square difference over the nutrients both
# products have (at least MIN_SHARED). Similarity = exp(-distance), so
# identical profiles score 1 and the score halves at distance 0.69.
NUM_FEATURES <- c("energy", "protein", "fat", "saturated_fatty_acid",
                  "carbohydrates", "sugar", "dietary_fiber", "salt_filled")
MIN_SHARED <- 5

numeric_similarity <- function(d) {
  p <- d[d$active == 0, ]
  a <- d[d$active == 1, ]
  sumsq <- matrix(0, nrow(p), nrow(a)); n <- matrix(0L, nrow(p), nrow(a))
  per_feature <- list()
  for (f in NUM_FEATURES) {
    xa <- log1p(pmax(a[[f]], 0)); xp <- log1p(pmax(p[[f]], 0))
    s <- sd(xa, na.rm = TRUE)
    diff2 <- outer(xp / s, xa / s, "-")^2
    ok <- !is.na(diff2)
    sumsq[ok] <- sumsq[ok] + diff2[ok]
    n <- n + ok
  }
  dist <- sqrt(sumsq / pmax(n, 1))
  score <- exp(-dist)
  score[n < MIN_SHARED] <- 0
  dimnames(score) <- list(p$id, a$id)
  list(score = score, shared = n, pending_ids = p$id, active_ids = a$id)
}

# Best match per pending product (feeds Q1, Q5, Q6)
best_matches <- function(sim) {
  s <- sim$score
  j <- max.col(s, ties.method = "first")
  out <- data.frame(id = rownames(s), best_id = colnames(s)[j],
             best_score = s[cbind(seq_len(nrow(s)), j)],
             barcode_match = if (!is.null(sim$barcode)) rowSums(sim$barcode) > 0 else FALSE,
             comparable = TRUE, stringsAsFactors = FALSE)
  if (!is.null(sim$shared)) {
    # numeric: a product with fewer than MIN_SHARED nutrients cannot be compared
    out$comparable <- apply(sim$shared, 1, max) >= MIN_SHARED
    out$best_score[!out$comparable] <- NA
    out$best_id[!out$comparable] <- NA
  }
  out
}

DEFAULT_THRESHOLD <- c(text = 0.80, numeric = 0.95)

# Known duplicates for calibration (text only): pairs of ACTIVE products that
# share a barcode. Their scores show how many real duplicates a threshold keeps.
known_duplicate_scores <- function(d) {
  a <- d[d$active == 1, ]
  codes <- unlist(a$barcode_list); owner <- rep(seq_len(nrow(a)), lengths(a$barcode_list))
  dup <- unique(codes[duplicated(codes)])
  pairs <- t(vapply(dup, function(c) { o <- unique(owner[codes == c]); c(o[1], o[2]) }, numeric(2)))
  pairs <- pairs[!is.na(pairs[, 2]), , drop = FALSE]
  i <- pairs[, 1]; j <- pairs[, 2]
  nm <- function(x) ifelse(is.na(x), "", x)
  s <- pmax(stringdist::stringsim(nm(a$name_norm[i]), nm(a$name_norm[j]), method = "cosine", q = 3),
            stringdist::stringsim(nm(a$name_norm[i]), nm(a$remarks_norm[j]), method = "cosine", q = 3),
            stringdist::stringsim(nm(a$remarks_norm[i]), nm(a$name_norm[j]), method = "cosine", q = 3), na.rm = TRUE)
  b <- stringdist::stringsim(a$brand_norm[i], a$brand_norm[j], method = "jw", p = 0.1)
  ifelse(is.na(b), s, TEXT_WEIGHTS["name"] * s + TEXT_WEIGHTS["brand"] * b)
}

# D3 output helpers (JS side in www/charts.js)
d3Output <- function(id, kind, height = "320px") {
  shiny::div(id = id, class = paste0("d3-", kind), style = paste0("width:100%;height:", height))
}
renderD3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  func <- shiny::exprToFunction(expr, env, quoted = TRUE)
  shiny::createRenderFunction(func, function(value, session, name, ...) value)
}

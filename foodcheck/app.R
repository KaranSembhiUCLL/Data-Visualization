# ===========================================================================
# FoodCheck - Team F8 - Validation of food products
# Shiny app built from app_diagram.mmd. Every output answers one user
# question (Q1-Q8). Run with:  shiny::runApp()
# ===========================================================================
library(shiny)
library(bslib)
library(DT)

for (f in list.files("R", full.names = TRUE)) source(f, encoding = "UTF-8")

# Data is read once at start-up. The two files are NEVER merged:
# input$dataset decides which one every screen works on.
DATA <- list(
  text    = load_text("data/text_food.xlsx"),
  numeric = load_numeric("data/numeric_food.xlsx")
)
SIM_CACHE <- new.env()   # similarity() result per dataset, computed once

pill <- function(status) {
  cls <- c("Possible duplicate" = "dup", "New" = "new", "Too little data" = "na")[status]
  sprintf('<span class="pill %s">%s</span>', cls, status)
}
sel_opt <- function(i) if (!length(i) || is.na(i[1])) list(mode = "single") else list(mode = "single", selected = i)
fmt <- function(x, d = 1) ifelse(is.na(x), "\u2014", formatC(x, format = "f", digits = d, big.mark = ","))

# ---------------------------------------------------------------- UI ------
ui <- page_navbar(
  title = "FoodCheck \u00b7 Team F8",
  fillable = FALSE,
  theme = bs_theme(
    version = 5, bg = "#fbf8f1", fg = "#1e2b25", primary = "#1e2b25",
    base_font = font_collection("IBM Plex Sans", "system-ui", "sans-serif"),
    heading_font = font_collection("Fraunces", "Georgia", "serif"),
    code_font = font_collection("IBM Plex Mono", "monospace")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600&family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(src = "d3.min.js"),
    tags$script(src = "charts.js")
  ),
  sidebar = sidebar(
    width = 270,
    radioButtons("dataset", "Dataset", c("Text (names, brands, barcodes)" = "text",
                                         "Numeric (nutrients per 100 g)" = "numeric")),
    sliderInput("threshold", "Similarity threshold", min = 0, max = 1, value = 0.8, step = 0.01),
    p(class = "hint", "A pending product whose closest active product scores at or above this value is flagged as a possible duplicate."),
    numericInput("top_n", "Closest matches to show (N)", value = 5, min = 1, max = 25),
    radioButtons("sort_by", "Sort categories by", c("Unvalidated count" = "count", "Proportion unvalidated" = "proportion"))
  ),

  # ---- tab 1: the dietitian's workflow -----------------------------------
  nav_panel(
    "Validation desk",
    uiOutput("status_counts"),
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 8),
      card(card_header(span(class = "step", "1"), "Where is the backlog?", span(class = "qtag", "Q4")),
           p(class = "hint", "Click a category to filter the list. Click it again to clear."),
           uiOutput("uncat_note"),
           d3Output("category_bar", "bar", "auto")),
      card(card_header(span(class = "step", "2"), "Pending products, most likely duplicates first", span(class = "qtag", "Q1")),
           uiOutput("category_chip"),
           DTOutput("pending_table"))
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(5, 7),
      card(card_header(span(class = "step", "3"), "Closest active products", span(class = "qtag", "Q8")),
           uiOutput("match_summary"), DTOutput("match_table")),
      card(card_header(span(class = "step", "4"), "Side-by-side comparison", span(class = "qtag", "Q2")),
           uiOutput("compare_table"))
    ),
    card(card_header(span(class = "step", "5"), "Genuinely new? Look up nutritional values online", span(class = "qtag", "Q3")),
         actionButton("enrich", "Auto-Enrich from Open Food Facts", class = "btn-dark", width = "fit-content"),
         uiOutput("enrich_table"))
  ),

  # ---- tab 2: choosing the threshold --------------------------------------
  nav_panel(
    "Threshold lab",
    layout_columns(
      fill = FALSE,
      col_widths = c(8, 4),
      card(card_header("How close is each pending product to its best match?", span(class = "qtag", "Q6")),
           d3Output("score_hist", "hist", "380px")),
      card(card_header("What does this threshold mean?", span(class = "qtag", "Q5")),
           uiOutput("threshold_story"))
    )
  ),

  # ---- tab 3: data quality (numeric file) ---------------------------------
  nav_panel(
    "Data quality",
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 9),
      card(card_header("Filter"),
           selectInput("nutrient", "Nutrient", c("All nutrients" = "all", setNames(names(NUTRIENTS), NUTRIENTS))),
           p(class = "hint", "Pending products from the numeric file with missing required values or values that cannot be true. Starch, fibre and sodium are optional on EU labels, so missing values there are not flagged."),
           uiOutput("quality_counts")),
      card(card_header("Products to reject or enrich", span(class = "qtag", "Q7")), DTOutput("quality_table"))
    )
  )
)

# ------------------------------------------------------------ server ------
server <- function(input, output, session) {

  # state that several views share (diagram: reactiveVal)
  selected_item      <- reactiveVal(NULL)
  selected_candidate <- reactiveVal(NULL)
  selected_category  <- reactiveVal(NULL)

  db <- reactive(DATA[[input$dataset]])

  similarity <- reactive({
    ds <- input$dataset
    if (is.null(SIM_CACHE[[ds]])) {
      withProgress(message = "Comparing every pending product with every active product\u2026", {
        sim <- if (ds == "text") text_similarity(db()) else numeric_similarity(db())
        sim$best <- best_matches(sim)
        sim$known <- if (ds == "text") known_duplicate_scores(db()) else numeric(0)
        SIM_CACHE[[ds]] <- sim
      })
    }
    SIM_CACHE[[ds]]
  })

  # cheap step after the heavy one: apply the threshold
  flagged <- reactive({
    b <- similarity()$best
    b$status <- ifelse(!b$comparable, "Too little data",
                       ifelse(b$best_score >= input$threshold, "Possible duplicate", "New"))
    b
  })

  # reset everything when the dataset changes, so an ID from the other file is never used
  observeEvent(input$dataset, {
    selected_item(NULL); selected_candidate(NULL); selected_category(NULL)
    updateSliderInput(session, "threshold", value = DEFAULT_THRESHOLD[[input$dataset]])
  }, ignoreInit = TRUE)

  # ---------------- Q5 status counts --------------------------------------
  output$status_counts <- renderUI({
    f <- flagged()
    last <- if (input$dataset == "text")
      value_box("Same barcode as an active product", sum(f$barcode_match), p("near-certain duplicates"), theme = "light")
    else value_box("Too little data to compare", sum(f$status == "Too little data"), p("fewer than 5 nutrients known"), theme = "light")
    layout_columns(
      fill = FALSE,
      value_box("Pending products", nrow(f), p(if (input$dataset == "text") "text file" else "numeric file"), theme = "light"),
      value_box("Possible duplicates", sum(f$status == "Possible duplicate"), p(sprintf("score \u2265 %.2f", input$threshold)), theme = value_box_theme(bg = "#fbe3d9", fg = "#7a2a0e")),
      value_box("New products", sum(f$status == "New"), p("need validation + values"), theme = value_box_theme(bg = "#dde8f6", fg = "#1d4f86")),
      last
    )
  })

  # ---------------- Q4 category bar ---------------------------------------
  category_stats <- reactive({
    d <- db()
    long <- data.frame(cat = unlist(d$cat_list), active = rep(d$active, lengths(d$cat_list)))
    s <- aggregate(cbind(pending = active == 0, active = active == 1) ~ cat, data = long, FUN = sum)
    s$prop <- s$pending / (s$pending + s$active)
    s <- s[s$pending > 0, ]
    s[order(-(if (input$sort_by == "proportion") s$prop else s$pending)), ]
  })
  output$uncat_note <- renderUI({
    s <- category_stats(); u <- s[s$cat == "Uncategorised", ]
    if (!nrow(u)) return(NULL)
    n <- sum(flagged()$id %in% db()$id[db()$active == 0])
    p(strong(sprintf("%d of %d", u$pending, n)), " pending products have no category. ",
      actionLink("show_uncat", "Show them"))
  })
  observeEvent(input$show_uncat, selected_category("Uncategorised"))
  output$category_bar <- renderD3({
    s <- category_stats(); s <- s[s$cat != "Uncategorised", ]
    list(cat = I(s$cat), pending = I(s$pending), active = I(s$active), prop = I(s$prop),
         sort_by = input$sort_by, selected = selected_category())
  })
  observeEvent(input$category_bar_click, {
    clicked <- input$category_bar_click
    selected_category(if (identical(clicked, selected_category())) NULL else clicked)
  })
  output$category_chip <- renderUI({
    req(selected_category())
    div(style = "margin-bottom:.5rem", span(class = "chip", "Category: ", selected_category(),
                                            actionLink("clear_cat", "\u00d7")))
  })
  observeEvent(input$clear_cat, selected_category(NULL))

  # ---------------- Q1 pending list ---------------------------------------
  pending_view <- reactive({
    d <- db(); f <- flagged()
    item <- d[match(f$id, d$id), ]
    best <- d[match(f$best_id, d$id), ]
    v <- data.frame(id = f$id, product = item$label, status = f$status, score = f$best_score,
                    best = ifelse(is.na(best$label), "\u2014", best$label), barcode = f$barcode_match,
                    stringsAsFactors = FALSE)
    if (!is.null(selected_category())) {
      keep <- vapply(item$cat_list, function(c) selected_category() %in% c, logical(1))
      v <- v[keep, ]
    }
    v[order(-v$barcode, -ifelse(is.na(v$score), -1, v$score)), ]
  })
  output$pending_table <- renderDT({
    v <- pending_view()
    sel <- match(isolate(selected_item()), v$id)
    show <- data.frame(
      Product = ifelse(v$barcode, paste0(htmltools::htmlEscape(v$product), ' <span class="pill bad">same barcode</span>'), htmltools::htmlEscape(v$product)),
      Status = pill(v$status), Score = v$score,
      `Closest active product` = htmltools::htmlEscape(v$best), check.names = FALSE)
    datatable(show, escape = FALSE, rownames = FALSE, fillContainer = FALSE,
              selection = sel_opt(sel),
              options = list(pageLength = 8, dom = "ftp", ordering = FALSE, scrollX = TRUE)) |>
      formatRound("Score", 2) |>
      formatStyle("Score", background = styleColorBar(c(0, 1), "#f3c9b8"),
                  backgroundSize = "95% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })
  observeEvent(input$pending_table_rows_selected, {
    id <- pending_view()$id[input$pending_table_rows_selected]
    if (!identical(id, selected_item())) {
      selected_item(id)
      # new item -> old candidate is no longer valid; start from the best match
      selected_candidate(flagged()$best_id[flagged()$id == id])
    }
  })

  # ---------------- Q8 closest matches ------------------------------------
  candidates <- reactive({
    req(selected_item())
    s <- similarity()$score[selected_item(), ]
    o <- order(-s)[seq_len(min(input$top_n %||% 5, length(s)))]
    list(top = data.frame(id = names(s)[o], score = s[o], stringsAsFactors = FALSE),
         above = sum(s >= input$threshold))
  })
  output$match_summary <- renderUI({
    if (is.null(selected_item())) return(p(class = "hint", "Select a pending product in step 2."))
    f <- flagged()[flagged()$id == selected_item(), ]
    if (f$status == "Too little data")
      return(p(class = "hint", "This product has fewer than 5 nutrients filled in, so it cannot be compared. Check it in the Data quality tab."))
    c <- candidates()
    p(strong(c$above), sprintf(" active product%s score \u2265 %.2f. ", ifelse(c$above == 1, "", "s"), input$threshold),
      if (c$above > 1) "Several variants may already exist." else "",
      span(class = "hint", "Click a row to compare it."))
  })
  output$match_table <- renderDT({
    req(selected_item())
    f <- flagged()[flagged()$id == selected_item(), ]
    req(f$status != "Too little data")
    t <- candidates()$top; d <- db()
    sel <- match(isolate(selected_candidate()), t$id)
    show <- data.frame(`#` = seq_len(nrow(t)), `Active product` = d$label[match(t$id, d$id)],
                       Score = t$score, check.names = FALSE)
    datatable(show, rownames = FALSE, fillContainer = FALSE, selection = sel_opt(sel),
              options = list(dom = "t", ordering = FALSE, pageLength = 25)) |>
      formatRound("Score", 2) |>
      formatStyle("Score", color = styleInterval(input$threshold - 1e-9, c("#1d4f86", "#9c3412")), fontWeight = "600")
  })
  observeEvent(input$match_table_rows_selected, {
    id <- candidates()$top$id[input$match_table_rows_selected]
    if (!identical(id, selected_candidate())) selected_candidate(id)
  })

  # ---------------- Q2 comparison -----------------------------------------
  mark <- function(s) {
    if (is.na(s)) return('<td class="m none" title="missing">\u2013</td>')
    if (s >= 0.9) '<td class="m same" title="same">\u25cf</td>'
    else if (s >= 0.6) '<td class="m close" title="similar">\u25d0</td>'
    else '<td class="m diff" title="different">\u25cb</td>'
  }
  comparison <- reactive({
    req(selected_item(), selected_candidate())
    d <- db()
    list(p = d[d$id == selected_item(), ], c = d[d$id == selected_candidate(), ],
         score = similarity()$score[selected_item(), selected_candidate()])
  })
  output$compare_table <- renderUI({
    if (is.null(selected_item())) return(p(class = "hint", "Select a pending product (step 2) and a match (step 3)."))
    if (is.null(selected_candidate()) || is.na(selected_candidate())) return(p(class = "hint", "Select a match in step 3."))
    x <- comparison(); P <- x$p; C <- x$c
    e <- function(v) ifelse(is.na(v) | v == "", '<span class="hint">\u2014</span>', htmltools::htmlEscape(v))
    if (input$dataset == "text") {
      sim <- function(a, b, m = "cosine") if (is.na(a) || is.na(b)) NA else stringdist::stringsim(a, b, method = m, q = 3)
      code_same <- if (!length(P$barcode_list[[1]]) || !length(C$barcode_list[[1]])) NA else as.numeric(any(P$barcode_list[[1]] %in% C$barcode_list[[1]]))
      cat_same <- if (any(c(P$cat_list[[1]], C$cat_list[[1]]) == "Uncategorised")) NA else as.numeric(any(P$cat_list[[1]] %in% C$cat_list[[1]]))
      rows <- list(
        c("Name", e(P$name), e(C$name), mark(max(sim(P$name_norm, C$name_norm), sim(P$name_norm, C$remarks_norm), na.rm = TRUE))),
        c("French name (remarks)", e(P$remarks), e(C$remarks), mark(sim(P$remarks_norm, C$remarks_norm))),
        c("Brand", e(P$brands), e(C$brands), mark(sim(P$brand_norm, C$brand_norm, "jw"))),
        c("Barcode", e(P$barcode), e(C$barcode), mark(code_same)),
        c("Category", e(P$categories), e(C$categories), mark(cat_same)),
        c("Source (bron)", e(P$bron), e(C$bron), mark(NA)))
    } else {
      rows <- lapply(c("energy", "protein", "fat", "saturated_fatty_acid", "carbohydrates", "sugar", "dietary_fiber", "salt_filled"), function(f) {
        a <- P[[f]]; b <- C[[f]]
        close <- if (is.na(a) || is.na(b)) NA else { dlt <- abs(a - b); if (dlt <= max(0.5, 0.05 * max(a, b))) 1 else if (dlt <= max(2, 0.2 * max(a, b))) 0.7 else 0 }
        lab <- if (f == "salt_filled") "Salt" else NUTRIENTS[[f]]
        c(lab, fmt(a), fmt(b), mark(close))
      })
      rows <- c(list(c("Category", e(P$categories), e(C$categories), mark(NA))), rows)
    }
    body <- paste0(vapply(rows, function(r) sprintf("<tr><th>%s</th><td>%s</td><td>%s</td>%s</tr>", r[1], r[2], r[3], r[4]), ""), collapse = "")
    dup <- x$score >= input$threshold
    tagList(
      HTML(sprintf('<table class="cmp"><tr><th></th><th>Pending \u00b7 %s</th><th>Active \u00b7 %s</th><th></th></tr>%s</table>', P$id, C$id, body)),
      div(class = "verdict", HTML(sprintf(
        "Score <b class='score'>%.2f</b> %s threshold %.2f \u2192 %s",
        x$score, if (dup) "\u2265" else "<", input$threshold,
        if (dup && input$dataset == "numeric") "<b>same nutrient profile:</b> likely a duplicate, but nutrients alone are no proof. Check the label before copying."
        else if (dup) "<b>likely the same product:</b> copy the values from the active product."
        else "<b>probably a new product:</b> validate it and look up its values (step 5)."))),
      p(class = "hint", style = "margin-top:.4rem", "\u25cf same \u00a0 \u25d0 similar \u00a0 \u25cb different \u00a0 \u2013 missing")
    )
  })

  # ---------------- Q3 web enrichment -------------------------------------
  web_values <- eventReactive(input$enrich, {
    req(input$dataset == "text", selected_item())
    item <- db()[db()$id == selected_item(), ]
    res <- tryCatch(lookup_nutrition(item), error = function(e) list(error = conditionMessage(e)))
    list(id = selected_item(), res = res)
  })
  output$enrich_table <- renderUI({
    if (input$dataset == "numeric")
      return(p(class = "hint", "Only available for the text file: the numeric file has no product name or barcode to search with."))
    if (is.null(selected_item())) return(p(class = "hint", "Select a pending product first."))
    w <- if (input$enrich > 0) web_values() else NULL
    if (is.null(w) || !identical(w$id, selected_item()))
      return(p(class = "hint", "Searches by barcode first, then by name and brand. The values are a proposal: the dietitian confirms them."))
    if (is.null(w$res)) return(p("Nothing found on Open Food Facts for this product. The dietitian has to enter the values by hand."))
    if (!is.null(w$res$error)) return(p(class = "pill bad", w$res$error))
    v <- w$res$values
    tagList(
      p(strong(w$res$product), span(class = "hint", paste0(" \u00b7 found by ", w$res$how, " \u00b7 ")),
        a("open source page \u2197", href = w$res$url, target = "_blank")),
      HTML(paste0('<table class="cmp"><tr><th>Nutrient</th><td><b>Proposed value / 100 g</b></td></tr>',
                  paste0(sprintf("<tr><th>%s</th><td class='mono'>%s</td></tr>", v$nutrient, fmt(v$value)), collapse = ""),
                  "</table>"))
    )
  })

  # ---------------- Q6 histogram ------------------------------------------
  output$score_hist <- renderD3({
    sim <- similarity()
    s <- sim$best$best_score[sim$best$comparable]
    list(scores = I(s), known = I(sim$known), threshold = input$threshold, min_x = 0)
  })
  observeEvent(input$score_hist_threshold, updateSliderInput(session, "threshold", value = input$score_hist_threshold))
  output$threshold_story <- renderUI({
    f <- flagged(); k <- similarity()$known; th <- input$threshold
    tagList(
      p(sprintf("At threshold %.2f:", th)),
      tags$ul(
        tags$li(strong(sum(f$status == "Possible duplicate")), " pending products flagged as possible duplicates"),
        tags$li(strong(sum(f$status == "New")), " treated as new"),
        if (any(f$status == "Too little data")) tags$li(strong(sum(f$status == "Too little data")), " cannot be compared (too few nutrients)")
      ),
      if (length(k)) p(sprintf("Of the %d known duplicates (active products that share a barcode), ", length(k)),
                       strong(sprintf("%.0f%%", 100 * mean(k >= th))),
                       " would be found by name and brand alone. Names differ a lot (Dutch vs French, extra words), which is why a shared barcode always counts as a match.")
      else p("The numeric file has no barcodes or names, so there are no known duplicates to test against. Nutrients alone cannot prove two products are the same: 316 rows in this file have exactly the same values as another row."),
      p(class = "hint", "Lower threshold: more real duplicates found, but more false alarms for the dietitian. Higher threshold: fewer false alarms, more duplicates slip into the database. Drag the dashed line or use the slider.")
    )
  })

  # ---------------- Q7 data quality (numeric file only) ------------------
  quality_flags <- reactive({
    q <- quality_issues(DATA$numeric)
    if (input$nutrient != "all") q <- q[q$nutrient == input$nutrient, ]
    q
  })
  output$quality_counts <- renderUI({
    q <- quality_flags()
    tagList(
      p(strong(length(unique(q$id[q$severity == "Impossible"]))), " products with impossible values"),
      p(strong(length(unique(q$id[q$severity == "Suspicious"]))), " with energy that does not match the macros"),
      p(strong(length(unique(q$id[q$severity == "Missing"]))), " with missing required values")
    )
  })
  output$quality_table <- renderDT({
    q <- quality_flags(); d <- DATA$numeric
    cls <- c(Impossible = "bad", Suspicious = "warn", Missing = "na")[as.character(q$severity)]
    show <- data.frame(Product = d$label[match(q$id, d$id)],
                       Nutrient = ifelse(q$nutrient %in% names(NUTRIENTS), NUTRIENTS[q$nutrient], q$nutrient),
                       Value = q$value, Reason = q$issue,
                       Flag = sprintf('<span class="pill %s">%s</span>', cls, q$severity), check.names = FALSE)
    datatable(show, escape = FALSE, rownames = FALSE, fillContainer = FALSE, selection = "none",
              options = list(pageLength = 15, dom = "ftp", order = list()))
  })
}

shinyApp(ui, server)

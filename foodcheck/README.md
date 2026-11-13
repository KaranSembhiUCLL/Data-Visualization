# FoodCheck · Team F8 — Validation of food products

A Shiny dashboard that helps a dietitian decide, for every unvalidated product
(`active = 0`), whether it is a **duplicate** of an existing product, a
**new** product that needs nutritional values, or an **invalid** entry.
It is built from `app_diagram.mmd`: every output answers one user question (Q1–Q8).

## Run it

1. Install R (≥ 4.2) and the packages (one time):
   ```r
   install.packages(c("shiny", "bslib", "DT", "stringdist", "readxl", "httr2", "jsonlite"))
   ```
2. Open this folder in RStudio and click **Run App**, or run `shiny::runApp()` in this folder.

The first time you switch to a dataset, the app compares every pending product with every
active product. That takes about 5 seconds for the text file and 1 second for the numeric file.
D3 is included in `www/`, so the charts work offline. Only Auto-Enrich (Q3) needs internet.

## Files

| File | What it does |
|---|---|
| `app.R` | UI and server, wired as in the diagram |
| `R/clean.R` | Loading and cleaning (fixes broken accents, splits barcodes, fills salt from sodium) |
| `R/similarity.R` | Definition of "similar" for each dataset |
| `R/quality.R` | Rules for missing and impossible values (Q7) |
| `R/enrich.R` | Open Food Facts lookup (Q3) |
| `www/charts.js` | The two D3 charts (Q4 category bar, Q6 histogram) |
| `data/` | The two original Excel files. They are never merged. |

## Our definition of "similar"

**Text file** (names, brands, barcodes)
- A **shared barcode** means the same product, so the score is 1. Cells with several barcodes are split, and leading zeros are ignored.
- Otherwise: `score = 0.8 × name + 0.2 × brand`, or `score = name` if a brand is missing.
  - *name*: cosine similarity of character 3-grams. It handles word order and typos. We compare against both the active product's name and its French name (`remarks`), because scanned products often come in French while the database is in Dutch.
  - *brand*: Jaro-Winkler similarity.
- Default threshold **0.80**.

**Numeric file** (nutrients per 100 g only, no names)
- Nutrients used: energy, protein, fat, saturated fat, carbohydrates, sugar, fibre and salt. Salt is filled from sodium ÷ 400 where it is missing. Starch is left out because it is missing in 83% of rows.
- Each nutrient is `log1p`-transformed and divided by its spread, so energy (hundreds of kcal) does not drown out salt (around 1 g).
- `distance` = root-mean-square difference over the nutrients both products have, and `score = exp(−distance)`.
- Products with fewer than 5 nutrients get the status **Too little data** rather than "new".
- Default threshold **0.95**. Nutrients alone are no proof: 316 rows in the file share exactly the same values with another row.

## Findings that shaped the design
- 122 of 152 pending text products (and 122 of 154 numeric ones) have **no category**. Q4 shows this in a separate line rather than as one huge bar.
- 11 pending text products share a barcode with an active product, so they are near-certain duplicates.
- Of the 117 pairs of active products that share a barcode (known duplicates), only 37% score ≥ 0.80 on name and brand. Names alone miss many duplicates, which is why the barcode rule matters. The Threshold lab shows this live.
- About 3,100 text rows had broken accents (`NestlÃ©` instead of `Nestlé`). They are repaired when the data is loaded.
- Impossible values in the numeric file, for example 320 g of protein and 1,380 kcal per 100 g, are listed in the Data quality tab.

## Changes to the diagram (see `app_diagram.mmd`)
- Selecting a new pending product now **resets the selected candidate** to its best match. Before, the comparison could show an old candidate.
- `web_values()` works on the **text file only**, because the numeric file has nothing to search for.
- Dragging the threshold line in the histogram updates the slider (`input$score_hist_threshold`).

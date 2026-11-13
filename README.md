What is the project about?

A food diary app has a database of products. Products checked by a dietitian are active = 1. When a user scans a product that isn’t in the database yet, it is added as not validated: active = 0.

The problem is that such a “new” product often already exists under a slightly different name. “Creamy Rasberry-Yogurt” is the same product as “Yoghurt Creamy framboos”. If the dietitian approves everything blindly, the database fills up with duplicates.

Our app helps the dietitian choose one of three options for every new product:

It is a duplicate. Copy the values from the existing product.
It is genuinely new. Look up the nutritional values online.
It is wrong. For example 320 g of protein per 100 g. Reject it.
The two files

We received two Excel files, and we are not allowed to merge them:

text_food: name, brand, barcode, category and a French name (remarks). No nutritional values.
numeric_food: only nutritional values such as energy, fat, sugar and salt. No names.

So each file needs its own definition of “similar”. That is the core of the assignment.

How we did it

1. Cleaning the data

Broken accents: about 3,100 rows showed “NestlÃ©” instead of “Nestlé”. We repaired them.
Barcodes: some cells held several barcodes separated by ;. We split them.
Salt and sodium: they carry the same information (salt = sodium ÷ 400), so we use only one of them.

2. “Similar” in the text file

Same barcode: then it is certainly the same product, and the score is 1.
Otherwise: score = 0.8 × how similar the names are + 0.2 × how similar the brands are.
Names: we cut each name into pieces of 3 letters (“yog”, “ogh”, “ghu”…) and count how many pieces two names share. This also works with typos and when words are in a different order.
Also the French name: scanned products are often in French, while the database is in Dutch. So we also compare with the French name in remarks.

3. “Similar” in the numeric file

Distance between profiles: we measure how far apart the nutritional values of two products are.
Scale first: otherwise energy (hundreds of kcal) counts far more than salt (about 1 g).
Too little data: a product with fewer than 5 nutrients filled in gets the status “too little data”. It can’t be compared, so we shouldn’t call it “new” either.

4. Choosing the threshold: our biggest struggle

From what score do you call something a duplicate? There are no labels. Our solution: in the database, 117 pairs of validated products share a barcode. They are certainly duplicates, so we use them as a test set.

The result: at a threshold of 0.80, name and brand alone find only 37% of those real duplicates. Names differ too much (Dutch versus French, extra words such as the pack size). That is why the barcode rule is essential. In the app you can move the threshold and see the effect immediately.

5. The app (Shiny)

Each question from our homework (Q1–Q8) became one card in the app. The Mermaid diagram was the blueprint. Building the app revealed two gaps in the diagram:

The comparison got stuck: when you picked another product, the comparison still showed the old one. This is fixed now.
Online lookup can’t work for the numeric file: there is no name or barcode to search with.

6. Machine learning: still a plan

Right now the app simply finds the closest products by that score (nearest neighbour), and we chose the weights 0.8 and 0.2 ourselves. The next step is a logistic regression:

Input: for each pair of products, how similar the name, brand and category are.
Examples: the 117 barcode pairs as “duplicate”, random pairs as “not duplicate”.
Result: the model learns the best weights itself and gives a probability instead of a guessed score.
The slides in one sentence each
Cover: who we are.
What works: the app and the live demo.
Definition of “similar”: our most important choice, per file.
37%: the problem, and why the barcode matters.
Diagram to app: how the questions and the Mermaid diagram became the app.
ML and next steps: we end with a question for the lecturers.

The one thing to remember for questions afterwards: names alone are not enough to find duplicates. That is why we use the barcode, and why we want a model to learn the weights.

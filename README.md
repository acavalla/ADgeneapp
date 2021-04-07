# AD gene shiny app

A fully cited RShiny app to indicate genes' significance in Alzheimer disease or in microglial cell identity, for potential targeting in therapies.

To run:
1) Clone this repo.
2) In terminal, run `R` to open an R REPL, then run
```
list <- c("shiny", "shinyjs", "DT", "fmsb", "gridExtra", "stringi", "dplyr", "ggplot2", "ggrepel")
for (i in 1:length(list)){
  install.packages(list[i])
}
```
3) Quit the REPL by running `q()` and pressing `n` followed by `enter`
4) Run `R -e "shiny::runApp('./Microglia_AD_genes_apps.R')"`
5) Terminal will print `Listening on ` followed by a url; enter that url into your browser

DEMOS
6) First tab (Dropdown list): select APOE from the dropdown list to see all available info about that crucial Alzheimer gene
7) Second tab (Text input): copy & paste the genes in the file `dummy.tsv` to see the output from inputting a list of genes
8) Third tab (File upload): upload `dummy.tsv` to see the output from uploading a file

Screenshots

Dropdown List Features:
1. Dropdown list to select a gene
2. Indicates whether the gene was mentioned in any of two seminal microglial gene papers and seven seminal Alzheimer disease papers, with links out to those papers, and fold change, P value, and RPKM where data is available
3. Sliders (see 8)
4. Links to further papers mentioning this gene
5. Radar plot, indicating which type of brain cell this gene is associated with (microglia, astrocyte, neurone, oligodendocyte); scale indicated on the side, and the font size of the cell identities changes based on whether it's the most significant
6. Whether it is indicated as being in the top 50 of a paper by Kelley et al
7. JavaScript link to display info on what a radar plot is
8. Volcano plot (data from Galatro et al), indicating fold change by P value. Significant thresholds alterable by sliders (from point 3); data points labelled by color relative to thresholds

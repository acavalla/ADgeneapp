# AD gene shiny app

A fully cited RShiny app to indicate genes' significance in Alzheimer disease or in microglial cell identity, for potential targeting in therapies.

To run:
1. Clone this repo
2. In terminal, run `R` to open an R REPL, then run
```
list <- c("shiny", "shinyjs", "DT", "fmsb", "gridExtra", "stringi", "dplyr", "ggplot2", "ggrepel")
for (i in 1:length(list)){
  install.packages(list[i])
}
```
3. Quit the REPL by running `q()` and pressing `n` followed by `enter`
4. Run `R -e "shiny::runApp('./Microglia_AD_genes_apps.R')"`
5. Terminal will print `Listening on ` followed by a url; enter that url into your browser
6. (The following instructions are to demo the app's functionality) First tab (Dropdown list): select APOE from the dropdown list to see all available info about that crucial Alzheimer gene
7. Second tab (Text input): copy & paste the genes from the file `dummy.tsv` into the form to see the output from inputting a list of genes
8. Third tab (File upload): upload `dummy.tsv` to see the output from uploading a file

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

This is code I wrote in 2018 without any formal training or guidance, so while it works, looking back at it now is somewhat painful! Here are some things I would improve:
- Naming of variables, including tsv files. Calling a file full of genetic data 'extras' doesn't explain much! I can't remember what the sources for these different excel files were now and I feel like the work / payoff balance would be skewed heavily in the direction of work, so I'll be sure not to make that mistake again
- Some variables are just named x or y, which isn't very descriptive for someone looking at the code
- selectizeInput should be moved to server. Having this in the ui section slows performance
- Everything is in one huge 600-line file. So much for srp...
- It's not the most aesthetically pleasing
- Sliders should be next to the plot they control
- The toggle text should be wrapped

What I like:
- I like the coloured dots on the volcano plot and that they change colour based on the movable thresholds, controlled by the sliders
- There is some level of explanatory commentary
- When I wrote this I didn't have a great understanding of how to use Git, so I was pleased to find I still had all the bits and that I could hook them up to work!
- I like knowing how much I've improved since then!

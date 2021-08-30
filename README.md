# Voter Representation and Redistricting

[copy abstract here]

## Replication

To replicate the figures and analyses in the paper, run the scripts in `R/` in order:

``` r
lapply(c(Sys.glob("R/*.R"), Sys.glob("R/**/*.R")), source)
```

Then run `r rmarkdown::render("paper/paper.Rmd")` to generate the paper.


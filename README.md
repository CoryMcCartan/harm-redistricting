# Measuring Individual Partisan Harm in Redistricting

### [Cory McCartan](https://corymccartan.github.io) and [Christopher T. Kenny](https://www.christophertkenny.com)

[copy abstract here]

## Replication

To replicate the figures and analyses in the paper, run the scripts in `R/` in order:

``` r
lapply(sort(Sys.glob("R/*.R")), source)
```

Then run `rmarkdown::render("paper/paper.Rmd")` to generate the paper.


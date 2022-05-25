# Individual and Differential Harm in Redistricting

### [Cory McCartan](https://corymccartan.github.io) and [Christopher T. Kenny](https://www.christophertkenny.com)

We propose a unified framework for evaluating the impact of a districting plan on voters overall and in different groups.
Considering the harm done to individual voters by their inability to elect their chosen candidate, and taking into account the range of counterfactual possibilities, we derive a set of measures that flexibly adapts to local geography and constraints, different types of elections, and different groups of study.
We provide recommendations for estimating these measures from historical election returns and discuss connections to commonly used redistricting metrics.
Finally, we apply our framework to study partisan gerrymandering in New Jersey, voting rights litigation in Alabama, and the racial impacts of city council district boundaries in Boston.

## Replication

To replicate the figures and analyses in the paper, run the scripts in `R/` in order:

``` r
lapply(sort(Sys.glob("R/*.R")), source)
```

Then run `rmarkdown::render("paper/paper.Rmd")` to generate the paper.


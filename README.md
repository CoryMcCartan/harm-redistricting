# Individual and Differential Harm in Redistricting

### [Cory McCartan](https://corymccartan.com) and [Christopher T. Kenny](https://www.christophertkenny.com)

Social scientists have developed dozens of measures for assessing partisan bias in redistricting.
But these measures cannot be easily adapted to other groups, including those defined by race, class, or geography.
Nor are they applicable to single- or no-party contexts such as local redistricting.
To overcome these limitations, we propose a unified framework of harm for evaluating the impacts of a districting plan on individual voters and the groups to which they belong.
We consider a voter harmed if their chosen candidate is not elected under the current plan, but would be under a different plan.
Harm improves on existing measures by both focusing on the choices of individual voters and directly incorporating counterfactual plans.
We discuss strategies for estimating harm, and demonstrate the utility of our framework through analyses of partisan gerrymandering in New Jersey, voting rights litigation in Alabama, and racial dynamics of Boston City Council elections.

## Replication

To replicate the figures and analyses in the paper, run the scripts in `R/` in order:

``` r
lapply(sort(Sys.glob("R/*.R")), source)
```

Then run `rmarkdown::render("paper/harm.Rmd")` to generate the paper.


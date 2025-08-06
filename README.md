# [Individual and Differential Harm in Redistricting](https://osf.io/preprints/socarxiv/nc2x7/)

### [Cory McCartan](https://corymccartan.com) and [Christopher T. Kenny](https://www.christophertkenny.com)

![Harm schematic](https://user-images.githubusercontent.com/2958471/187107731-ac78132f-479a-4c5c-bc6d-546d33f20500.png)

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

Then run `quarto::quarto_render("paper/harm.qmd")` to generate the paper.


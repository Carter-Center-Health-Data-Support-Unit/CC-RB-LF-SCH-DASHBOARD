
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CC-RB-LF-SCH-DASHBOARD

[![contributions
welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues)

The goal of CC-RB-LF-SCH-DASHBOARD repository is to run the data
pipeline necessary to prepare the data for the CC-RB-LF-SCH Dashboard.

## Data Sources

list all data sources here in bullet format. I think best to include
file name and short description then you can share the path to each file
in an email/DM. We will link to all data sources with `keys` and **never
put data into the repo**

1.  *fill here*
2.  *etc.*

## Structure

The repository is based around an R project (`.rproj`). The structure
will closely follow R package design protocol, but will not be built
into a package at this stage. We avoid creating a `package-project` as
we will want to minimize complications when/if we deploy a `{targets}`
workflow.

-   **R** - contains pure functions
-   **scripts** - scripts/exploratory work done in `.R` files
-   **rmds** - work done in `.Rmd` files.
-   **note** - it might make sense to `usethis::use_vignette()` rather
    than `rmds`as typical for package development. We can assess this
    later on.
-   **tests** - automatically created by `usethis::use_testthat()` for
    storing tests.

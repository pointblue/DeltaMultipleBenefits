
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- <img src="man/figures/PB_logo_RGB_Full_Color_cs.jpg" align="left" alt="" width="180" /><br><br> -->

# DeltaMultipleBenefits

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/380353580.svg)](https://zenodo.org/badge/latestdoi/380353580)
[![](https://img.shields.io/badge/devel%20version-1.0.0-blue.svg)](https://github.com/pointblue/DeltaMultipleBenefits)
[![License: GPL (\>=
3)](https://img.shields.io/badge/license-GPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/GPL%20(%3E=%203))
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

Achieving the long-term vision of a resilient Sacramento-San Joaquin
River Delta ecosystem requires meeting multiple goals simultaneously,
including self-sustaining wildlife populations and thriving communities
of people, in an evolving landscape. However, to effectively plan and
implement policies and land management decisions intended to achieve
multiple goals, it is essential to be able to identify the potential
trade-offs of changes to the landscape across multiple goals. Meeting
this challenge would support effective policy and resource management
decisions and address several science priorities identified in Science
Action Agendas for the Delta.

In Phase 1 of this project, we built on our partnerships in the Delta
and broader Central Valley, and on our expertise in multiple-benefit
conservation and avian ecology in agricultural landscapes, to help meet
this challenge by addressing two main goals: (1) identify Priority Bird
Conservation Areas in the Delta to support effective restoration and
management decisions, and (2) develop a science-based framework for
forecasting the net benefits or trade-offs of proposed or anticipated
landscape changes on multiple goals. The ultimate goals of this project
were to support communication among Delta community members about the
projected synergies and trade-offs among multiple metrics, facilitate
the identification of, and community support for, solutions to address
these trade-offs, and thereby support effective decision-making and
policy to reach the Delta Plan’s multiple goals. Phase 1 results are now
available in our <a
href="https://www.kristendybala.com/files/Dybala2023_DeltaMultipleBenefits_Report.pdf"
target="_blank">Final Project Report</a>, with peer-reviewed manuscripts
underway and multiple supporting data products now available (see
below).

This `DeltaMultipleBenefits` R package serves as our open-source,
science-based framework that supports estimating the net impacts of
scenarios of landscape change in the Sacramento-San Joaquin River Delta
to identify projected benefits and trade-offs across multiple metrics.
The package supports land use planning, management, conservation, and
community engagement in the Sacramento-San Joaquin River Delta by
providing tools for applying existing models and data to user-supplied
landscapes to estimate a range of benefits to the Delta community and
compare a baseline landscape to proposed or anticipated future landscape
scenarios. Currently, the benefit categories addressed include:
Agricultural Livelihoods, Water Quality, Climate Change Resilience, and
Biodiversity Support. Each category is represented by multiple
individual metrics, representing either values applied to each land
cover class and summarized over the entire landscape or the total amount
of suitable habitat projected from species distribution models. By
comparing landscape totals for each metric estimated from a baseline
landscape and alternative scenarios of future landscape change, the
expected direction and magnitude of the net change in each metric is
estimated.

## Installation

<!--You can install the released version of DeltaMultipleBenefits from [CRAN](https://CRAN.R-project.org) with:
&#10;``` r
install.packages("DeltaMultipleBenefits")
```
&#10;And the development version from [GitHub](https://github.com/) with:-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pointblue/DeltaMultipleBenefits")
```

## How to use

This package includes a vignette that serves as a tutorial outlining the
major steps of analyzing alternative Delta landscapes and comparing them
to each other, including:

1.  Preparing new landscape scenarios for analysis
2.  Summarizing the net change in the total area of each land cover
    class
3.  Estimating the net change in simple metrics  
4.  Estimating the net change in metrics informed by spatial models

## Supporting data

<a href="https://apps.wildlife.ca.gov/bios6/?bookmark=356"
target="_blank"><img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/arrow-up-right-from-square.svg" width="18" height="18"></a>
Baseline and projected future land use and land cover in the
Sacramento-San Joaquin Delta

<a href="https://doi.org/10.5281/zenodo.7504874"
target="_blank"><img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/arrow-up-right-from-square.svg" width="18" height="18"></a>
Metrics Data for Quantifying Multidimensional Impacts of Landscape
Change in California’s Sacramento–San Joaquin Delta. DOI:
10.5281/zenodo.7504874 *(also included in the `DeltaMultipleBenefits`
package)*

<a href="https://doi.org/10.5281/zenodo.7531945"
target="_blank"><img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/arrow-up-right-from-square.svg" width="18" height="18"></a>
Distribution models for riparian landbirds and waterbirds in the
Sacramento-San Joaquin Delta. DOI: 10.5281/zenodo.7531945

<a href="https://doi.org/10.5281/zenodo.7672193"
target="_blank"><img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/arrow-up-right-from-square.svg" width="18" height="18"></a>
Supplemental Spatial Data for Quantifying Multidimensional Impacts of
Landscape Change in California’s Sacramento–San Joaquin Delta. DOI:
10.5281/zenodo.7672193

## Relevant Literature

Dybala KE, Sesser K, Reiter M, Hickey C, Gardali T. 2023. *Final Project
Report: Trade-offs and Co-benefits of Landscape Change Scenarios on Bird
Communities and Ecosystem Services in the Sacramento-San Joaquin River
Delta.* Point Blue Conservation Science, Petaluma, CA<br> <a
href="https://www.kristendybala.com/files/Dybala2023_DeltaMultipleBenefits_Report.pdf"
target="_blank"><img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/file-pdf.svg" width="20" height="20"></a>

Dybala K, Sesser K, Reiter M, Shuford WD, Golet GH, Hickey C, Gardali T
(2023) Priority Bird Conservation Areas in California’s Sacramento–San
Joaquin Delta. San Francisco Estuary and Watershed Science 21(3). DOI:
[10.15447/sfews.2023v21iss3art4](https://doi.org/10.15447/sfews.2023v21iss3art4)<br>
<a
href="https://www.kristendybala.com/files/Dybala2023_PriorityBirdConservationAreas.pdf"
target="_blank"><img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/file-pdf.svg" width="20" height="20"></a>

Dybala KE, Reiter ME, Hickey CM (*In review*) Multiple-benefit
Conservation in Practice: A Framework for Quantifying Multi-dimensional
Impacts of Landscape Change in California’s Sacramento–San Joaquin Delta

## Funding Statement

These data were developed as part of the project “Trade-offs and
Co-benefits of Landscape Change on Bird Communities and Ecosystem
Services in the Sacramento–San Joaquin River Delta”, funded by
Proposition 1 Delta Water Quality and Ecosystem Restoration Program,
Grant Agreement Number – Q1996022, administered by the California
Department of Fish and Wildlife.

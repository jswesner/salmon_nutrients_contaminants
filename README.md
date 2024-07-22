
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Shifts in Pacific salmon community alter continental-scale nutrient and contaminant delivery

This page provides data and code for *Brandt et al. Shifts in Pacific
salmon community alter continental-scale nutrient and contaminant
delivery*.

All figures and tables in the manuscript can be recreated by running the
R scripts in the `code` folder. The scripts are named in order (e.g., 1,
2, 3…), so that script 2 will not work without running script 1 first,
and so forth.

The scripts provide the following:

**1) Import_data.R**: Imports and wrangles contaminant and nutrient data

**2)** **Model contaminants and nutrients.R**: Imports data from script
1 and fits hierarchical Bayesian models estimating the concentrations of
four nutrients and four contaminants in Pacific Salmon tissues. Prior
predictive simulation is also conducted and justified.

**3) Model salmon time series.R**: Imports and wrangles data for salmon
escapement abundance and salmon individual mass. Multiplies
mass\*abundance for each collection to generate salmon escapement
biomass. Fits Bayesian Generalized Additive Models to estimate the
escapement of salmon biomass across time, species, and regions. NOTE:
The data included here are NOT the raw data. Instead, the data are
simulated and to be used for example code only. For the correct raw
data, contact Dr. Greg Ruggerone: <gruggerone@nrccorp.com>.

**4a) Prior Predictive Checks.R**: Conducts prior predictive checks on
the models fit in steps 2 and 3.

**4b) Posterior Predictive Checks.R**: Conducts posterior predictive
checks on the models fit in steps 2 and 3.

**5) Stack Posteriors.R**: Combines posteriors from the separate models
for contaminants and nutrients into a single data file for ease of use
later.

**6) Model Contaminants x Salmon.R**: Multiplies the posteriors for
contaminants/nutrients by the posteriors for salmon escapement to
generate a distribution of contaminant and nutrient escapement.

**7a) Wrangle Plot Data.R**: Wrangles the posteriors to make underlying
data for the figures in the main text and in the Extended Data.

**7b) Make Plots.R** Uses the wrangled data from 7a to make the figures
in the main text and in the Extended Data.

**8) Sensitivity Analysis.R**: Conducts a sensitivity analysis to
determine the relative contributions of salmon abundance, salmon
individual mass, and contaminant/nutrient concentrations to variation in
contaminant/nutrient transport.

**9) Summary Tabls.R**: Computes summary statistics for the main text,
Extended Data, and Supplementary Information

**10) Check influence of 1980 Bering Sockeye outlier.R**: Compares
salmon escapement models that include or don’t include 1980 data on
Bering Sockeye to check for influence of large escapement in that year.

**11) data release.R**. Wrangles data for USGS data release.

**12) sanity checks.R**. Various checks of the data compared to models.
e.g., checks that the modeled escapement overlaps with raw data, and
that raw estimates of contaminant/nutrient transport also match with
modeled estimates, etc.

**13) Biotransport per fish.R.** Computes biotransport per individual
fish.

Two additional scripts are included:

**functions.R**: Contains custom functions

**simulate_salmon_data.R**. Demonstrates method to randomize the salmon
data for this release.

# Software
R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
Copyright (C) 2024 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

REBL Score Calculator
================
Chris Donovan, Dr. Trisha Shrum

October 04, 2025

- [Introduction](#introduction)
- [What it does currently](#what-it-does-currently)
- [What it will do eventually](#what-it-will-do-eventually)
- [References](#references)

# Introduction

The REBL Score Calculator is a Shiny App that makes it easy to
administer the Repeated Environmental Behavior Latent (REBL) Scale. [You
can find the app deployed here](https://cdonov12.w3.uvm.edu/reblcalc/).
It is built on the `rebl` package, [with documentation available
here](https://chrisdonovan307.github.io/rebl/). The app currently
provides a pretty restricted range of use cases, but we plan to make it
more flexible and broadly applicable in the fall of 2025.

# What it does currently

- Get REBL scores using Conditional Maximum Likelihood (CML) models with
  the `ltm` package (Rizopoulos 2006)
- Use test linking with `plink` to scale CML-based REBL scores against
  our baseline of over 1000 participants.

# What it will do eventually

- Run Marginal Maximum Likelihood (MML) models with the `eRm` package
  (Mair and Hatzinger 2007).
- Compare MML scores against our baseline of participants.
- Allow for easy comparisons of panel data with different sets of REBL
  items.

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-mair2007" class="csl-entry">

Mair, Patrick, and Reinhold Hatzinger. 2007. “CML Based Estimation of
Extended Rasch Models with the eRm Package in r.” *Psychology Science*
49 (1): 26–43.

</div>

<div id="ref-rizopoulos2006" class="csl-entry">

Rizopoulos, Dimitris. 2006. “Ltm: An r Package for Latent Variable
Modeling and Item Response Theory Analyses.” *Journal of Statistical
Software* 17 (5): 1–25.

</div>

</div>

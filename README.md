# An evolutionary attractor model for sapwood cross section in relation to leaf area

[Mark Westoby](http://bio.mq.edu.au/research/groups/ecology//westoby/mark.htm),
[William K. Cornwell](http://www.phylodiversity.net/wcornwell/),
[Daniel Falster](http://danielfalster.com/)

This repository contains all the code used in the manuscript:

Westoby, M, W. K. Cornwell, and D. S. Falster. “An Evolutionary Attractor Model for Sapwood Cross Section in Relation to Leaf Area.” *Journal of Theoretical Biology* **303** (2012): 98–109. doi: [10.1016/j.jtbi.2012.03.008](http://doi.org/10.1016/j.jtbi.2012.03.008).

**Abstract:** Sapwood cross-sectional area per unit leaf area (SA:LA) is an influential trait that plants coordinate with physical environment and with other traits. We develop theory for SA:LA and also for root surface area per leaf area (RA:LA) on the premise that plants maximizing the surplus of revenue over costs should have competitive advantage. SA:LA is predicted to increase in water-relations environments that reduce photosynthetic revenue, including low soil water potential, high water vapor pressure deficit (VPD), and low atmospheric CO2. Because sapwood has costs, SA:LA adjustment does not completely offset difficult water relations. Where sapwood costs are large, as in tall plants, optimal SA:LA may actually decline with (say) high VPD. Large soil-to-root resistance caps the benefits that can be obtained from increasing SA:LA. Where a plant can adjust water-absorbing surface area of root per leaf area (RA:LA) as well as SA:LA, optimal RA:SA is not affected by VPD, CO2 or plant height. If selection favours increased height more so than increased revenue-minus-cost, then height is predicted to rise substantially under improved water-relations environments such as high-CO2 atmospheres. Evolutionary-attractor theory for SA:LA and RA:LA complements models that take whole-plant conductivity per leaf area as a parameter.

## Running

To reproduce the figures from the paper, run the command

```
make
```

This will build processed versions of the data in the output directory. Alternative you can open `R` and type

```
source(`analysis.R`)
```

A separate file  `Analytical solutions.m` contain matlab code used to derive the analytical solution for allocation of nitrogen presented in the appendix of the paper.

## Further details

Please note:

- This code was archived post-publication, so there is no link to the code at the journal's website.
- Code was written between 2006-2010 by Daniel Falster and William Cornwell.
- The code included in this release has been slightly restructured to improve readability and code quality. The version run for publication is available [here](https://github.com/dfalster/Westoby_2012_JTB_sapwood_model/commit/20215f539f78f7ecc73635e41844032cdea6c92b).

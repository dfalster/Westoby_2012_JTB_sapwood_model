# An evolutionary attractor model for sapwood cross section in relation to leaf area

[Mark Westoby](http://bio.mq.edu.au/research/groups/ecology//westoby/mark.htm),
[William K. Cornwell](http://www.phylodiversity.net/wcornwell/),
[Daniel Falster](http://danielfalster.com/)

This repository contains all the code used in the manuscript:

Westoby, M, W. K. Cornwell, and D. S. Falster. “An Evolutionary Attractor Model for Sapwood Cross Section in Relation to Leaf Area.” *Journal of Theoretical Biology* **303** (2012): 98–109. doi: [10.1016/j.jtbi.2012.03.008](http://doi.org/10.1016/j.jtbi.2012.03.008).

**Abstract:** Sapwood cross-sectional area per unit leaf area (SA:LA) is an influential trait that plants coordinate with physical environment and with other traits. We develop theory for SA:LA and also for root surface area per leaf area (RA:LA) on the premise that plants maximizing the surplus of revenue over costs should have competitive advantage. SA:LA is predicted to increase in water-relations environments that reduce photosynthetic revenue, including low soil water potential, high water vapor pressure deficit (VPD), and low atmospheric CO2. Because sapwood has costs, SA:LA adjustment does not completely offset difficult water relations. Where sapwood costs are large, as in tall plants, optimal SA:LA may actually decline with (say) high VPD. Large soil-to-root resistance caps the benefits that can be obtained from increasing SA:LA. Where a plant can adjust water-absorbing surface area of root per leaf area (RA:LA) as well as SA:LA, optimal RA:SA is not affected by VPD, CO2 or plant height. If selection favours increased height more so than increased revenue-minus-cost, then height is predicted to rise substantially under improved water-relations environments such as high-CO2 atmospheres. Evolutionary-attractor theory for SA:LA and RA:LA complements models that take whole-plant conductivity per leaf area as a parameter.

## Running the code

All analyses were done in `R`. All code needed to reproduce the results is included in this repository in the `analysis.R` file. Figures will be output to a directory called `output`.

These results were last reproduced using R 3.6.1. To reproduce these results with this version of R, and without having to clone and download the repository, you can access an interactive RStudio session by opening a container hosted by [Binder](http://mybinder.org): 

[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dfalster/Westoby_2012_JTB_sapwood_model/master?urlpath=rstudio)

To ensure long-term [computational reproducibility](https://www.britishecologicalsociety.org/wp-content/uploads/2017/12/guide-to-reproducible-code.pdf) of this work, we have created a [Docker](http://dockerhub.com) image to enable others to reproduce these results on their local machines using the same software and versions we used to conduct the original analysis. Instructions for reproducing this work using the docker image are available at the bottom of the page. 

## Material included in the repository include:

- `R/`: directory containing functions used in analysis
- `Analytical solutions.m`: contains matlab code used to derive the analytical solution for allocation of nitrogen presented in the appendix of the paper
- `DESCRIPTION`: A machine-readable [compendium]() file containing key metadata and dependencies 
- `license.md`: License for the materials
- `Dockerfile` & `.binder/Dockerfile`: files used to generate docker containers for long-term reproducibility


## Citation

For archival purposes, the code used to produce figures for publication has been lodged with figshare [here](http://dx.doi.org/10.6084/m9.figshare.1005160). This is the same as code included in [this github release](https://github.com/dfalster/Westoby_2012_JTB_sapwood_model/releases/tag/v1.0).

To cite this code:

```
	Westoby_2012_JTB_sapwood_model. Daniel Falster, Will Cornwell. figshare.
	http://dx.doi.org/10.6084/m9.figshare.1005160
```

## Further details

Please note:

- This code was archived post-publication, so there is no link to the code at the journal's website.
- Code was written between 2006-2010 by Daniel Falster and William Cornwell.
- The code included in this release has been slightly restructured to improve readability and code quality. The version run for publication is available [here](https://github.com/dfalster/Westoby_2012_JTB_sapwood_model/commit/20215f539f78f7ecc73635e41844032cdea6c92b).

## Running via Docker

If you have Docker installed, you can recreate the computing environment as follows in the terminal. 

From the directory you'd like this repo saved in, clone the repository:

```
git clone https://github.com/dfalster/Westoby_2012_JTB_sapwood_model.git
```

Then fetch the container:

```
docker pull traitecoevo/westoby_2012_jtb_sapwood_model
```

Navigate to the downloaded repo, then launch the container using the following code (it will map your current working directory inside the docker container): 

```
docker run --user root -v $(pwd):/home/rstudio/ -p 8787:8787 -e DISABLE_AUTH=true traitecoevo/westoby_2012_jtb_sapwood_model
```

The code above initialises a docker container, which runs an RStudio session accessed by pointing your browser to [localhost:8787](http://localhost:8787). For more instructions on running docker, see the info from [rocker](https://hub.docker.com/r/rocker/rstudio).

### NOTE: Building the docker image

For posterity, the docker image was built off [`rocker/verse:3.6.1` container](https://hub.docker.com/r/rocker/verse) via the following command, in a terminal contained within the downloaded repo:

```
docker build -t traitecoevo/westoby_2012_jtb_sapwood_model .
```

and was then pushed to [dockerhub](https://cloud.docker.com/u/traitecoevo/repository/docker/traitecoevo/westoby_2012_jtb_sapwood_model). The image used by binder builds off this container, adding extra features needed by binder, as described in [rocker/binder](https://hub.docker.com/r/rocker/binder/dockerfile).


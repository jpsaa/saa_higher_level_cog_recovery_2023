# Trajectory and factors impacting higher-level cognitive recovery in the first year after stroke

This repository contains code needed to reproduce the article:

**Saa JP, Koh G, Yap P, Tse T, Baum C, Uribe DE, Windecker SM, Ma H, Davis S, Donnan G, Carey LM**. "Mapping higher-level cognition in the first year
post-stroke across countries", (submitted to *NNR*).

Badge to pull data (non-functional; will be added some time in the future when data becomes public). 
[![DOI](https://zenodo.org/badge/11128/RemkoDuursma/baadanalysis.svg)](https://zenodo.org/badge/latestdoi/11128/RemkoDuursma/baadanalysis)

## Running the code

All analyses were done in `R`. All code needed to reproduce the submitted manuscript is included in this repository. To reproduce the results in this study, run the code contained in the `analysis.R` file. Figures, tables, and other results will be output to respective directories. Supplementary materials can be found in the directory `ms`.

If you are reproducing these results on your own machine, first download the code and then install the required packages listed in the `DESCRIPTION` file. This can be achieved by opening the Rstudio project and running:

```{r}
#install.packages("remotes")
remotes::install_deps()
```

You can access an interactive RStudio session with the required software pre-installed by opening a container hosted by [Binder](http://mybinder.org): 

[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jpsaa/saa_higher_level_cog_recovery_2023/master?urlpath=rstudio)

To ensure long-term [computational reproducibility](https://www.britishecologicalsociety.org/wp-content/uploads/2017/12/guide-to-reproducible-code.pdf) of this work, we have created a [Docker](http://dockerhub.com) image to enable others to reproduce these results on their local machines using identical software (and versions) we used to conduct the original analysis. Instructions for reproducing this work using the docker image are available at the bottom of this page. 

## Material included in the repository include:

- `data/`: Raw data
- `R/`: directory containing functions used in analysis
- `ms/`: directory containing manuscript 
- `figures/`, `tables/`, and `results/`: directories containing outputs of analysis 
- `DESCRIPTION`: A machine-readable compendium file containing key metadata and dependencies
- `LICENSE`: License for the materials
- `Dockerfile` & `.binder/Dockerfile`: files used to generate docker containers for long-term reproducibility

## Running via Docker

If you have Docker installed, you can recreate the computing environment as follows in the terminal. 

From the directory you would like this repository saved in, clone the repository:

```
git clone https://github.com/jpsaa/saa_higher_level_cog_recovery_2023.git
```

Then fetch the container:

```
docker pull jpsaa/saa_higher_level_cog_recovery_2023
```

Navigate to the downloaded repo, then launch the container using the following code (it will map your current working directory inside the docker container): 

```
docker run --user root -v $(pwd):/home/rstudio/ -p 8787:8787 -e DISABLE_AUTH=true jpsaa/saa_higher_level_cog_recovery_2023
```

The code above initialises a docker container, which runs an RStudio session accessed by pointing your browser to [localhost:8787](http://localhost:8787). For more instructions on running docker, see detailed info from [rocker](https://hub.docker.com/r/rocker/rstudio).

### NOTE: Building the docker image

For posterity, the docker image was built off [`rocker/verse:3.5.1` container](https://hub.docker.com/r/rocker/verse) via the following command, in a terminal contained within the downloaded repo:

```
docker build -t jpsaa/saa_higher_level_cog_recovery_2023.
```

and was then pushed to [dockerhub](https://cloud.docker.com/u/smwindecker/repository/docker/jpsaa/saa_higher_level_cog_recovery_2020). The image used by binder builds off this container, adding extra features needed by binder, as described in [rocker/binder](https://hub.docker.com/r/rocker/binder/dockerfile).


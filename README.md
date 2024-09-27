# Handling Missingness, Failures, and Non-Convergence in Simulation Studies: A Review of Current Practices and Recommendations

This repository contains data, code, and output related to the paper

> Pawel, S., Bartoš, F., Siepe, B. S., Lohmann, A. (2024). Handling Missingness, Failures, and Non-Convergence in Simulation Studies: A Review of Current Practices and Recommendations. <https://github.com/SamCH93/missSim>

  
A BibTeX entry is given by

```BibTeX
@article{Pawel2024,
  year = {2024},
  author = {Samuel Pawel and Franti{\v{s}}ek Barto{\v{s}} and Bj{\"o}rn S. Siepe and Anna Lohmann},
  title = {Handling Missingness, Failures, and Non-Convergence in Simulation Studies: A Review of Current Practices and Recommendations},
  url = {https://github.com/SamCH93/missSim},
  note = {Preprint}
}
```

The preregistered protocol of our literature review can be found at
<https://doi.org/10.17605/OSF.IO/PMV2J>

## Reproducing our results

To reproduce the case study, refer to the files in `case-study/`

* `case-study/carter2019-data-cleaning.R` R script to clean the
  `res.wide.red.RData` data from Carter et al. (2019,
  <https://doi.org/10.1177/2515245919847196>) and create
  `case-study/carter2019.rda`. The file `res.wide.red.RData` is too large for a
  GitHub repository. If you want to reproduce this step, clone
  <https://github.com/nicebread/meta-showdown> and run the R script
  `3-resultsFramework.R` to create `res.wide.red.RData` (Warning: This
  repository is several GBs in size!). However, this step is not necessary for
  reproducing the rest of our analysis as we also provide the cleaned summary
  data
* `case-study/carter2019.rda` cleaned summary data in rda format (required for
  analysis)
* `case-study/carter2019-analysis.Qmd` quarto file containing R code for case
  study analysis
* `case-study/carter2019-analysis.html` case study analysis output containing
  also information on computational environment (OS, R, R packages) used to run
  analysis
* `case-study/figures/` figure outputs used in the paper (these will be
  overwritten when rerunning the code)


To reproduce the literature review analyses, refer to the files in
`literature-review/` and `data/`

* `literature-review/data_cleaning.R` R script to clean and merge the literature
  review data from the four coders. Note that the data from the four coders are
  not provided in our repository due to them containing sensitive comments
  intended solely for discussions among the coders. However, running this script
  is not necessary to reproduce the rest of our analysis, as we also provide the
  cleaned literature review data.
* `data/` contains cleaned literature review data files in RDS and xlsx formats
  (required for analysis)
* `literature-review/analysis.Qmd` quarto file containing R code for literature
  review analysis
* `literature-review/analysis.html` literature review analysis analysis output
  containing also information on computational environment (OS, R, R packages)
  used to run analysis
* `literature-review/figures/` figure outputs used in the paper (these will be
  overwritten when rerunning the code)
* `literature-review/coding-agreement/` quarto files for agreement randomization
  and analysis
  
To reproduce our analyses using a Docker container that encapsulates the
computational environment used, refer to the files

* `Dockerfile` Dockerfile to recreate the computational environment used in the
  simulation study
* `Makefile` Makefile to conveniently build and run the Docker analysis: Make
  sure to have Docker and Make installed, then run `make docker` from the root
  directory of this git repository. This will install all necessary
  dependencies. RStudio Server can then be opened from a browser
  (<http://localhost:8787>), and the R and quarto files in `case-study/` and
  `literature-review/` can be rerun

Note that the Docker analysis only recreates the environment for running our
analyses, not the environment for running the `3-resultsFramework.R` script from
Carter et al. (2019). Refer to their GitHub repository
(<https://github.com/nicebread/meta-showdown>) for details on their
computational environment.


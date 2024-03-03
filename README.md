osacc: Accountants' Attitudes towards Open Science
================

This is the repo for the in-development study

> Gassen, Joachim, Jacqueline Klug and Victor van Pelt (2024): Accountants' Attitutes Towards Open Science.

This repository contains all the data and code to reproduce our work. In addition, we include the [oTree](https://www.otree.org) research materials that we used to conduct the survey and the experiment embedded in it.

If you are interested in reading the study, you need to build it ;-) Hang on.


### How do I create the output?

This video tutorial shows how to implement option 1 from below and reproduce our analysis in less than ten minutes. 

<p>&nbsp;</p>
<p align="center">
<a href="https://www.youtube.com/watch?v=rGf-gJgkZ38">
<img src="https://img.youtube.com/vi/rGf-gJgkZ38/mqdefault.jpg" alt="From Zero to Reproduction in Less than 10 Minutes!">
</a>
</p>
<p>&nbsp;</p>


You have three options: 

1. GitHub codespaces (potentially the easiest): 

If you have a GitHub account (it is free), you should be able to use [GitHub codespaces](https://github.com/features/codespaces). In this case: Click on "Code" (the green button) above this text, then on the "Codepsaces" tab, then on "create a codespace on main." Wait patiently. Eventually, Visual Studio Code will open in your browser. Run 'make all' in the terminal window. Wait again and continue reading down below.

2. Local Visual Studio Code and Devcontainer (for the pros): 

If you have [docker](https://www.docker.com) and [visual studio code](https://code.visualstudio.com) installed on your system, you should be able to reproduce our analysis by spinning up the development container included in the repository and running `make all` in the terminal (see below).

3. For R users:

Alternatively, if you have RStudio installed, the following process should enable you to reproduce our output locally.

- You will need make/Rtools: For Windows: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/). For MacOS: You need to install the Mac OS developer tools. Open a terminal and run `xcode-select --install`. Follow the instructions. For Linux: We have never seen a Unix environment without 'make'. 
- Download, clone or fork this repository to your local computing environment.
- Create an R Studio project in the folder.
- Before building everything you most likely need to install additional packages. See the code below for installing the packages.
- Run 'make all' either via the terminal or by identifying the 'Build All' button in the 'Build' tab (normally in the upper right quadrant of the RStudio screen). 

Regardless which route you take, after running `make all`, eventually, you should be greeted with several files in the `data/generated` folder. The current version of the paper and a presentation slide deck should be available in the `output` folder. Congratulations! 


### Libraries required to run the code

The code has been developed based on R 4.2 and packages as of around Feb 2024. Out of courtesy towards your local computing environment, the code does not install any packages automatically. If you want to make sure that you have all packages installed before running the code, you can run the snippet below.

```
install_package_if_missing <- function(pkg) {
  if (! pkg %in% installed.packages()[, "Package"]) install.packages(pkg)
}
install_package_if_missing("tidyverse")
install_package_if_missing("lubridate")
install_package_if_missing("rmarkdown")
install_package_if_missing("knitr")
install_package_if_missing("kableExtra")
install_package_if_missing("quarto")
install_package_if_missing("modelsummary")
install_package_if_missing("ggbeeswarm")
install_package_if_missing("devtools")
install_package_if_missing("xml2")
install_package_if_missing("countrycode")
install_package_if_missing("gt")
install_package_if_missing("showtext")

# In addition, if you have no working LaTeX environment, consider
# installing the neat tinytex LateX distribution. It is lightweight and
# you can install it from within R! See https://yihui.org/tinytex/
# To install it, run from the R console:

install_package_if_missing('tinytex')
tinytex::install_tinytex()
```

### Thoughts about the study?

If you have reproduced and even read our study, we would be super interested in hearing your views. Besides reaching out to us via email, you could also start a public discussion by opening up a GitHub Issue here in this repository. Be nice though, as this is a really fresh, rough, and early stage draft that you are reading ;-)


### Disclaimer

This project has been developed jointly with the [Open Science Data Center](https://www.accounting-for-transparency.de/projects/open-science-data-center/) of the [TRR 266 "Accounting for Transparency"](https://www.accounting-for-transparency.de). 

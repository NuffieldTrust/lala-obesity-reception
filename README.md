# Childhood obesity: is where you live important?

<b>lala-obesity-reception: Local area linked analysis of obese and overweight reception children</b>

<b>Project status: complete</b>

## Project description
This project looked at which local area characteristics are associated with the prevalence of obese or overweight reception children. It used data at the upper tier local authority level to look at the association between childhood obesity and local area factors using linear regression models. 

* The final report can be found [here](https://www.nuffieldtrust.org.uk/research/childhood-obesity-is-where-you-live-important) 
* The code is available in [`r/`](https://github.com/NuffieldTrust/lala-obesity-reception/tree/main/r)

## Data sources

The childhood obesity data was sourced from the [National Child Measurement Programme](https://digital.nhs.uk/data-and-information/publications/statistical/national-child-measurement-programme).
The local area characteristics were sourced from a variety of different places including governement websites, see the [technical annex](https://www.nuffieldtrust.org.uk/research/childhood-obesity-is-where-you-live-important) for details. 
The map shapefile was sourced from [ONS Geoportal](https://geoportal.statistics.gov.uk/maps/counties-and-unitary-authorities-december-2017-ew-bfe).

<i>NOTE: For some local area characteristics the Hospital Episode Statistics (© NHS Digital 2022) were used.</i>

## Requirements
The code was written in R using version 3.6.2. The following packages are needed:
* tidyverse
* tidylog
* janitor
* psych
* tidytext
* ggforce 
* rgdal
* sf
* cowplot
* writexl
* ggpubr
* pacman
* fs
* plyr
* RColorBrewer
* lm.beta
* lmtest
* ciTools
* MASS
* mctest
* relaimpo

## Usage
* [01_requirements.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/01_requirements.R): Set up needed to run all code in the project - <b>run once per session</b>
* [02_load_data.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/02_load_data.R): Check for, import and set up datafiles - <b>run once per session</b>
* [03_describe_dependent_national.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/03_describe_dependent_national.R): Distribution of overweight and obese reception children geographically and over time
* [04_describe_dependent_local.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/04_describe_dependent_local.R): Distribution of overweight and obese reception children by local authority and over time
* [05_describe_independent_local.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/05_describe_independent_local.R): Summarise independent variables at local authority level
* [06_correlation_dependent_independent.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/06_correlation_dependent_independent.R): Check the relationship between overweight and obese reception children values and independent variables
* [07_simple_linear_regression.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/07_simple_linear_regression.R): Unadjusted linear regression models of each independent variable indivdually with overweight and obese values
* [08_blocks_multiple_linear_regression.R](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/r/08_blocks_multiple_linear_regression.R): Adjusted linear regression models based on variable themes and then final model

## Code authors
Eilís Keeble - [Twitter](https://twitter.com/eiliskeeble) - [GitHub](https://github.com/eiliskeeble)

## License
This project is licensed under the [MIT License](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/LICENSE).

## Suggested citation
Fisher E, Keeble E, Paddison C, Cheung R & Hargreaves D (2022) Childhood obesity: is where you live important?. Research report, Nuffield Trust

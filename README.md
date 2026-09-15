# ePROM\_model\_v2



### Cost-Effectiveness Simulation Model for Complex Digital Interventions Monitoring Electronic Patient-Reported Outcomes (ePROMs) in Cancer Care



This project contains code for running an interactive R Shiny application containing a Health Economic (HE) decision analytic model designed to simulate the cost-effectiveness of ePROM-monitoring interventions in oncology and palliative care.



#### Description



This project contains the R shiny script for the interactive model web app, accessible at: https://01a09102-7c5b-5a70-39ee-aa21ed789c55.share.connect.posit.cloud/ 



#### Dependencies



This programme runs in R 4.4.1. R libraries must be installed before running the scripts using the following command: 

```

install.packages(c('plumber', 'swagger', 'plotly', 'ggplot2', 'scales', 'jsonlite', 'httr', 

&#x20;  'shinyfullscreen', 'shinyWidgets', 'shinyBS', 'shinycssloaders', 'progressr', 

&#x20;  'shiny.fluent', 'purrr', 'bslib', 'shinythemes', 'progress', 'data.table', 

&#x20;  'stringr', 'dplyr', 'tidyr', 'shiny', 'stats', 'graphics', 'grDevices', 

&#x20;  'utils', 'datasets', 'methods', 'arrayhelpers', 'matrixStats', 'car'))

```



### Installing



To install, download the files as a zip folder.



#### Executing programme locally



The R scripts and data files are self-contained, i.e., the programmes can be run from the folder using the following steps:

1\. Run `app.R`.

4\. The web app will open in the Shiny viewer.



### Help



For help \& advice, please contact the authors.



### Authors



Marek Atter<br />

Paul Mitchell



#### Version History

* 0.4 

  * New GitHub project
  * Hosted on Posit Cloud
  * Simplified scripts into a single R script
* 0.3

  * Previously at: https://github.com/matter-edi/ePROM-model
  * Separate `plumbr` code into two separate scripts: `plumber.R`, `server.R` 
* 0.2

  * Various bug fixes and optimizations
  * See \[commit change]() or See \[release history]()
* 0.1

  * Initial Release







### Acknowledgments



Project supervisors:

Prof Peter Hall

Prof Katrin Cresswell




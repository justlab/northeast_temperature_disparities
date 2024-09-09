About:
This repository holds the simulation and analysis code for the research project "Residential segregation and summertime air temperature across 13 northeastern U.S. states: potential implications for energy burden": Daniel Carrión _et al_ 2024 Environ. Res. Lett. 19 084005 
https://doi.org/10.1088/1748-9326/ad5b77 

...

Installation:
Make sure you have R version 4.x installed, then download or clone this repository to a directory on your computer.

Running the code:
To replicate this analysis, start an R or RStudio session in the directory where you've downloaded this repository. In the R console, run:

```
library(targets)
tar_make()
```
And it will begin the workflow provided in the `_targets.R` file. You can cancel the run at any time, and any completed targets will be skipped the next time you run `tar_make()`.

When the workflow is finished, it will place figures and tables in the `data/` folder.

The `targets` package gives many ways to examine your run of the workflow. For a useful flowchart showing status and execution time, try: `tar_visnetwork(targets_only = TRUE, label = 'time')`.

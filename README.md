# pmcfigurebot
Query figures indexed by PubMed Central and collect metadata

## PubMed Image Query
The GitHub Action is configured to run `scripts/fetch_figures_v2.R` on demand and
quarterly in order to query PMC for any new
figures published since the last run. The script downloads .jpg files and 
prepares a .yml file with metadata for each figure and its parent paper.

## Request Your Own Figure
Open an issue using the `Fetch this figure` providing a PMCID and a figure number. This will trigger the repo to collect this figure and add it to the queue for the next run of the PFOCR pipeline.

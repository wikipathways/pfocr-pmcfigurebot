This folder holds the resulting .jpg and .yml files from a PMC figure query. New files will be added by consecutive runs, but the log file is overwritten.

You can also manually deposit pairs of files, following these guidelines. They will be processed in the next batch.

1. **Image file.** This must be jpeg format with .jpg extension. The filename must include the PMCID and figure filename separated by double underscore, e.g., PMC5541263__41388_2017_Article_BFonc2016467_Fig8_HTML.jpg.
2. **Metadata file.** This must be yaml format with .yml extension, and with the same filename as the associated image file, e.g., PMC5541263__41388_2017_Article_BFonc2016467_Fig8_HTML.yml. The file should include the following fields and syntax (including start and end lines with triple dashes). Note: keywords should always be formated a list (like below).
```
---
figid: PMC5541263__41388_2017_Article_BFonc2016467_Fig6_HTML
pmcid: PMC5541263
image_filename: 41388_2017_Article_BFonc2016467_Fig6_HTML.jpg
figure_link: /pmc/articles/PMC5541263/figure/Fig6/
number: Figure 6
figure_title: ''
caption: 'ATRA directs breast cancer cell state changes and decreases breast CSCs
  through suppression of PKCG. Schematic showing CSC division patterns: AD, SD
  or SC patterns.'
article_title: Retinoic acid directs breast cancer cell state changes through regulation
  of TET2-PKCG pathway.
citation: M-J Wu, et al. Oncogene. 2017 Jun 1;36(22):3193-3206.
year: '2017'
doi: 10.1038/onc.2016.467
journal_title: Oncogene
journal_nlm_ta: Oncogene
publisher_name: Nature Publishing Group UK
keywords:
- Breast cancer
---
```

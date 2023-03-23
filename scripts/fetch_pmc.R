## fetch figures and metadata from PMC

## NOTE: query qualifier for figure captions [CAPT] is clearly broken and only hits on a fraction of caption titles.
##  the "imagesdocsum" report type does a better job of actually searching captions, e.g.:
# https://www.ncbi.nlm.nih.gov/pmc/?term=(signaling+pathway)+AND+(2019+[pdat])&report=imagesdocsum&dispmax=100 
## (11349 hits with "signaling pathway" in every caption title or caption body)
# https://www.ncbi.nlm.nih.gov/pmc/?term=(signaling+pathway[CAPT])+AND+(2019+[pdat])&report=imagesdocsum&dispmax=100
## (244 hits with "signaling pathway" ONLY in caption titles)
# https://www.ncbi.nlm.nih.gov/pmc/?term=(signaling+pathway[CAPT])+AND+(2019+[pdat])
## (2775 hits when "report=imagesdocsum" is excluded)

## NOTE: the imagesdocsum" report is not supported by NCBI's eutils, so we'll have to go with HTML scraping. 
##  The pagination of pmc output is not apparent, however...

## Example queries for what is possible
# https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=asthma[mesh]+AND+leukotrienes[mesh]+AND+2009[pdat]&usehistory=y&retmax=500&retStart=0
# https://www.ncbi.nlm.nih.gov/pmc/?term=signaling+pathway+AND+2018+[pdat]&report=imagesdocsum&dispmax=100
# https://www.ncbi.nlm.nih.gov/pmc/?term=((((((((((signaling+pathway)+OR+regulatory+pathway)+OR+disease+pathway)+OR+drug+pathway)+OR+metabolic+pathway)+OR+biosynthetic+pathway)+OR+synthesis+pathway)+OR+cancer+pathway)+OR+response+pathway)+OR+cycle+pathway)+AND+(\%222019/01/01\%22[PUBDATE]+%3A+\%223000\%22[PUBDATE])&report=imagesdocsum&dispmax=100#
## Network query:
# https://www.ncbi.nlm.nih.gov/pmc/?term=((network)+OR+PPI)+AND+(%222019/01/01%22[PUBDATE]+%3A+%223000%22[PUBDATE])&report=imagesdocsum&dispmax=100
## WikiPathways social media post:
# https://www.ncbi.nlm.nih.gov/pmc/?term=(wikipathways+OR+pathvisio)+AND+(%222022/10/01%22[PUBDATE]+%3A+%223000/01/01%22[PUBDATE])&report=imagesdocsum&dispmax=100

library(utils)
library(rvest)
library(xml2)
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)
library(yaml)
library(httr)
library(jpeg)
library(lubridate) #for -months() operation

###############
## BUILD QUERY 
###############

config <- yaml::read_yaml("query_config.yml")
#terms
query.terms <- gsub(" ", "-", config$terms) #dash indicates phrases
if (length(query.terms) > 1){
  query.terms <- paste(query.terms, collapse = "+")
}
#date
query.date <- ""
if (is.null(config$date_range)){
  if (is.null(config$last_run)){
    from.date <- format(Sys.Date() - months(1), "%Y/%m/%d")
    
  } else {
    from.date <- config$last_run
  }
  query.date <- paste0(from.date,"[PUBDATE]+%3A+3000/01/01[PUBDATE]")
} else {
  query.date <- config$date_range
  if (length(query.date) > 1){
    query.date <- paste(query.date, collapse = "[PUBDATE]+%3A+")
    query.date <- paste0(query.date , "[PUBDATE]")
  }
}

term.arg <- paste0("term=(",query.terms,")+AND+(",query.date,")")

query.url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/?",
                    term.arg,
                    "&report=imagesdocsum",
                    "&dispmax=100")

##############
## SCRAPE PMC 
##############

cat(query.url, file="figures/fetch.log")

## Parse page
page.source <- xml2::read_html(query.url) 
image_filename <- page.source %>%
  rvest::html_nodes(".rprt_img") %>%
  rvest::html_node("img") %>%
  rvest::html_attr("src-large") %>%
  stringr::str_match("bin/(.*\\.jpg)") %>%
  as.data.frame() %>%
  dplyr::select(2) %>%
  as.matrix() %>%
  as.character()

## log last_run
config$last_run <- format(Sys.Date(), "%Y/%m/%d")
yaml::write_yaml(config, "query_config.yml")

## check for results
if(!length(image_filename) > 0){
  cat("\n0 results", file="figures/fetch.log", append = T)
} else {
  titles <- page.source %>%
    rvest::html_nodes(".rprt_img") %>%
    rvest::html_node(xpath='..') %>%
    rvest::html_node(".rprt_cont") %>%
    rvest::html_node(".title") %>%
    rvest::html_text() %>%
    stringr::str_split("\\s+From: ", simplify = TRUE)
  article_title <- titles[,2] %>% 
    stringr::str_trim()
  number <- page.source %>%
    rvest::html_nodes(".rprt_img") %>%
    rvest::html_node("img") %>%
    rvest::html_attr("alt")
  caption <- page.source %>%
    rvest::html_nodes(".rprt_img") %>%
    rvest::html_node(xpath = "..") %>%
    rvest::html_node(".rprt_cont") %>%
    rvest::html_node(".supp") %>%
    rvest::html_text()
  figure_link <- page.source %>%
    rvest::html_nodes(".rprt_img") %>%
    rvest::html_attr("image-link")
  citation <- page.source %>%
    rvest::html_nodes(".rprt_img") %>%
    rvest::html_node(xpath='..') %>%
    rvest::html_node(".rprt_cont") %>%
    rvest::html_node(".aux") %>%
    rvest::html_text() %>%
    stringr::str_remove(fixed("CitationFull text"))
  pmcid <- page.source %>%
    rvest::html_nodes(".rprt_img") %>%
    rvest::html_node(xpath='..') %>%
    rvest::html_node(".rprt_cont") %>%
    rvest::html_node(".title") %>%
    rvest::html_node("a") %>%
    rvest::html_attr("href") %>%
    stringr::str_match("PMC\\d+") %>%
    as.character()
  
  ## Extract best figure title from analysis of provided figure number, title and caption
  temp.df <- data.frame(n = number, t = titles[, 1], c = caption, stringsAsFactors = FALSE) %>%
    mutate(t = str_trim(str_remove(
      t, fixed(
        as.character(
          if_else(
            number != "",
            number,
            "a string just to suppress the empty search patterns warning message"
          )
        )
      )
    ))) %>%
    mutate(t = str_trim(str_remove(
      t,
      "\\.$"
    ))) %>%
    mutate(t = if_else(!is.na(str_match(t,"^\\. .*")[,1]),
                       str_remove(t, "^\\. "), 
                       t)) %>%
    mutate(c = str_trim(str_replace(
      c,
      "\\.\\.", "\\."
    ))) %>%
    mutate(c = if_else(is.na(c), t, c)) %>%
    mutate(t = str_trim(str_remove(
      t,
      "\\.+$"
    ))) %>%
    mutate(n = str_trim(str_replace(n, "\\.$", "")))
  number <- as.character(temp.df[, 1])
  figure_title <- as.character(temp.df[, 2])
  caption <- as.character(temp.df[, 3])
  
  ## Prepare df and write to R.object and tsv
  df <- data.frame(pmcid, image_filename, figure_link, number, figure_title, caption, article_title, citation) 
  df <- unique(df)
  
  ## Log run
  cat(paste("\n",nrow(df), "results"), file="figures/fetch.log", append = T)
  
  ## For each figure...
  for (a in 1:nrow(df)){
    #slice of df from above
    article.data <- df[a,]
    
    #################
    ## MORE METADATA
    #################
    md.query <- paste0("https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:",gsub("PMC","", article.data$pmcid),"&metadataPrefix=pmc_fm")
    md.source <- xml2::read_html(md.query) 
    doi <- md.source %>%
      rvest::html_node(xpath=".//article-id[contains(@pub-id-type, 'doi')]") %>%
      rvest::html_text()
    journal_title <- md.source %>%
      rvest::html_node(xpath=".//journal-title") %>%
      rvest::html_text()
    journal_nlm_ta <- md.source %>%
      rvest::html_node(xpath=".//journal-id[contains(@journal-id-type, 'nlm-ta')]") %>%
      rvest::html_text()
    journal_iso_abbrev <- md.source %>%
      rvest::html_node(xpath=".//journal-id[contains(@journal-id-type, 'iso-abbrev')]") %>%
      rvest::html_text()
    publisher_name <- md.source %>%
      rvest::html_node(xpath=".//publisher-name") %>%
      rvest::html_text()
    keywords <- md.source %>% 
      rvest::html_nodes(xpath=".//kwd") %>% 
      purrr::map(~rvest::html_text(.)) %>%
      unlist() %>% 
      unique()
    
    md.data <- data.frame(doi,journal_title, journal_nlm_ta, publisher_name)
    
    #################
    ## MAKE MEMORIES
    #################
    
    ## write yml
    fn <- paste(article.data$pmcid,
                gsub(".jpg$","",article.data$image_filename),
                sep = "__")
    yml.path = file.path('figures',paste(fn, "yml", sep = "."))
    write("---", yml.path, append = F)
    write(yaml::as.yaml(article.data), yml.path, append = T)
    write(yaml::as.yaml(md.data), yml.path, append = T)
    write("keywords:", yml.path, append = T)
    write(yaml::as.yaml(keywords), yml.path, append = T)
    write("---", yml.path, append = T)
    
    ## download image from PMC, politely
    img.from.path = paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/",
                           article.data$pmcid,
                           "/bin/",article.data$image_filename)
    img.to.path = file.path('figures',paste(fn, "jpg", sep = "."))
    headers = c(
      `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
    )
    res <- httr::GET(url = img.from.path, httr::add_headers(.headers=headers))
    jpg <- jpeg::readJPEG(res$content)
    jpeg::writeJPEG(jpg, img.to.path)
  }
}

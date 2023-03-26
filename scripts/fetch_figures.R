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
library(purrr)
library(yaml)
library(httr)
library(jpeg)
library(lubridate) #for -months() operation
library(RSelenium)
library(dplyr)
library(magrittr)
library(stringr)

# Set up the remote web driver using Selenium/standalone-firefox
remDr <- remoteDriver(
  remoteServerAddr = "localhost", 
  port = 4445L
)
cat("Attempting to open remoteDriver\n")
remDr$open()

###############
## BUILD QUERY 
###############
cat("Building query\n")
config <- yaml::read_yaml("query_config.yml")
#terms
query.terms <- gsub(" ", "-", config$terms) #dash indicates phrases
if (length(query.terms) > 1){
  query.terms <- paste(query.terms, collapse = "+")
}
#date
query.date <- ""
from.date <- format(Sys.Date() - months(1), "%Y/%m/%d")
to.date <- "3000/01/01" #always try to get latest; also, end dates can return odd results
if (is.null(config$date_range)){
  if (!is.null(config$last_run)){
    from.date <- config$last_run
    if (as.Date(from.date) + months(1) > Sys.Date()) #repeat fetch including prior month
      from.date <- format(as.Date(from.date) - months(1), "%Y/%m/%d")
  }
  query.date <- paste0(from.date,"[PUBDATE]+%3A+",to.date,"[PUBDATE]")
} else {
  query.date <- config$date_range
  if (length(query.date) == 2){
    query.date <- paste(query.date, collapse = "[PUBDATE]+%3A+")
    query.date <- paste0(query.date , "[PUBDATE]")
  }
  #extract from.date to be used later
  from.date <- stringr::str_extract(query.date, "\\d{4}/\\d{2}/\\d{2}")
}
term.arg <- paste0("term=(",query.terms,")+AND+(",query.date,")")
query.url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/?",
                    term.arg,
                    "&report=imagesdocsum",
                    "&dispmax=100")
# log it
cat(query.url)
cat(query.url, file="figures/fetch.log")
cat(paste("\n", query.date), file="figures/fetch.log", append = T)

##############
## SCRAPE PMC 
##############

## go to page
remDr$navigate(query.url)

## get page count
page.count <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes(".title_and_pager") %>%
  rvest::html_node(".pagination") %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("page")
page.count <- as.integer(page.count[4])

cat(paste("\n",page.count," pages of results"), file="figures/fetch.log", append = T)

exfigids <- read.table(config$exclude_figids, sep = "\t", stringsAsFactors = F)[,1]
res.fig.count <- 0

for (i in 1:page.count){
  cat(sprintf("\nPage %i of %i", i, page.count))
  cat(sprintf("\nPage %i of %i", i, page.count), file="figures/fetch.log", append = T)
  
  ## Parse page
  page.source <- xml2::read_html(remDr$getPageSource()[[1]])
  image_filename <- page.source %>%
    rvest::html_nodes(".rprt_img") %>%
    rvest::html_node("img") %>%
    rvest::html_attr("src-large") %>%
    stringr::str_match("bin/(.*\\.jpg)") %>%
    as.data.frame() %>%
    dplyr::select(2) %>%
    as.matrix() %>%
    as.character()
  
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
      stringr::str_remove(stringr::fixed("CitationFull text"))
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
      dplyr::mutate(t = str_trim(str_remove(
        t, stringr::fixed(
          as.character(
            if_else(
              number != "",
              number,
              "a string just to suppress the empty search patterns warning message"
            )
          )
        )
      ))) %>%
      dplyr::mutate(t = str_trim(str_remove(
        t,
        "\\.$"
      ))) %>%
      dplyr::mutate(t = if_else(!is.na(str_match(t,"^\\. .*")[,1]),
                         str_remove(t, "^\\. "), 
                         t)) %>%
      dplyr::mutate(c = str_trim(str_replace(
        c,
        "\\.\\.", "\\."
      ))) %>%
      dplyr::mutate(c = if_else(is.na(c), t, c)) %>%
      dplyr::mutate(t = str_trim(str_remove(
        t,
        "\\.+$"
      ))) %>%
      dplyr::mutate(n = str_trim(str_replace(n, "\\.$", "")))
    number <- as.character(temp.df[, 1])
    figure_title <- as.character(temp.df[, 2])
    caption <- as.character(temp.df[, 3])
    
    ## Construct a few more fields
    figid <- paste(pmcid, sub(".jpg$", "", image_filename), sep = "__")
    year <- rep(substr(from.date,1,4), length(image_filename)) # if not in citation
    cit.year <- stringr::str_extract(citation, "(?<=\\s)\\d{4}") # try citation
    year <- ifelse(!is.na(cit.year), cit.year, year) # keep citation if found

    ## Prepare df and write to R.object and tsv
    df <- data.frame(figid, pmcid, image_filename, figure_link, number, figure_title, caption, article_title, citation, year) 
    df <- unique(df)
    
    page.fig.count <- 0
    
    ## For each figure...
    for (a in 1:nrow(df)){
      # check exclude list
      if (df[a,"figid"] %in% exfigids)
        next
      cat(sprintf("\nFigure %i of %i (%i)", a, nrow(df),res.fig.count+1))
        
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
        unique() %>%
        trimws()
      
      md.data <- data.frame(doi,journal_title, journal_nlm_ta, publisher_name) %>%
        mutate_all(~if_else(is.na(.), "", as.character(.)))      
      
      #################
      ## MAKE MEMORIES
      #################
      
      ## write yml
      yml.path = file.path('figures',paste(article.data$figid, "yml", sep = "."))
      write("---", yml.path, append = F)
      write(yaml::as.yaml(article.data), yml.path, append = T)
      write(yaml::as.yaml(md.data), yml.path, append = T)
      write("keywords:", yml.path, append = T)
      if(length(keywords)>1){ # as.yaml makes list
        write(yaml::as.yaml(keywords), yml.path, append = T)
      } else if (length(keywords)==1) { # manually make list of one
        write(paste("-",yaml::as.yaml(keywords)), yml.path, append = T)
      } else { #leave empty
      }
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
      content_type <- headers(res)$`Content-Type`
      if (content_type == "image/jpeg"){
        jpg <- jpeg::readJPEG(res$content)
        jpeg::writeJPEG(jpg, img.to.path)
      } else {
        #cat("\nNo image.")
      }
      
      #record pmicd
      exfigids.add <- c(exfigids,article.data$figid)
      
      #increment counters
      page.fig.count <- page.fig.count+1
      res.fig.count <- res.fig.count+1
      
      #take a breath
      Sys.sleep(1)
      
    } # end for each figure
    
    ## Log results per page
    cat(sprintf("\n%i results (%i new figures)", nrow(df),page.fig.count), file="figures/fetch.log", append = T)
    
  } # end if results on page 
  
  # Turn the page
  if (i < page.count-1){
    next.page.button <- remDr$findElement(using = "xpath", "//*[@class='active page_link next']")
    next.page.button$clickElement()
    #remDr$screenshot(display = TRUE)
    Sys.sleep(3)
  }
} #end for each page

## Log final results 
cat(sprintf("\n%i new figures total",res.fig.count), file="figures/fetch.log", append = T)

## log last_run
config$last_run <- format(as.Date(from.date) + months(1), "%Y/%m/%d")
yaml::write_yaml(config, "query_config.yml")

## update figid exclude list
write.table(exfigids,config$exclude_figids, sep = "\t", row.names = F, col.names = F)

## Close up shop
remDr$close()

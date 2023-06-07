## fetch figures and metadata from PMC

## USAGE: Each month, GitHub Actions initiate a fresh search for pathway figures 
# from the prior 12 month period, collecting any that have not previously been 
# collected. This script is run each day (multiple times if necessary) in order
# to perform this collection, thus completing ~12 days into a given month. 

# NOTE: Figure files should be downloaded periodically to avoid repo bloat.

#-------------------------------------------------------------------------------
## DEV NOTES: query qualifier for figure captions [CAPT] is clearly broken and only hits on a fraction of caption titles.
##  the "imagesdocsum" report type does a better job of actually searching captions, e.g.:
# https://www.ncbi.nlm.nih.gov/pmc/?term=(signaling+pathway)+AND+(2019+[pdat])&report=imagesdocsum&dispmax=100 
## (11349 hits with "signaling pathway" in every caption title or caption body)
# https://www.ncbi.nlm.nih.gov/pmc/?term=(signaling+pathway[CAPT])+AND+(2019+[pdat])&report=imagesdocsum&dispmax=100
## (244 hits with "signaling pathway" ONLY in caption titles)
# https://www.ncbi.nlm.nih.gov/pmc/?term=(signaling+pathway[CAPT])+AND+(2019+[pdat])
## (2775 hits when "report=imagesdocsum" is excluded)

## DEV NOTES: the imagesdocsum" report is not supported by NCBI's eutils, so we'll have to go with HTML scraping. 
##  The pagination of pmc output is not apparent, however...

## DEV NOTES: Example queries for what is possible
# https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=asthma[mesh]+AND+leukotrienes[mesh]+AND+2009[pdat]&usehistory=y&retmax=500&retStart=0
# https://www.ncbi.nlm.nih.gov/pmc/?term=signaling+pathway+AND+2018+[pdat]&report=imagesdocsum&dispmax=100
# https://www.ncbi.nlm.nih.gov/pmc/?term=((((((((((signaling+pathway)+OR+regulatory+pathway)+OR+disease+pathway)+OR+drug+pathway)+OR+metabolic+pathway)+OR+biosynthetic+pathway)+OR+synthesis+pathway)+OR+cancer+pathway)+OR+response+pathway)+OR+cycle+pathway)+AND+(\%222019/01/01\%22[PUBDATE]+%3A+\%223000\%22[PUBDATE])&report=imagesdocsum&dispmax=100#
## Network query:
# https://www.ncbi.nlm.nih.gov/pmc/?term=((network)+OR+PPI)+AND+(%222019/01/01%22[PUBDATE]+%3A+%223000%22[PUBDATE])&report=imagesdocsum&dispmax=100
## WikiPathways social media post:
# https://www.ncbi.nlm.nih.gov/pmc/?term=(wikipathways+OR+pathvisio)+AND+(%222022/10/01%22[PUBDATE]+%3A+%223000/01/01%22[PUBDATE])&report=imagesdocsum&dispmax=100
#-------------------------------------------------------------------------------

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

# Keep track of time to stay under 6 hour limit per GH Action job.
# Exit at 5 hours and let subsequent runs complete a given query
job_start_time <- Sys.time()
exit_flag <- FALSE

# Multiple attempts of a given query or until successful
for (j in 1:5) {
  reload.flag <- FALSE
  
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
  from.date <- config$last_run
  if (as.Date(from.date) > Sys.Date()){
    cat(paste("\n\nNothing more to collect this month. See last_run date.\n"))
    cat(paste("\n\nNothing more to collect this month. See last_run date.\n"), file="figures/fetch.log", append = T)
    break 
  }
  to.date <- format(as.Date(from.date) + months(1), "%Y/%m/%d")
  if (as.Date(to.date) + months(3) > Sys.Date())
    to.date <- "3000/01/01" #near-future end dates behave oddly; this seems to work better
  query.date <- paste0(from.date,"[PUBDATE]+%3A+",to.date,"[PUBDATE]")
  term.arg <- paste0("term=(",query.terms,")+AND+(",query.date,")")
  query.url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/?",
                      term.arg,
                      "&report=imagesdocsum",
                      "&dispmax=100")
  # log it
  cat(query.url)
  cat(paste("\n\n", query.date,"\n"))
  cat(query.url, file="figures/fetch.log", append = T)
  cat(paste("\n\n", query.date,"\n"), file="figures/fetch.log", append = T)
  
  ##############
  ## SCRAPE PMC 
  ##############
  
  # Set up the remote web driver using selenium/standalone-firefox
  remDr <- remoteDriver(
    remoteServerAddr = "localhost", 
    port = 4445L
  )
  cat("Attempting to open remoteDriver\n")
  remDr$open()
  
  ## go to page
  tryCatch({
    remDr$navigate(query.url)
  }, error = function(e) {
    next #try again
  })
  
  ## get page count if more than 100
    item.count <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
      rvest::html_nodes(".title_and_pager") %>%
      rvest::html_node(".result_count") %>%
      rvest::html_text()
    item.count <- as.integer(sub("Items: ","",item.count[1]))
    
    page.count <- 1
    if (is.na(item.count))  {
      page.count <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
        rvest::html_nodes(".title_and_pager") %>%
        rvest::html_node(".pagination")
      if (is.na(page.count[1])){
        page.count <- 0
      } else {
        page.count <- page.count %>%
          rvest::html_nodes("a") %>%
          rvest::html_attr("page")
        page.count <- as.integer(page.count[4])
      }
    }
  
  cat(paste("\n",page.count," pages of results"), file="figures/fetch.log", append = T)
  
  if (page.count > 0){ # if any results
    
    exfigids <- read.table(config$exclude_figids, sep = "\t", stringsAsFactors = F)[,1]
    
    res.fig.count <- 0
    
    for (i in 1:page.count){
      cat(sprintf("\nPage %i of %i", i, page.count))
      cat(sprintf("\nPage %i of %i", i, page.count), file="figures/fetch.log", append = T)
      
      ## Parse page
      page.source <- NULL
      get.page.flag <- FALSE
      for (p in 1:5){
        page.source <- tryCatch({
          xml2::read_html(remDr$getPageSource()[[1]])
        }, error = function(e) {
          NULL
        })
        if (is.null(page.source) || inherits(page.source, "try-error")){
          Sys.sleep(3)
          next
        } else {
          get.page.flag <- TRUE
          break
        }
      }
      if(!get.page.flag){
        ## Exit out of page loop if page read fails
        reload.flag <- TRUE
        break
      }
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
        
        ## Log run
        cat(paste("\n",nrow(df), "results"), file="figures/fetch.log", append = T)
        
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
          
          md.source <- NULL
          get.md.flag <- FALSE
          for (m in 1:5){
            md.source <- tryCatch({
              xml2::read_html(md.query) 
            }, error = function(e) {
              NULL
            })
            if (is.null(md.source) || inherits(md.source, "try-error")){
              Sys.sleep(3)
              next
            } else {
              get.md.flag <- TRUE
              break
            }
          }
          if(!get.md.flag){
            ## Exit out of page loop if metadata read fails
            reload.flag <- TRUE
            break
          } 
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
          img.to.path = file.path('figures',paste(article.data$figid, "jpg", sep = "."))
          headers = c(
            `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
          )
          res <- NULL
          get.image.flag <- FALSE
          for (h in 1:5){
            res <- tryCatch({
              httr::GET(url = img.from.path, httr::add_headers(.headers=headers))
            }, error = function(e) {
              NULL
            })
            if (is.null(res) || httr::http_error(res)){
              Sys.sleep(3)
              next
            } else {
              get.image.flag <- TRUE
              break
            }
          }
          if(get.image.flag){
            content_type <- headers(res)$`Content-Type`
            if (content_type == "image/jpeg"){
              jpg <- jpeg::readJPEG(res$content)
              jpeg::writeJPEG(jpg, img.to.path)
            } else {
              cat("\nNo image.")
            }
          } else {
            cat("\nImage url failed.")
          }
          
          #record figid
          exfigids <- c(exfigids,article.data$figid)
          write.table(exfigids,config$exclude_figids, sep = "\t", row.names = F, col.names = F)
          
          #increment counters
          page.fig.count <- page.fig.count+1
          res.fig.count <- res.fig.count+1
          
          #take a breath
          Sys.sleep(1)
          
        } # end for each figure
        
        ## Log results per page
        cat(sprintf("\n%i results (%i new figures)", nrow(df),page.fig.count))
        cat(sprintf("\n%i results (%i new figures)", nrow(df),page.fig.count), file="figures/fetch.log", append = T)
        
      } # end if results on page 
      
      # Check time elapsed
      check_time <- difftime(Sys.time(), job_start_time, units = "hours")
      if (check_time >= 5) {
        message("Total runtime nearing 6 hour limit. Time to wrap things up!")
        exit_flag <- TRUE
        break
      }
      
      # Turn the page
      if (i < page.count-1){
        result <- NULL
        for (i in 1:15) {
          result <- tryCatch({
            remDr$findElement(using = "xpath", "//*[@class='active page_link next']")
          }, error = function(e) {
            NULL
          })
          if (!is.null(result)) {
            break  # exit for loop
          }
          Sys.sleep(1)
        }
        # check if successful over 15 seconds
        if (!is.null(result)) {
          next.page.button <- remDr$findElement(using = "xpath", "//*[@class='active page_link next']")
          next.page.button$clickElement()
          #remDr$screenshot(display = TRUE)
        } else {
          message("Page failed to load after 15 seconds. Reloading query results and trying again...")
          reload.flag <- TRUE
        }
      } #end page turn
      
      ## Exit out of page loop if page turn aborted
      if (reload.flag)
        break 
    } #end for each page
    
    ## Log totals for this round 
    cat(sprintf("\n\n%i new figures total\n",res.fig.count))
    cat(sprintf("\n\n%i new figures total\n",res.fig.count), file="figures/fetch.log", append = T)
    
    ## Exit if time is running out
    if (exit_flag)
      break
    
    ## Reload query if prior result aborted
    if (reload.flag)
      next 
    
  } # end if any results
  
  ## log last_run
  config$last_run <- to.date
  yaml::write_yaml(config, "query_config.yml")
  
  ## Close up shop
  remDr$close()
  
  ## If we get to the end, then exit retry loop
  break
  
} #end retry loop for a given query

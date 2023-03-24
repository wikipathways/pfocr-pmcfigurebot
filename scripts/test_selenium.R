## TEST

library(utils)
library(RSelenium)
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
library(netstat)

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
to.date <- "3000/01/01"
if (is.null(config$date_range)){
  if (is.null(config$last_run)){
    from.date <- format(Sys.Date() - months(1), "%Y/%m/%d")
  } else {
    from.date <- config$last_run
    if (as.Date(from.date) > Sys.Date())
      stop("Invalid date range. Trying to use last_run as start of range.")
    to.date <- format(as.Date(from.date) + months(1), "%Y/%m/%d")
  }
  query.date <- paste0(from.date,"[PUBDATE]+%3A+",to.date,"[PUBDATE]")
} else {
  query.date <- config$date_range
  if (length(query.date) == 2){
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

# launch Docker.app
# docker pull selenium/standalone-firefox
# docker run -d -p 4445:4444 selenium/standalone-firefox

# Start a Selenium server and open a browser
rD <- rsDriver(port = netstat::free_port())
remDr <- rD[["client"]]
remDr$open()
remDr$navigate(query.url)
remDr$screenshot(display = TRUE)

## Close up shop
remDr$closeall()



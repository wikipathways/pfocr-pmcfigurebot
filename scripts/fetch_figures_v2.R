# ============================================================================
# NCBI PMC Figure Fetcher
# ============================================================================
#
# This script searches the PubMed Central (PMC) database for articles 
# containing figure captions with specific keywords related to pathways. 
# It then downloads the relevant figures and their associated metadata.
#
# Key Features:
# 1. Searches PMC using the E-utilities API
# 2. Extracts figure information from XML content
# 3. Downloads figure images and saves associated metadata in YAML format
# 4. Implements retry logic for robust image downloading
# 5. Respects NCBI's rate limits and best practices
#
# Dependencies:
# - rentrez: for interacting with NCBI's E-utilities
# - xml2: for parsing XML content
# - httr: for making HTTP requests
# - yaml: for reading config and writing metadata
# - stringr: for string manipulation
# - dplyr: for data manipulation
# - lubridate: for date manipulation
#
# Usage:
# Run via GitHub Action at off-peak hours
#
# Output:
# - Downloaded images are saved in the "figures" directory
# - Each image has an associated YAML file with metadata
#
# Notes: E-utilities Usage Limits
# - 3 requests per second
# - Off-peak hours: 9 PM to 5 AM Eastern Time on weekdays or anytime on weekends
# - NCBI requires that all requests include a unique user-agent string that 
#   identifies your tool or application and includes contact information.
# ============================================================================

# Keep track of time to stay under 6 hour limit per GH Action job.
# Exit at 5 hours and let subsequent runs complete a given query
job_start_time <- Sys.time()
exit_flag <- FALSE

library(rentrez)
library(xml2)
library(httr)
library(yaml)
library(tools)
library(stringr)
library(dplyr)
library(lubridate)

# Logging function
log_message <- function(message, log_file = "figure_fetch.log") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0(timestamp, " - ", message, "\n")
  cat(log_entry)
  write(log_entry, log_file, append = TRUE)
}

# Initialize counters
total_figures_passed <- 0
new_figures_downloaded <- 0

# Read config file
read_config <- function(file_path = "query_config.yml") {
  config <- yaml.load_file(file_path)
  log_message(paste("Configuration loaded from", file_path))
  return(config)
}

# Prepare a date range to query
prepare_date_range <- function(config) {
  
  # If a date_range is provided, then we use it
  if (!is.null(config$date_range) && length(config$date_range) == 2) {
    return(config$date_range)
  }
  
  # Otherwise, we consider last_run
  if (!is.null(config$last_run)) {
    last_run_date <- ymd(config$last_run)
    current_date <- Sys.Date()
    
    if (last_run_date > current_date) {
      log_message("Neither date_range nor last_run indicate a valid date range.")
      return(NULL)
    } else {
      start_date <- format(last_run_date, "%Y/%m/%d")
      end_date <- format(last_run_date %m+% months(1), "%Y/%m/%d")
      return(list(start_date, end_date))
    }
  }
  
  log_message("Neither date_range nor last_run indicate a valid date range.")
  return(NULL)
}


# Function to split XML content at the end of article nodes and significantly
# reduce bloat by removing <body> and <back> content
safe_read_xml <- function(xml_content) {
  
  # Split the content into individual articles and remove scraps at the end
  xml_articles <- str_split(xml_content, "</article>")[[1]]
  xml_articles <- xml_articles[-length(xml_articles)]
  
  # Process each article
  processed_articles <- lapply(xml_articles, function(article) {
    tryCatch({
      # Make valid XML by adding back the closing </article> tag and
      # removing the isolated <pmc-articleset> tag
      article <- paste0(article, "</article>")
      article <- sub("<pmc-articleset>", "", article)
      
      # Read each article and remove <body> and <back> sections
      doc <- read_xml(article)
      xml_remove(xml_find_all(doc, "//body | //back"))
      
      return(doc)
    }, error = function(e) {
      warning("Error processing article: ", e$message)
      return(NULL)
    })
  })
  
  # Remove any NULL entries (failed processing)
  processed_articles <- processed_articles[!sapply(processed_articles, is.null)]
  
  return(processed_articles)
}

# Function to search PMC and fetch full text
# Update the search_pmc function to use the new query
search_pmc <- function(terms, date_range, retmax = 3000, max_attempts = 3, retry_delay = 3) {
  query <- paste(
    terms,
    sprintf('AND (%s[PUBDATE] : %s[PUBDATE])', date_range[1], date_range[2])
  )
  log_message(paste("PMC search query:", query))
  
  # Function to perform a search or fetch with retries
  perform_with_retry <- function(action, ...) {
    for (attempt in 1:max_attempts) {
      tryCatch({
        result <- action(...)
        return(result)  # If successful, return the result
      }, error = function(e) {
        if (attempt < max_attempts) {
          cat(sprintf("Attempt %d failed: %s. Retrying in %d seconds...\n", 
                      attempt, e$message, retry_delay))
          Sys.sleep(retry_delay)
        } else {
          cat(sprintf("All %d attempts failed. Last error: %s\n", 
                      max_attempts, e$message))
          stop(e)  # Re-throw the last error if all attempts fail
        }
      })
    }
  }
  
  # Search PMC with retries
  search_result <- perform_with_retry(entrez_search, 
                                      db = "pmc", 
                                      term = query, 
                                      retmax = retmax, 
                                      use_history = TRUE)
  
  # Fetch full text with retries
  fetch_result <- perform_with_retry(entrez_fetch,
                                     db = "pmc", 
                                     web_history = search_result$web_history, 
                                     rettype = "xml", 
                                     retmax = retmax)
  
  log_message(paste("PMC search completed. Fetched", 
                    min(retmax,search_result$count), 
                    "results (max set to",
                    paste0(retmax,").")))
  
  return(fetch_result)
}

# Function to extract figures from XML content
extract_figures <- function(xml_content, exfigids) {
  
  # Parse XML into list of article objects
  processed_articles <- safe_read_xml(xml_content)
  
  # Initialize results list
  all_results <- list()
  
  # Process each article
  cat(paste("Processing",length(processed_articles),"articles\n"))
  for (article in processed_articles) {
    cat(paste("Processing article", j, "of", length(processed_articles),"\n"))
    journal_nlm_ta <- xml_text(xml_find_first(article, ".//journal-id[@journal-id-type='nlm-ta']"))
    publisher_name <- xml_text(xml_find_first(article, ".//publisher-name"))
    article_title <- xml_text(xml_find_first(article, ".//article-title"))
    volume <- xml_text(xml_find_first(article, ".//volume"))
    issue <- xml_text(xml_find_first(article, ".//issue"))
    # elocation_id <- xml_text(xml_find_first(article, ".//elocation-id"))
    surname <- xml_text(xml_find_first(article, ".//surname"))
    given_names <- xml_text(xml_find_first(article, ".//given-names"))
    
    # Extract pub-date and year for online epub
    epub_date_node <- xml_find_first(article, ".//pub-date[@pub-type='epub']")
    if (is.na(epub_date_node)) {
      epub_date_node <- xml_find_first(article, ".//pub-date")
    }
    eyear <- xml_text(xml_find_first(epub_date_node, ".//year"))
    emonth <- xml_text(xml_find_first(epub_date_node, ".//month"))
    eday <-xml_text(xml_find_first(epub_date_node, ".//day"))
    
    epub_date <- paste(eyear, emonth, eday, sep = "-")
    epub_date <- gsub("NA", "", epub_date)  # Remove NA values
    epub_date <- trimws(epub_date)  # Trim whitespace
    
    # Extract pub-date and year for PMC collection
    pub_date_node <- xml_find_first(article, ".//pub-date[@pub-type='collection']")
    if (is.na(pub_date_node)) {
      pub_date_node <- xml_find_first(article, ".//pub-date")
    }
    year <- xml_text(xml_find_first(pub_date_node, ".//year"))
    month <- xml_text(xml_find_first(pub_date_node, ".//month"))
    day <-xml_text(xml_find_first(pub_date_node, ".//day"))
    
    month_abbr <- NA
    if (!is.na(month) && !is.na(as.integer(month))) {
      month_abbr <- month.abb[as.integer(month)]
    }
    
    pub_date <- paste(year, month, day, sep = "-")
    pub_date <- gsub("NA", "", pub_date)  # Remove NA values
    pub_date <- trimws(pub_date)  # Trim whitespace
    
    cit_date <- paste(year, month_abbr, day)
    cit_date <- gsub("NA", "", cit_date)  # Remove NA values
    cit_date <- trimws(cit_date)  # Trim whitespace
    
    citation <- paste0(given_names," ",surname,", et al. ",journal_nlm_ta,". ",
                       cit_date,";",volume,"(",issue,").")
    
    # Extract keywords
    keywords <- xml_text(xml_find_all(article, ".//kwd"))
    
    # Extract PMCID and DOI for this article
    doi <- xml_text(xml_find_first(article, ".//article-id[@pub-id-type='doi']"))
    pmcid <- xml_text(xml_find_first(article, ".//article-id[@pub-id-type='pmc']"))
    pmcid <- paste0("PMC", pmcid)
    
    # Extract figures for this article
    figs <- xml_find_all(article, ".//fig")
    
    # Process each figure in this article
    for (fig in figs) {
      
      # Confirm keyword in caption
      caption <- xml_text(xml_find_first(fig, ".//caption"))
      if (grepl("pathway", caption, ignore.case = TRUE)) {
        
        # Extract label and figure title
        label <- xml_text(xml_find_first(fig, ".//label"))
        figure_title <- xml_text(xml_find_first(fig, ".//title"))
        og_figure_title <- figure_title
        
        # Clean figure title
        if (!is.na(figure_title) && nchar(trimws(figure_title)) > 6) {
          # Remove label from the start of figure_title if present
          figure_title <- str_remove(figure_title, paste0("^", label))
          
          # Remove leading/trailing periods or spaces, and replace double periods
          figure_title <- figure_title %>%
            str_trim() %>%
            str_remove("^\\s*\\.+\\s*") %>%
            str_remove("\\s*\\.+\\s*$") %>%
            str_replace_all("\\.{2,}", ".")
        }
        
        # Clean caption
        if (!is.na(caption) && nchar(trimws(caption)) > 6) {
          # Remove label from the start of caption if present
          caption <- str_remove(caption, paste0("^", label))
          
          # Remove figure title from the start of caption if present
          caption <- str_remove(caption, paste0("^", og_figure_title))
          
          # Remove leading periods or spaces, and replace double periods
          caption <- caption %>%
            str_trim() %>%
            str_remove("^\\s*\\.+\\s*") %>%
            str_replace_all("\\.{2,}", ".")
        }
        
        # Use first sentence of caption or article_title if 
        # figure_title is NA, empty, or too short
        if (is.na(figure_title) || nchar(trimws(figure_title)) < 7) {
          if (!is.na(caption) && nchar(trimws(caption)) > 7) {
            first_sentence <- str_extract(caption, "^[^.!?]+[.!?]")
            figure_title <- if (!is.na(first_sentence) && nchar(first_sentence) > 7) {
              first_sentence
            } else {
              article_title
            }
          } else {
            figure_title <- article_title
          }
        }
        
        # Use figure_title if caption is NA, empty, or too short
        if (is.na(caption) || nchar(trimws(caption)) < 7) {
          caption <- figure_title
        }
        
        
        # Extract xlink:href
        graphic_node <- xml_find_first(fig, ".//graphic")
        xlink_href <- xml_attr(graphic_node, "href")
        
        # Create derived values
        image_url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/",
                            pmcid,
                            "/bin/",
                            xlink_href,
                            ".jpg")
        figid <- paste(pmcid, xlink_href, sep="__")
        
        # Check if this figure has already been processed
        if (figid %in% exfigids){
          sprintf("Skipping %s\n", figid)
          next
        }
        
        # Create figure link
        figure_number <- gsub("^.*?([0-9]+)$", "\\1", label)
        figure_link <- paste0("/pmc/articles/", pmcid, "/figure/F", figure_number, "/")
        
        # Add figure data to results
        figure_data <- list(
          label = label,
          caption = caption,
          pmcid = pmcid,
          doi = doi,
          xlink_href = xlink_href,
          image_url = image_url,
          figid = figid,
          figure_title = figure_title,
          figure_link = figure_link,
          journal_title = journal_title,
          journal_nlm_ta = journal_nlm_ta,
          publisher_name = publisher_name,
          article_title = article_title,
          pub_date = pub_date,
          epub_date = epub_date,
          year = year,
          citation = citation,
          keywords = keywords
        )
        
        all_results <- c(all_results, list(figure_data))
      }
    }
    # Check time elapsed
    check_time <- difftime(Sys.time(), job_start_time, units = "hours")
    if (check_time >= 5) {
      exit_flag <- TRUE
      break
    }
  }
  
  log_message(paste("Extracted", length(all_results), "figures from XML content"))
  total_figures_passed <<- length(all_results)
  return(all_results)
}

# Function to display figure information (for debugging)
display_figure_info <- function(figure) {
  cat("PMCID:", figure$pmcid, "\n")
  cat("Label:", figure$label, "\n")
  cat("Caption:", substr(figure$caption, 1, 100), "...\n")  # Display first 100 chars of caption
  cat("Image URL:", figure$image_url, "\n")
  cat("Figure ID:", figure$figid, "\n")
  cat("Figure Title:", figure$figure_title, "\n")
  cat("Figure Link:", figure$figure_link, "\n\n")
}

# Modified process_figures function
process_figures <- function(figures, exfigids, config, output_dir = "figures", max_attempts = 3, retry_delay = 3) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (i in seq_along(figures)) {
    cat(paste("Processing figure", i, "of", length(figures),"\n"))
    
    # Create a valid filename
    filename <- paste0(figures[[i]]$figid, ".jpg")
    filepath <- file.path(output_dir, filename)
    
    # Attempt to download the image with retries
    download_successful <- FALSE
    attempt <- 1
    
    while (!download_successful && attempt <= max_attempts) {
      tryCatch({
        response <- GET(figures[[i]]$image_url, 
                        user_agent("fetch_figures_v2/1.0 (alex.pico@gladstone.ucsf.edu)"),
                        write_disk(filepath, overwrite = TRUE),
                        timeout(30))
        
        if (status_code(response) == 200) {
          #cat("Successfully downloaded image to", filepath, "\n")
          download_successful <- TRUE
          
          # Create YAML data
          yaml_data <- list(
            figid = figures[[i]]$figid,
            pmcid = figures[[i]]$pmcid,
            image_filename = basename(filepath),
            figure_link = figures[[i]]$figure_link,
            number = figures[[i]]$label,
            figure_title = figures[[i]]$figure_title,
            caption = figures[[i]]$caption,
            article_title = figures[[i]]$article_title,
            citation = figures[[i]]$citation,
            year = figures[[i]]$year,
            pub_date = figures[[i]]$pub_date,
            epub_date = figures[[i]]$epub_date,
            doi = figures[[i]]$doi,
            journal_title = figures[[i]]$journal_title,
            journa_nlm_ta = figures[[i]]$journal_nlm_ta,
            publisher_name = figures[[i]]$publisher_name,
            keywords = figures[[i]]$keywords
          )
          
          # Write YAML file
          yaml_filename <- paste0(tools::file_path_sans_ext(filepath), ".yml")
          write("---", yaml_filename, append = FALSE)
          yaml_con <- file(yaml_filename, "a")
          write_yaml(yaml_data, yaml_con)
          close(yaml_con)
          write("---", yaml_filename, append = TRUE)
          #cat("Successfully wrote YAML file to", yaml_filename, "\n")
          
          # Add figid to prior_figid_results.tsv
          exfigids <- c(exfigids,figures[[i]]$figid)
          write.table(exfigids,config$exclude_figids, sep = "\t", row.names = F, col.names = F)
          
        } else {
          cat("Attempt", attempt, "failed to download image. Status code:", status_code(response), "\n")
          cat("Response content:\n")
          cat(content(response, "text"), "\n")
        }
      }, error = function(e) {
        cat("Error occurred during attempt", attempt, ":", e$message, "\n")
      })
      
      if (!download_successful && attempt < max_attempts) {
        cat("Retrying in", retry_delay, "seconds...\n")
        Sys.sleep(retry_delay)
      }
      
      attempt <- attempt + 1
    }
    
    if (download_successful) {
      new_figures_downloaded <<- new_figures_downloaded + 1
      #log_message(paste("Successfully downloaded image to", filepath))
      #log_message(paste("Successfully wrote YAML file to", yaml_filename))
    } else {
      log_message(paste("Failed to download image for",figures[[i]]$figid,"after", max_attempts, "attempts."))
    }
    
    # Check time elapsed
    check_time <- difftime(Sys.time(), job_start_time, units = "hours")
    if (check_time >= 5) {
      exit_flag <- TRUE
      break
    }
    
    # Respect rate limits
    Sys.sleep(0.5)  # Wait a half second between requests
  }
}

# Main execution
log_message("Starting NCBI PMC Figure Fetcher")

config <- read_config()
terms <- config$terms
exfigids <- read.table(config$exclude_figids, sep = "\t", stringsAsFactors = F)[,1]
date_range <- prepare_date_range(config)

if (!is.null(date_range)) {
  xml_content <- search_pmc(terms, date_range)
  figures <- extract_figures(xml_content, exfigids)
  process_figures(figures, exfigids, config)
  
  # Update config upon successful completion
  if (!exit_flag){
    config$last_run <- date_range[[2]]
    yaml::write_yaml(config, "query_config.yml")
  } else {
    log_message("Total runtime nearing 6 hour limit. To be continued...")
  }
  
  # Log summary
  log_message(paste("Total pathway figures found:", total_figures_passed))
  log_message(paste("New pathway figures downloaded:", new_figures_downloaded))
} else {
  log_message("Unable to proceed due to invalid date range")
}

log_message("NCBI PMC Figure Fetcher completed")
log_message("==========================================================")


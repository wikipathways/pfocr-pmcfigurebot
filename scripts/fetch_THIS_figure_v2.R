# ============================================================================
# NCBI PMC Single Figure Fetcher
# ============================================================================
#
# This script fetches a specific figure from a given PubMed Central (PMC) article
# and downloads the figure image along with its associated metadata.
#
# Usage: Rscript fetch_THIS_figure_v2.R <PMCID> <figure_number>
#   e.g., Rscript fetch_THIS_figure_v2.R PMC9949038 3
#
# Output:
# - Downloaded image is saved in the "figures" directory
# - Associated YAML file with metadata is saved alongside the image
#
# Dependencies: rentrez, xml2, httr, yaml, stringr, tools
# ============================================================================

library(rentrez)
library(xml2)
library(httr)
library(yaml)
library(stringr)
library(tools)

# Function to log messages
log_message <- function(message, log_file = "figure_fetch.log") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0(timestamp, " - ", message, "\n")
  cat(log_entry)
  write(log_entry, log_file, append = TRUE)
}

# Function to fetch article XML
fetch_article_xml <- function(pmcid) {
  tryCatch({
    xml_content <- entrez_fetch(db = "pmc", id = pmcid, rettype = "xml")
    return(read_xml(xml_content))
  }, error = function(e) {
    cat(paste("Error fetching article XML:", e$message))
    quit(status = 1)
  })
}

# Function to extract figure data
extract_figure_data <- function(xml, figure_number) {
  # Extract article metadata
  journal_title <- xml_text(xml_find_first(xml, ".//journal-title"))
  journal_nlm_ta <- xml_text(xml_find_first(xml, ".//journal-id[@journal-id-type='nlm-ta']"))
  publisher_name <- xml_text(xml_find_first(xml, ".//publisher-name"))
  article_title <- xml_text(xml_find_first(xml, ".//article-title"))
  volume <- xml_text(xml_find_first(xml, ".//volume"))
  issue <- xml_text(xml_find_first(xml, ".//issue"))
  surname <- xml_text(xml_find_first(xml, ".//surname"))
  given_names <- xml_text(xml_find_first(xml, ".//given-names"))
  
  # Extract dates
  pub_date_node <- xml_find_first(xml, ".//pub-date[@pub-type='collection']")
  if (is.na(pub_date_node)) {
    pub_date_node <- xml_find_first(xml, ".//pub-date")
  }
  year <- xml_text(xml_find_first(pub_date_node, ".//year"))
  month <- xml_text(xml_find_first(pub_date_node, ".//month"))
  day <- xml_text(xml_find_first(pub_date_node, ".//day"))
  
  pub_date <- paste(year, month, day, sep = "-")
  pub_date <- gsub("NA", "", pub_date)
  pub_date <- trimws(pub_date)
  
  # Extract epub date
  epub_date_node <- xml_find_first(xml, ".//pub-date[@pub-type='epub']")
  if (is.na(epub_date_node)) {
    epub_date_node <- xml_find_first(xml, ".//pub-date")
  }
  eyear <- xml_text(xml_find_first(epub_date_node, ".//year"))
  emonth <- xml_text(xml_find_first(epub_date_node, ".//month"))
  eday <- xml_text(xml_find_first(epub_date_node, ".//day"))
  
  epub_date <- paste(eyear, emonth, eday, sep = "-")
  epub_date <- gsub("NA", "", epub_date)
  epub_date <- trimws(epub_date)
  
  # Create citation
  month_abbr <- NA
  if (!is.na(month) && !is.na(as.integer(month))) {
    month_abbr <- month.abb[as.integer(month)]
  }
  cit_date <- paste(year, month_abbr, day)
  cit_date <- gsub("NA", "", cit_date)
  cit_date <- trimws(cit_date)
  citation <- paste0(given_names," ",surname,", et al. ",journal_nlm_ta,". ",
                     cit_date,";",volume,"(",issue,").")
  
  # Extract keywords
  keywords <- xml_text(xml_find_all(xml, ".//kwd"))
  
  # Extract PMCID and DOI
  pmcid <- xml_text(xml_find_first(xml, ".//article-id[@pub-id-type='pmc']"))
  pmcid <- paste0("PMC", pmcid)
  doi <- xml_text(xml_find_first(xml, ".//article-id[@pub-id-type='doi']"))
  
  # Find the specific figure
  figs <- xml_find_all(xml, ".//fig")
  target_fig <- NULL
  for (fig in figs) {
    label <- xml_text(xml_find_first(fig, ".//label"))
    label_number <- as.integer(str_extract(label, "\\d+(?!\\d)(?=[A-Za-z]?)(?=.?)"))
    figure_number <- as.integer(str_extract(figure_number, "\\d+(?!\\d)(?=[A-Za-z.]?$)"))
    if (!is.na(label_number) && label_number == figure_number) {
      target_fig <- fig
      break
    }
  }
  
  if (is.null(target_fig)) {
    cat(paste("Figure", figure_number, "not found in", pmcid))
    quit(status = 1)
  }
  
  # Extract figure data
  label <- xml_text(xml_find_first(target_fig, ".//label"))
  caption <- xml_text(xml_find_first(target_fig, ".//caption"))
  figure_title <- xml_text(xml_find_first(target_fig, ".//title"))
  
  # Clean figure title
  if (!is.na(figure_title) && nchar(trimws(figure_title)) > 6) {
    if (!is.na(label) && startsWith(figure_title, label)) {
      figure_title <- substr(figure_title, nchar(label) + 1, nchar(figure_title))
    }
    figure_title <- trimws(figure_title, whitespace = "[\\s\\.]+")
    while (grepl("\\.\\.", figure_title)) {
      figure_title <- gsub("\\.\\.", ".", figure_title)
    }
  }
  
  # Clean caption
  if (!is.na(caption) && nchar(trimws(caption)) > 6) {
    if (!is.na(label) && startsWith(caption, label)) {
      caption <- substr(caption, nchar(label) + 1, nchar(caption))
    }
    if (!is.na(figure_title) && startsWith(caption, figure_title)) {
      caption <- substr(caption, nchar(figure_title) + 1, nchar(caption))
    }
    caption <- trimws(caption, whitespace = "[\\s\\.]+")
    while (grepl("\\.\\.", caption)) {
      caption <- gsub("\\.\\.", ".", caption)
    }
  }
  
  # Use first sentence of caption or article_title if figure_title is invalid
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
  
  # Use figure_title if caption is invalid
  if (is.na(caption) || nchar(trimws(caption)) < 7) {
    caption <- figure_title
  }
  
  # Extract image URL
  graphic_node <- xml_find_first(target_fig, ".//graphic")
  xlink_href <- xml_attr(graphic_node, "href")
  image_url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/",
                      pmcid, "/bin/", xlink_href, ".jpg")
  
  # Create figid and figure_link
  figid <- paste(pmcid, xlink_href, sep="__")
  figure_link <- paste0("/pmc/articles/", pmcid, "/figure/", xlink_href, "/")
  
  return(list(
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
  ))
}

# Function to download figure and create YAML
process_figure <- function(figure_data, output_dir = "figures") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  filename <- paste0(figure_data$figid, ".jpg")
  filepath <- file.path(output_dir, filename)
  
  # Download image
  tryCatch({
    response <- GET(figure_data$image_url, 
                    user_agent("fetch_this_figure_v2/1.0 (alex.pico@gladstone.ucsf.edu)"),
                    write_disk(filepath, overwrite = TRUE),
                    timeout(30))
    
    if (status_code(response) == 200) {
      log_message(paste("Successfully downloaded image to", filepath))
      
      # Create YAML data
      yaml_data <- list(
        figid = figure_data$figid,
        pmcid = figure_data$pmcid,
        image_filename = basename(filepath),
        figure_link = figure_data$figure_link,
        number = figure_data$label,
        figure_title = figure_data$figure_title,
        caption = figure_data$caption,
        article_title = figure_data$article_title,
        citation = figure_data$citation,
        year = figure_data$year,
        pub_date = figure_data$pub_date,
        epub_date = figure_data$epub_date,
        doi = figure_data$doi,
        journal_title = figure_data$journal_title,
        journal_nlm_ta = figure_data$journal_nlm_ta,
        publisher_name = figure_data$publisher_name,
        keywords = figure_data$keywords
      )
      
      # Write YAML file
      yaml_filename <- paste0(tools::file_path_sans_ext(filepath), ".yml")
      write("---", yaml_filename, append = FALSE)
      yaml_con <- file(yaml_filename, "a")
      write_yaml(yaml_data, yaml_con)
      close(yaml_con)
      write("---", yaml_filename, append = TRUE)
      log_message(paste("Successfully wrote YAML file to", yaml_filename))
      
      # Store figid in "keep" file to override automl
      file_path <- "keep_figids_for_ocr.txt"
      if (!file.exists(file_path)) {
        write(figure_data$figid, file = file_path)
        log_message(paste("Created", file_path, "and added", figure_data$figid))
      } else {
        existing_figids <- readLines(file_path)
        if (!(figure_data$figid %in% existing_figids)) {
          write(figure_data$figid, file = file_path, append = TRUE)
          log_message(paste("Added", figure_data$figid, "to", file_path))
        } else {
          log_message(paste(figure_data$figid, "already exists in", file_path))
        }
      }
    } else {
      cat(paste("Failed to download image. Status code:", status_code(response)))
      quit(status = 1)
    }
  }, error = function(e) {
    cat(paste("Error occurred while downloading image:", e$message))
    quit(status = 1)
  })
}

# Main execution
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  cat("Usage: Rscript fetch_THIS_figure_v2.R <PMCID> <figure_number>\n")
  cat("Example: Rscript fetch_THIS_figure_v2.R PMC9949038 3\n")
  quit(status = 1)
}

pmcid <- args[1]
figure_number <- as.integer(args[2])

log_message(paste("Starting NCBI PMC Single Figure Fetcher for", pmcid, "Figure", figure_number))

xml <- fetch_article_xml(pmcid)
if (!is.null(xml)) {
  figure_data <- extract_figure_data(xml, figure_number)
  if (!is.null(figure_data)) {
    process_figure(figure_data)
  } else {
    cat(paste("Failed to extract figure data for", pmcid, "Figure", figure_number))
    quit(status = 1)
  }
} else {
  cat(paste("Failed to fetch article XML for", pmcid))
  quit(status = 1)
}

log_message("NCBI PMC Single Figure Fetcher completed")
log_message("==========================================================")


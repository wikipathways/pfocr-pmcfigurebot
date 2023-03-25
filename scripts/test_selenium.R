library(RSelenium)

# Set up the remote web driver using Selenium/standalone-firefox
remDr <- remoteDriver(
  verbose=T,
  remoteServerAddr = Sys.getenv("SELENIUM_REMOTE_URL"), 
  port = 4445,
  extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list(
        "--headless",
        "--disable-gpu",
        "--no-sandbox",
        "--disable-dev-shm-usage"
      )
    )
  )
)
remDr$open()

# Navigate to a webpage and extract some data
query.url <- "https://www.ncbi.nlm.nih.gov/pmc/?term=((((((((((((signaling+pathway)+OR+signalling+pathway)+OR+regulatory+pathway)+OR+disease+pathway)+OR+drug+pathway)+OR+metabolic+pathway)+OR+biosynthetic+pathway)+OR+synthesis+pathway)+OR+cancer+pathway)+OR+response+pathway)+OR+cycle+pathway))+AND+(2021/10/01[PUBDATE]+%3A+2021/11/01[PUBDATE])&report=imagesdocsum&dispmax=100"
remDr$navigate("https://wikipathways.org")
titleElem <- remDr$findElement(using = "css", "h2")
titleText <- titleElem$getElementText()

# Print the extracted data
cat("Title:", titleText, "\n")

# Close the web driver
remDr$close()

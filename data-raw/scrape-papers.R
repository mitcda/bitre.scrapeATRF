### =========================================================================
### Filename:     scrape-papers.R
### Created:      2017-09-28
### Updated:      <2018-07-01 13:09:30 david at grover>
### Author:       David Mitchell <david.p.mitchell@homemail.com.au>
### Description:  Code to scrape paper author(s), year, title, URL into a
###               single table.
### 
### Notes:        https://github.com/hadley/rvest/issues/12
### =========================================================================

###### Section 0 - Libraries & settings
library(magrittr);
library(urltools);     ## URL parsing and composing
library(xml2);         ## 
library(rvest);        ## Web scraping functions
library(httr);         ## 
library(readxl);       ## Read Excel files
library(tidyr);
library(dplyr);

### Settings
DEBUG <- FALSE



###### Section 1 - Import ATRF paper references
paper_url <- "http://atrf.info/papers/index.aspx";

####  Return the URLs of the paper archive for each ATRF conference
atrf_forums <- paper_url %>%
  html_session %>%
  read_html %>%
  html_nodes("a") %>%
  Map(function(x) data.frame(href = html_attr(x, "href"),
                             year = html_text(x)) %>%
                  filter(grepl("\\d{4}", year)),
      .) %>%
  bind_rows();


#### Return the URLs of the paper archive for each ATRF conference
#' @name return_nodes
#' @title Return selected HTML nodes Create new PostgreSQL database table
#' @description Function to extract specified HTML node elements from a specified URL and return in
#'   a table.
#' @importFrom httr parse_url modify_url 
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @param base_url base URL
#' @param path path to append to \code{base_url}
#' @param xpath xpath format node selection string.
#' @return data frame comprising selected nodes.
#' @export
#' @author David Mitchell (david.mitchell@@infrastructure.gov.au)
return_nodes <- function(base_url, path, xpath) {
  z <- base_url %>% parse_url %>% modify_url(path = path) %>% read_html %>%
    html_nodes(xpath = xpath) %>% html_text();
}

###  Return data frame containing citation details for all ATRF conference papers
atrf_papers <- mapply(function(x, y)
  data.frame(
    author_title = return_nodes(paper_url, x, "//li//a[contains(@class, 'attach')]/ancestor::li"),
    year = y,
    path = return_nodes(paper_url, x, paste("//li//a[contains(@class, 'attach')",
                                           "and",
                                           "not(contains(@class, 'attach-doc'))]/@href"))
  ),
  atrf_forums$href, atrf_forums$year, SIMPLIFY=FALSE) %>%
  bind_rows;


###### Section 2 - Refine and finalise ATRF paper references

##### 2.1 - Clean-up references
atrf_papers <- atrf_papers %>%
  mutate_at(vars(author_title), funs(sub("^(.+)\\s*PDF:\\s*\\d+\\w+.*$", "\\1", .))) %>%
  mutate(authors = sub("^(.+)(\\.|—|—)(.+)$", "\\1", author_title) %>% trimws,
         title = sub("^(.+)(\\.|—)(.+)$", "\\3", author_title) %>% trimws,
         base_url = paper_url) %>% #,
  ## full_url = paper_url %>% parse_url %>% modify_url(path = path)) %>%
  mutate_at(vars(authors), funs(gsub("\\s*,\\s*", " and ", .)));


##### 2.2 - Add keywords
#### Add keywords field
add_keywords <- function(x) {
  x %>%
    mutate(keywords = "") %>%
    ## freight & logistics
    mutate(keywords = ifelse(grepl("freight", title, ignore.case=TRUE),
                             paste(keywords, "freight"), keywords)) %>%
    mutate(keywords = ifelse(grepl("logistics", title, ignore.case=TRUE),
                             paste(keywords, "logistics"), keywords)) %>%
    ## traffic modelling, forecasting & simulation
    mutate(keywords = ifelse(grepl("model+ing", title, ignore.case=TRUE),
                             paste(keywords, "modelling"), keywords)) %>%
    mutate(keywords = ifelse(grepl("forecast(s|ing)*", title, ignore.case=TRUE),
                             paste(keywords, "forecasting"), keywords)) %>%
    mutate(keywords = ifelse(grepl("simulation", title, ignore.case=TRUE),
                             paste(keywords, "simulation"), keywords)) %>%
    ## transport analysis & big data
    mutate(keywords = ifelse(grepl("big data", title, ignore.case=TRUE),
                             paste(keywords, "big data"), keywords)) %>%
    ## active transport (cycling, walking, etc.)
    mutate(keywords = ifelse(grepl("cycling", title, ignore.case=TRUE),
                             paste(keywords, "cycling"), keywords)) %>%
    mutate(keywords = ifelse(grepl("walking", title, ignore.case=TRUE),
                             paste(keywords, "walking"), keywords)) %>%
    mutate(keywords = ifelse(grepl("active.+transport", title, ignore.case=TRUE) |
                             grepl("cycling", title, ignore.case=TRUE) |
                             grepl("walking", title, ignore.case=TRUE),
                             paste(keywords, "active transport"), keywords)) %>%
    ## car transport
    mutate(keywords = ifelse(grepl(" car", title, ignore.case=TRUE),
                             paste(keywords, "car"), keywords)) %>%
    ## public transport
    mutate(keywords = ifelse(grepl(" bus", title, ignore.case=TRUE),
                             paste(keywords, "bus"), keywords)) %>%
    mutate(keywords = ifelse(grepl("passenger.+rail", title, ignore.case=TRUE),
                             paste(keywords, "rail"), keywords)) %>%
    mutate(keywords = ifelse(grepl("tram", title, ignore.case=TRUE),
                             paste(keywords, "tram"), keywords)) %>%
    mutate(keywords = ifelse(grepl(" bus", title, ignore.case=TRUE) |
                             grepl("passenger.+rail", title, ignore.case=TRUE) |
                             grepl("tram", title, ignore.case=TRUE) |
                             grepl("transit", title, ignore.case=TRUE),
                             paste(keywords, "public transport"), keywords)) %>%
    ## transport & land use / urban design
    mutate(keywords = ifelse(grepl("land.+use", title, ignore.case=TRUE),
                             paste(keywords, "land use"), keywords)) %>%
    ## planning and policy
    mutate(keywords = ifelse(grepl("planning", title, ignore.case=TRUE),
                             paste(keywords, "planning"), keywords)) %>%
    mutate(keywords = ifelse(grepl("policy", title, ignore.case=TRUE),
                             paste(keywords, "policy"), keywords)) %>%
    ## transport economics, impacts and environment
    mutate(keywords = ifelse(grepl("transport economics", title, ignore.case=TRUE),
                             paste(keywords, "transport economics"), keywords)) %>%
    mutate(keywords = ifelse(grepl("environment", title, ignore.case=TRUE),
                             paste(keywords, "environment"), keywords)) %>%
    ## aviation / maritime
    mutate(keywords = ifelse(grepl(" (air|aviation)", title, ignore.case=TRUE),
                             paste(keywords, "aviation"), keywords)) %>%
    mutate(keywords = ifelse(grepl(" (sea|coastal|maritime)", title, ignore.case=TRUE),
                             paste(keywords, "maritime"), keywords)) %>%
    ## ITS connected and autonomous vehicles
    mutate(keywords = ifelse(grepl(" its ", title, ignore.case=TRUE),
                             paste(keywords, "ITS"), keywords)) %>%
    mutate(keywords = ifelse(grepl("intelligent transport system", title, ignore.case=TRUE),
                             paste(keywords, "ITS"), keywords)) %>%
    mutate(keywords = ifelse(grepl(" (cav|autonomous vehicle)", title, ignore.case=TRUE),
                             paste(keywords, "CAV"), keywords)) %>%
    ## safety
    mutate(keywords = ifelse(grepl(" (safety|crash|accident)", title, ignore.case=TRUE),
                             paste(keywords, "safety"), keywords)) %>%
    ## transport funding and pricing
    mutate(keywords = ifelse(grepl(" (investment|funding)", title, ignore.case=TRUE),
                             paste(keywords, "funding"), keywords)) %>%
    mutate(keywords = ifelse(grepl(" (pricing|prices|charging|charges)", title, ignore.case=TRUE),
                             paste(keywords, "funding"), keywords)) %>%
    mutate(keywords = ifelse(grepl(" (toll road|tolls|tolling)", title, ignore.case=TRUE),
                             paste(keywords, "toll roads"), keywords)) %>%
    ## travel behaviour & demand management
    mutate(keywords = ifelse(grepl("(travel)*\\s*behaviour", title, ignore.case=TRUE),
                             paste(keywords, "travel behaviour"), keywords)) %>%
    mutate(keywords = ifelse(grepl("demand management", title, ignore.case=TRUE),
                             paste(keywords, "demand management"), keywords)) %>%
    ## traffic engineering & management
    mutate(keywords = ifelse(grepl("traffic engineering", title, ignore.case=TRUE),
                             paste(keywords, "traffic engineering"), keywords));
}

atrf_papers %<>% add_keywords;


##### 2.3 - Add proceedings reference details
atrf_proceedings <- read.csv(file.path("data-raw", "ATRF-Proceedings-Details.csv"))

### Write results to atrf_reflist data frame
.atrf_papers <- atrf_papers %>%
  inner_join(atrf_proceedings %>% mutate_at(vars(year), funs(as.character)),
             by="year") %>%
  select(authors, year, title, booktitle, host_organisation, location, keywords, base_url, path);

write.csv(.atrf_papers, file=file.path("data-raw", "ATRF-Paper-References.csv"), row.names=FALSE);

###### Section 4 - Write data sets files
devtools::use_data(atrf_papers, overwrite=TRUE);

## -------------------------------- EOF ---------------------------------------

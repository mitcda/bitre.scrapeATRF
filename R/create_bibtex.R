
#### Return the URLs of the paper archive for each ATRF conference
#' @name create_bibtex
#' @title Create BibTeX formatted Return selected HTML nodes Create new PostgreSQL database table
#' @description Create BibTeX formatted entry.
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join
#' @param df data frame
#' @return a character vector of BibTeX formatted .
#' @export
#' @author David Mitchell (david.mitchell@@infrastructure.gov.au)
append_atrf <- function(df) {
  x <- df %>%
    inner_join(atrf_proceedings,
               by="year") %>%
    }


#### Return the URLs of the paper archive for each ATRF conference
#' @name create_bibtex
#' @title Create BibTeX formatted Return selected HTML nodes Create new PostgreSQL database table
#' @description Create BibTeX formatted entry.
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by n
#' @param df data frame
#' @return a character vector of BibTeX formatted .
#' @export
#' @author David Mitchell (david.mitchell@@infrastructure.gov.au)
format_bibtex <- function(df) {
  x <- df %>%
    group_by(authors, year) %>%
    mutate(num = n(),
           supp_ref = LETTERS[1:n()]) %>%
    ungroup;
  z <- sprintf(paste("@InProceedings{%s:%s:%s,\n",
                     "author = {%s},\n",
                     "year = {%s},\n",
                     "title = {%s},\n",
                     "booktitle = {%s},\n",
                     "organization = {%s},\n",
                     "note = {%s},\n",
                     "location = {%s}\n"
                     "}"),
               x$authors, x$year, x$supp_ref,
               x$authors, x$year,
               x$title, x$booktitle, x$host_organization, x$location);
  return(z);
}

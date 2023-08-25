.get_company_reports <- function(.node) {
  heading_ <- rvest::html_text(rvest::html_elements(.node, ".heading"))
  download_ <- rvest::html_elements(.node, "a") %>%
    rvest::html_attr("href") %>%
    unique() %>%
    rvest::url_absolute(., "https://www.annualreports.com/")
  year_ <- as.integer(stringi::stri_extract_first_regex(heading_, "\\d{4}"))
  
  out_ <- tibble::tibble(
    heading = heading_,
    year = year_,
    report_link = download_
  )
  
  return(out_)
}

get_company_reports <- function(.html) {
  archived_ <- rvest::html_elements(.html, ".archived_report_content_block")
  
  if (length(archived_) == 0) {
    return(tibble::tibble())
  }
  
  li_ <- rvest::html_elements(archived_, "li")
  
  out_ <- dplyr::bind_rows(purrr::map(li_, .get_company_reports))
  
  return(out_)
}
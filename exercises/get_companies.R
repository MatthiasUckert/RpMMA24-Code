.get_company <- function(.node) {
  
  node_companyName_ <- rvest::html_element(.node, ".companyName")
  node_industryName_ <- rvest::html_elements(.node, ".industryName")
  node_sectorName_ <- rvest::html_elements(.node, ".sectorName")
  
  tibble::tibble(
    company_name = rvest::html_text(node_companyName_),
    company_link = rvest::html_attr(rvest::html_elements(node_companyName_, "a"), "href"),
    industry_name = rvest::html_text(node_industryName_),
    sector_name = rvest::html_text(node_sectorName_)
  ) %>%
    dplyr::mutate(
      company_link = rvest::url_absolute(company_link, "https://www.annualreports.com/"),
      dplyr::across(dplyr::everything(), trimws)
    ) %>%
    dplyr::mutate(
      company_id = basename(company_link), .before = company_name
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ purrr::set_names(., company_id))
    )
}

get_companies <- function(.progress = TRUE, .n = Inf) {
  html_ <- rvest::read_html("https://www.annualreports.com/Companies?a=#")
  li_ <- rvest::html_elements(html_, "li")
  n_ <- ifelse(.n > length(li_), length(li_), .n)
  li_ <- li_[seq_len(n_)]
  
  out_ <- purrr::map(li_, .get_company, .progress = .progress)
  out_ <- dplyr::bind_rows(out_)
}
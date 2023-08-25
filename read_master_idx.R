read_master_idx <- function(.path) {
  out_ <- readr::read_delim(.path, "|", skip = 9, col_types = readr::cols()) %>%
    dplyr::slice(-1) %>%
    janitor::clean_names()
  
  return(out_)
}
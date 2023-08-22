# .true_vec <- c("Elite" = 12000, "Premium" = 28000)
#  .your_vec <- mat_ac
check_named_vector <- function(.your_vec, .true_vec) {
  if (is.null(.your_vec)) {
    msg_ <- "Please Calculate the Indirect Cost"
  } else {
    check1_ <- all(.your_vec == .true_vec)
    check2_ <- all(names(.your_vec) == names(.true_vec))
    
    if (check1_ & check2_) {
      msg_ <- "Success: Correct Values and Names"
    } else if (check1_ & !check2_) {
      msg_ <- "Error: Correct Values but Incorrect Names"
    } else if (!check1_ & check2_) {
      msg_ <- "Error: Correct Names but Incorrect Values"
    } else {
      msg_ <- "Error: Neither Correct Names nor Correct Values"
    }
  }
  
  cat(msg_)
  
}


format_table <- function(.tab) {
  cols_format <- colnames(.tab)
  
  cols_format <- cols_format[!cols_format %in% c("sign", "var")]
  
  gt_table <- gt::gt(.tab)
  
  # Making the header bold
  
  gt_table <- gt_table %>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = list(
        gt::cells_column_labels(columns = everything())
      )
    )
  
  # Making rows with "=" and empty sign bold
  gt_table <- gt_table %>%
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = list(
        gt::cells_body(
          rows = which(.tab$sign %in% c("=", "")),
          columns = gt::everything()
        )
      )
    )
  
  # Making negative values red
  
  for (col in cols_format) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(gt::cell_text(color = "red")),
        locations = list(
          gt::cells_body(
            columns = col,
            rows = which(.tab[[col]] < 0)
          )
        )
      )
  }
  
  # Formatting numbers
  
  gt_table <- gt_table %>%
    gt::fmt_number(
      columns = cols_format,
      decimals = 0,
      use_seps = TRUE,
      accounting = TRUE
    )
  
  return(gt_table)
}


color_bins <- function(.val) {
  ifelse(.val < 0, "red", "green")
}

display_histogram <- function(.lst) {
  make_table_row <- function(.vec) {
    tibble::as_tibble(t(.vec))
  }
  
  dplyr::bind_rows(purrr::map(.lst, make_table_row), .id = "seed") %>%
    dplyr::mutate(Total = rowSums(.[, -1])) %>%
    tidyr::pivot_longer(cols = !dplyr::matches("seed")) %>%
    ggplot2::ggplot(ggplot2::aes(x = value, fill = color_bins(value))) +
    ggplot2::geom_histogram(bins = 100, color = "white") +
    ggplot2::facet_wrap(~name, scales = "free") +

    # Improve the appearance of the axis

    ggplot2::scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
    ggplot2::scale_fill_identity() +

    # Add theme and labels

    ggplot2::labs(
      title = "Histogram of Values by Name",
      x = "Contribution Margin",
      y = "Frequency",
      fill = "Sign"
    ) +

    # Use a theme from ggthemes and customize

    ggthemes::theme_tufte() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


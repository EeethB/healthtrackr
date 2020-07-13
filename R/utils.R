date_of_id <- function(i) {
  df_threads %>%
    purrr::pluck("id", i) %>%
    gmailr::gm_thread() %>%
    purrr::pluck("messages", 1) %>%
    gmailr::gm_date()
}

from_of_id <- function(i) {
  df_threads %>%
    purrr::pluck("id", i) %>%
    gmailr::gm_thread() %>%
    purrr::pluck("messages", 1) %>%
    gmailr::gm_from()
}

# Packages and functions-------------------------------------------------------
library(gmailr)
library(dplyr)
library(lubridate)
library(purrr)

date_of_id <- function(i) {
  df_threads %>%
    pluck("id", i) %>%
    gm_thread() %>%
    pluck("messages", 1) %>%
    gm_date()
}

from_of_id <- function(i) {
  df_threads %>%
    pluck("id", i) %>%
    gm_thread() %>%
    pluck("messages", 1) %>%
    gm_from()
}

# Configure gmail auth --------------------------------------------------------
gm_auth_configure(path = "credentials.json")
gm_auth(cache = ".secret")

# Get all threads -------------------------------------------------------------
# threads <- gm_threads("after:2020/5/25 before:2020/6/9")
threads <- gm_threads()
act_threads <- threads[[1]]$threads
df_threads <- do.call(rbind, act_threads) %>%
  as_tibble()

df_threads_info <- df_threads %>%
  mutate(
    recd = do.call(rbind, lapply(1:nrow(.), date_of_id)) %>% dmy_hms(),
    from = do.call(rbind, lapply(1:nrow(.), from_of_id))
  ) %>%
  tidyr::unnest(cols = from)

# Create a health tracker table -----------------------------------------------
init <- function(day) {
  tibble(
    date = date(day),
    ex_wgt = 0, ex_run = 0, ex_walk = 0, ex_climb = 0,
    fd_fruit = 0, fd_veg = 0
  )
}

add_health <- function(hh, date = today(), col, quant) {
  new_row <- init(date)
  new_row[[col]] <- c(quant)
  hh %>%
    rbind(new_row) %>%
    group_by(date) %>%
    summarise(across(.fns = sum))
}

health_hist <- add_health(health_hist, col = "ex_wgt", quant = 35)

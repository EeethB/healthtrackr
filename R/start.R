
library(gmailr)
library(dplyr)
library(lubridate)
library(purrr)

gm_auth_configure(path = "credentials.json")
gm_auth()
threads <- gm_threads()

act_threads <- threads[[1]]$threads
df_threads <- do.call(rbind, act_threads) %>%
  as_tibble %>%
  tidyr::unnest(cols = names(.))

df_threads$snippet[[4]]

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

df_threads_info <- df_threads %>%
  mutate(
    recd = do.call(rbind, lapply(1:nrow(.), date_of_id)) %>% dmy_hms(),
    from = do.call(rbind, lapply(1:nrow(.), from_of_id))
  ) %>%
  tidyr::unnest(cols = from)

df_threads_info

# view the latest thread
my_threads <- gm_threads(num_results = 10)

# retrieve the latest thread by retrieving the first ID

latest_thread <- gm_thread(gm_id(my_threads)[[1]])

# The messages in the thread will now be in a list
latest_thread$messages

# Retrieve parts of a specific message with the accessors
my_msg <- latest_thread$messages[[1]]

gm_to(my_msg)
gm_from(my_msg)
gm_date(my_msg)
gm_subject(my_msg)
gm_body(my_msg)





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

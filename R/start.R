# Packages and functions-------------------------------------------------------
library(gmailr)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)

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
    fd_fruit = 0, fd_veg = 0,
    con_w = FALSE, con_dgt1 = FALSE, con_dgt2 = FALSE,
    hlt_wgt = 0, hlt_rat = -1, hlt_phys = -1, hlt_ment = -1, hlt_spir = -1,
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

health_hist <- init(ymd("2020-07-06"))

health_hist <- add_health(health_hist, col = "ex_wgt", quant = 35)

# Translate emails into health data -------------------------------------------
words <- tibble::tribble(
  ~word,                    ~col,
  "run",                    "ex_run",
  "ran",                    "ex_run",
  "jog",                    "ex_run",
  "lift",                   "ex_wgt",
  "weight.*[:digit:]{1,2}", "ex_wgt",
  "walk",                   "ex_walk",
  "climb",                  "ex_climb",
  "boulder",                "ex_climb",
  "veg",                    "fd_veg",
  "fruit",                  "fd_fruit",
  "carlie",                 "con_w",
  "Isla",                   "con_dgt1",
  "Lea",                    "con_dgt2",
  "pound",                  "hlt_wgt",
  "weight.*[:digit:]{3,}",  "hlt_wgt"
)

mail_body <- "Ran for 30 min!"

low_body <- mail_body %>%
  str_replace_all("[:punct:]", "") %>%
  tolower()

val <- low_body %>%
  str_extract_all("[:digit:]") %>%
  unlist() %>%
  paste(collapse = "") %>%
  as.numeric()

type <- words %>%
  mutate(match = str_detect(low_body, word)) %>%
  filter(match) %>%
  pluck("col", 1)

health_hist <- add_health(health_hist, col = type, quant = val)

health_hist

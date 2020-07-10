# Packages and functions-------------------------------------------------------
library(gmailr)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
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
gm_auth_configure(path = here::here("./credentials.json"))
gm_auth(email = "ejb.healthtrackr@gmail.com", cache = ".secret", path = here::here("./credentials.json"))

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
  tidyr::unnest(cols = everything()) %>%
  filter(str_detect(from, "9207285600"))

# Create a health tracker table -----------------------------------------------
init <- function(day = today()) {
  tibble(
    date = date(day),
    ex_wgt = 0, ex_run = 0, ex_walk = 0, ex_climb = 0,
    fd_fruit = 0, fd_veg = 0, fd_water = 0,
    con_w = 0, con_dgt1 = 0, con_dgt2 = 0,
    hlt_wgt = 0, hlt_rat = 0, hlt_phys = 0, hlt_ment = 0, hlt_spir = 0,
  )
}

add_health <- function(prior, date = today(), col, quant) {
  new_row <- init(date)
  new_row[[col]] <- c(quant)
  prior %>%
    rbind(new_row) %>%
    group_by(date) %>%
    summarise(across(.fns = sum))
}


health_hist <- init()

# Translate emails into health data -------------------------------------------

parse_body <- function(snippet, recd) {

  words <- tibble::tribble(
    ~word,                    ~col,
    "run",                    "ex_run",
    "ran",                    "ex_run",
    "jog",                    "ex_run",
    "lift",                   "ex_wgt",
    "weight.*[:digit:]{1,2}", "ex_wgt",
    "[:digit:]{1,2}.*weight", "ex_wgt",
    "walk",                   "ex_walk",
    "climb",                  "ex_climb",
    "boulder",                "ex_climb",
    "veg",                    "fd_veg",
    "fruit",                  "fd_fruit",
    "water",                  "fd_water",
    "carlie",                 "con_w",
    "isla",                   "con_dgt1",
    "lea",                    "con_dgt2",
    "pound",                  "hlt_wgt",
    "weight.*[:digit:]{3,}",  "hlt_wgt",
    "[:digit:]{3,}.*weight",  "hlt_wgt",
    "phys",                   "hlt_phys",
    "ment",                   "hlt_ment",
    "spir",                   "hlt_spir",
    "total",                  "hlt_rat",
    "overall",                "hlt_rat"
  )

  low_body <- snippet %>%
    str_replace_all("[:punct:]", "") %>%
    tolower()

  val <- low_body %>%
    str_extract_all("[:digit:]") %>%
    unlist() %>%
    paste(collapse = "") %>%
    as.numeric() %>%
    tidyr::replace_na(1)

  type <- words %>%
    mutate(match = str_detect(low_body, word)) %>%
    filter(match) %>%
    pluck("col", 1)

  if (str_detect(low_body, "yesterday")) {
    use_date <- recd - days(1)
  } else {
    use_date <- recd
  }

  add_health(health_hist, col = type, quant = val, date = use_date)

}

hlt_list <- pmap(select(df_threads_info, snippet, recd), parse_body)

final <- do.call(rbind, hlt_list) %>%
  group_by(date) %>%
  summarise(across(.fns = sum))

write_csv(
  read_csv("R/Archive/health.csv"),
  glue::glue("R/Archive/health-{today()}.csv")
)
write_csv(final, "R/Archive/health.csv")

print(final, width = Inf)

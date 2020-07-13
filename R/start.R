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
print("Configure")
gm_auth_configure(path = here::here("./credentials.json"))
gm_auth(email = "ejb.healthtrackr@gmail.com", cache = ".secret", path = here::here("./credentials.json"))

# Get all threads -------------------------------------------------------------
print("Threads")
# threads <- gm_threads("after:2020/5/25 before:2020/6/9")
threads <- gm_threads("from:9207285600")
act_threads <- threads[[1]]$threads
df_threads <- do.call(rbind, act_threads) %>%
  as_tibble()

print("Metadata")
df_threads_info <- df_threads %>%
  mutate(
    recd = do.call(rbind, lapply(1:nrow(.), date_of_id)) %>% dmy_hms(),
    from = do.call(rbind, lapply(1:nrow(.), from_of_id))
  ) %>%
  tidyr::unnest(cols = everything())
# %>%
  # filter(str_detect(from, "9207285600"))

# Create a health tracker table -----------------------------------------------
init <- function(day = today()) {
  tibble(
    date = date(day),
    ex_wgt = NA_real_, ex_run = NA_real_, ex_walk = NA_real_, ex_climb = NA_real_, ex_lbr = NA_real_,
    fd_fruit = NA_real_, fd_veg = NA_real_, fd_water = NA_real_,
    con_w = 0, con_dgt1 = 0, con_dgt2 = 0,
    hlt_wgt = NA_real_, hlt_rat = NA_real_, hlt_phys = NA_real_, hlt_ment = NA_real_, hlt_spir = NA_real_,
  )
}

add_health <- function(prior, date = today(), col, quant) {
  new_row <- init(date)
  new_row[[col]] <- c(quant)
  prior %>%
    rbind(new_row) %>%
    group_by(date) %>%
    summarise(across(.fns = sum, na.rm = TRUE))
}


health_hist <- init()

# Translate emails into health data -------------------------------------------

parse_body <- function(snippet, recd) {

  pins::board_register(
    "github",
    name = "gh",
    repo = "EeethB/healthtrackr",
    path = "/pins/",
    token = read_lines("./../../healthtrackr-token.txt")
  )

  word_map <- pins::pin_get("word_map", "gh")

  low_body <- snippet %>%
    str_replace_all("[:punct:]", "") %>%
    tolower()

  val <- low_body %>%
    str_extract_all("[:digit:]") %>%
    unlist() %>%
    paste(collapse = "") %>%
    as.numeric() %>%
    tidyr::replace_na(1)

  type <- word_map %>%
    mutate(match = str_detect(low_body, word)) %>%
    filter(match) %>%
    pluck("col", 1)

  use_date <- recd - days(str_detect(low_body, "yesterday"))

  if (is.null(type)) {
    print(glue::glue("Body text \"{snippet}\" did not match a health category"))
    return(init())
  } else {
    add_health(health_hist, col = type, quant = val, date = use_date)
  }

}

print("Parse")
hlt_list <- pmap(select(df_threads_info, snippet, recd), parse_body)

final <- do.call(rbind, hlt_list) %>%
  group_by(date) %>%
  summarise(across(.fns = sum, na.rm = TRUE))

print("Save")
write_csv(
  read_csv("R/Archive/health.csv"),
  glue::glue("R/Archive/health-{today()}.csv")
)
write_csv(final, "R/Archive/health.csv")

print(final, width = Inf)

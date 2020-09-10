# Packages and functions-------------------------------------------------------
library(gmailr)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(purrr)

date_of_id <- function(id) {
  map(id, ~ gm_thread(.) %>% pluck("messages", 1) %>% gm_date())
}

options(dplyr.summarise.inform = FALSE)

# Configure gmail auth --------------------------------------------------------
write("Configure", "status.txt")
gm_auth_configure(path = here::here("./credentials.json"))
gm_auth(email = "ejb.healthtrackr@gmail.com")
# Get all threads -------------------------------------------------------------
write("Threads", "status.txt")
get_threads <- function(date) {
  threads <- gm_threads(str_glue("from:9207285600 after:{date} before:{date + days(1)}"))
  act_threads <- threads[[1]]$threads
  df_threads <- do.call(rbind, act_threads) %>%
    as_tibble()
}

# dates <- seq(ymd("2020-07-07"), today(), by = "day")
dates <- seq(ymd("2020-08-03"), ymd("2020-08-03"), by = "day")

df_threads <- dates %>%
  map(get_threads) %>%
  bind_rows() %>%
  tidyr::unnest(cols = everything())

write("Metadata", "status.txt")
df_threads_info <- df_threads  %>%
  mutate(
    recd = unlist(date_of_id(id) %>% dmy_hms()),
    snippet = str_split(snippet, "\n")
  ) %>%
  tidyr::unnest_longer(snippet)

# Create a health tracker table -----------------------------------------------
init <- function(day = today()) {
  tibble(
    date = date(day),
    ex_wgt = NA_real_, ex_run = NA_real_, ex_walk = NA_real_,
      ex_climb = NA_real_, ex_lbr = NA_real_, ex_bike = NA_real_,
    fd_fruit = NA_real_, fd_veg = NA_real_, fd_water = NA_real_,
    con_w = 0, con_dgt1 = 0, con_dgt2 = 0,
    hlt_wgt = NA_real_, hlt_rat = NA_real_, hlt_phys = NA_real_,
      hlt_ment = NA_real_, hlt_emot = NA_real_, hlt_spir = NA_real_,
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

pins::board_register(
  "github",
  name = "gh",
  repo = "EeethB/healthtrackr",
  path = "/pins/",
  token = read_lines(here::here("./../../healthtrackr-token.txt"))
)

parse_body <- function(snippet, recd) {

  write(as.character(recd), "status.txt")
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

write("Parse", "status.txt")
df_health <- pmap(select(df_threads_info, snippet, recd), parse_body) %>%
  bind_rows() %>%
  group_by(date) %>%
  summarise(across(.fns = sum, na.rm = TRUE))

write("Save", "status.txt")
write_csv(
  read_csv(here::here("R/Archive/health.csv")),
  glue::glue(here::here("R/Archive/health-{today()}.csv"))
)
write_csv(df_health, here::here("R/Archive/health.csv"))

print(df_health, width = Inf)

df_analysis <- df_health %>%
  na_if(0)

library(ggplot2)

ggplot(df_health) + geom_point(aes(hlt_emot, hlt_rat))

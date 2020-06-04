library(gmailr)
library(dplyr)
library(lubridate)

# gm_auth_configure(path = "credentials.json")
# gm_auth()
threads <- gm_threads()
as_tibble(threads)
act_threads <- threads[[1]]$threads
df_threads <- do.call(rbind, act_threads) %>%
  as_tibble %>%
  tidyr::unnest(cols = names(.))

df_threads$snippet[[4]]
init <- function(day) {
  tibble(
    date = date(day),
    ex_wgt = 0, ex_run = 0, ex_walk = 0, ex_climb = 0,
    fd_fruit = 0, fd_veg = 0
  )
}
init(ymd("1970-04-16"))

health_hist <- init(ymd("1970-04-16"))

# NA,    NA,      NA,      NA,       NA,        NA,        NA
# ymd("1970-04-17"), 30, 45, 75, 10, 2, 3

add_day <- function(hh, date) {
  rbind(hh, init(date))
}

health_hist <- add_day(health_hist, ymd("1970-04-17"))
health_hist

health_hist %>%
  group_by(date) %>%
  summarise(across(.fns = sum))

add_health <- function(hh, date = today(), col, quant) {
  new_row <- init(date)
  new_row[[col]] <- c(quant)
  hh %>%
    rbind(new_row) %>%
    group_by(date) %>%
    summarise(across(.fns = sum)) %>%
    print()
}

health_hist <- add_health(health_hist, col = "ex_wgt", quant = 35)

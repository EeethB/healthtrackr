pins::board_register_github(
  name = "gh",
  repo = "EeethB/healthtrackr",
  path = "/pins/",
  token = read_lines("./../../healthtrackr-token.txt")
)

word_map <- tibble::tribble(
  ~word,                    ~col,
  "run",                    "ex_run",
  "ran",                    "ex_run",
  "jog",                    "ex_run",
  "lift",                   "ex_wgt",
  "weight.*[:digit:]{1,2}", "ex_wgt",
  "[:digit:]{1,2}.*weight", "ex_wgt",
  "walk",                   "ex_walk",
  "labor",                  "ex_lbr",
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

pins::pin(word_map, name = "word_map", board = "gh")

pins::pin_get("word_map", board = "gh")


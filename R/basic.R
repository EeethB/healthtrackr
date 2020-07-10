library(gmailr)

gm_auth_configure(path = here::here("./credentials.json"))
gm_auth(
  email = "ejb.healthtrackr@gmail.com",
  cache = ".secret",
  path = here::here("./credentials.json")
)

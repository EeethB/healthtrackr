library(gmailr)
gm_auth_configure(path = here::here("./credentials.json"))
gm_auth(
  email = "ejb.healthtrackr@gmail.com"
)
gm_threads()

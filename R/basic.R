library(pins)

pins::board_register(
  "github",
  name = "gh",
  repo = "EeethB/healthtrackr",
  path = "pins",
  token = "97033126ce9b62728d3800e0c6e911e692d85431"
)

pin(mtcars, board = "gh")

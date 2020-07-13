library(pins)

pins::board_register(
  "github",
  name = "gh",
  repo = "EeethB/healthtrackr",
  path = "pins",
  token = read_lines("./../../healthtrackr-token.txt")
)

pin(mtcars, board = "gh")

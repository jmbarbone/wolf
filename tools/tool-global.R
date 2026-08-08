devtools::load_all()
w <- wolf("global-test")
w$add_handler(
  stdout = StdoutHandler,
  stderr = StderrHandler,
  "log",
  "yaml12"
)

w$on()
message("this is a message")
warning("this is a warning")
message("this is another")

w$entries
w$handlers$log$entries

w$off()
message("and another another")
w$on()
message("and one more")
w$off()

w$handlers$yaml12$entries
w$handlers$yaml12$read()
w$handlers$yaml12$read_lines() |> writeLines()

w2 <- wolf("global-test-2")
w2$add_handler(
  stdout = StdoutHandler,
  stderr = StderrHandler,
  "log",
  "yaml12"
)

w2$on()
message("this is a message")
warning("this is a warning")
message("this is another")

w2$entries
w2$handlers$log$entries

w2$off()
message("and another another")
w2$on()
message("and one more")
w2$off()

w2$handlers$yaml12$entries
w2$handlers$yaml12$read()
w2$handlers$yaml12$read_lines() |> writeLines()

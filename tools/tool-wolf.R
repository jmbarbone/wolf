devtools::load_all(".")
w <- wolf()
# w$add_handler(LogHandler$new("my-log", cclear = TRUE))
# w$add_handler(TableHandler$new("my-txt", clear = TRUE))
# w$add_handler(CsvHandler$new("my-csv", clear = TRUE))
# w$add_handler(Tsv2Handler$new("my-tsv2", clear = TRUE))
# w$add_handler(DcfHandler$new("my-dcf", clear = TRUE))
# my_json <- JsonHandler$new("my-json", clear = TRUE)
# my_json$delay <- FALSE
# w$add_handler(my_json)
w$add_handler(yaml = YamlHandler)
w$add_handler(yaml12 = Yaml12Handler)
# w$add_handler(JsonlHandler$new("my-jsonl", clear = TRUE))
w$log(message(toString(colnames(penguins))))
w$log(warning("that"))
w$info("this is a MESSAGE")
w$debug("DeBuG mY cOdE")
w$warn("WARN WARN")
w$error("fdjskalfdsj")
w$debug(toString(unclass(head(penguins))))
w$entries
try(w$log(stop("this still shows up?")))
# rm(w)

rm(my_json)

read.dcf("wolf-wolf.log")
read.csv("wolf-wolf.csv.log")
read.table(
  "wolf-wolf.tsv.log",
  sep = "\t",
  header = TRUE
)

# file.remove(list.files(".", "\\.log$"))

pup <- function() {
  as.character(sys.call(sys.parent()))[1L]
  # name <- as.character(as.list(eval.parent(match.call()))[[1L]])
  # wolf(name)
}

foo <- function(...) {
  pup()
}

foo(1)

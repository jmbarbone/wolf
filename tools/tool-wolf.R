devtools::load_all(".")
w <- wolf()
w$add_handler("log", "dcf", "csv", "csv", "tsv")
w$add_handler("logger", "log")
w$log(message(toString(colnames(penguins))))
w$log(warning("that"))
w$info("this is a MESSAGE")
w$debug("DeBuG mY cOdE")
w$warn("WARN WARN")
w$error("fdjskalfdsj")
w$debug(toString(unclass(head(penguins))))

try(w$log(stop("this still shows up?")))

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

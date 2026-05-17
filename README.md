
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wolf

<!-- badges: start -->

<!-- badges: end -->

The goal of wolf is to generate simple and flexible logging for **R**.

## Installation

You can install the development version of wolf like so:

``` r
pak::pak("github::jmbarbone/wolf")
```

## Example

You can create a new logging object with a call to `wolf()`. The `Wolf`
class contains methods for including built-in handlers.

Simple insert you logging message into the method.

The `"stdout"` handler ensures that all log messages are printed to the
console.

``` r
library(wolf)
w <- wolf("readme")
w$add_handler("stdout")
w$info("Hello, wolf!")
#> 2026-02-03 23:51:22 [info] (readme) Hello, wolf!
#> Hello, wolf!
w$debug("This is a debug message.")
#> 2026-02-03 23:51:22 [debug] (readme) This is a debug message.
#> This is a debug message.
```

The `Wolf` class also has a `$log()` method, which accepts an expression
and picks up on conditions that are signaled.

The **message** below is still printed, but we’ve also stored the
message in the signalled condition.

``` r
hello <- function() {
  message("I'm inside a function")
  NULL
}

w$log(hello())
#> 2026-02-03 23:51:22 [message] (readme) I'm inside a function
#> I'm inside a function
try(w$log(df$column))
#> 2026-02-03 23:51:22 [error] (readme) object of type 'closure' is not subsettable
#> Error : object of type 'closure' is not subsettable
```

## Other logging packages

…

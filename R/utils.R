silent <- function(expr) {
  suppressWarnings(suppressMessages(
    capture.output(result <- eval(expr, envir = parent.frame()))
  ))
  result
}

# A package reference in a function argument default value.
# https://github.com/rstudio/packrat/issues/630
indirect <- function(x = emo::ji("see_no_evil")) {
  x
}

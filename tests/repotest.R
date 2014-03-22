library(repomania)
x <- readice()
x
##x1 <- readice(catalog$date, filename = sprintf("%s.sdat", tempfile()))

y <- readice(catalog$date[c(1, 5, 10)])

y


par(xaxs = "i", yaxs = "i")
p <- c(1, 2, 3)
plot(expand.grid(p ,p), type = 'p', pch = 20, cex = 10, col="red", xlim = c(-1, 5), ylim = c(xlim = c(-1, 4)), axes = F, main= "Check In")
text(1, 3, "1", col='white')
text(2, 3, "2", col='white')
text(3, 3, "3", col='white')
text(1, 2, "4", col='white')
text(2, 2, "5", col='white')
text(3, 2, "6", col='white')
text(1, 1, "7", col='white')
text(2, 1, "8", col='white')
text(3, 1, "9", col='white')

random_lib <- list(
  a <-list(c(1,2), c(1, 4), c(1, 5)),
  b <-list(c(2,1), c(2, 5), c(2, 3)),
  c <-list(c(3,2), c(3, 5), c(3, 6)),
  d <-list(c(4,1), c(4, 5), c(4, 7)),
  e <-list(c(5,2), c(5, 4), c(5, 6), c(5, 8)),
  f <-list(c(6,3), c(6, 5), c(6, 9)),
  g <-list(c(7,4), c(7, 5), c(7, 8)),
  h <-list(c(8,7), c(8, 5), c(8, 9)),
  i <-list(c(9,6), c(9, 5), c(9, 8))
)

random_generate <- function(x){
  repeat {
    m <- x
    n <- sample(random_lib[[m]], 1)[[1]][2]
    o <- sample(random_lib[[n]], 1)[[1]][2]
    p <- sample(random_lib[[o]], 1)[[1]][2]
    random_out <- c(m, n, o, p)
    if (sum(duplicated(random_out))==FALSE) {
      break
    }
  }
  return(random_out)
}

checkin <- random_generate(sample(1:9, 1))

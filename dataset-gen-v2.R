normalize <- function(x, b=FALSE) {
  x <- round(255*(x-min(x))/(max(x)-min(x)),0)
  if (b) x[x >= 245] <- 255
  return(x)
}

rotate_mtx <- function(x) t(apply(x, 2, rev))

flip_mtx <- function(x) x[,ncol(x):1]

display_pattern <- function(x) {
  image(
    z    = x,
    zlim = c(0,255),
    asp  = 1,
    col  = grey(0:23/24),
    axes = F
  )
}

display_grid <- function(d) {
  par(mfrow=c(6,8), mai=c(0.1,0.1,0.1,0.1))
  for (ii in 1:48) {
    display_pattern(matrix(unlist(d[sample(1:nrow(d),1),]), ncol=20))
  }
}

get_samples <- function(x, side=20) {
  mat <- matrix(NA, nrow=331*331*8, ncol=400)
  for (rr in 1:(nrow(x)-side+1)) {
    print(rr)
    for (cc in 1:(ncol(x)-side+1)) {
      x_1  <- x[rr:(rr+side-1), cc:(cc+side-1)]
      x_2  <- rotate_mtx(x_1)
      x_3  <- rotate_mtx(x_2)
      x_4  <- rotate_mtx(x_3)
      x_1f <- flip_mtx(x_1)
      x_2f <- flip_mtx(x_2)
      x_3f <- flip_mtx(x_3)
      x_4f <- flip_mtx(x_4)
      
      mat[(rr-1)*331*8+(cc-1)*8+1,1:400] <- matrix(x_1, nrow=1)
      mat[(rr-1)*331*8+(cc-1)*8+2,1:400] <- matrix(x_2, nrow=1)
      mat[(rr-1)*331*8+(cc-1)*8+3,1:400] <- matrix(x_3, nrow=1)
      mat[(rr-1)*331*8+(cc-1)*8+4,1:400] <- matrix(x_4, nrow=1)
      mat[(rr-1)*331*8+(cc-1)*8+5,1:400] <- matrix(x_1f, nrow=1)
      mat[(rr-1)*331*8+(cc-1)*8+6,1:400] <- matrix(x_2f, nrow=1)
      mat[(rr-1)*331*8+(cc-1)*8+7,1:400] <- matrix(x_3f, nrow=1)
      mat[(rr-1)*331*8+(cc-1)*8+8,1:400] <- matrix(x_4f, nrow=1)
    }
  }
  return(mat)
}

# Dots
a <- png::readPNG(source = "A2.png")[,,3]
a <- normalize(a)
mat_a <- get_samples(a)
df_a  <- as.data.frame(mat_a)
save(df_a, file = "a.RData")

# Lines
b <- png::readPNG(source = "B2.png")[,,3]
b <- normalize(b, b=TRUE)
mat_b <- get_samples(b)
df_b  <- as.data.frame(mat_b)
save(df_b, file = "b.RData")

# Zebra stripes
c <- png::readPNG(source = "C2.png")[,,3]
c <- normalize(c)
mat_c <- get_samples(c)
df_c  <- as.data.frame(mat_c)
save(df_c, file = "c.RData")
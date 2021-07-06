f <- function(x){
  return ((x^3 - x^2 + 5*x +5*sin(2*x + x^2) * cos(2*x + x^2) + 3)*exp(-x))
}


#task 1. Right rectangles method.

right_tectangles_square <- function(f, a, b, n){

  square = 0
  
  h = (b - a) / n

  for (i in c(0:n-1)){
    
  square <- square +f(a + h*i) * h
  
  }
  
  return (square)
}

#task 2. Center rectangles method.
center_rectangles_square <- function(f, a, b, n){

  square <-  0
  
  h = (b - a) / n
  
  for (i in c(0:n-1)){
  square <- square + f(a + h*i + h/2) * h
  }
  return (square)
}


#task 3. Trapezes method.

trapeze_square <- function(f, a, b, n){

  square <- 0

    h = (b - a) / n
  
    for (i in c(0:n-1)){
    square <- square + (f(a + h*i) + f(a + h*(i + 1)))
  }
  
  return (square*(h/2))
}

#task 4. Simpson's method.

simpson_square <- function(f, a, b, n) {
  
  h <- (b - a) / n
  
  xj <- seq.int(a, b, length.out = n + 1)
  xj <- xj[-1]
  xj <- xj[-length(xj)]
  square <- (h / 3) * (f(a) + 2 * sum(f(xj[seq.int(2, length(xj), 2)])) + 4 * sum(f(xj[seq.int(1, length(xj), 2)])) + f(b))
  
  return(square)
  
}

file <- file('Integrals.txt', open='w')
count <- 0

input_or_file <- as.integer(readline('1 - input, \n2-file -> '))
if (input_or_file == 1){
while (TRUE){
  count <- count + 1
  
  
  #Parameters were chosen by User
  method <- as.integer(readline(prompt = '1 - right rectangles method,
                                \n2-center rectangles,
                                \n3-trapeze method,
                                \n4-Simpson method,
                                \n5-exit.
                                Choose the number between 1 and 5.'))
  
  a <- as.numeric(readline('left cut -> '))
  b <- as.numeric(readline('right cut ->'))
  n <- as.integer(readline('number of divisions ->'))
  
  switch (method,
    write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', " Rigth Rectangle Square ", ' result :', right_tectangles_square(f, a, b, n), '\n--------------------------------'), file = 'Integrals.txt', append = TRUE),
    write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', " Center Rectangle Square ", ' result :', center_rectangles_square(f, a, b, n), '\n--------------------------------'), file = 'Integrals.txt', append = TRUE),
    write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', " Trapeze Square", ' result :', trapeze_square(f, a, b, n), '\n--------------------------------'),file = 'Integrals.txt', append = TRUE),
    write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', ' Simpson Square', ' result :', simpson_square(f, a, b, n), '\n--------------------------------'), file = 'Integrals.txt', append = TRUE),
    break
  )

}
}else{
    file <- file(as.character(readline('filename -> ')), open='r')
    iter_data <- as.numeric(readLines(file))
    while (TRUE){
      count <- count + 1
      
      
      #Parameters were chosen by User
      method <- iter_data[1]
      
      a <- iter_data[2]
      b <- iter_data[3]
      n <- iter_data[4]
      
      switch (method,
              write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', " Rigth Rectangle Square ", ' result :', right_tectangles_square(f, a, b, n), '\n--------------------------------'), file = 'Integrals.txt', append = TRUE),
              write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', " Center Rectangle Square ", ' result :', center_rectangles_square(f, a, b, n), '\n--------------------------------'), file = 'Integrals.txt', append = TRUE),
              write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', " Trapeze Square", ' result :', trapeze_square(f, a, b, n), '\n--------------------------------'),file = 'Integrals.txt', append = TRUE),
              write(c('ID: ', count, ' left cut: ', a, ' right cut: ', b, ' number of divisions: ', n, ' used method: ', ' Simpson Square', ' result :', simpson_square(f, a, b, n), '\n--------------------------------'), file = 'Integrals.txt', append = TRUE),
              break
      )
  }
}



# Iterations method



?stopifnot

size_of_matrix <- as.integer(readline('Input size of matrix ->'))
stopifnot(size_of_matrix>2, sqrt(size_of_matrix) %% 1 == 0)


oper_matrix <- vector()

for (i in c(1:size_of_matrix)){
  oper_matrix[i] <- as.numeric(readline())
  stopifnot(class(oper_matrix[i]) == 'numeric')
}


oper_matrix <- matrix(oper_matrix, ncol = sqrt(size_of_matrix))

free_vector <- c()

for (i in c(1:(sqrt(size_of_matrix)))){
  free_vector[i] <- as.numeric(readline())
  stopifnot(class(free_vector[i]) == 'numeric')
}

n_iter <- as.integer(readline('input the number of iterations -> '))
stopifnot(n_iter>=0)

eps <- as.numeric(readline('Input accuracy -> '))
stopifnot(eps>0)

u0 <- rnorm(sqrt(size_of_matrix))

maxaf <- max(oper_matrix, free_vector)
oper_matrix <- oper_matrix/maxaf
free_vector <- free_vector/maxaf
B_matrix <- diag(rep(1,ncol(oper_matrix))) - oper_matrix
u1 <- as.vector(B_matrix%*%u0 + free_vector)
for (i in c(1:n_iter)){
  if (max(abs(u1 - u0)) > eps){
    u0 <- u1
    u1 <- B_matrix%*%u0 + free_vector
  }
  else break
}
print((as.vector(u1)))






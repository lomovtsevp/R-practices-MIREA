#task 1
x_square <- function(x){
  return (x^2)
}

args <- seq(-5, 5,  0.1)
plot(args, x_square(args), type = 'l', lwd=5, col='pink')
text(0,25, Sys.time())

#task 2
marks <- matrix(0,nrow = 4, ncol = 3)
for (i in 1:nrow(marks)){
  for (j in 1:ncol(marks))
    marks[i, j] <- as.integer(readline())
}

colnames(marks) <- c('Math', 'English', 'Physics')
row.names(marks) <- c('Ivanov', 'Petrov', 'Sidorov', 'Nosov')
pie(marks[2,] , main = 'Marks', col=c('red','green', 'blue'))

#task 3

hyperbolic_f <- function(x){
  return ((3*x^2 - 30*x + 74.417)/(x-5))
}

args_1 <- seq(-10,10, 0.1)
roots <- c((30 - sqrt((-30)^2 - 4*3*74.417))/(2*3), (30 + sqrt((-30)^2 - 4*3*74.417))/(2*3))
roots
  
plot(args_1, hyperbolic_f(args_1), type = 'l', lwd=10, col='brown')

#task 4

cubical_fun <- function(x) (x^3 - 18*x^2 + 106.25*x -205.5)
plot(x_args, cubical_fun(x_args), type = 'l', col = 'red', lwd=10)

# task 5

ellipse <- function(x, a=1, b=1){
  y_1 <- sqrt(b^2* (1-(x^2/a^2)))
  y_2 <- -sqrt((b^2*(1-(x^2/a^2))))
  df <- cbind('y_1' = y_1, 'y_2' = y_2)
  return(df)
}

x_args <- seq(-5,5, 0.1)
y_args <- seq(-5,5, 0.1)

plot(ellipse(x_args)[,1],x_args, type ='l', xlim = c(-3,3), lwd=10, col='blue')
lines(ellipse(x_args)[,2], x_args,  type= 'l',lwd=10, col='blue')

#task 6

hyperbola <- function(x, a=3, b=4) {
 y_1 <- sqrt((b^2*(1+(x^2/a^2))))
 y_2 <- -sqrt((b^2*(1+(x^2/a^2))))
 return (cbind('y_1' = y_1, 'y_2'=y_2))
}

plot(hyperbola(x_args)[,1], x_args, type = 'l', xlim= c(-10,10), ylim= c(-10,10), lwd=10, col='violet')
lines(hyperbola(x_args)[,2], x_args, lwd=10, col='violet')

#task 7

hyperboloid <- function(x, y, a=3, b=4, c=5) {
  z_1 <- sqrt(c^2 * ((x^2/a^2) + (y^2/b^2) - 1))
  return (z_1)
}

z <- outer(x_args, y_args,hyperboloid)
z_2 <- -outer(x_args, y_args,hyperboloid)

persp(x_args, y_args, z, col='purple')
persp(x_args, y_args, z_2, col='orange')


#task 8 
tor <- function (x, y, r=3, R=4){
  return (sqrt(-x^2-y^2-R^2-r^2+4*R^2*(x^2+y^2)))
}

z_tor <- outer(x_args, y_args, tor)

persp(x_args, y_args ,z_tor, col='cyan')
persp(x_args, y_args, -z_tor, col='pink')

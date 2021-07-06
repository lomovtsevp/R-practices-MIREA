#Practice 2
# task 1
matrix <- matrix(data=3, nrow=3, ncol=4, byrow=TRUE)
print(matrix)
matrix[1, 3] <- 4
matrix[2, 1] <- 1
matrix[3, 2] <- NA
matrix[3, 4] <- 1
print(matrix)


# task 2
a <- c(1,3,4,9,NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9,10,13,1,20)
cols <- cbind(a,b,c)
rows <- rbind(a,b,c)
print(cols)
print(rows)

# iterations method
# task 1
A <- diag(c(4,9), 2, 2)
rownames(A) <- c('eq1', 'eq2')
colnames(A) <- c('x1', 'x2')

# task 2
ev <- eigen(A)
print(ev$values)

#task 3
B <- diag(nrow = 2, ncol = 2) - A
print(B)

#task 4
f <- c(4, 2)
u <- c(0.2, -0.3)

# task 5
u_result <- solve(A, f)
print(u_result)

# task 6
u1 <- B%*%u + f
u2 <- B%*%u1 + f
u3 <- B%*%u2 + f
u4 <- B%*%u3 + f
u5 <- B%*%u4 + f
u6 <- B%*%u5 + f
u7 <- B%*%u6 + f

# task 7

print(c(u7, u_result))

# task 8

A <- A/max(A)
print(A)
f <- f/max(A)
print(f)

# task 9

ev <- eigen(A)
print(ev$values)

B <- diag(nrow = 2, ncol = 2) - A
print(B)

u_result2 <- solve(A, f)
print(u_result)

u11 <- B%*%u + f
u22 <- B%*%u11 + f
u33 <- B%*%u22 + f
u44 <- B%*%u33 + f
u55 <- B%*%u44 + f
u66 <- B%*%u55 + f
u77 <- B%*%u66 + f


print(c(u77, u_result2))


# part 3

# task 1
step <- 1 # Шаг сетки

dekart_begin <- -5 # Начало сетки

dekart_end <- 5 # Конец сетки

# Задание сеточной поверхности

x <- seq(from = dekart_begin, to = dekart_end, by = step)

y <- x

# Задание двумерной функции на координатной сетке

surface_matrix <- outer(X = x,
                        
                        Y = y,
                        
                        FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))

dimnames(surface_matrix) <- list(x, y)

print(as.character(c('number of matrix elements', length(surface_matrix),
                          '“number of rows:', length(surface_matrix[,1]),
                          'number of columns', length(surface_matrix[1,]),
                          'sum of main diag elements', sum(diag(surface_matrix)),
                          'sum of middle row elements:', rowSums(surface_matrix)[(length(surface_matrix) + 1)/2], 
                          'sum of middle column elements:',colSums(surface_matrix)[(length(surface_matrix) + 1)/2],
                          'row sums:', rowSums(surface_matrix),
                          'col sums:', colSums(surface_matrix))))

# part 3 cars

cars_matrix <- as.matrix(cars)

cars_speed <- cbind(1,cars_matrix[,1])

cars_dist <- cars_matrix[,2]

alpha <- solve((t(cars_speed)%*%cars_speed)) %*% t(cars_speed) %*% cars_dist
alpha <- as.vector(alpha)
alpha_c <- alpha[1]
alpha_x <- alpha[2]

cars_speed_lm <- cars_matrix[,1]
cars_dist_lm <-  alpha_c + cars_speed_lm * alpha_x
dist_residuals <- cars_dist_lm - cars_dist
print('mean', mean(dist_residuals))
print('std', sd(dist_residuals))
print(cars_dist_lm)

plot(cars_dist_lm, cars_dist)

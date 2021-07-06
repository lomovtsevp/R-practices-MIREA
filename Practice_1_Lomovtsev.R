# PART 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#task 1 
#swap variables x and y

x <- 1
y <- 3

x <- x + y
y <- x - y
x <- x - y

print(c(x,y))

#task 2

x <- 3.5
y <- "2,6"
z <- 1.78
h <- TRUE

print (c(class(x), class(y), class(z), class(h)))

h <- as.numeric(h) 
print(h)

y <- sub(",",".", y)
y <- as.numeric(y) 
print(y)

x <- as.character(x)
print(x)

#task3

dohod <- 1573
dohod <- log(dohod)
print(dohod)

# PART 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# task 1

vector <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)

print(vector[1])
print(vector[length(vector)])
print(vector[3:5])
print(vector[vector == 2])
print(vector[vector > 4])
print(vector[vector %% 3 == 0])
print(vector[(vector > 4) & (vector %% 3 == 0)])
print(vector[(vector < 1) | (vector > 5)])
print(vector[vector == 0])
print(vector[(vector >= 2) & (vector <= 8)])

#task 2

vector_NA <- c(3,4,5,6)
print(vector_NA)

vector_NA[length(vector_NA)] <- NA
print(vector_NA)

#task 3

vector_NA_indexes <- c(NA, 2, NA, NA, 3)
print(which(is.na(vector_NA_indexes)))

#task 4

count_of_na_vector <- c(NA, 2, 3, NA)

print(length(count_of_na_vector[is.na(count_of_na_vector)]))

#task 5


respondent_ids <- c(1,2,3,4,5,6,7,8,9,10,11,1,1,1,1,1,2,3,4,5,6,7) # на 100 тоже будет работать
unique_ids <- unique(respondent_ids) # отбираем униальные id
print(unique_ids)

#task 6
#variance 1
countries <- c('France', 'Italy', 'Spain')
year <- seq(2015, 2020, 1)

#variance 2
#countries <- c('France', 'France', 'France', 'France', 'France', 'Italy', 'Italy', 'Italy', 'Italy', 'Italy', 'Spain', 'Spain', 'Spain', 'Spain', 'Spain')
#year <- c(2019, 2020, 2020, 2018, 2017, 2019, 2020, 2020, 2018, 2017, 2019, 2020, 2020, 2018, 2017)


#task 7 

income <- c(10000, 32000, 28000, 150000, 65000, 1573)

mean_value <- sum(income) / length(income)

print(mean_value)

income <- ifelse(income > mean_value, 1, 0)

print(income)

#task 8

#  N = 5, p = 2.87

coords <- as.numeric(readLines(file('coords.txt', open='r')))

p <- 2.87

l_norm = sum(abs(coords ^ p)) ^ (1/p)

print(l_norm) 

fileConn <- file("result.txt", open = 'w')

writeLines(as.character(l_norm), fileConn)

close(fileConn)

# task 9

coords_2 <- as.numeric(readLines(file('coords.txt', open='r')))
#print(coords_2)

for (i in c(1:length(coords_2)-1)){
  coords_2[i] <- coords_2[i+1] - coords_2[i]
}

diff_1 = coords_2

diff_2 = diff_1

for (i in c(1:length(diff_1)-1)){
  diff_2[i] <- diff_1[i+1] - diff_1[i]
}


file_result <- file("diff_vectors.txt", open = 'w')

writeLines(as.character(c('diff_1:',diff_1,'diff_2', diff_2)), file_result)

close(file_result)

download.file('https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/ECG_yurchenkov.txt', destfile = 'yurchenkov.txt')

data_yurchenkov <- readLines(file('yurchenkov.txt', open='r'))

for (i in c(1:length(data_yurchenkov))){
  if (data_yurchenkov[i] == 'Время(мс)	0	1	2	3	4')
    break
  else {
    data_yurchenkov <- data_yurchenkov[-i]
  }
}

final_data <- read.csv('yurchenkov.txt', sep='\t', skip = 46)

colnames(final_data) <- c('time', 'v1', 'v2', 'v3', 'v4', 'v5')
for (i in 1:ncol(final_data)){
  final_data[,i] <- gsub(',', '.', final_data[,i])
  final_data[,i] <- as.numeric(final_data[,i])
}
final_data <- na.omit(final_data)

num_of_stages <- function(x){
  count_stages <- 0
  for (i in (1:length(x))){
    if (x[i] == 0){
      count_stages <- count_stages + 1
    }
  }
  return (count_stages)
}

get_ms_of_exp <- function(x){
  vector_of_ms <- vector()
  for (i in (2:(length(x)-1))){
    if (x[i+1] == 0){
      vector_of_ms <- append(vector_of_ms, x[i])
    }
  }
  return (vector_of_ms)
}


table(cut(x = as.integer(final_data[,1]), breaks = seq(1, length(final_data), (length(final_data) - 1)/ncol(final_data))))

number_stages <- num_of_stages(final_data[,1])

ends_of_exps <- get_ms_of_exp(final_data[,1])

library(matrixStats)

#describing statistics

colSds(as.matrix(final_data))
colVars(as.matrix(final_data))
colSums(as.matrix(final_data))
colMeans(as.matrix(final_data))
colMedians(as.matrix(final_data))

summary(final_data)

#plotting
library(ggplot2)
ggplot(final_data[1:250,], aes(time, v1))+
  geom_point(size=3)+
  geom_area(fill='green')

ggplot(final_data[1:250,], aes(time, v2))+
  geom_point(size=2)+
  geom_area(fill = 'red')
split_data <- split(final_data, cumsum(final_data[,1] == '0'))

for (i in split_data){
  print(summary(i))
}

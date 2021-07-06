# task 1

info <- list(c("Jane", "Michael", "Mary", "George"), c(8, 6, 28, 45), c(0, 1, 0, 1))

print(info[[1]][2])
print(info[[3]])

info <- list(names=c("Jane", "Michael", "Mary", "George"), age=c(8, 6, 28, 45), gender=c(0, 1, 0, 1))

print(info$names)

info$drinks <- c('juice', 'tea', 'rum', 'coffee')

print(info$drinks)

# task 2

index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
I <- unlist(strsplit(index, ';'))
I <- as.numeric(sub(",",".", I))
print(I)

# task 3

##install.packages(c('dplyr', 'tidyr', 'stringr'))


# task 4

df <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

length(df)

print(colnames(df))

str(df)

library('tidyr')

df <- unite(df, region, c(Province.State, Country.Region))

library(matrixStats)
df_stats <- data.frame(region=df[,1], Lat=df[,2], Long=df[,3], sumx=rowSums(df[c(4:ncol(df))]), meanx=rowMeans(df[c(4:ncol(df))]), sdx=rowSds(as.matrix(df[,c(4:ncol(df))]), na.rm=TRUE))

library('dplyr')
df_t<- t(as.matrix(df[, 4:ncol(df)]))
colnames(df_t) <- df$region
rownames(df_t)

rownames(df_t) <- gsub('X', '', rownames(df_t))
rownames(df_t) <- gsub('\\.','-', rownames(df_t))
rownames(df_t) <- format(as.Date(rownames(df_t), '%m-%d-%y'), "%Y-%m-%d")
df_t <- as.data.frame(df_t)

write.csv(df_t,'covid.csv')
write.table(df_t,'covid.txt')

#install.packages('xlsx')
library('xlsx')
write.xlsx(df_t,'covid.xlsx',sheetName = 'sheet1')

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

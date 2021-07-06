covid_data <- df_t
covid_data

library(ggplot2)
colnames(covid_data) <- gsub('_','', colnames(covid_data))

#plotting geom_point
ggplot(covid_data, mapping = aes(x = rownames(covid_data),y = Russia))+
  geom_point(col='yellow', size=5)+
  labs(x='Время с начала пандемии', title = 'Динамика роста заболеваемости в России', subtitle = paste("Данные от", min(rownames(covid_data))))
ggplot(covid_data, mapping = aes(x = rownames(covid_data),y = Austria))+
  geom_point(size=5, col='purple')+
  labs(x='Время с начала пандемии', title = 'Динамика роста заболеваемости в Австрии', subtitle = paste("Данные от", min(rownames(covid_data))))
ggplot(covid_data, mapping = aes(x = rownames(covid_data),y = Netherlands))+
  geom_point(col='blue', size=5)+
  labs(x='Время с начала пандемии', title = 'Динамика роста заболеваемости в Нидерландах', subtitle = paste("Данные от", min(rownames(covid_data))))

#plotting hist 
ggplot(data = covid_data, mapping = aes(Japan)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15,
                 color = "green", 
                 fill = "white") +
  geom_density(fill="black", alpha = 0.2)+
  labs(x='Япония', y='Плотность')
  
ggplot(data = covid_data, mapping = aes(Zambia)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15,
                 color = "red", 
                 fill = "pink") +
  geom_density(fill="blue", alpha = 0.2)+
  labs(x='Замбия', y='Плотность')

ggplot(data = covid_data, mapping = aes(France)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15,
                 color = "orange", 
                 fill = "yellow") +
  geom_density(fill="blue", alpha = 0.1)+
  labs(x='Франция', y='Плотность')

dev.off()

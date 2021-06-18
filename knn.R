#######################
## KNN
#######################

library(dplyr)
uri <- "https://raw.githubusercontent.com/ctruciosm/ctruciosm.github.io/master/datasets/knn_exemplo.txt"
knn_exemplo <- read.table(uri, header = TRUE)
glimpse(knn_exemplo)


library(ggplot2)
ggplot(knn_exemplo) + 
  geom_point(aes(x = V1, y = V2, color= Grupo))


# Carregando os pacotes
library(tidymodels)
library(kknn)
# Especificamos o modelo
knn_spec <- nearest_neighbor(neighbors = 14, p = 2) %>%
  set_engine("kknn") %>% 
  set_mode("classification") 
# Ajustamos o modelo (fit)
knn_fit <- knn_spec %>% 
  fit(factor(Grupo) ~ V1 + V2, data = knn_exemplo)
# Classificando new_obs (predict)
new_obs <- data.frame(V1 = 0.7, V2 = 0.4)
yhat <- predict(knn_fit, new_data = new_obs)
yhat




#### Case

# Importando os dados
uri <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
data <- read.table(uri, sep=",")
glimpse(data)

data <- data %>% select(-V1) %>% rename(Diagnostico = V2) %>% na.omit()
dim(data)
glimpse(data)

# Spliting data
set.seed(1234)
data_split <- initial_split(data, prop = 3/4, strata = Diagnostico)
train_data <- training(data_split)
test_data <- testing(data_split)
x_test_data <- test_data %>% select(-Diagnostico)
y_test_data <- test_data %>% select(Diagnostico)


library(tidymodels)
library(kknn)
# Especificamos o modelo
knn_spec <- nearest_neighbor(neighbors = 5) %>%
  set_engine("kknn") %>% 
  set_mode("classification") 
# Ajustamos o modelo (fit)
knn_fit <- knn_spec %>% 
  fit(factor(Diagnostico) ~ ., data = train_data)
# Clasificando as novas observações (predict)
yhat_test <- predict(knn_fit, new_data = x_test_data)
table(yhat_test$.pred_class,y_test_data$Diagnostico)


table(train_data$Diagnostico)
m = floor(sqrt(mean(table(train_data$Diagnostico))))
knn_spec <- nearest_neighbor(neighbors = m) %>% set_engine("kknn") %>% set_mode("classification") 
# Ajustamos o modelo (fit)
knn_fit <- knn_spec %>% fit(factor(Diagnostico) ~ ., data = train_data)
yhat_test <- predict(knn_fit, new_data = x_test_data)
table(yhat_test$.pred_class,y_test_data$Diagnostico)




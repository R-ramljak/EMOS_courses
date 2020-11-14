## Marco Ramljak, Assignment Statistical Data Editing Q3

library(tidyverse)
library(VIM)

data <- read.csv2("3 Statistical Data Editing and Imputation/EMOS Exercise.csv")


# 3 kinds of imputations for x1 and x2
# nearest neighbor hot deck with all fully observed auxiliary variables (univarite, x1) --> x1.imp.1
# nearest neighbor hot deck with all fully observed auxiliary variables (univarite, x2) --> x2.imp.1
# Regression imputation with all fully observed auxiliary variables (univarite, x1) --> x1.imp.2
# Regression imputation with all fully observed auxiliary variables (univarite, x2) --> x2.imp.1
# nearest neighbor hot deck with all fully observed auxiliary variables (multivariate, x1 & x2) --> x1.imp.3
# Regression imputation with all fully observed auxiliary variables (multivariate, x1 & x2) --> x2.imp.3

data.normalized <- data %>% 
  rownames_to_column("id") %>% 
  mutate_at(vars(-id, -X1, -X2), ~ . / sum(.)) %>% 
  mutate_at(vars(X1, X2), as.numeric)
  

aux.var <- paste0("X", 3:8)
formula.x1 <- as.formula(paste("X1 ~ ", paste(aux.var, collapse = "+")))
formula.x2 <- as.formula(paste("X2 ~ ", paste(aux.var, collapse = "+")))

#kNN
x1.imp.1 <- kNN(data = data.normalized, variable = "X1", dist_var = aux.var) %>% select(id, x1.imp.1 = X1)
x2.imp.1 <- kNN(data = data.normalized, variable = "X2", dist_var = aux.var) %>% select(id, x2.imp.1 = X2)
x.knn.imp.3 <- kNN(data = data.normalized, variable = c("X1", "X2"), dist_var = aux.var) %>% select(id, x1.imp.2 = X1, x2.imp.2 = X2)

x1.imp.2 <- regressionImp(formula = formula.x1, data = data.normalized) %>% select(id, x1.imp.3 = X2)
x2.imp.2 <- regressionImp(formula = formula.x2, data = data.normalized) %>% select(id, x2.imp.3 = X2)
# how to do both?

final <- data.normalized %>% 
  left_join(x1.imp.1, by = "id") %>% 
  left_join(x2.imp.1, by = "id") %>% 
  left_join(x1.imp.2, by = "id") %>% 
  left_join(x2.imp.2, by = "id") %>% 
  left_join(x.knn.imp.3, by = "id") %>% 
  # left_join(x.knn.imp.3, by = "id") %>% 
  select(id, X1, X2, matches("x1|x2")) %>% 
  summarise_at(vars(-id), mean, na.rm = T) %>% 
  pivot_longer(cols = starts_with("x"), names_to = "method", values_to = "value") %>% 
  arrange(method) %>% 
  mutate(description = case_when(method == "X1" ~ "Unimputed",
                                 method == "X2" ~ "Unimputed",
                                 method == "x1.imp.1" ~ "KNN univariate",
                                 method == "x2.imp.1" ~ "KNN univariate",
                                 method == "x1.imp.3" ~ "KNN multivariate",
                                 method == "x2.imp.3" ~ "KNN multivariate",
                                 method == "x1.imp.2" ~ "Regression univariate",
                                 method == "x2.imp.2" ~ "Regression univariate"))
  


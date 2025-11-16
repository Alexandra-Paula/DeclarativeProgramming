install.packages("dotenv")
library(dotenv)

dotenv::load_dot_env(file = ".env")

dataset_path <- Sys.getenv("/Users/alexandramanea/OneDriveBackup/UTM/anul\ 3/sem1/PD/R/Lab4_Data_processing_analysis/auto-mpg.data")

cars_data <- read.table(dataset_path, header = FALSE, sep = "", quote = "\"", stringsAsFactors = FALSE)

student_data <- read.table("/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4_Data_processing_analysis/student-por.csv",
                           header = TRUE,
                           sep = ";",
                           stringsAsFactors = FALSE)

head(cars_data, 15)
colnames(cars_data) <- c("mpg","cyl","disp","hp","wt","qsec","model_year","origin","car_name")

# Exercise 1
str(cars_data)
sapply(cars_data, class)

str(student_data)
sapply(student_data, class)

# Exercise 2 Auto MPG
numeric_cols <- c("mpg","cyl","disp","hp","wt","qsec","model_year","origin")

for(col in numeric_cols){
  cars_data[[col]][cars_data[[col]] == "?"] <- NA
  cars_data[[col]] <- as.numeric(cars_data[[col]])
}

numeric_cols <- sapply(cars_data, is.numeric)

apply(cars_data[, numeric_cols], 2, mean, na.rm = TRUE)

apply(cars_data[, numeric_cols], 2, median, na.rm = TRUE)

apply(cars_data[, numeric_cols], 2, min, na.rm = TRUE)

apply(cars_data[, numeric_cols], 2, max, na.rm = TRUE)

apply(cars_data[, numeric_cols], 2, sd, na.rm = TRUE)

summary(cars_data[, numeric_cols])

# Exercice 2 Student
numeric_cols_logical <- sapply(student_data, is.numeric)

apply(student_data[, numeric_cols_logical], 2, mean, na.rm = TRUE)
apply(student_data[, numeric_cols_logical], 2, median, na.rm = TRUE)
apply(student_data[, numeric_cols_logical], 2, min, na.rm = TRUE)
apply(student_data[, numeric_cols_logical], 2, max, na.rm = TRUE)
apply(student_data[, numeric_cols_logical], 2, sd, na.rm = TRUE)

summary(student_data[, numeric_cols_logical])

# Exercise 3 Auto MPG
numeric_cols <- sapply(cars_data, is.numeric)
cars_numeric <- cars_data[, numeric_cols]

cor_matrix <- cor(cars_numeric, use = "complete.obs") 

print(cor_matrix)

write.csv(cor_matrix, file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/correlation_matrix.csv", row.names = TRUE)

# Exercice 3 Student
numeric_cols <- sapply(student_data, is.numeric)
student_numeric <- student_data[, numeric_cols]

cor_matrix <- cor(student_numeric, use = "complete.obs") 

print(cor_matrix)

write.csv(cor_matrix, file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/student_correlation_matrix.csv", row.names = TRUE)

# Exercise 4 Auto MPG
covariate_matrix <- cor_matrix

cor_table <- as.data.frame(as.table(covariate_matrix))

high_covariate <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)

print(high_covariate)

write.csv(high_covariate, file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/high_covariates.csv", row.names = FALSE)

# Exercise 4 Student
covariate_matrix <- cor_matrix

cor_table <- as.data.frame(as.table(covariate_matrix))

high_covariate <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)

print(high_covariate)

write.csv(high_covariate, file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/student_high_covariates.csv", row.names = FALSE)


# Exercise 5 Auto MPG
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("png")

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

analyse <- function(data) {
  # Selectăm doar coloanele numerice
  numeric_cols <- sapply(student_data, is.numeric)
  data_numeric <- student_data[, numeric_cols]
  
  # 1. Statistici descriptive
  stats <- data.frame(
    mean = apply(data_numeric, 2, mean, na.rm = TRUE),
    median = apply(data_numeric, 2, median, na.rm = TRUE),
    min = apply(data_numeric, 2, min, na.rm = TRUE),
    max = apply(data_numeric, 2, max, na.rm = TRUE),
    sd = apply(data_numeric, 2, sd, na.rm = TRUE)
  )
  print(stats)
  
  # 2. Matricea de corelație
  cor_matrix <- cor(data_numeric, use = "complete.obs")
  print(cor_matrix)
  
  # Salvăm matricea de corelație
  write.csv(cor_matrix, file = "correlation_matrix.csv", row.names = TRUE)
  
  # 3. Identificăm corelațiile puternice
  cor_table <- as.data.frame(as.table(cor_matrix))
  high_cor <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)
  print(high_cor)
  
  # Salvăm corelațiile mari
  write.csv(high_cor, file = "high_covariates.csv", row.names = FALSE)
  
  # 4. Boxplot-uri folosind ggplot2 + gridExtra
  plots <- lapply(names(data_numeric), function(col){
    ggplot(data, aes_string(y = col)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      ggtitle(paste("Boxplot pentru", col)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Aranjăm toate boxplot-urile într-o grilă
  do.call(grid.arrange, c(plots, ncol = 2))
}

analyse(student_data)

# Exercice 5 Student
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

analyse <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols]

  stats <- data.frame(
    mean = apply(data_numeric, 2, mean, na.rm = TRUE),
    median = apply(data_numeric, 2, median, na.rm = TRUE),
    min = apply(data_numeric, 2, min, na.rm = TRUE),
    max = apply(data_numeric, 2, max, na.rm = TRUE),
    sd = apply(data_numeric, 2, sd, na.rm = TRUE)
  )
  print(stats)
  
  cor_matrix <- cor(data_numeric, use = "complete.obs")
  print(cor_matrix)
  
  write.csv(cor_matrix, file = "correlation_matrix.csv", row.names = TRUE)
  
  cor_table <- as.data.frame(as.table(cor_matrix))
  high_cor <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)
  print(high_cor)
  
  write.csv(high_cor, file = "high_covariates.csv", row.names = FALSE)
  
  plots <- lapply(names(data_numeric), function(col){
    ggplot(data, aes_string(y = col)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      ggtitle(paste("Boxplot pentru", col)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  do.call(grid.arrange, c(plots, ncol = 2))
}

analyse(cars_data)

# Exercise 6 Auto MPG
numeric_cols <- sapply(cars_data, is.numeric)
cars_numeric <- cars_data[, numeric_cols]

pairs(cars_numeric,
      main = "Pairwise plot pentru variabilele numerice",
      pch = 19,          
      col = "blue") 

# Exercise 6 Student
numeric_cols <- sapply(student_data, is.numeric)
student_numeric <- student_data[, numeric_cols]

pairs(student_numeric,
      main = "Pairwise plot pentru variabilele numerice",
      pch = 19,          
      col = "blue") 

# Exercise 7 Auto MPG
install.packages("reshape2")
library(GGally)
library(ggplot2)
library(reshape2)
library(gridExtra)

numeric_cols <- sapply(cars_data, is.numeric)
cars_numeric <- cars_data[, numeric_cols]

pair_plot <- ggpairs(cars_numeric) +
  ggtitle("Pairwise plot for auto-mpg")

cars_melt <- melt(cars_numeric)
box_plot <- ggplot(cars_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  ggtitle("Boxplot for auto-mpg") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(grobs = list(ggmatrix_gtable(pair_plot), box_plot), ncol = 2)

# Exercise 7 Student

library(GGally)
library(ggplot2)
library(reshape2)
library(gridExtra)

numeric_cols <- sapply(student_data, is.numeric)
student_numeric <- student_data[, numeric_cols]

pair_plot_std <- ggpairs(student_numeric) +
  ggtitle("Pairwise plot for Student-por")

student_melt <- melt(student_numeric)
box_plot_std <- ggplot(student_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  ggtitle("Boxplot for student-por") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(grobs = list(ggmatrix_gtable(pair_plot_std), box_plot_std), ncol = 2)

# Exercise 8 Auto MPG
numeric_cols <- sapply(cars_data, is.numeric)

for(col in names(cars_data)[numeric_cols]){
  medie <- mean(cars_data[[col]], na.rm = TRUE)
  cars_data[[col]][is.na(cars_data[[col]])] <- medie
}

categorical_cols <- sapply(cars_data, is.character)

for(col in names(cars_data)[categorical_cols]){
  tab <- table(cars_data[[col]])
  mod <- names(tab)[which.max(tab)]
  cars_data[[col]][is.na(cars_data[[col]]) | cars_data[[col]] == ""] <- mod
}

sapply(cars_data, function(x) sum(is.na(x)))

# Exercise 8 Student 

numeric_cols <- sapply(student_data, is.numeric)

for(col in names(student_data)[numeric_cols]){
  medie <- mean(student_data[[col]], na.rm = TRUE)
  student_data[[col]][is.na(student_data[[col]])] <- medie
}

categorical_cols <- sapply(student_data, is.character)

for(col in names(student_data)[categorical_cols]){
  tab <- table(student_data[[col]])
  mod <- names(tab)[which.max(tab)]
  student_data[[col]][is.na(student_data[[col]]) | student_data[[col]] == ""] <- mod
}

sapply(student_data, function(x) sum(is.na(x)))

# Exercise 9 Auto MPG
set.seed(123)  

n <- nrow(cars_data)
indices <- sample(1:n) 

train_size <- floor(0.7 * n)
train_indices <- indices[1:train_size]
train_data <- cars_data[train_indices, ]

test_indices <- indices[(train_size + 1):n]
test_data <- cars_data[test_indices, ]

dim(train_data)  
dim(test_data)   

# Exercise 9 Student

set.seed(123)  

n <- nrow(student_data)
indices <- sample(1:n) 

train_size <- floor(0.8 * n)
train_indices <- indices[1:train_size]
train_data <- student_data[train_indices, ]

test_indices <- indices[(train_size + 1):n]
test_data <- student_data[test_indices, ]

dim(train_data)  
dim(test_data) 

# Exercise 10 Auto MPG
set.seed(123) 

n <- nrow(cars_data)
indices <- sample(1:n)  

sample_size <- floor(n / 3) 
total_used <- sample_size * 3  
indices <- sample(1:n)

sample1 <- cars_data[indices[1:sample_size], ]
sample2 <- cars_data[indices[(sample_size + 1):(2*sample_size)], ]
sample3 <- cars_data[indices[(2*sample_size + 1):total_used], ]

dim(sample1)  
dim(sample2)
dim(sample3)

# Exercise 10 Student
set.seed(123) 

n <- nrow(student_data)
indices <- sample(1:n)  

sample_size <- floor(n / 3) 
total_used <- sample_size * 3  

sample1 <- student_data[indices[1:sample_size], ]
sample2 <- student_data[indices[(sample_size + 1):(2*sample_size)], ]
sample3 <- student_data[indices[(2*sample_size + 1):total_used], ]

dim(sample1)  
dim(sample2)
dim(sample3)

# Exercice 11 Auto MPG
library(GGally)
library(ggplot2)
library(gridExtra)

analyse <- function(data) {
  # Select numeric columns
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols]
  
  # Statistici descriptive
  stats_mean <- apply(data_numeric, 2, mean, na.rm = TRUE)
  stats_median <- apply(data_numeric, 2, median, na.rm = TRUE)
  stats_min <- apply(data_numeric, 2, min, na.rm = TRUE)
  stats_max <- apply(data_numeric, 2, max, na.rm = TRUE)
  stats_sd <- apply(data_numeric, 2, sd, na.rm = TRUE)
  
  print(data.frame(mean = stats_mean,
                   median = stats_median,
                   min = stats_min,
                   max = stats_max,
                   sd = stats_sd))
  
  # Matrice de corelație
  cor_matrix <- cor(data_numeric, use = "complete.obs")
  print(cor_matrix)
  
  write.csv(cor_matrix, 
            file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/correlation_matrix.csv",
            row.names = TRUE)
  
  # Corelații mari
  cor_table <- as.data.frame(as.table(cor_matrix))
  high_cor <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)
  print(high_cor)
  
  write.csv(high_cor, 
            file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/high_covariates.csv",
            row.names = FALSE)
  
  # Pairplot
  pair_plot <- ggpairs(data_numeric) +
    ggtitle("Pairwise plot - auto-mpg")
  
  # Boxplot-uri
  box_plots <- lapply(colnames(data_numeric), function(col) {
    ggplot(data_numeric, aes_string(x = "''", y = col)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      labs(title = paste("Boxplot pentru", col), x = "", y = col) +
      theme_minimal()
  })
  
  # Aranjează boxplot-urile într-o grilă
  box_grid <- do.call(gridExtra::grid.arrange, c(box_plots, ncol = 2))
  
  # Afișează pairplot și boxplot-uri
  print(pair_plot)
  print(box_grid)
}

# Apel funcție
analyse(cars_data)

# Exercice 11 Student
library(GGally)
library(ggplot2)
library(gridExtra)

analyse <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols]
  
  stats_mean <- apply(data_numeric, 2, mean, na.rm = TRUE)
  stats_median <- apply(data_numeric, 2, median, na.rm = TRUE)
  stats_min <- apply(data_numeric, 2, min, na.rm = TRUE)
  stats_max <- apply(data_numeric, 2, max, na.rm = TRUE)
  stats_sd <- apply(data_numeric, 2, sd, na.rm = TRUE)
  
  print(data.frame(mean = stats_mean,
                   median = stats_median,
                   min = stats_min,
                   max = stats_max,
                   sd = stats_sd))
  
  cor_matrix <- cor(data_numeric, use = "complete.obs")
  print(cor_matrix)
  
  write.csv(cor_matrix, 
            file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/student_correlation_matrix.csv",
            row.names = TRUE)
  
  cor_table <- as.data.frame(as.table(cor_matrix))
  high_cor <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)
  print(high_cor)
  
  write.csv(high_cor, 
            file = "/Users/alexandramanea/OneDriveBackup/UTM/anul 3/sem1/PD/R/Lab4/student_high_covariates.csv",
            row.names = FALSE)
  
  pair_plot <- ggpairs(data_numeric) +
    ggtitle("Pairwise plot - student - por")
  
  box_plots <- lapply(colnames(data_numeric), function(col) {
    ggplot(data_numeric, aes_string(x = "''", y = col)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      labs(title = paste("Boxplot pentru", col), x = "", y = col) +
      theme_minimal()
  })
  
  box_grid <- do.call(gridExtra::grid.arrange, c(box_plots, ncol = 2))
  
  print(pair_plot)
  print(box_grid)
}

analyse(student_data)

# Exercice 12 Auto MPG
library(ggplot2)
library(GGally)
library(gridExtra)

analysis_by_class <- function(data, class_col) {
  # Verifică dacă coloana există
  if (!class_col %in% colnames(data)) {
    stop("Coloana specificată nu există în dataset.")
  }
  
  classes <- unique(data[[class_col]])
  
  for (cls in classes) {
    cat("\n==============================\n")
    cat("Analiză pentru clasa:", cls, "\n")
    cat("==============================\n")
    
    data_subset <- data[data[[class_col]] == cls, ]
    
    # Select numeric columns
    numeric_cols <- sapply(data_subset, is.numeric)
    data_numeric <- data_subset[, numeric_cols]
    
    # Statistici descriptive
    stats_mean <- apply(data_numeric, 2, mean, na.rm = TRUE)
    stats_median <- apply(data_numeric, 2, median, na.rm = TRUE)
    stats_min <- apply(data_numeric, 2, min, na.rm = TRUE)
    stats_max <- apply(data_numeric, 2, max, na.rm = TRUE)
    stats_sd <- apply(data_numeric, 2, sd, na.rm = TRUE)
    
    print(data.frame(mean = stats_mean,
                     median = stats_median,
                     min = stats_min,
                     max = stats_max,
                     sd = stats_sd))
    
    # Matrice de corelație
    cor_matrix <- cor(data_numeric, use = "complete.obs")
    print(cor_matrix)
    
    # Corelații mari
    cor_table <- as.data.frame(as.table(cor_matrix))
    high_cor <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)
    if (nrow(high_cor) > 0) {
      cat("Corelații mari (>0.7):\n")
      print(high_cor)
    } else {
      cat("Nu există corelații mari.\n")
    }
    
    # Pairplot
    pair_plot <- ggpairs(data_numeric) +
      ggtitle(paste("Pairwise plot - clasa", cls))
    
    # Boxplot-uri
    box_plots <- lapply(colnames(data_numeric), function(col) {
      ggplot(data_numeric, aes_string(x = "''", y = col)) +
        geom_boxplot(fill = "lightblue", color = "darkblue") +
        labs(title = paste("Boxplot pentru", col), x = "", y = col) +
        theme_minimal()
    })
    
    # Afișează pairplot și boxplot-uri
    print(pair_plot)
    do.call(gridExtra::grid.arrange, c(box_plots, ncol = 2))
  }
}

analysis_by_class(cars_data, "cyl")  

# Exercice 12 Student
library(ggplot2)
library(GGally)
library(gridExtra)

analysis_by_class_student <- function(data, class_col) {
  if (!class_col %in% colnames(data)) {
    stop("Coloana specificată nu există în dataset.")
  }
  
  classes <- unique(data[[class_col]])
  
  for (cls in classes) {
    cat("Analiză pentru clasa:", cls, "\n")
    
    data_subset <- data[data[[class_col]] == cls, ]
    
    numeric_cols <- sapply(data_subset, is.numeric)
    data_numeric <- data_subset[, numeric_cols]
    
    stats_mean <- apply(data_numeric, 2, mean, na.rm = TRUE)
    stats_median <- apply(data_numeric, 2, median, na.rm = TRUE)
    stats_min <- apply(data_numeric, 2, min, na.rm = TRUE)
    stats_max <- apply(data_numeric, 2, max, na.rm = TRUE)
    stats_sd <- apply(data_numeric, 2, sd, na.rm = TRUE)
    
    print(data.frame(mean = stats_mean,
                     median = stats_median,
                     min = stats_min,
                     max = stats_max,
                     sd = stats_sd))
    
    cor_matrix <- cor(data_numeric, use = "complete.obs")
    print(cor_matrix)
    
    cor_table <- as.data.frame(as.table(cor_matrix))
    high_cor <- subset(cor_table, abs(Freq) > 0.7 & Freq != 1)
    if (nrow(high_cor) > 0) {
      cat("Corelații mari (>0.7):\n")
      print(high_cor)
    } else {
      cat("Nu există corelații mari.\n")
    }
    
    pair_plot <- ggpairs(data_numeric) +
      ggtitle(paste("Pairwise plot - clasa", cls))
    
    box_plots <- lapply(colnames(data_numeric), function(col) {
      ggplot(data_numeric, aes_string(x = "''", y = col)) +
        geom_boxplot(fill = "lightblue", color = "darkblue") +
        labs(title = paste("Boxplot pentru", col), x = "", y = col) +
        theme_minimal()
    })
    
    print(pair_plot)
    do.call(gridExtra::grid.arrange, c(box_plots, ncol = 2))
  }
}

analysis_by_class(student_data, "cyl")  


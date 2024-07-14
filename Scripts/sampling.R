library(readr)
library(dplyr)
library(stringr)

# Set working directory
setwd("C:/Users/Pc/Desktop/Redes/Data")

# Read data from CSV files
animelist <- read_csv("animelist.csv")
anime <- read_csv("anime.csv")

# Display structure of the 'anime' dataframe
str(anime)

# Filter out adult content and select specific columns
anime <- anime %>%
  select(MAL_ID, Name, Score, Genres) %>%
  filter(!str_detect(Genres, "\\Hentai\\b")) %>%
  as.data.frame()

# Display structure of the 'animelist' dataframe
str(animelist)

# Group 'animelist' by 'anime_id' and count occurrences for watching_status == 2
# 2 means the anime has already been watched
animelist_group <- animelist %>%
  filter(watching_status == 2) %>%
  group_by(anime_id) %>%
  summarise(num = n()) %>%
  arrange(desc(num)) %>%
  as.data.frame()

# Left join 'anime' with 'animelist_group' and arrange by 'num'
anime <- anime %>%
  left_join(animelist_group, by = c('MAL_ID' = 'anime_id')) %>%
  arrange(desc(num)) %>%
  as.data.frame()

# Count the occurrences of each genre
genres_list <- unlist(strsplit(anime$Genres, ", "))
genre_counts <- table(genres_list)
genre_counts <- sort(genre_counts, decreasing = TRUE)

# Print the genre counts
print(genre_counts)

# Exclude genres with fewer than 100 occurrences
generos_excluir <- rownames(which(genre_counts < 100, arr.ind = TRUE))

# Create indicator variables for genres
anime <- anime %>%
  filter(!is.na(Genres) | Genres != "") %>%
  as.data.frame()

# List of unique genres
genre_list <- unique(unlist(strsplit(anime$Genres, ", ")))

# Create a matrix for genre indicators
genre_matrix <- matrix(0, nrow = nrow(anime), ncol = length(genre_list), dimnames = list(NULL, genre_list))

# Populate the genre indicator matrix
for (i in 1:nrow(anime)) {
  genres <- unlist(strsplit(anime$Genres[i], ", "))
  genre_matrix[i, genres] <- 1
}

# Combine the genre indicator matrix with the 'anime' dataframe
anime <- cbind(anime, genre_matrix)

# Remove columns for excluded genres
anime <- anime %>%
  select(-any_of(generos_excluir))

# Update the list of genres
genres_list <- setdiff(genres_list, generos_excluir)

# Initialize a vector to store standard deviations for each genre
desviaciones_estandar <- vector("numeric", length = length(genres_list))

# Calculate standard deviations for each genre
for (i in 1:length(genres_list)) {
  desviaciones_estandar[i] <- sd(anime[anime[, genres_list[i]] == 1, 'Score'], na.rm = TRUE)
}

# Display structure of the 'anime' dataframe
str(anime)

# Initialize a vector to store population sizes for each genre
tamanos_poblacion <- vector("numeric", length = length(genres_list))

# Calculate population sizes for each genre
for (i in 1:length(genres_list)-1) {
  tamanos_poblacion[i] <- length(anime[anime[, genres_list[i]] == 1, 'Score'])
}

# Display standard deviations and population sizes
desviaciones_estandar
tamanos_poblacion

# Function to calculate sample size
tam.muestra <- function(alfa, epsilon, s, N = Inf) {
  za2 <- qnorm(1 - alfa / 2)
  
  if (N == Inf) {
    n <- (s * za2 / epsilon)^2
  } else {
    n <- N * ((za2 * s)^2) / ((N - 1) * epsilon^2 + (za2 * s)^2)
  }
  
  return(ceiling(n))
}

# Define parameters for sample size calculation
alfa <- 0.15 # Confidence level
epsilon <- 0.2 # Margin of error

# Initialize a vector to store sample sizes for each genre
tamanos_muestra <- vector("numeric", length = length(genres_list))

# Calculate sample sizes for each genre
for (i in 1:length(genres_list)) {
  N <- tamanos_poblacion[i]
  s <- desviaciones_estandar[i]
  tamanos_muestra[i] <- tam.muestra(alfa, epsilon, s, N)
}

# Display the sample sizes
tamanos_muestra <- tamanos_muestra
sum(tamanos_muestra)
sum(tamanos_poblacion)

# Initialize a vector to store sampled data
muestras <- vector()

# Set random seed for reproducibility
set.seed(123)

# Display structure of the 'anime' dataframe
str(anime)

# Perform random sampling for each genre
for (i in 1:(length(genres_list) - 1)) {
  muestras <- c(muestras, sample(anime[anime[, genres_list[i]] == 1, 'MAL_ID'], size = tamanos_muestra[i], replace = FALSE))
}

# Remove duplicate samples and convert to dataframe
muestras <- unique(muestras)
length(muestras)
muestras <- as.data.frame(muestras)

# Display the first few rows of the sampled data
head(muestras)

# Write the sampled data to a CSV file
write.csv(muestras, 'muestras.txt')

# Display the sampled data
muestras

library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(writexl)
library(igraph)

# Set working directory
setwd("C:/Users/Pc/Desktop/Redes/Data")

# Read data from CSV files
animelist <- read_csv("animelist.csv")
anime <- read_csv("anime.csv")
anime_with_synopsis <- read_csv("anime_with_synopsis.csv")

### NLP ####

# Display structure of 'anime_with_synopsis' dataframe
str(anime_with_synopsis)
# Display 'sypnopsis' column of 'anime_with_synopsis' dataframe
anime_with_synopsis$sypnopsis

# Filter 'anime_with_synopsis' dataframe
anime_with_synopsis <- anime_with_synopsis %>%
  filter(!str_detect(Genres, "\\Hentai\\b")) %>%  # Exclude genres containing 'Hentai'
  filter(!is.na(anime_with_synopsis$sypnopsis)) %>%  # Remove rows with missing synopsis
  filter(!str_detect(sypnopsis, 'No synopsis information has been added to this title. Help improve our database by adding a synopsis here.')) %>%  # Remove rows with default placeholder text
  as.data.frame()

# Clean the 'sypnopsis' text data
anime_with_synopsis <- within(anime_with_synopsis, {
  synopsis <- str_to_lower(sypnopsis)  # Convert to lowercase
  synopsis <- str_replace_all(synopsis, "[^a-z ]", "")  # Remove non-alphabetic characters
  synopsis <- str_replace_all(synopsis, "usic", " music ")  # Replace 'usic' with 'music'
  synopsis <- str_squish(synopsis)  # Remove extra whitespaces
})

# Concatenate all synopsis into a single string separated by '|'
synopsis <- paste(anime_with_synopsis$synopsis, collapse = " | ")

# Remove names attribute
names(synopsis) <- NULL
# Convert to tibble
synopsis <- tibble(line = 1:length(synopsis), text = synopsis)

# Tokenize text by splitting on whitespace
synopsis <- synopsis %>%
  unnest_tokens(input = text, output = word, token = "regex", pattern = "\\s+")

# Join with stop words and remove them
synopsis <- synopsis %>%
  anti_join(x = ., y = stop_words)

# Extract tokens
tokens <- synopsis$word

# Create bigrams from tokens
bigrams <- sapply(1:(length(tokens) - 1), function(i) paste(tokens[i], tokens[i + 1]))

# Convert bigrams to a vector
bigrams <- as.vector(bigrams)

# Convert bigrams to dataframe
bigrams <- as.data.frame(bigrams)

# Filter out undesired bigrams
bigrams <- bigrams %>%
  filter(!str_detect(bigrams, "^\\||\\|$")) %>%
  filter(!str_detect(bigrams, "\\bsource\\b")) %>%
  as.data.frame()

# Count bigram frequencies
bigrams <- bigrams %>% count(bigrams, sort = TRUE)

# Display the top bigrams
head(bigrams)

# Calculate skewness for different thresholds
threshold <- unique(bigrams$n)
count <- bigrams$n
library(EnvStats)
s <- NULL
for (i in 1:length(threshold)) { 
  s[i] <- skewness(bigrams[count > threshold[i], ]$n)
}

# Plot skewness vs threshold
plot(threshold, s, xlim = c(0, 100), ylab = 'skewness')

# Filter bigrams with frequency >= 20
bigrams <- bigrams %>% filter(n >= 20) %>% as.data.frame()

# Split bigrams into two words
split <- str_split_fixed(bigrams$bigrams, " ", 2)

# Create new columns for the words
word_1 <- split[, 1]
word_2 <- split[, 2]
w <- bigrams$n

# Create dataframe for bigrams
bigrams2 <- cbind(word_1, word_2, w)
bigrams2 <- as.data.frame(bigrams2)

# Display top bigrams
head(bigrams, 6)
head(bigrams2)

# Create graph from bigrams
g <- bigrams2 %>%
  filter(word_1 != word_2) %>%
  graph_from_data_frame(directed = FALSE)

# Find the largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# Create subgraph of the largest component
g2 <- igraph::induced_subgraph(g, vert_ids)

# Apply different community detection algorithms
kc_leading_eigen <- cluster_leading_eigen(g2)
kc_walktrap <- cluster_walktrap(g2)
kc_louvain <- cluster_louvain(g2)
kc_label_prop <- cluster_label_prop(g2)
kc_spinglass <- cluster_spinglass(g2)
kc_infomap <- cluster_infomap(g2)

# Calculate modularity for each algorithm
modularity(kc_leading_eigen)
modularity(kc_walktrap)
modularity(kc_louvain)
modularity(kc_label_prop)
modularity(kc_spinglass)
modularity(kc_infomap)

# Display unique memberships of Louvain algorithm
unique(kc_louvain$membership)

# Summarize the graph
summary(g2)

# Extract word and cluster information
word <- V(g2)$name
cluster <- kc_louvain$membership

# Create dataframe for clusters
cluster <- cbind(word, cluster)
cluster <- as.data.frame(cluster)

# Split clusters into list
cluster_list <- split(cluster$word, cluster$cluster)

# Initialize an empty dataframe to store results
finall_synopsis <- data.frame()

# Loop through each row of the dataframe
for (j in 1:nrow(anime_with_synopsis)) {
  # Split the synopsis into words
  synopsis_split <- unlist(strsplit(anime_with_synopsis$synopsis[j], split = ' '))
  
  # Create a temporary dataframe with words and their identifiers
  temp_df <- data.frame(
    token = synopsis_split,
    id = seq(j, length.out = length(synopsis_split))
  )
  
  # Add the temporary dataframe to the final result
  finall_synopsis <- rbind(finall_synopsis, temp_df)
}

# Display top rows of the final synopsis dataframe
head(finall_synopsis)

# Group by token and id, then summarize the counts
finall_synopsis <- finall_synopsis %>%
  group_by(token, id) %>%
  summarise(count = n()) %>%
  as.data.frame()

# Display structure of the final synopsis and cluster dataframes
str(finall_synopsis)
str(cluster)

# Merge the final synopsis with cluster data
finall_synopsis <- finall_synopsis %>%
  left_join(cluster, by = c('token' = 'word')) %>%
  filter(!(is.na(cluster))) %>%
  as.data.frame()

# Summarize frequencies by id and cluster
frequency_synopsis <- finall_synopsis %>%
  group_by(id, cluster) %>%
  summarise(frequency = sum(count)) %>%
  as.data.frame()

library(dplyr)
library(tidyr)

# Reshape the dataframe to wide format
frequency_synopsis <- frequency_synopsis %>%
  pivot_wider(names_from = cluster, values_from = frequency, values_fill = 0)

# Generate indices from 1 to the number of rows in the dataframe
index = seq_len(nrow(anime_with_synopsis))

# Add index to the dataframe and merge with frequency synopsis
anime_with_synopsis <- cbind(index, anime_with_synopsis)
anime_with_synopsis <- as.data.frame(anime_with_synopsis)
anime_with_synopsis <- anime_with_synopsis %>%
left_join(frequency_synopsis, by = c('index' = 'id'))

#### bipartite network ###########################

# Read data from CSV file
animelist <- read_csv("animelist.csv")

# Assign 'anime_with_synopsis' to 'anime'
anime <- anime_with_synopsis

# Display structure of 'animelist' dataframe
str(animelist)

# Filter and group 'animelist' dataframe
animelist_group <- animelist %>%
  filter(watching_status == 2) %>%  # Filter by watching status
  group_by(anime_id) %>%  # Group by anime ID
  summarise(num = n()) %>%  # Count occurrences
  arrange(desc(num)) %>%  # Sort by count in descending order
  as.data.frame()

# Merge 'anime' dataframe with 'animelist_group'
anime <- anime %>%
  left_join(animelist_group, by = c('MAL_ID' = 'anime_id')) %>%
  arrange(desc(num)) %>%  # Sort by count in descending order
  as.data.frame()

# Filter out rows with missing or empty genres
anime <- anime %>% filter(!is.na(Genres) | Genres != "")

# Extract unique genres
genre_list <- unique(unlist(strsplit(anime$Genres, ", ")))

# Create a matrix to indicate genres
genre_matrix <- matrix(0, nrow = nrow(anime), ncol = length(genre_list), dimnames = list(NULL, genre_list))

# Fill the genre matrix
for (i in 1:nrow(anime)) {
  genres <- unlist(strsplit(anime$Genres[i], ", "))
  genre_matrix[i, genres] <- 1
}

# Combine the genre matrix with 'anime' dataframe
anime2 <- cbind(anime, genre_matrix)

# Extract anime IDs
id <- muestras$muestras

# Filter 'animelist' dataframe by selected anime IDs
animelist <- animelist %>%
  filter(anime_id %in% id) %>%
  as.data.frame()

# Create new user IDs
user_id <- unique(animelist$user_id)
user_new_id <- seq(1, length(user_id))
user <- data.frame(user_id, user_new_id)

# Create new anime IDs
anime_id <- unique(animelist$anime_id)
anime_new_id <- seq(length(user_id) + 1, length(anime_id) + length(user_id))
anime_id <- data.frame(anime_id, anime_new_id)

# Identify users and anime based on watching status
Identificados <- animelist %>%
  filter(watching_status == 2) %>%
  left_join(anime_id, by = c('anime_id' = 'anime_id')) %>%
  left_join(user, by = c('user_id' = 'user_id')) %>%
  as.data.frame()

# Get unique anime IDs
u <- unique(Identificados$anime_id)

# Merge genre information with identified anime
anime_generes <- anime2 %>%
  right_join(anime_id, by = c('MAL_ID' = 'anime_id')) %>%
  filter(MAL_ID %in% u) %>%
  arrange(anime_new_id) %>%
  as.data.frame()

# Create data frame for bipartite graph
A <- Identificados %>%
  select(user_new_id, anime_new_id) %>%
  as.data.frame()

# Define types for bipartite graph
type <- c(rep(TRUE, length(unique(A$user_new_id))), rep(FALSE, length(unique(A$anime_new_id))))

# Create bipartite graph
g <- graph_from_data_frame(A)

# Set type attribute for bipartite projection
V(g)$type <- type
V(g)$name

# Compute bipartite projection
projection <- bipartite_projection(g, which = c('false'))

# Convert projection to adjacency matrix
A <- as_adjacency_matrix(projection, attr = "weight")
A <- as.matrix(A)

# Display top rows of adjacency matrix
head(A)

# Initialize new dataframe
N_A <- NULL

# Get IDs from adjacency matrix
ids <- as.numeric(colnames(A))

# Merge genre information with anime IDs
anime2 <- anime %>%
  right_join(anime_id, by = c('MAL_ID' = 'anime_id')) %>%
  filter(MAL_ID %in% u) %>%
  as.data.frame()

# Arrange 'num' column by new anime IDs
num <- anime2 %>%
  select(anime_new_id, num) %>%
  arrange(anime_new_id) %>%
  as.data.frame()

# Initialize matrix
X <- matrix(nrow = 0, ncol = length(ids))

# Create binary matrix based on thresholds
for (i in 1:length(ids)) {
  X <- rbind(X, sapply(A[i, ], function(x) ifelse(num$num[i] * 0.75 <= x, 1, 0)))
}

# Create undirected graph from adjacency matrix
g <- graph_from_adjacency_matrix(X)
g <- as.undirected(g)

# Display summary of graph
summary(g)

# Display structure of 'anime_generes' dataframe
str(anime_generes)

# Assign attributes to graph vertices
V(g)$Score <- anime_generes$Score
V(g)$num <- anime_generes$num
V(g)$Mystery <- anime_generes$Mystery
V(g)$Police <- anime_generes$Police
V(g)$Psychological <- anime_generes$Psychological
V(g)$Supernatural <- anime_generes$Supernatural
V(g)$Thriller <- anime_generes$Thriller
V(g)$Shounen <- anime_generes$Shounen
V(g)$Action <- anime_generes$Action
V(g)$Military <- anime_generes$Military
V(g)$Super_Power <- anime_generes$`Super Power`
V(g)$Drama <- anime_generes$Drama
V(g)$Fantasy <- anime_generes$Fantasy
V(g)$Game <- anime_generes$Game
V(g)$Adventure <- anime_generes$Adventure
V(g)$Romance <- anime_generes$Romance
V(g)$Comedy <- anime_generes$Comedy
V(g)$Magic <- anime_generes$Magic
V(g)$Slice_of_Life <- anime_generes$`Slice of Life`
V(g)$School <- anime_generes$School
V(g)$Sci_Fi <- anime_generes$`Sci-Fi`
V(g)$Mecha <- anime_generes$Mecha
V(g)$Martial_Arts <- anime_generes$`Martial Arts`
V(g)$Parody <- anime_generes$Parody
V(g)$Ecchi <- anime_generes$Ecchi
V(g)$Horror <- anime_generes$Horror
V(g)$Seinen <- anime_generes$Seinen
V(g)$Vampire <- anime_generes$Vampire
V(g)$Demons <- anime_generes$Demons
V(g)$Dementia <- anime_generes$Dementia
V(g)$Music <- anime_generes$Music
V(g)$Space <- anime_generes$Space
V(g)$Shoujo <- anime_generes$Shoujo
V(g)$Historical <- anime_generes$Historical
V(g)$Samurai <- anime_generes$Samurai
V(g)$Harem <- anime_generes$Harem
V(g)$Sports <- anime_generes$Sports
V(g)$Josei <- anime_generes$Josei
V(g)$Kids <- anime_generes$Kids
V(g)$Shoujo_Ai <- anime_generes$`Shounen Ai`
V(g)$Cars <- anime_generes$Cars

# Define a function to replace NA with 0
replace_na_with_zero <- function(attribute) {
  attribute[is.na(attribute)] <- 0
  return(attribute)
}

# Replace NA values with 0 for each attribute
V(g)$cluster_1 <- replace_na_with_zero(anime_generes$`1`)
V(g)$cluster_2 <- replace_na_with_zero(anime_generes$`2`)
V(g)$cluster_3 <- replace_na_with_zero(anime_generes$`3`)
V(g)$cluster_4 <- replace_na_with_zero(anime_generes$`4`)
V(g)$cluster_5 <- replace_na_with_zero(anime_generes$`5`)
V(g)$cluster_6 <- replace_na_with_zero(anime_generes$`6`)
V(g)$cluster_7 <- replace_na_with_zero(anime_generes$`7`)
V(g)$cluster_8 <- replace_na_with_zero(anime_generes$`8`)
V(g)$cluster_9 <- replace_na_with_zero(anime_generes$`9`)
V(g)$cluster_10 <- replace_na_with_zero(anime_generes$`10`)
V(g)$cluster_11 <- replace_na_with_zero(anime_generes$`11`)
V(g)$cluster_12 <- replace_na_with_zero(anime_generes$`12`)
V(g)$cluster_13 <- replace_na_with_zero(anime_generes$`13`)
V(g)$cluster_14 <- replace_na_with_zero(anime_generes$`14`)

# Identify the largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)

# Get vertex IDs of the largest component
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# Create subgraph of the largest component
g2 <- igraph::induced_subgraph(g, vert_ids)

# Display summary of the subgraph
summary(g2)
summary(g)

library(network)
library(intergraph)
library(ergm)

# Convert igraph object to network object
g_network <- asNetwork(g2)

##################### ERGM ########################

# Define ERGM model formula
ergm_model <- formula(g_network ~ edges + nodecov(~cbind(
  cluster_1, cluster_2, cluster_3, cluster_4, cluster_5, 
  cluster_6, cluster_7, cluster_8, cluster_9, cluster_10, 
  cluster_11, cluster_12, cluster_13, cluster_14
)))

# Set seed for reproducibility
set.seed(42)

# Fit the ERGM model
ergm_fit <- ergm(formula = ergm_model)

# Display summary of the fitted model
summary(ergm_fit)

# Set seed for reproducibility
set.seed(42)

# Conduct goodness-of-fit analysis
ergm_gof <- gof(object = ergm_fit)

# Display p-values for the goodness-of-fit test
ergm_gof$pval.model

# Summarize p-values for degree distribution
summary(ergm_gof$pval.deg)

# Summarize p-values for edgewise shared partner distribution
summary(ergm_gof$pval.espart)

# Summarize p-values for minimum geodesic distance distribution
summary(ergm_gof$pval.dist)

# Conduct a linear regression analysis on the 'Score' variable using attributes
reg <- lm(
  as.numeric(Score) ~  + `1` + `2` + `3` + 
    `4` + `5` + `6` + `7` + `8` + 
    `9` + `10` + `11` + `12` + `13` + 
    `14`, 
  data = anime
)

# Display summary of the linear regression model
summary(reg)

# Plot diagnostic plots for the linear regression model
plot(reg)

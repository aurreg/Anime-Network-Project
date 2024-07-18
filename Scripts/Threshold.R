#### bipartite network ###########################


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
muestras <- read_csv("muestras.txt")
id<-muestras$muestras


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

thresholds<-seq(0.15,0.95,0.10)

mean_absolute_error<-NULL

set.seed(42)

for(j in 1:length( thresholds)){ 

# Initialize matrix
X <- matrix(nrow = 0, ncol = length(ids))



# Create binary matrix based on thresholds
for (i in 1:length(ids)) {
  X <- rbind(X, sapply(A[i, ], function(x) ifelse(num$num[i] *  thresholds[j] <= x, 1, 0)))
}

# Create undirected graph from adjacency matrix
g <- graph_from_adjacency_matrix(X)
g <- as.undirected(g)

# Display summary of graph
summary(g)

# Display structure of 'anime_generes' dataframe
str(anime_generes)


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
V(g)$cluster_15 <- replace_na_with_zero(anime_generes$`15`)
V(g)$cluster_16 <- replace_na_with_zero(anime_generes$`16`)
V(g)$cluster_17 <- replace_na_with_zero(anime_generes$`17`)
V(g)$cluster_18 <- replace_na_with_zero(anime_generes$`18`)

# Identify the largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)

# Get vertex IDs of the largest component
vert_ids <- V(g)[components$membership == biggest_cluster_id]

absolute_error<-NULL

for (i in 1:10){


# Create subgraph of the largest component
g2 <- igraph::induced_subgraph(g, sample(vert_ids,300))

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
  cluster_11, cluster_12, cluster_13, cluster_14, cluster_15,
  cluster_16,cluster_17,cluster_18
)))

# Set seed for reproducibility
set.seed(42)

# Fit the ERGM model
ergm_fit <- ergm(formula = ergm_model)

# Display summary of the fitted model
summary(ergm_fit)


# Set seed for reproducibility
set.seed(42)

ergm_gof<-gof(ergm_fit)

absolute_error[i]<- mean(abs(0.5-ergm_gof$pval.model[,5]))

}
mean_absolute_error[i]<-mean(absolute_error)
}
mean_absolute_error

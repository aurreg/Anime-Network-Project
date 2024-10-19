library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(writexl)
library(igraph)
library(network)
library(intergraph)
library(ergm)
library(wordcloud)
# Set working directory
setwd("C:/Users/Pc/Desktop/Anime-Network-Project/Data")

# Read data from CSV files
animelist <- read_csv("animelist.csv")
anime <- read_csv("anime.csv")
anime_with_synopsis <- read_csv("anime_with_synopsis.csv")

### NLP ####

# Display structure of 'anime_with_synopsis' dataframe
str(anime_with_synopsis)


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
synopsis2 <- synopsis %>%
  filter(!str_detect(word, "\\bsource\\b")) %>%
  filter(!str_detect(word, "^\\||\\|$")) %>%
  anti_join(x = ., y = stop_words)
 

synopsis2  %>% 
  count(word, sort = TRUE) %>%
  head(n = 10)

set.seed(123)
synopsis2 %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(words = word, freq = n, max.words =300, colors = 'purple2'))


# Extract tokens
tokens <- synopsis$word

# Create bigrams from tokens and avoid multiple edges
bigrams <- sapply(1:(length(tokens) - 1), function(i) {
  words <- sort(c(tokens[i], tokens[i + 1]))
  paste(words, collapse = " ")
})


# Convert bigrams to a vector
bigrams <- as.vector(bigrams)

# Convert bigrams to dataframe
bigrams <- as.data.frame(bigrams)

# Filter out undesired bigrams
bigrams <- bigrams %>%
  filter(!str_detect(bigrams, "^\\||\\|$")) %>%
  filter(!str_detect(bigrams, "\\bsource\\b")) %>%
  as.data.frame()

bigrams<-bigrams$bigrams

bigrams<- str_split_fixed(bigrams, " ", 2)

word_1 <- bigrams[, 1]
word_2 <- bigrams[, 2]

bigrams2 <- cbind(word_1, word_2)
bigrams2 <- as.data.frame(bigrams2)

bigrams2<-bigrams2%>%
  filter(!word_1 %in% stop_words$word & !word_2 %in% stop_words$word)%>%
  filter(word_1 != word_2)%>%
  count(word_1, word_2, sort = TRUE) %>%
  rename(weight = n)

# Calculate skewness for different thresholds
threshold <- unique(bigrams2$weight)
count <- bigrams2$weight
library(EnvStats)
s <- NULL
for (i in 1:length(threshold)) { 
  s[i] <- skewness(bigrams2[count > threshold[i], ]$weight)
  #hist(bigrams[count > threshold[i], ]$n)
  
}

# Plot skewness vs threshold
plot(threshold, s, 
     xlim = c(0, 100), 
     ylim = range(s, na.rm = TRUE),
     type = 'b',                   # Connect points with lines
     pch = 19,                     # Point character
     col = 'blue',                 # Point color
     xlab = 'Threshold',           # X-axis label
     ylab = 'Skewness',            # Y-axis label
     main = '',  # Title
     cex.main = 1.5,               # Title size
     cex.lab = 1.2,                # Axis label size
     cex.axis = 1.1,               # Axis tick label size
     cex = 0.5)                    # Point size


#curve(45 / sqrt(x), from = 0.1, to = 100, add = TRUE, col = 'red', lwd = 2)
# Adding grid lines
grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')

# Adding a horizontal line at y=0 for reference
abline(v = 20, col = 'red', lty = 2)
# Create graph from bigrams

head(bigrams2,10)

g <- bigrams2%>%
  filter(weight > 20) %>%
  select(word_1,word_2)%>%
  graph_from_data_frame(directed = FALSE)

g<-igraph::simplify(g) 

# Find the largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# Create subgraph of the largest component
g2 <- igraph::induced_subgraph(g, vert_ids)


set.seed(42)

# Applying different community detection algorithms on graph g2
kc_edge_betweenness <- cluster_edge_betweenness(g2)
kc_fast_greedy <- cluster_fast_greedy(g2)
kc_infomap <- cluster_infomap(g2)
kc_label_prop <- cluster_label_prop(g2)
kc_leading_eigen <- cluster_leading_eigen(g2)
kc_leiden <- cluster_leiden(g2)
kc_louvain <- cluster_louvain(g2)
kc_spinglass <- cluster_spinglass(g2)
kc_walktrap <- cluster_walktrap(g2)

# Calculating modularity for each community detection method
mod_edge_betweenness <- modularity(kc_edge_betweenness)
mod_fast_greedy <- modularity(kc_fast_greedy)
mod_infomap <- modularity(kc_infomap)
mod_label_prop <- modularity(kc_label_prop)
mod_leading_eigen <- modularity(kc_leading_eigen)
mod_leiden <- modularity(g2, kc_leiden$membership)
mod_louvain <- modularity(kc_louvain)
mod_spinglass <- modularity(kc_spinglass)
mod_walktrap <- modularity(kc_walktrap)

# Creating a named vector of modularity values
modularity_values <- c(
  Edge_Betweenness = mod_edge_betweenness,
  Fast_Greedy = mod_fast_greedy,
  Infomap = mod_infomap,
  Label_Propagation = mod_label_prop,
  Leading_Eigen = mod_leading_eigen,
  Leiden = mod_leiden,
  Louvain = mod_louvain,
  Spinglass = mod_spinglass,
  Walktrap = mod_walktrap
)

# Finding the method with the highest modularity
best_method <- names(which.max(modularity_values))
best_modularity <- max(modularity_values)

# Printing the method with the highest modularity
cat("Based on the modularity values, the method with the highest modularity is", best_method, "\n")
cat("Modularity value:", best_modularity, "\n")

# Display unique memberships of Louvain algorithm
max(kc_louvain$membership)

cols <- c(brewer.pal(9,"Set1")[1:9],brewer.pal(8,"Set2")[1:7],brewer.pal(8,"Set2")[1:7],brewer.pal(12,"Set3")[1:3])

plot(g2, layout = layout_with_kk, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.6), 
     vertex.frame.color =adjustcolor(cols[kc_louvain$membership],1), vertex.size = ifelse(0.8*degree(g2)>1,0.8*degree(g2),1),  
     vertex.label = NA, edge.color=adjustcolor('gray',0.8),
     vertex.label.color = 'black', vertex.label.cex = .7, vertex.label.dist = 1)


tab <- cbind(c("Dist. media","Grado media","Grado desviación","Número clan","Densidad","Transitividad","Asortatividad"),
             round(c(mean_distance(g2), mean(degree(g2)), sd(degree(g2)), clique.number(g2), edge_density(g2),
                     transitivity(g2), assortativity_degree(g2)),4)
)
tab


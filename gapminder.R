library(gapminder)
library(tidyverse)
library(factoextra)

clusters <- kmeans(
  x = gapminder[, 4:6], 
  centers = 2, 
  nstart = 10)

# Plot each cluster as a shape
plot(
  x = gapminder$lifeExp, 
  y = gapminder$gdpPercap, 
  col = as.numeric(gapminder$continent), 
  pch = clusters$cluster)

# Plot centroid of clusters
points(
  x = clusters$centers[, "lifeExp"], 
  y = clusters$centers[, "gdpPercap"],
  pch = 4, 
  lwd = 4, 
  col = "blue")

# View a table of the clusters
table(
  x = clusters$cluster, 
  y = gapminder$continent)

fviz_nbclust( gapminder[, 4:6], kmeans, method = "wss")
fviz_nbclust( gapminder[, 4:6], kmeans, method = "silhouette")
fviz_nbclust( gapminder[, 4:6], kmeans, nstart = 25, method = "gap_stat", nboot = 50)




#Start here

by_year <- gapminder %>%
  filter(year == 2007)

score <- by_year %>%
  mutate( score = lifeExp * gdpPercap)


clusters <- kmeans(
  x = by_year[, 4:6], 
  centers = 2, 
  nstart = 10)

# Plot each cluster as a shape
plot(
  x = by_year$lifeExp, 
  y = by_year$gdpPercap, 
  col = as.numeric(by_year$continent), 
  pch = clusters$cluster)

# Plot centroid of clusters
points(
  x = clusters$centers[, "lifeExp"], 
  y = clusters$centers[, "gdpPercap"],
  pch = 4, 
  lwd = 4, 
  col = "blue")

# View a table of the clusters
table(
  x = clusters$cluster, 
  y = gapminder$continent)

fviz_nbclust( score[, 4:7], kmeans, method = "wss")
fviz_nbclust( score[, 4:7], kmeans, method = "silhouette")
fviz_nbclust( score[, 4:7], kmeans, nstart = 25, method = "gap_stat", nboot = 50)


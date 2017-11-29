library(gapminder)

clusters <- kmeans(
  x = gapminder[, 4:6], 
  centers = 5, 
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
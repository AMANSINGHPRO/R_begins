#Tidyverse chapter 1
library(gapminder)
library(dplyr)
library(ggplot2)

dim(gapminder)

# Filter, mutate, and arrange the gapminder dataset
gapminder_2007 <- gapminder %>%
  filter( year == 2007 ) %>%
  mutate( lifeExpMonths = 12*lifeExp ) %>%
  arrange(desc(lifeExpMonths))


gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()


# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot( gapminder_1952, aes( x = pop, y = lifeExp )) +
  geom_point()


# Change this plot to put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  facet_wrap( ~ continent )


# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap( ~ year )

# Summarize to find the median life expectancy
gapminder %>%
  summarize(medianLifeExp = median(lifeExp))


# Filter for 1957 then summarize the median life expectancy
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median (lifeExp),
            maxGdpPercap = max(gdpPercap))


# Find median life expectancy and maximum GDP per capita in each year
gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp), 
            maxGdpPercap = max(gdpPercap))


# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each year/continent combination
a <- gapminder %>%
  filter( year == 1952) %>%
  group_by(continent, year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))


by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
ggplot(by_year, aes(x = year, y = medianLifeExp)) +
  geom_point() +
  expand_limits(y = 0)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) +
  geom_point() +
  expand_limits(y = 0)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap),
            medianLifeExp = median(lifeExp))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, color = continent)) +
  geom_point()

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) +
  geom_line() +
  expand_limits(y = 0)


# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

# Summarize the median gdpPercap by year and continent in 1952
by_continent <- gapminder %>%
  filter(year == 1952) %>%
  group_by(continent) %>%
  summarize( medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = medianGdpPercap)) +
  geom_col()

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter( year == 1952, continent == "Oceania" )

# Create a bar plot of gdpPerCap by country
ggplot(oceania_1952, aes(x = country, y = gdpPercap )) +
  geom_col()


gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop)
ggplot(gapminder_1952, aes(x = pop)) +
  geom_histogram()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
ggplot(gapminder_1952, aes(x = pop)) +
  geom_histogram() +
  scale_x_log10()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Comparing GDP per capita across continents")

cat ("abc")

Europe <- gapminder %>%
  + filter( continent == "Europe")

Europe

Asia <- gapminder %>%
   filter( continent == "Asia")

Asia

unique(gapminder$continent)

Africa <- gapminder %>%
   filter( continent == "Africa")

Africa

Americas <- gapminder %>%
   filter( continent == "Americas")

Americas

Oceania <- gapminder %>%
   filter( continent == "Oceania")
Oceania


US <- gapminder %>%
   filter( country == "United States")
US

US_bylifeExp <- gapminder %>%
  filter( country == "United States") %>%
  arrange(lifeExp)
US_bylifeExp

US_bylifeExp <- gapminder %>%
     filter( continent == "Americas") %>%
     #arrange(desc(lifeExp)) %>%
     mutate( gdp = gdpPercap * pop ) %>%
     arrange(desc(gdp))
US_bylifeExp

#Improved
afghanistan_country_only <- gapminder %>%
  filter( country == "Afghanistan") %>%
  arrange(lifeExp)

ggplot( afghanistan_country_only, aes(x  = as.factor(year), y = lifeExp)) +
  geom_col()

#Filter by all continents
filter_by_continent_Asia <- gapminder %>%
  filter(continent == "Asia")

filter_by_continent_Europe <- gapminder %>%
  filter(continent == "Europe")

filter_by_continent_Africa <- gapminder %>%
  filter(continent == "Africa")

filter_by_continent_Americas <- gapminder %>%
  filter(continent == "Americas")

filter_by_continent_Oceania <- gapminder %>%
  filter(continent == "Oceania")

#arrange continents by numerical values
arrange_by_lifeExp_Asia <- filter_by_continent_Asia %>%
  arrange(desc(lifeExp))

arrange_by_lifeExp_Europe <- filter_by_continent_Europe %>%
  arrange(desc(lifeExp))

arrange_by_lifeExp_Africa <- filter_by_continent_Africa %>%
  arrange(desc(lifeExp))

arrange_by_lifeExp_Americas <- filter_by_continent_Americas %>%
  arrange(desc(lifeExp))

arrange_by_lifeExp_Oceania <- filter_by_continent_Oceania %>%
  arrange(desc(lifeExp))

by_cont <- gapminder %>%
   group_by(continent) %>%
   summarize(meanLifeExp = mean(lifeExp),
   totalpop = sum(as.numeric(pop)))

by_year_gap <- gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalpop = sum(as.numeric(pop)))

by_count <- gapminder %>%
  group_by(country) %>%
  summarize(meanLifeExp = mean(lifeExp),
            meanpop = mean(pop),
            meanGdppercap = mean(gdpPercap))

by_count <- gapminder %>%
     group_by(country) %>%
     summarize(meanLifeExp = mean(lifeExp),
     meanpop = mean(pop),
     meanGdppercap = mean(gdpPercap)) %>%
     arrange(desc(meanLifeExp))

try_count <- gapminder %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp),
            meanpop = mean(pop),
            meanGdppercap = mean(gdpPercap)) %>%
  arrange(desc(meanLifeExp))

tryy_count <- gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            meanpop = mean(pop),
            meanGdppercap = mean(gdpPercap)) %>%
  arrange(desc(meanLifeExp))

#Friday
group_by_year_in_Asia <- filter_by_continent_Asia %>%
  group_by(year) %>%
  summarize(meanlifeExp = mean(lifeExp),
            meanpop = mean(pop),
            meangdpPercap = mean(gdpPercap))
View(group_by_year_in_Asia)
ggplot(group_by_year_in_Asia, aes(x = year, y = meanlifeExp)) + geom_line()

trygroup_by_year_in_Asia <- gapminder %>%
  group_by( continent, year ) %>%
  summarize(meanlifeExp = mean(lifeExp),
            meanpop = mean(pop),
            meangdpPercap = mean(gdpPercap))
View(trygroup_by_year_in_Asia)
ggplot(trygroup_by_year_in_Asia, aes(x = year, y = meanlifeExp, color = continent)) + geom_line()

try_group_by_year_in_country <- gapminder %>%
  group_by( country ) %>%
  summarize(meanlifeExp = mean(lifeExp),
            meanpop = mean(pop),
            meangdpPercap = mean(gdpPercap))
View(try_group_by_year_in_country)
ggplot(try_group_by_year_in_country, aes( y = meanlifeExp, color = country)) + geom_histogram()

ggplot(gapminder, aes(x = year, y = lifeExp, colour = country)) + geom_line() + theme(legend.position = "none")

a <- gapminder %>%
  filter( continent == "Africa")

ggplot(a, aes(x = year, y = lifeExp, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(a, aes(x = year, y = gdpPercap, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(a, aes(x = year, y = pop, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

Asia <- gapminder %>%
  filter( continent == "Asia")

ggplot(Asia, aes(x = year, y = lifeExp, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Asia, aes(x = year, y = gdpPercap, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Asia, aes(x = year, y = pop, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))


Europe <- gapminder %>%
  filter( continent == "Europe")

ggplot(Europe, aes(x = year, y = lifeExp, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Europe, aes(x = year, y = gdpPercap, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Europe, aes(x = year, y = pop, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))


Americas <- gapminder %>%
  filter( continent == "Americas")

ggplot(Americas, aes(x = year, y = lifeExp, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Americas, aes(x = year, y = gdpPercap, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Americas, aes(x = year, y = pop, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))


Oceania <- gapminder %>%
  filter( continent == "Oceania")

ggplot(Oceania, aes(x = year, y = lifeExp, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Oceania, aes(x = year, y = gdpPercap, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(Oceania, aes(x = year, y = pop, colour = country)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), cex = 0.5))

ggplot(gapminder, aes(pop, colour = continent)) + geom_freqpoly()
ggplot(gapminder, aes(gdpPercap, colour = continent)) + geom_freqpoly()
ggplot(gapminder, aes(lifeExp)) + geom_freqpoly()



gapminder %>% filter(country == "Argentina")  %>%
  ggplot(aes(x= log(pop))) + 
  geom_histogram(bins = 5)
  





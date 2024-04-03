install.packages("tidyverse")
install.packages("maps")
install.packages("plotly")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(maps)
library(plotly)
library(dplyr)

unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

data_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country", "year"))

data_join <- full_join(unicef_metadata, unicef_indicator_2, by = c("country", "year" = "time_period"))

data_join <- unicef_metadata %>%
  full_join(unicef_indicator_1, by = c("country", "year")) %>%
  full_join(unicef_indicator_2,  by = c("country", "year" = "time_period"))

# map

map_world <- map_data("world")

data_join_2020 <- unicef_indicator_1 %>%
  filter(year == 2020)

map_data_join_2020 <- full_join(data_join_2020, map_world, by = c("country" = "region"))

map <- ggplot(map_data_join_2020) +
  aes(x = long, y = lat, group = group, fill = obs_value, country = country) +
  geom_polygon() +
  labs(title = paste("Proportion of Schools with Limited Sanitation in 2020")) +
  labs(fill = paste("%")) +
  labs(y = NULL, x = NULL) +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "#333333")

ggplotly(map)

# time-series chart

metadata_join_2020 <- unicef_metadata %>%
  filter(year == 2020)

metadata_join_nicaragua <- unicef_metadata %>%
  filter(country == "Nicaragua")

time_series_20yrs <- unicef_metadata %>%
  filter(country == "Nicaragua", year > 2001 & year < 2020) %>%
  ggplot(aes(year, `Life expectancy at birth, total (years)`, group = country)) +
  geom_line(color = "#40E0D0") +
  labs(title = "Nicaragua's Life Expectancy Over 20 years") +
  labs(y = "life expectancy") 

ggplotly(time_series_20yrs)

scatter_world <- unicef_metadata %>%
  filter(`Life expectancy at birth, total (years)` > 40) %>%
  ggplot() +
  aes(year, `Life expectancy at birth, total (years)`, group = country, color = country) +
  geom_line() +
  labs(x = "Year", y = "Life expectancy", title = "Life Expectancy For All Countries") +
  theme(legend.position = "none")

ggplotly(scatter_world)

time_series_nic <- metadata_join_nicaragua %>%
  ggplot(aes(year, `Life expectancy at birth, total (years)`)) +
  geom_line(color = "navy") +
  labs(x = "Year", y = "Life expectancy", title = "Nicaragua's Life Expectancy Since 1960") +
  theme(legend.position = "none")

ggplotly(time_series_nic)

unicef_metadata %>%
  filter(country == "Nicaragua", year > 1990 & year < 2020) %>%
  ggplot(aes(year, `Life expectancy at birth, total (years)`, group = country, color = country)) +
  geom_line(color = "navy") +
  labs(title = "Nicaragua's Life Expectancy Over 30 years") +
  labs(y = "life expectancy")



metadata_join_2020 <- unicef_metadata %>%
  filter(year == 2020)

metadata_join_nicaragua <- unicef_metadata %>%
  filter(country == "Nicaragua")

time_series_nic <- metadata_join_nicaragua %>%
  ggplot() +
  aes(year, `Life expectancy at birth, total (years)`) +
  geom_line(color = "navy") +
  labs(x = "Year", y = "Life expectancy", title = "Nicaragua's Life Expectancy Since 1960") +
  theme(legend.position = "none") +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major.y = element_line("lightgrey"),  
        panel.grid.minor = element_line("lightgrey"))

ggplotly(time_series_nic)

time_series_20yrs <- unicef_metadata %>%
  filter(country == "Nicaragua", year > 2001 & year < 2020) %>%
  ggplot() +
  aes(year, `Life expectancy at birth, total (years)`, group = country) +
  geom_line(color = "#40E0D0") +
  labs(title = "Nicaragua's Life Expectancy Over 20 years") +
  labs(y = "life expectancy") +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major.y = element_line("lightgrey"),  
        panel.grid.minor = element_line("lightgrey"))

ggplotly(time_series_20yrs)



# scatter plot

sanitation_GDP_2020 <- data_join %>%
  filter(`GDP per capita (constant 2015 US$)` > 0, year == 2020, obs_value.x > 0)

# Calculate linear regression
lm_model <- lm(obs_value.x ~ `GDP per capita (constant 2015 US$)`, data = sanitation_GDP_2020)

# Create scatter plot
ggplot(sanitation_GDP_2020, aes(x = `GDP per capita (constant 2015 US$)`, y = obs_value.x, color = country)) +
  geom_point(color = "#40E0D0") +
  theme(legend.position = "none", axis.title.y = element_text(size = 8), axis.title.x = element_text(size = 8)) +
  # Add linear regression line
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "navy") +
  labs(title = "Limited Sanitation in Schools and GDP per capita in Countries") +
  labs(y = "Proportion of Schools 
       with Limited Sanitation", x = "GDP per capita")



sanitation_GDP_2020 <- data_join %>%
  filter(`GDP per capita (constant 2015 US$)` > 0, year == 2020, obs_value.x > 0)

# Calculate linear regression
lm_model <- lm(obs_value.x ~ `GDP per capita (constant 2015 US$)`, data = sanitation_GDP_2020)

# Create scatter plot
ggplot(sanitation_GDP_2020, aes(x = `GDP per capita (constant 2015 US$)`, y = obs_value.x, color = country)) +
  geom_point(color = "#40E0D0") +
  theme(legend.position = "none", axis.title.y = element_text(size = 8), axis.title.x = element_text(size = 8)) +
  # Add linear regression line
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "navy") +
  labs(title = "Limited Sanitation in Schools and GDP per capita in Countries") +
  labs(y = "Proportion of Schools 
       with Limited Sanitation", x = "GDP per capita") +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major.y = element_line("lightgrey"),  
        panel.grid.minor = element_line("lightgrey"))

sanitation_GDP_2020 <- data_join %>%
  filter(`GDP per capita (constant 2015 US$)` > 0, year == 2020, obs_value.x > 0)

# Calculate linear regression
lm_model <- lm(obs_value.x ~ `GDP per capita (constant 2015 US$)`, data = sanitation_GDP_2020)

# Create scatter plot
scatterplot <- ggplot(sanitation_GDP_2020) +
  aes(x = `GDP per capita (constant 2015 US$)`, y = obs_value.x) +
  geom_point(color = "#40E0D0") +
  theme(legend.position = "none", axis.title.y = element_text(size = 8), axis.title.x = element_text(size = 8)) +
  # Add linear regression line
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "navy") +
  labs(title = "Limited Sanitation in Schools and GDP per capita in Countries") +
  labs(y = "Proportion of Schools 
       with Limited Sanitation", x = "GDP per capita") +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major.y = element_line("lightgrey"),  
        panel.grid.minor = element_line("lightgrey"))

ggplotly(scatterplot)

# bar chart 

bar_chart <- unicef_metadata %>%
  filter(country == "Nicaragua", year > 2000 & year < 2020) %>%
  ggplot(aes(year, `GDP per capita (constant 2015 US$)`, fill = country)) +
  geom_col() +
  scale_fill_manual(values = "lightblue") +
  labs(title = "Nicaragua's GDP over 20 years") +
  labs(y = NULL, x = NULL) +
  theme(legend.position = "none", panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "lightgray"),  
        panel.grid.minor = element_blank(),
        axis.line = element_blank())

ggplotly(bar_chart)

# Load data into dataframes
population_df = as.data.frame( read.csv("C:/repos/population_pyramid_analysis/pjangroup.csv",header=T,sep=";",dec=".") )
#setwd("/home/alvaro/repos/data_science/population_pyramid_analysis")
#population_df = as.data.frame( read.csv("pjangroup.csv",header=T,sep=";",dec=".") )

# Take only population pyramid data for 2001
population_df = population_df[population_df$TIME_PERIOD == '2021',]


# Remove unused columns
population_df[8] = NULL
population_df[1:4] = list(NULL)
initial_df = population_df

# install.packages("tidyverse")
library(tidyverse)

# Tidy our data to get one column per country and one column per combination of sex/age_group
population_df = population_df %>% pivot_wider(names_from = c("sex", "age"), values_from = "OBS_VALUE")

# Remove all countries with NA values
population_df = population_df[complete.cases(population_df),]
copy_df = population_df
population_df[1] = NULL

# Normalize data (21 age_groups, 3 sexes)
F_total = population_df$F_TOTAL
M_total = population_df$M_TOTAL
T_total = population_df$T_TOTAL

# Remove totals columns for clustering
population_df$F_TOTAL = NULL
population_df$M_TOTAL = NULL
population_df$T_TOTAL = NULL

population_df[1:18] = population_df[1:18] / F_total
population_df[19:36] = population_df[19:36] / M_total
population_df[37:54] = population_df[37:54] / M_total


# Clustering
kmdata = as.matrix(population_df)
wss = numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)
# Plot graphic to see the elbow
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

km = kmeans(kmdata, 3, nstart = 25)
km

# Prepare data and clustering results for plotting
df = as.data.frame(kmdata[,1:54])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

# install.packages("countrycode")
library(countrycode)
countries = countrycode(copy_df$geo, "iso2c", "country.name")
country_clusters = data.frame("Country" = countries, "cluster2001" = km$cluster)

# Population pyramids
plot_pyramid_df <- function(df) {
  
  g = ggplot(df, aes(x = age, fill = sex,
                     y = ifelse(test = sex == "M",
                                yes = -Population, no = Population))) + 
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = abs, limits = max(df$Population) * c(-1,1)) +
    coord_flip()
  g + labs(x = "Age", y = "Population") +labs(title = "Population pyramid")
}

plot_pyramid_country <- function(country) {
  df = initial_df[initial_df$geo==country,]
  df = df[df$age!='TOTAL',]
  df = df[df$sex!='T',]
  g = ggplot(df, aes(x = age, fill = sex,
                     y = ifelse(test = sex == "M",
                                yes = -OBS_VALUE, no = OBS_VALUE))) + 
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = abs, limits = max(df$OBS_VALUE) * c(-1,1)) +
    coord_flip()
  
  g + labs(x = "Age", y = "Population") +labs(title = "Population pyramid", subtitle = country)
}

# Take a random country dataframe just to extract sex and age_groups to use for centroids
esp_df = initial_df[initial_df$geo=='ES',]
esp_df = esp_df[esp_df$age!='TOTAL',]
centers_df = as.data.frame(t(centers)) 
centers_df = cbind(sex = esp_df$sex, age=esp_df$age ,centers_df)

# Prepare centroid data to plot
names(cluster1_df)[3] = "Population"
names(cluster2_df)[3] = "Population"
names(cluster3_df)[3] = "Population"

# Plot population pyramids by centroids
plot_pyramid_df(cluster1_df)
plot_pyramid_df(cluster2_df)
plot_pyramid_df(cluster3_df)

# Plot population pyramids by country
plot_pyramid_country('FR')
plot_pyramid_country('ME')
plot_pyramid_country('RO')

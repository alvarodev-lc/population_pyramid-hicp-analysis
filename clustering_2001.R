# Load data into dataframes
population_df = as.data.frame( read.csv("C:/repos/population_pyramid_analysis/pjangroup.csv",header=T,sep=";",dec=".") )
#setwd("/home/alvaro/repos/data_science/population_pyramid_analysis")
#population_df = as.data.frame( read.csv("pjangroup.csv",header=T,sep=";",dec=".") )

# Take only population pyramid data for 2001
population_df = population_df[population_df$TIME_PERIOD == '2001',]


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

population_df[4:21] = population_df[4:21] / F_total
population_df[22:39] = population_df[22:39] / M_total
population_df[40:57] = population_df[40:57] / M_total

# Remove totals columns for clustering
population_df[1:3] = list(NULL)


# Clustering
kmdata_population = as.matrix(population_df)
kmdata = kmdata_population[,1:54]
wss = numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)
# Plot graphic to see the elbow
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

km = kmeans(kmdata, 3, nstart = 25)
km

# Prepare data and clustering results for plotting
df = as.data.frame(kmdata_population[,1:54])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

# install.packages("countrycode")
library(countrycode)
countries = countrycode(copy_df$geo, "iso2c", "country.name")
country_clusters = data.frame("Country" = countries, "cluster2001" = km$cluster)

# Poblational pyramids
plot_pyramid_df <- function(df) {
  
  g = ggplot(df, aes(x = age, fill = sex,
                     y = ifelse(test = sex == "M",
                                yes = -Poblation, no = Poblation))) + 
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = abs, limits = max(df$Poblation) * c(-1,1)) +
    coord_flip()
  g + labs(x = "Age", y = "Poblation") +labs(title = "Poblation pyramid")
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

  g + labs(x = "Age", y = "Poblation") +labs(title = "Poblation pyramid", subtitle = country)
}

# Take a random country dataframe just to extract sex and age_groups to use for centroids
esp_df = initial_df[initial_df$geo=='ES',]
esp_df = esp_df[esp_df$age!='TOTAL',]
esp_df = esp_df[esp_df$sex!='T',]
centers_df = as.data.frame(t(centers)) 
centers_df =centers_df[-c(37:54),]
centers_df = cbind(sex = esp_df$sex, age=esp_df$age ,centers_df)

# Prepare centroid data to plot
cluster1_df = centers_df[,c("sex","age", "1")]
cluster2_df = centers_df[,c("sex","age", "2")]
cluster3_df = centers_df[,c("sex","age", "3")]
names(cluster1_df)[3] = "Poblation"
names(cluster2_df)[3] = "Poblation"
names(cluster3_df)[3] = "Poblation"

# Plot poblation pyramids by centroids
plot_pyramid_df(cluster1_df)
plot_pyramid_df(cluster2_df)
plot_pyramid_df(cluster3_df)

# Plot poblation pyramids by country
plot_pyramid_country('FR')
plot_pyramid_country('ES')
plot_pyramid_country('MT')

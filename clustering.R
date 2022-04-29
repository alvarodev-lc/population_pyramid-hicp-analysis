# Load data into dataframes
population_df = as.data.frame( read.csv("C:/repos/population_pyramid_analysis/pjangroup.csv",header=T,sep=";",dec=".") )

# Take only population pyramid data for 2001, remove sex=T
population_df = population_df[population_df$TIME_PERIOD == '2001',]
# population_df = population_df[population_df$sex != 'T',]
# population_df = population_df[population_df$age != 'TOTAL',]

# Remove unused columns
population_df[8] = NULL
population_df[1:4] = list(NULL)

# install.packages("tidyverse")
library(tidyverse)

# Tidy our data to get one column per country and one column per combination of sex/age_group
population_df = population_df %>% pivot_wider(names_from = c("sex", "age"), values_from = "OBS_VALUE")

# Remove all countries with NA values
population_df = population_df[complete.cases(population_df),]
population_df[1] = NULL

# Normalize data (21 age_groups, 3 sexes)

F_total = population_df$F_TOTAL
M_total = population_df$M_TOTAL
T_total = population_df$T_TOTAL

# Normalize F_AgeGroup columns with F_total values
population_df[4:21] = population_df[4:21] / F_total
population_df[22:39] = population_df[22:39] / M_total
population_df[40:57] = population_df[40:57] / M_total

# Set X_total columns to 1
# population_df[1] = population_df[1] / F_total
# population_df[2] = population_df[2] / M_total
# population_df[3] = population_df[3] / T_total
population_df[1:3] = list(NULL)




# Clustering
kmdata_population = as.matrix(population_df)
kmdata = kmdata_population[,1:54]
kmdata
wss = numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)
# Plot graphic to see the elbow
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

km = kmeans(kmdata, 3, nstart = 25)

# Prepare data and clustering results for plotting
df = as.data.frame(kmdata_population[,1:54])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

# install.packages("factoextra")
library(ggpubr)
library(factoextra)

fviz_cluster(km, data = kmdata,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# country_clusters = data.frame("Country" = population_df$geo, "cluster2001" = km$cluster)

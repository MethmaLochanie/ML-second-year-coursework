library(NbClust)
library(readxl)
library(dplyr)
library(caret)
library(factoextra)
library(cluster)

#subtask 2 
#e part

vehicles  <- read_xlsx("vehicles.xlsx")
vehicles <- vehicles[,-20]
vehicles <- vehicles[,-1]

# Outlier detection and removal
outliers <- apply(vehicles, 2, function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  return(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))
})

# Remove outliers
vehicles_no_outliers <- vehicles[!apply(outliers, 1, any),]


vehicles_no_outliers
boxplot(vehicles_no_outliers)
boxplot(vehicles)

scaled_data <- scale(vehicles_no_outliers)
#View(scaled_data)

boxplot(scaled_data)

#calculating PCA
pca_vehicle = prcomp(scaled_data, center = TRUE, scale = TRUE)
pca_vehicle

# Display the eigenvalues
print(pca_vehicle$sdev^2)

# Display the eigenvectors
print(pca_vehicle$rotation)

# Calculate cumulative score per principal component
cumulative_score <- cumsum(pca_vehicle$sdev^2 / sum(pca_vehicle$sdev^2)) * 100

# Identify number of principal components that provide at least 92% cumulative score
num_PCs <- which(cumulative_score > 92)[1]

# Create new transformed dataset with chosen principal components
vehicles_transformed <- predict(pca_vehicle, newdata = scaled_data)[,1:num_PCs]

vehicles_transformed
pca_vehicle

#f part
#NBclust method
set.seed(123)
clusterNo=NbClust(vehicles_transformed,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
#2

#elbow method
fviz_nbclust(vehicles_transformed, kmeans, method = "wss")
#4

#gap stastic method
fviz_nbclust(vehicles_transformed, kmeans, method = 'gap_stat')
#4


#silhouette method
fviz_nbclust(vehicles_transformed, kmeans, method = 'silhouette')
#2

#g part
k <- 3
set.seed(123)
kmeans_output <- kmeans(vehicles_transformed, centers = k)
kmeans_output



fviz_cluster(kmeans_output, data = vehicles_transformed)

wss = kmeans_output$tot.withinss
bss = kmeans_output$betweenss


wss
bss


#tss
mean_data <- colMeans(vehicles_transformed)
deviation <- vehicles_transformed - mean_data
tss <- sum(deviation^2)

wss
bss
cat("BSS/TSS Ratio:", bss/tss)


#h part
sil_pca <- silhouette(kmeans_output$cluster, dist(vehicles_transformed))
fviz_silhouette(sil_pca)
avg_sil_width_pca <- mean(sil_pca[, "sil_width"])


#i part
# Calculating Calinski-Harabasz Index
calinski_harabasz_pca <- function(cluster_result, data) {
  k <- length(unique(cluster_result$cluster))
  n <- nrow(data)
  BSS <- cluster_result$betweenss
  WSS <- cluster_result$tot.withinss
  
  ch_index <- ((n - k) / (k - 1)) * (BSS / WSS)
  return(ch_index)
}

ch_index_pca <- calinski_harabasz_pca(kmeans_output, vehicles_transformed)
ch_index_pca


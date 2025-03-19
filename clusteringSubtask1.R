library(NbClust)
library(readxl)
library(factoextra)
library(cluster)

#sub task 1
#a part
vehicles_data  <- read_xlsx("vehicles.xlsx")

vehicles_data <- vehicles_data[,-20]

vehicles_data <- vehicles_data[,-1]
boxplot(vehicles_data)

# Outlier detection and removal
outliers <- apply(vehicles_data, 2, function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  return(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))
})

# Remove outliers
vehicles_no_outliers <- vehicles_data[!apply(outliers, 1, any),]
vehicles_no_outliers

#normalize and scaled the data
scaled_vehicle_norm_data <- data.frame(scale(vehicles_no_outliers))
boxplot(scaled_vehicle_norm_data)

#b part

#NBclust method
set.seed(123)
clusterNo=NbClust(scaled_vehicle_norm_data,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
#3 clusters

#elbow method
fviz_nbclust(scaled_vehicle_norm_data, kmeans, method = "wss") + 
geom_vline(xintercept = 3, linetype =3)
#3 clusters

#gap stastic method
fviz_nbclust(scaled_vehicle_norm_data, kmeans, method = 'gap_stat')
#3 clusters


#silhouette method
fviz_nbclust(scaled_vehicle_norm_data, kmeans, method = 'silhouette')
#2 clusters

#c part

k <- 3
set.seed(123)
kmeans_output <- kmeans(scaled_vehicle_norm_data, centers = k)
kmeans_output

fviz_cluster(kmeans_output, data = scaled_vehicle_norm_data)

wss = kmeans_output$tot.withinss
bss = kmeans_output$betweenss



#tss
mean_data <- colMeans(scaled_vehicle_norm_data)
deviation <- scaled_vehicle_norm_data - mean_data
tss <- sum(deviation^2)

wss
bss
tss
cat("BSS/TSS Ratio:", bss/tss)

#d part
sil_obj <- silhouette(kmeans_output$cluster, dist(scaled_vehicle_norm_data))
fviz_silhouette(sil_obj)
avg_sil_width_obj <- mean(sil_obj[,"sil_width"])
avg_sil_width_obj

#subtask 2 
#e part
#calculating PCA
pca_vehicles = prcomp(scaled_vehicle_norm_data, center = TRUE, scale = TRUE)
summary(pca_vehicles)

# Display the eigenvalues
print(pca_vehicles$sdev^2)

# Display the eigenvectors
print(pca_vehicles$rotation)

# Calculate cumulative score per principal component
cumulative_score <- cumsum(pca_vehicles$sdev^2 / sum(pca_vehicles$sdev^2)) * 100

# Identify number of principal components that provide at least 92% cumulative score
num_PCs <- which(cumulative_score > 92)[1]

# Create new transformed dataset with chosen principal components
vehicles_transformed <- predict(pca_vehicles, newdata = scaled_vehicle_norm_data)[,1:num_PCs]
vehicles_transformed

#f part
#NBclust method
set.seed(123)
clusterNo=NbClust(vehicles_transformed,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
#2 clusters

#elbow method
fviz_nbclust(vehicles_transformed, kmeans, method = "wss") + 
geom_vline(xintercept = 3, linetype =3)
#3 clusters

#gap stastic method
fviz_nbclust(vehicles_transformed, kmeans, method = 'gap_stat')
#3 clusters


#silhouette method
fviz_nbclust(vehicles_transformed, kmeans, method = 'silhouette')
#2 clusters

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
tss
cat("BSS/TSS Ratio:", bss/tss)


#h part
sil_pca <- silhouette(kmeans_output$cluster, dist(vehicles_transformed))
fviz_silhouette(sil_pca)
avg_sil_width_pca <- mean(sil_pca[, "sil_width"])
avg_sil_width_pca


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
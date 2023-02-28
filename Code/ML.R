library(factoextra)
library(cluster)
library(dplyr)
set.seed(123)

Housing_Data_Nolb1_Nu <- read.csv("C:/Users/mksai/Downloads/Housing_Data_Nolb1_Nu_1.csv")
Housing_Data_Nolb1_Nu<-Housing_Data_Nolb1_Nu[,-1]

Housing_Data_Nolb1_Nu_sample <- Housing_Data_Nolb1_Nu %>% 
  slice_sample(n = 200, replace = FALSE)
write.csv(Housing_Data_Nolb1_Nu_sample, file = "Housing_Data_Nolb1_Nu_sample.csv", row.names = FALSE)
Housing_Data_Nolb1_Nu_norm<-scale(Housing_Data_Nolb1_Nu_sample)
Housing_Data_Nolb1_Nu_norm

# Determining K values
# Elbow method
plot_1<-fviz_nbclust(Housing_Data_Nolb1_Nu_norm,method = "wss",FUN = kmeans,k.max = 5)
plot_1 + ggtitle("Using Elbow Method, KMeans")
# Silhouette Method
plot_2<-fviz_nbclust(Housing_Data_Nolb1_Nu_norm,method = "silhouette",FUN = kmeans,k.max = 5)
plot_2 + ggtitle("Using Silhouette Method, KMeans")
# Gap Statistic Method
plot_3<-fviz_nbclust(Housing_Data_Nolb1_Nu_norm,method = "gap_stat",FUN = kmeans,k.max = 5)
plot_3 + ggtitle("Using Gap Statistic Method, KMeans")


Kmeans<-kmeans(Housing_Data_Nolb1_Nu_norm, centers=2)
fviz_cluster(Kmeans, Housing_Data_Nolb1_Nu_sample, main="KMeans Resulting Clusters with K=2 for Housing_Data_Sample")

(Dist_Housing_Data_norm<- dist(Housing_Data_Nolb1_Nu_norm, method = "minkowski", p=2))
(HClust_Ward_Euc_N_3D <- hclust(Dist_Housing_Data_norm, method = "average" ))
plot(HClust_Ward_Euc_N_3D, cex=0.9, hang=-1, main = "Hierarchial Clustering For K=2")
rect.hclust(HClust_Ward_Euc_N_3D, k=2)

cos_sim_matrix <- crossprod(Housing_Data_Nolb1_Nu_norm) / (sqrt(colSums(Housing_Data_Nolb1_Nu_norm^2)) %*% t(sqrt(colSums(Housing_Data_Nolb1_Nu_norm^2))))

# perform hierarchical clustering with cosine similarity
hc <- hclust(as.dist(1 - cos_sim_matrix))

# plot dendrogram
plot(hc,main="Cosine Similarity as the distance measure for the hclust")
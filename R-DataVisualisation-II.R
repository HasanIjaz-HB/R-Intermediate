load(file = "covidClustering.RData")
head(covidData)
summary(covidData)

rownames(covidData) <- covidData$countries
covidData<-covidData[,-12]
covidData<-covidData[,-11]
covidData<-covidData[,-1]


#correlation matrix
cor(covidData[,1:9])
library(GGally)
GGally::ggpairs(covidData[,1:9])

#totalrecovered and totalcases have a high correlation 0.958
#population and newcases have a very low correlation 0.349

#heatmap
library(gplots)
heatmap.2(cor(covidData[,c(1, 2, 3,4,5,6,7,8,9)]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", cellnote = round(cor(covidData[,c(1, 2, 3,4,5,6,7,8,9)]),2), notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#normalization
n_covidData <- clusterSim::data.Normalization(covidData, type="n1", normalization="column")
summary(n_covidData)

# Silhouette Indeks (Silhouette Index) for different k values
kValues <- c(3,5,7)
Silhouette_Ind <- 0

for(i in kValues){
  set.seed(1)
  kMeansModel <- kmeans(x = n_covidData, centers = i)
  data_Silhouette <- cluster::silhouette(kMeansModel$cluster, dist(n_covidData, method = "euclidean"))
  Silhouette_Ind[i-1] <- mean(data_Silhouette[,c("sil_width")])
}

Silhouette_Ind

plot(kValues, Silhouette_Ind,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
grid(NULL, NULL, lty=6, col="grey", lwd=2)

#scatterpllot
library(ggplot2)
ggplot(covidData, aes(y = seriousCritical, x = totalDeaths, colour = cluster)) + geom_point(alpha = 0.6)


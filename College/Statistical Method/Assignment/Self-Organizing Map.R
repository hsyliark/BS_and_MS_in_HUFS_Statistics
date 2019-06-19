# Self-Organizing Map (자기조직화지도) #

install.packages("kohonen")
library(kohonen)

?xyf

data(wines)
set.seed(7)

training <- sample(nrow(wines), 120)
Xtraining <- scale(wines[training, ]) #scale : Centering and Scaling
Xtest <- scale(wines[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))

xyf.wines <- xyf(Xtraining,
                 factor(wine.classes[training]),
                 grid = somgrid(10, 10, "hexagonal"))

xyf.prediction <- predict(xyf.wines, newdata=Xtest)
table(wine.classes[-training], xyf.prediction$prediction)

plot(xyf.wines, type="changes")
plot(xyf.wines, type="codes", main = c("Codes X", "Codes Y"))
plot(xyf.wines, type="counts")

xyfpredictions <- classmat2classvec(predict(xyf.wines)$unit.predictions)
bgcols <- c("gray", "pink", "lightgreen")
plot(xyf.wines, type="mapping", col = wine.classes,
     labels = wine.classes, bgcol = bgcols[as.integer(xyfpredictions)], 
     main = "Mapping plot for wine data")



## Germancredit data (Default=1 --> Bad credit)

germancredit <- read.csv("C:/Users/user/Dropbox/수업자료/대학/(4)Senior/
                         4-2/통계적방법론(1전공)/Mid-term Assignment/
                         germancredit1.csv",sep=",",header=T)
german.classes <- germancredit[,1]
set.seed(10)

Gtraining <- sample(nrow(germancredit), 700)
GXtraining <- scale(germancredit[Gtraining, ]) #scale : Centering and Scaling
GXtest <- scale(germancredit[-Gtraining, ],
               center = attr(GXtraining, "scaled:center"),
               scale = attr(GXtraining, "scaled:scale"))

xyf.german <- xyf(GXtraining,
                 factor(german.classes[Gtraining]),
                 grid = somgrid(10, 10, "hexagonal"))

xyf.Gprediction <- predict(xyf.german, newdata=GXtest)
table(german.classes[-Gtraining], xyf.Gprediction$prediction)

plot(xyf.german, type="changes")
plot(xyf.german, type="codes", main = c("Codes X", "Codes Y"))
plot(xyf.german, type="counts")

Gxyfpredictions <- classmat2classvec(xyf.Gprediction$unit.predictions)
bgcols <- c("lightblue","pink")
plot(xyf.german, type="mapping", col = german.classes+1, labels = german.classes,
     pchs = german.classes+1, bgcol = bgcols[as.integer(Gxyfpredictions)+1], 
     main = "Mapping plot for Germancredit data")



### 2011 Irish Census data ###

data <- read.csv("../census_data/AllThemesTablesSA.csv",sep=",",header=T)
......
marital_data <- data[, c("T1_2SGLT", "T1_2MART", "T1_2SEPT", 
                         "T1_2DIVT", "T1_2WIDT", "T1_2T")]
marital_percents <- data.frame(t(apply(marital_data, 
                                       1, function(x) {x[1:5]/x[6]})) * 100)
names(marital_percents) <- c("single_percent", "married_percent", 
                             "separated_percent", 
                             "divorced_percent", "widow_percent")  

# average household size
household_data <- data[, 331:339]
mean_household_size <- c(1:8)
results$avr_household_size <- apply(household_data, 1, function(x){
  size <- sum(x[1:length(mean_household_size)] * mean_household_size) / x[length(x)]
})
......
names(data)
[1] "id"                   "avr_age"              "avr_household_size"   "avr_education_level" 
[5] "avr_num_cars"         "avr_health"           "rented_percent"       "unemployment_percent"
[9] "internet_percent"     "single_percent"       "married_percent"      "separated_percent"   
[13] "divorced_percent"     "widow_percent" 

require(kohonen)

data_train <- data[, c(2,4,5,8)]

data_train_matrix <- as.matrix(scale(data_train))
names(data_train_matrix) <- names(data_train)

som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 n.hood = "circular",
                 keep.data = TRUE )

summary(som_model)

som map of size 20x20 with a hexagonal topology.
Training data included; dimension is 4046 by 4
Mean distance to the closest unit in the map: 0.1130107

coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
plot(som_model, type = "changes")
plot(som_model, type = "counts", 
     main="Node Counts", palette.name=coolBlueHotRed)

plot(som_model, type = "quality", 
     main="Node Quality/Distance", palette.name=coolBlueHotRed)

plot(som_model, type="dist.neighbours", 
     main = "SOM neighbour distances")
plot(som_model, type = "codes")

# Plot the heatmap for a variable at scaled / normalised values
plot(som_model, type = "property", property = som_model$codes[,2], 
     main=names(som_model$data)[2], palette.name=coolBlueHotRed)

# Plot the original scale heatmap for a variable from the training set:
var <- 4 #define the variable to plot
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), 
                          FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], 
     palette.name=coolBlueHotRed)
rm(var_unscaled, var)

#plot a variable from the original data set (will be uncapped etc.)
plotHeatMap(som_model, data, variable=0)

# Clustering SOM results #

# show the WCSS metric for kmeans for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- som_model$codes
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)

som_cluster <- cutree(hclust(dist(som_model$codes)), 6)
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)




























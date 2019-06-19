install.packages("kriging")
library(kriging)

install.packages("maps")
library(maps)

buyoe <- read.csv("C:/Users/student/Desktop/buyoe.csv", sep=",", header=T)

attach(buyoe)
head(buyoe)



m.larvae <- c()
for (i in 1:3) {
  for (j in 1:5) {
    m.larvae[position==i & row==j] <- mean(larvae[position==i & row==j])
  }
}

use.buyoe <- cbind(buyoe[,c(3,4)],m.larvae)

attach(use.buyoe)

use.buyoe1 <- use.buyoe[position==1,]
idx1 <- order(use.buyoe1$row)
use.buyoe2 <- use.buyoe[position==2,]
idx2 <- order(use.buyoe2$row)
use.buyoe3 <- use.buyoe[position==3,]
idx3 <- order(use.buyoe3$row)

use.buyoe1 <- use.buyoe1[idx1,]
use.buyoe2 <- use.buyoe2[idx2,]
use.buyoe3 <- use.buyoe3[idx3,]

use.buyoe <- rbind(use.buyoe1, use.buyoe2, use.buyoe3)

res.buyoe <- use.buyoe[c(110, 1110, 2110, 3110, 4110, 5110,
                         6110, 7110, 8000, 8110, 9110, 10110, 11110, 12110, 13110),]
res.buyoe

attach(res.buyoe)

install.packages("fields")
library(fields)
res.buyoe <- as.matrix(res.buyoe)
image.plot(res.buyoe)

install.packages("ggplot2")
library(ggplot2)
ggplot(res.buyoe, aes(x=row, y=position, z=m.larvae, fill=m.larvae)) +
  stat_contour(size=0.5) + geom_contour(binwidth=0.01)

    

kriging(field, row, larvae, model = "spherical", lags = 10, pixels = 100, polygons = NULL)



# Krige random data for a specified area using a list of polygons
library(maps)
usa <- map("usa", "main", plot = FALSE)
p <- list(data.frame(usa$x, usa$y))
# Create some random data
x <- runif(50, min(p[[1]][,1]), max(p[[1]][,1]))
y <- runif(50, min(p[[1]][,2]), max(p[[1]][,2]))
z <- rnorm(50)
# Krige and create the map
kriged <- kriging(x, y, z, polygons=p, pixels=300)
image(kriged, xlim = extendrange(x), ylim = extendrange(y))




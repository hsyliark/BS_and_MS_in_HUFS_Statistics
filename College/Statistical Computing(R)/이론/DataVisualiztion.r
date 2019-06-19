
#############################
#
#  Data Visualization
#
#############################
#############################
# Lattice plot
#############################
# Trellis Examples 
library(lattice) 
attach(mtcars)

# create factors with value labels 
gear.f<-factor(gear,levels=c(3,4,5), labels=c("3gears","4gears","5gears")) 
cyl.f <-factor(cyl,levels=c(4,6,8), labels=c("4cyl","6cyl","8cyl")) 

# kernel density plot 
densityplot(~mpg, 
            main="Density Plot", 
            xlab="Miles per Gallon")

# kernel density plots by factor level 
densityplot(~mpg|cyl.f, 
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon")

# kernel density plots by factor level (alternate layout) 
densityplot(~mpg|cyl.f, 
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon", 
            layout=c(1,3))

# boxplots for each combination of two factors 
bwplot(cyl.f~mpg|gear.f,
       ylab="Cylinders", xlab="Miles per Gallon", 
       main="Mileage by Cylinders and Gears", 
       layout=(c(1,3)))
       
# scatterplots for each combination of two factors 
xyplot(mpg~wt|cyl.f*gear.f, main="Scatterplots by Cylinders and Gears", 
       ylab="Miles per Gallon", xlab="Car Weight")
       
# 3d scatterplot by factor level 
cloud(mpg~wt*qsec|cyl.f, main="3D Scatterplot by Cylinders") 
       
# dotplot for each combination of two factors 
dotplot(cyl.f~mpg|gear.f, main="Dotplot Plot by Number of Gears and Cylinders",
        xlab="Miles Per Gallon")
       
# scatterplot matrix 
splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")
       
#############################
# ggplot
#############################
#
# Scatterplots with qplot()
# 
qplot(Sepal.Length, Petal.Length, data = iris)
# Plot Sepal.Length vs. Petal.Length, using data from the `iris` data frame.
       
qplot(Sepal.Length, Petal.Length, data = iris, color = Species)
       
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width)
# We see that Iris setosa flowers have the narrowest petals.
       
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width, alpha = I(0.7))
# By setting the alpha of each point to 0.7, we reduce the effects of overplotting.
       
qplot(Sepal.Length, Petal.Length, data = iris, color = Species,
      xlab = "Sepal Length", ylab = "Petal Length",
      main = "Sepal vs. Petal Length in Fisher's Iris data")
       
# These two invocations are equivalent.
qplot(Sepal.Length, Petal.Length, data = iris, geom = "point")
qplot(Sepal.Length, Petal.Length, data = iris)
       
       
#
# Barcharts: geom = “bar”
#
       
       
# Plot the number of movies each director has.
qplot(director, data = movies, geom = "bar", ylab = "# movies")
# By default, the height of each bar is simply a count.
# But we can also supply a different weight.
# Here the height of each bar is the total running time of the director's movies.
qplot(director, weight = minutes, data = movies, geom = "bar", ylab = "total length (min.)")
       
#
# Line charts: geom = “line”
#
       
qplot(Sepal.Length, Petal.Length, data = iris, geom = "line", color = Species)
# Using a line geom doesn't really make sense here, but hey.
       
# `Orange` is another built-in data frame that describes the growth of orange trees.
qplot(age, circumference, data = Orange, geom = "line",
      colour = Tree,
      main = "How does orange tree circumference vary with age?")
       
# We can also plot both points and lines.
qplot(age, circumference, data = Orange, geom = c("point", "line"), colour = Tree)
       
       
data(iris)
       
# which promptly produces an error, ‘No layers in plot’. This is because the data frame, 
# iris has been specified along with which 2 variables to plot, Length and Width but no 
# information on what graphical component to use.
       
ggplot(iris,aes(Sepal.Length,Sepal.Width))+geom_point()
       
ggplot(iris,aes(Sepal.Length,Sepal.Width))+geom_point(aes(Sepal.Width,Sepal.Length))
# Different from above?
       
#
# geom_boxplot
#
p <- ggplot(mtcars, aes(factor(cyl), mpg)) 
p + geom_boxplot()
       
# Or equivalently, 
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot")
       
p + geom_boxplot() + geom_jitter()
# with jitter... 반복해서 그려보세요~~
       
p + geom_boxplot() + coord_flip()
# 눕혀 그리기
       
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot") +  coord_flip()
# Or equivalently
       
p + geom_boxplot(outlier.colour = "green", outlier.size = 3)
       
# Add aesthetic mappings 
# Note that boxplots are automatically dodged when any aesthetic is a factor 
p + geom_boxplot(aes(fill = cyl))
       
p + geom_boxplot(aes(fill = factor(cyl)))
# 위의 명령어와 비교!!
       
p + geom_boxplot(aes(fill = factor(vs)))
       
p + geom_boxplot(aes(fill = factor(am)))
       
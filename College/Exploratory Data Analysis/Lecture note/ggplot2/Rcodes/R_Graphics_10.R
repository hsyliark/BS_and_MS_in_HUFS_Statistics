
###### R Graphics Lecture 10 : Using Colors in Plots ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 10.1. Setting the Colors of Objects
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(color='red')
library(MASS)
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill='red',color='black')


## 10.2. Mapping Variables to Colors
# method 1 - "fill" in "aes()"
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(color='black', position='dodge')
# method 2 - "aes(fill=)" in "geom"
ggplot(cabbage_exp, aes(x=Date, y=Weight)) +
  geom_bar(aes(fill=Cultivar), color='black', position='dodge')

# method 1
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) + geom_point()
# method 2
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(aes(color=cyl))


## 10.3. Using a Different Palette for a Discrete Variable
# See the table for discrete fill and color scales from the lecture notes

str(uspopage)
# base plot
p <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
# These three have the same effect
p
p + scale_fill_discrete()
p + scale_fill_hue()
p + scale_fill_hue(h=c(0,100))
# ColorBrewer palette
p + scale_fill_brewer()

h <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point()
# scale_xxx_hue() has the default lightness value 65
h
# Slightly darker
h + scale_color_hue(l=45)

# "RColorBrewer" provides a number of palettes 
library(RColorBrewer)
par(mar=c(0,4,0,0))
display.brewer.all()

p + scale_fill_brewer(palette='Oranges')
p + scale_fill_brewer(palette='YlGn')

# scale_xxx_grey() : default start=0.2 (bright) end=0.8 (dark)
p + scale_fill_grey()
# reverse the direction and use a different range of greys
p + scale_fill_grey(start=0.7, end=0)


## 10.4. Using a Manually Defined Palette for a Discrete Variable
h <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point()
h
# using color names
h + scale_color_manual(values=c('red','blue'))
# using RBG values
h + scale_color_manual(values=c('#CC6666','#7777DD'))

h + scale_color_manual(values=c(m='darkblue',f='pink'))

# see the names of colors defined in R
colors()


## 10.5. Using a Manually Defined Palette for a Continuous Variable
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=weightLb)) + 
  geom_point(size=3)

p
# with a gradient bewteen two colors
p + scale_color_gradient(low='black', high='white')
# a gradient with a white midpoint
p + scale_color_gradient2(low='red', mid='white', high='blue', midpoint=110)
library(scales) # "muted()" for a less-saturated version
p + scale_color_gradient2(low=muted('red'), mid='white', high=muted('blue'), 
                          midpoint=110)
# a gradient of n colors - note that only "colour" works here, not "color"
p + scale_color_gradientn(colours=c('darkred', 'orange', 'yellow', 'white'))
p + scale_color_gradientn(colours=c('red', 'blue', 'green', 'black'))



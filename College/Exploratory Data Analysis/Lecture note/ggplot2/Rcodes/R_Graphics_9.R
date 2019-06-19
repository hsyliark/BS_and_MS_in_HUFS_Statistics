
###### R Graphics Lecture 9 : Facets ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 9.1. Splitting Data into Subplots with Facets
str(mpg)
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
# faceted by drv, in vertically arranged subpanels
p + facet_grid(drv ~.)
# faceted by cyl, in horizontally arranged subpanels
p + facet_grid(.~cyl)
# splitted by drv (vertical) and cyl (horizontal)
p + facet_grid(drv ~ cyl)
# facet on class
p + facet_wrap(~class, nrow=2)
p + facet_wrap(~class, ncol=5)


## 9.2. Using Facets with Different Axes
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
# with free y scales
p + facet_grid(drv~cyl, scales='free_y')
# with free x and y scales
p + facet_grid(drv~cyl, scales='free')


## 9.3. Changing the Text of Facet Labels
# There is no way to raname Facet labels. You must change the data values
mpg2 <- mpg
levels(mpg2$drv)
levels(mpg2$drv) <- c('4wd', 'Front', 'Rear')
ggplot(mpg2, aes(x=displ,y=hwy)) + geom_point() + facet_grid(drv~.)


## 9.4. Changing the Appearance of Facet Labels
ggplot(cabbage_exp, aes(x=Cultivar, y=Weight)) + geom_bar(stat='identity') +
  facet_grid(.~Date) +
  theme(strip.text=element_text(face='bold',size=rel(1.5),color='red'),
        strip.background=element_rect(fill='lightblue', color='black', size=1))

###### R Graphics Lecture 11 : Miscellaneous Graphs ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 11.1. Making a Correlation Matrix
str(mtcars)
mtcars
# compute the correlation matrix
mcor <- cor(mtcars)
round(mcor, 2)  # print mcor and round to 2 digits
library(corrplot)
?corrplot
corrplot(mcor)
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)

# generate a lighter palette
cols <- colorRampPalette(c('blue', 'white', 'red'))
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45,
         col=cols(200), addCoef.col='black', order='AOE')
cols2 <- colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45,
         col=cols2(200), addCoef.col='black', order='AOE')



## 11.2. Creating a Network Graph

library(igraph)

# Specifying edges for a directed graph
gd <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(gd)

# For an undirected graph
gu <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed=FALSE)
# no labels
plot(gu, vertex.label=NA)

str(gd)
str(gu)

# Create a graph object from the data frame
str(madmen2)
head(madmen2)
g <- graph.data.frame(madmen2, directed=TRUE)
par(mar=rep(0,4)) # remove unnecessary margins
plot(g, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5,
     vertex.label=NA)

str(madmen)
head(madmen)
g <- graph.data.frame(madmen, directed=FALSE)
plot(g, layout=layout.circle, vertex.size=8, vertex.label=NA)

m <- madmen[1:nrow(madmen) %% 2 == 1, ]
g <- graph.data.frame(m, directed=FALSE)
# print out the names of each vertex
V(g)$name
plot(g, layout=layout.fruchterman.reingold,
     vertex.size = 4, # Smaller nodes
     vertex.label = V(g)$name,  # Set the labels
     vertex.label.cex = 0.8,  # Slightly smaller font
     vertex.label.dist =0.4,  # offset the labels
     vertex.label.color = 'red')

# view the edges
E(g)
# Set some of the edges the label "M"
E(g)[c(2,11,19)]$label <- 'M'
# set color of all to grey, and then color a few red
E(g)$color <- 'grey70'
E(g)[c(2,11,19)]$color <- 'red'
plot(g, layout=layout.fruchterman.reingold,
     vertex.size = 4, 
     vertex.color = rainbow(length(V(g))),
     vertex.label = V(g)$name,  
     vertex.label.cex = 0.8,  
     vertex.label.dist =0.4,  
     vertex.label.color = 'black')




## 11.3. Creating a Three-Dimensional Scatter Plot
library(rgl)
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type='s', size=0.75, lit=FALSE)

# function to interleave the elements of two vectors
interleave <- function(v1, v2) as.vector(rbind(v1,v2))
# plot the points
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab='Weight', ylab='Displacement', zlab='MPG',
       size=0.75, type='s', lit=FALSE)
# add segments
segments3d(interleave(mtcars$wt, mtcars$wt),
          interleave(mtcars$disp, mtcars$disp),
          interleave(mtcars$mpg, min(mtcars$mpg)),
          alpha=0.4, col='blue')
# draw the box
rgl.bbox(color='grey50',
         emission='grey50',
         xlen=0, ylen=0, zlen=0)
# set default color of future objects to black
rgl.material(color='black')
# add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++"
axes3d(edges=c("x--", "y+-", "z--"), ntick=6, cex=0.75)
# add axis labels.
mtext3d('Weight', edge="x--", line=2)
mtext3d('Displacement', edge="y+-", line=3)
mtext3d('MPG', edge='z--', line=3)

### try this part
predictgrid <- function(model, xvar, yvar, zvar, res=16, type=NULL){
  xrange <- range(model$model[[xvar]])
  yrange <- range(model$model[[yvar]])
  newdata <- expand.grid(x=seq(xrange[1], xrange[2], length.out=res),
                         y=seq(yrange[1], yrange[2], length.out=res))
  names(newdata) <- c(xvar, yvar)
  newdata[[zvar]] <- predict(model, newdata=newdata, type=type)
  newdata
}
df2mat <- function(p, xvar=NULL, yvar=NULL, zvar=NULL) {
  if(is.null(xvar)) xvar <- names(p)[1]
  if(is.null(yvar)) yvar <- names(p)[2]
  if(is.null(zvar)) zvar <- names(p)[3]
  x <- unique(p[[xvar]])
  y <- unique(p[[yvar]])
  z <- matrix(p[[zvar]], nrow=length(y), ncol=length(x))
  m <- list(x,y,z)
  names(m) <- c(xvar, yvar, zvar)
  m
}
interleave <- function(v1, v2) as.vector(rbind(v1,v2))
m <- mtcars
mod <- lm(mpg ~ wt + disp + wt:disp, data=m)
m$pred_mpg <- predict(mod)
mpgrid_df <- predictgrid(mod, 'wt', 'disp', 'mpg')
mpgrid_list <- df2mat(mpgrid_df)

plot3d(m$wt, m$disp, m$mpg, type='s', size=0.5, lit=FALSE)
spheres3d(m$wt, m$disp, m$pred_mpg, alpha=0.4, type='s', size=0.5, lit=FALSE)
segments3d(interleave(m$wt, m$wt),
           interleave(m$disp, m$disp),
           interleave(m$mpg, m$pred_mpg),
           alpha=0.4, col='red')
surface3d(mpgrid_list$wt, mpgrid_list$disp, mpgrid_list$mpg,
          alpha=0.4, front='lines', back='lines')
###

# spin 3d-plot
play3d(spin3d())
# spin on x-axis, at 4 rpm, for 20 seconds
play3d(spin3d(axis=c(1,0,0), rpm=4), duration=20)
# spin on z axis, at 4 rpm, for 5 seconds, 20 frames per seconds
# save it as a movie file
movie3d(spin3d(axis=c(0,0,1), rpm=4), duration=5, fps=20,
        movie='/Users/leesh12/Desktop/KCB_VisData/movie', type='gif')



## 11.4. Creating a Vector Field
head(isabel)
tail(isabel)
islice <- subset(isabel, z==min(z))
ggplot(islice, aes(x=x,y=y)) +
  geom_segment(aes(xend=x+vx/50, yend=y+vy/50), size=0.25)

# less resolution
islice <- subset(isabel, z==min(z))
every_n <- function(x, by=2){
  x <- sort(x)
  x[seq(1, length(x), by=by)]
}
keepx <- every_n(unique(isabel$x), by=4)
keepy <- every_n(unique(isabel$y), by=4)
islicesub <- subset(islice, x %in% keepx & y %in% keepy)
library(grid)
ggplot(islicesub, aes(x=x,y=y)) +
  geom_segment(aes(xend=x+vx/50, yend=y+vy/50),
               arrow=arrow(length=unit(0.1,'cm')), size=0.25)

# Map speed to alpha
ggplot(islicesub, aes(x=x,y=y)) +
  geom_segment(aes(xend=x+vx/50, yend=y+vy/50, alpha=speed),
               arrow=arrow(length=unit(0.1,'cm')), size=0.25)

# Map spped to color, and use USA map
usa <- map_data('usa')
ggplot(islicesub, aes(x=x, y=y)) +
  geom_segment(aes(xend=x+vx/50, yend=y+vy/50, color=speed),
               arrow=arrow(length=unit(0.1,"cm")), size=0.6) +
  scale_color_continuous(low='grey80', high='darkred') +
  geom_path(aes(x=long, y=lat, group=group), data=usa) +
  coord_cartesian(xlim=range(islicesub$x), ylim=range(islicesub$y))



## 11.5. Creating a Map
library(maps)
states_map <- map_data('usa')
ggplot(states_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill='white', color='black')
# geom_path (no fill) + Mercator projection
ggplot(states_map, aes(x=long, y=lat, group=group)) +
  geom_path() + coord_map('mercator')

# get map data for world
world_map <- map_data('world')
head(world_map)
tail(world_map)

sort(unique(world_map$region))

east_asia <- map_data('world', region=c('China', 'Japan', 'North Korea',
                                        'South Korea'))
ggplot(east_asia, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(color='black') +
  scale_fill_brewer(palette='Set2')

# creating a choropleth map
crimes <- data.frame(state=tolower(rownames(USArrests)), USArrests)
head(crimes)
state_map <- map_data('state')
crime_map <- merge(state_map, crimes, by.x='region', by.y='state')
head(crime_map)
library(plyr)
crime_map <- arrange(crime_map, group, order)
head(crime_map)
ggplot(crime_map, aes(x=long, y=lat, group=group, fill=Assault)) +
  geom_polygon(color='white') + coord_map('polyconic')

# clean map
theme_clean <- function(base_size=12){
  require(grid)
  theme_grey(base_size)
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0,'cm'),
    axis.ticks.margin = unit(0,'cm'),
    panel.margin = unit(0,'lines'),
    plot.margin = unit(c(0,0,0,0), 'lines'),
    complete = TRUE)
}
# find the quantile bounds
qa <- quantile(crimes$Assault, c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
# add a column of the quantile category
crimes$Assault_q <- cut(crimes$Assault, qa,
                        labels=c('0-20%', '20-40%', '40-60%', '60-80%', '80-100%'),
                        include.lowest=TRUE)
pal <- colorRampPalette(c('#559999','grey80','#BB650B'))(5)
ggplot(crimes, aes(map_id = state, fill=Assault_q)) +
  geom_map(map=state_map, color='black') +
  scale_fill_manual(values=pal) +
  expand_limits(x=states_map$long, y=states_map$lat) +
  coord_map('polyconic') +
  labs(fill='Assault Rate\nPercentile') +
  theme_clean()



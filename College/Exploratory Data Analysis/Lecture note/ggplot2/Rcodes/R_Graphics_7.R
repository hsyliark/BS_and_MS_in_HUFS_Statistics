
###### R Graphics Lecture 7 : Controlling the Overall Appearance of Graphs ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 7.1. Setting the Title of a Graph
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p + ggtitle('Age and Height of School Children')
p + ggtitle('Age and Height\nof School Children')
p + labs(title='Age and Height \nof School Children', x='Age', y='Height')
# move the title inside
p + ggtitle('Age and Height of School Children') + 
  theme(plot.title=element_text(vjust=-2.5))
# use a text annotation instead
p + annotate('text', x=mean(range(heightweight$ageYear)), y=Inf,
             label='Age and Height of School Children', vjust=1.5, size=5)


## 7.2. Changing the Appearance of Text
p + theme(axis.title.x=element_text(size=16, lineheight=0.9, family='Times',
                                    face='bold.italic', color='red'))
p + ggtitle('Age and Height\nof School Children') +
  theme(plot.title=element_text(size=rel(1.5), lineheight=0.9, family='Times',
                                face='bold.italic', color='darkblue'))
p + geom_text(aes(label=weightLb), size=4, family='Times', color='gold')
# See the text properties of theme elements and text geoms in the lecture note
# See the theme items that control text appearance in theme() in the lecture note


## 7.3. Using Themes
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
# Grey theme (the default)
p + theme_grey()
# Black-and-white theme
p + theme_bw()

# The default font family is Helvetica and the default size is 12
p + theme_grey(base_size=16, base_family='Times')

# Set default theme for current session
theme_set(theme_bw())
p
# Reset the default theme back to theme_grey()
theme_set(theme_grey())
p


## 7.4. Changing the Appearance of Theme Elements
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point()
# Options for the plotting area
p + theme(
  panel.grid.major = element_line(color='red'),
  panel.grid.minor = element_line(color='red', linetype='dashed', size=0.2),
  panel.background = element_rect(fill='lightblue'),
  panel.border = element_rect(color='blue', fill=NA, size=2))

# Options for text items
p + ggtitle('Plot title here') +
  theme(
    axis.title.x = element_text(color='red',size=14),
    axis.text.x = element_text(color='blue'),
    axis.title.y = element_text(color='red', size=14, angle=90),
    axis.text.y = element_text(color='blue'),
    plot.title = element_text(color='red', size=20, face='bold'))

# Options for the legend
p + theme(
  legend.background = element_rect(fill='grey85', color='red', size=1),
  legend.title = element_text(color='blue', face='bold', size=14),
  legend.text = element_text(color='red'),
  legend.key = element_rect(color='blue', size=0.25))

# Options for facets
p + facet_grid(sex ~ .) + theme(
  strip.background = element_rect(fill='pink'),
  strip.text.y = element_text(size=14, angle=90, face='bold'))

# See theme items that control text appearance in theme() from the lecture notes








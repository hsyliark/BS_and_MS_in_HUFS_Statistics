
# bar plot
hotdogs <- read.csv('http://datasets.flowingdata.com/hot-dog-contest-winners.csv',
                   sep=',', header=TRUE)
fill_colors <- c()
for( i in 1:length(hotdogs$New.record) ){
  if (hotdogs$New.record[i]==1) {
    fill_colors <- c(fill_colors, '#821122')
  } else {
    fill_colors <- c(fill_colors, '#cccccc')
  }
}
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors, border=NA, 
        space=0.3, xlab='Year', ylab='Hot dogs and buns (HDB) eaten',
        main='Nathan\'s Hot Dog Eating Contest Results, 1980-2010' )


# treemap
library('portfolio')
posts <- read.csv('http://datasets.flowingdata.com/post-data.txt')
map.market(id=posts$id, area=posts$views, group=posts$category,
           color=posts$comments, main='FlowingData Map')


# bubble chart
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.tsv',
                  header=TRUE, sep='\t')
radius <- sqrt(crime$population / pi)
symbols(crime$murder, crime$burglary, circles=radius, inches=0.35,
       fg='white', bg='red', xlab='Murder Rate', ylab='Burglary Rate')
text(jitter(crime$murder,factor=1), jitter(crime$burglary,factor=3), 
     crime$state, cex=0.5)


# heatmap
bball <- read.csv('http://datasets.flowingdata.com/ppg2008.csv',header=TRUE)
head(bball)
str(bball)
bball <- bball[order(bball$PTS,decreasing=FALSE),]
rownames(bball) <- bball$Name
bball <- bball[,-1]
is.matrix(bball)
bball_mat <- as.matrix(bball)
heatmap(bball_mat, Rowv=NA, Colv=NA, col=cm.colors(256),
                         scale='column', margins=c(5,10))
library(RColorBrewer)
heatmap(bball_mat, Rowv=NA, Colv=NA, col=brewer.pal(9,'Blues'),
        scale='column',margins=c(5,10))
red_colors <- c('#ffd3cd', '#ffc4bc', '#ffb5ab', '#ffa69a', '#ff9789', '#ff8978',
                '#ff7a67', '#ff6b56', '#ff5c45', '#ff4d34')
heatmap(bball_mat, Rowv=NA, Colv=NA, col=red_colors,
        scale='column',margins=c(5,10))


# Chernoff face
#install.packages('aplpack')
library(aplpack)
bball <- read.csv('http://datasets.flowingdata.com/ppg2008.csv',header=TRUE)
faces(bball[,2:16], labels=bball$Name, face.type=1)


# Nightingale chart
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState-formatted.csv')
str(crime)
rownames(crime)<-crime$state
crime <- crime[,2:7]
stars(crime, flip.labels=FALSE, key.loc=c(15,1.5), draw.segments=TRUE)

# multidimensional scaling
education <- read.csv('http://datasets.flowingdata.com/education.csv', header=TRUE)
str(education)
ed.dis <- dist(education[,-1])
ed.mds <- cmdscale(ed.dis)  # compute 2D coordinates
plot(ed.mds[,1],ed.mds[,2],type='n')
text(ed.mds[,1],ed.mds[,2],labels=education$state)
#install.packages('mclust')
library(mclust)
ed.mclust <- Mclust(ed.mds)
plot(ed.mclust, what='classification')


# maps
library(maps)
costcos <- read.csv('http://book.flowingdata.com/ch08/geocode/costcos-geocoded.csv')
map(database='state', col='#cccccc')
symbols(costcos$Longitude, costcos$Latitude, bg='#e2373f', fg='#ffffff', 
        lwd=0.5, circles=rep(1,nrow(costcos)), inches=0.05, add=TRUE)

map(database='state',region=c('California','Nevada','Oregon','Washington'),
    col='#cccccc')
symbols(costcos$Longitude, costcos$Latitude, bg='#e2373f', fg='#ffffff',
        lwd=0.5, circles=rep(1,nrow(costcos)), inches=0.05, add=TRUE)

# map with lines
faketrace <- read.csv('http://book.flowingdata.com/ch08/points/fake-trace.txt',
                      sep='\t')
faketrace
map(database='world', col='#cccccc')
lines(faketrace$longitude,faketrace$latitude,col='#bb4cd4',lwd=2)
symbols(faketrace$longitude, faketrace$latitude, lwd=1, bg='#bb4cd4', fg='#ffffff',
        circles=rep(1,nrow(faketrace)), inches=0.05, add=TRUE)


# map with bubles
fertility <- read.csv('http://book.flowingdata.com/ch08/points/adol-fertility.csv')
map('world', col='#cccccc')
symbols(fertility$longitude, fertility$latitude, circles=sqrt(fertility$ad_fert_rate),
        add=TRUE, inches=0.15, bg='#93ceef', fg='#ffffff')
text(135,39, 'Korea', cex=0.5)
text(140,33, 'Japan', cex=0.5)
text(-100,35, 'USA', cex=0.5)

# word cloud
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
path <- system.file('xkcd', package='RXKCD')
datafiles <- list.files(path)
xkcd.df <- read.csv(file.path(path, datafiles))
xkcd.corpus <- Corpus(DataframeSource(data.frame(xkcd.df[,3])))
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, tolower)
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords('english')))
tdm <- TermDocumentMatrix(xkcd.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word=names(v), freq=v)
d <- d[-1,]
pal <- brewer.pal(9, 'BuGn')
pal <- pal[-(1:2)]
wordcloud(d$word, d$freq, scale=c(8,0.3), min.freq=2, max.words=100, random.order=T, rot.per=0.15, colors=pal, vfont=c('sans serif', 'plain'))


require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)
u <- 'http://cran.r-project.org/web/packages/available_packages_by_date.html'
t <- readHTMLTable(u)[[1]]
ap.corpus <- Corpus(DataframeSource(data.frame(as.character(t[,3]))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords('english')))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m), decreasing=TRUE)
ap.d <- data.frame(word=names(ap.v), freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,'Dark2')
wordcloud(ap.d$word, ap.d$freq, scale=c(8,0.2), min.freq=3, max.words=Inf, random.order=FALSE, rot.per=0.15, colors=pal2)




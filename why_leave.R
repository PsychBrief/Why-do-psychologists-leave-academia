## Richard D. Morey
## 3 Nov 2017

# Redirect plots to temp directory
# We just want the variables
wd = getwd()
setwd(tempdir())
source('https://osf.io/x73zq/download?version=1')
setwd(wd)

## Prepare data

library(corrplot)
library(viridis)

props = table(plot.dat$reason, plot.dat$scores)
props = props/rowSums(props)
resp = c("(missing)","Not","Slightly","Moderately","Highly","Extremely")

leave.dat.sub2 = data.frame(lapply(leave.dat.sub,function(v) {
  v[v==0]=NA
  v
}))

rownames(props) = gsub("_", " ", rownames(props),fixed = TRUE)
colnames(leave.dat.sub2) = gsub("_", " ", colnames(leave.dat.sub2),fixed = TRUE)


ord = sort(colMeans(leave.dat.sub2,na.rm = TRUE))
props = props[names(ord),]

### Plots


quartzFonts(avenir = c("Avenir Book", "Avenir Black", 
                       "Avenir Book Oblique", 
                        "Avenir Black Oblique"))

png(filename = "respplot.png", width=1100, height=800,pointsize = 20)
par(mar = c(10,7,.5,.5),family = 'avenir')
plot(1,1,ty='n', asp=TRUE,
     ylim = c(.5, ncol(props)+.5),
     xlim = c(.5, nrow(props)+.5),
     axes=FALSE, ylab = "", xlab = "")

mtext("Response: Relevant?",2,6,adj=.5)

scale.rect = 1
cols = c(rgb(1,1,1,1), viridis::viridis(5))

for(i in 1:nrow(props)){
  for(j in 1:ncol(props)){
    r = sqrt(props[i,j])/2 * scale.rect
    rect(i - r,j - r, i + r, j + r, col = cols[j])
  }
}

axis(2, at = 1:ncol(props), lab = resp, las = 1, lty = 0)
axis(1, at = 1:nrow(props), lab = rownames(props), las = 2, lty = 0)
abline(v = 1:(nrow(props)-1)+.5, col =rgb(0,0,0,.2))

dev.off()

##
png(filename = "corplot.png", width=800, height=800,pointsize = 20)
par(family = 'avenir')
M <- cor(leave.dat.sub2,use = "pair")
corrplot(M, method = "ellipse",order = "hclust")
dev.off()


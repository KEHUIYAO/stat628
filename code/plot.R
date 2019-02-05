

##ggplot
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#Figure 1 Linear

library("ggplot2")
p1 = ggplot(dat, aes(x = AGE, y = BODYFAT)) + geom_point(alpha=0.9,size=0.3)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p2 = ggplot(dat, aes(x = WEIGHT, y = BODYFAT)) + geom_point(alpha=0.9,size=0.3)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p3= ggplot(dat, aes(x = HEIGHT, y = BODYFAT)) + geom_point(alpha=0.9,size=0.3)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p4 = ggplot(dat, aes(x = ADIPOSITY, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p5 = ggplot(dat, aes(x = NECK, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p6 = ggplot(dat, aes(x = CHEST, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p7 = ggplot(dat, aes(x = ABDOMEN, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p8=  ggplot(dat, aes(x = HIP, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p9 = ggplot(dat, aes(x = THIGH, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p10= ggplot(dat, aes(x = KNEE, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p11= ggplot(dat, aes(x = ANKLE, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p12= ggplot(dat, aes(x = BICEPS, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p13 =ggplot(dat, aes(x = FOREARM, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
p14 = ggplot(dat, aes(x = WRIST, y = BODYFAT)) + geom_point(alpha=0.9,size=0.1)  + stat_smooth(method = lm) +
  theme(plot.title = element_text(hjust = .5))
multiplot(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14, cols=7)

##figure 2
mod=final
modf <- fortify(mod)
ggplot(modf, aes(x = .fitted, y = .resid)) + geom_point()
library(ggfortify)
##residual 
autoplot(mod,which=1:2)
6## cook distance 

par(mfrow = 1)
autoplot(mod,which=4)
##############

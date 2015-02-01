#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

library(graphics)

data(airquality)
attach(airquality)

# Part (1):
# 
# Drop "Month" and "Day" :
#
#postscript("../../WriteUp/Graphics/Chapter1/prob_1_pairs_plot.eps", onefile=FALSE, horizontal=FALSE)
pairs(airquality[,1:4], panel = panel.smooth, main = "airquality data")
#dev.off()

# Part (3):
# 
#postscript("../../WriteUp/Graphics/Chapter1/prob_1_boxplot_by_month.eps", onefile=FALSE, horizontal=FALSE)
boxplot( Ozone ~ Month, data=airquality, xlab="month index; 5-9=May-Sept", ylab="ozone (ppb)" )
#dev.off()

#postscript("../../WriteUp/Graphics/Chapter1/prob_1_boxplot_by_day.eps", onefile=FALSE, horizontal=FALSE)
boxplot( Ozone ~ Day, data=airquality, xlab="day index", ylab="ozone (ppb)" )
#dev.off()

# Part (5):
# 
library(lattice)

#postscript("../../WriteUp/Graphics/Chapter1/prob_1_cloud_plot.eps", onefile=FALSE, horizontal=FALSE)
cloud( Ozone ~ Temp + Wind, data=airquality )
#dev.off()

# Part (6):
#
#postscript("../../WriteUp/Graphics/Chapter1/prob_1_coplot.eps", onefile=FALSE, horizontal=FALSE)
coplot( Ozone ~ Temp | Wind, data=airquality )
#dev.off()

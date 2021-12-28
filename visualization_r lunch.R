rm(list=ls())
library(ggcorrplot)
library(lattice)
library(grid)
library(ggplot2)
library(ggExtra)

#Sources:
https://learnr.wordpress.com/2009/08/26/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-final-part/
https://www.statmethods.net/advgraphs/trellis.html
https://github.com/SamanthaToet/ggplot2_useRs
https://github.com/SamanthaToet/Graphing
http://www.sthda.com/english/wiki/lattice-graphs
https://www.stat.auckland.ac.nz/~ihaka/787/lectures-trellis.pdf
http://www.sthda.com/english/wiki/add-legends-to-plots-in-r-software-the-easiest-way #legends
http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
http://r-statistics.co/Complete-Ggplot2-Tutorial-Part2-Customizing-Theme-With-R-Code.html
http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
http://www.ggplot2-exts.org/gallery/
https://www.r-graph-gallery.com/portfolio/data-art/
https://www.r-bloggers.com/visual-art-with-pi-using-ggplot2-circlize/
http://www.sthda.com/english/wiki/ggplot2-essentials
https://www.aridhia.com/technical-tutorials/the-fundamentals-of-ggplot-explained/

################################## Base ######################################
#### Graphics Devices ####

# Graphics devices are things where you can make a plot appear, EX. a window 
#   on your computer (screen device), PDF or JPG (file device). Plots have to 
#   be "sent" to a graphics device. On a mac the most common place is a screen
#   device launched with quartz()

# There are two types files devices: vector and bitmap
# Vector formats: 
# PDF: line graphics, resizes well, good for simple plots
# SVG (scalable vector graphics): xml based, good for interactive data
# and web-based plots
# Bitmap formats: don't resize well
# PNG (portable network graphics): line graphics, good for plotting 
# many, many points, lossless compression, doesn't resize well
# JPG: pictures, lossy compression, don't resize well, can be read by
# most web browsers
# TIFF: supports lossless compression

# To see what graphics devices are availibale on your system:
?Devices 

# To see current plotting device:
dev.cur()

# To reset defaults:
dev.off()


#### Parameters ####

# par() is used to specify global parameters, type par("your parameter")
# to find out what is set as that value 
# pch = the plotting symbol (default is open circle)
# lty = line type (default is solid)
# lwd = line width specified as an int
# col = plotting color  (can be found here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)
# xlab = char str for x axis label
# ylab = char str for y axis label
# las = orientation of x axis labels 
# bg = background color
# mar = margin size
# oma = outer margin size (default is 0) EX. title of multiple plots
# mfrow = number of plots per row, column (plots are filled row-wise)
# EX. mfrow(c(1, 2)) is one row with 2 cols, for side by side plots
# mfcol = number of plots per row, column (plots are filled column-wise)


#### Base R Graphics ####

# Functions:
# plot() make a scatterplot or other type of plot depending on class
# lines() add lines given vector of x values and corresponding y values
# points() add points to a plot 
# text() add text labels to a plot using specific x, y coordinates
# title() add annotations to x, y axis labels, title, subtitle, or 
#outer margins
# mtext() add arbitrary text to the margins
# axis() add axis ticks/labels

# Inclusive of Base PLotting

# 1.) Five Number Summary:
summary(iris$Sepal.Length)

# 2.) Boxplot:
boxplot(iris$Sepal.Length, col = "red")
# Add overlaying horizontal line at 12:
abline(h = 7.3)
# Show two boxplots side by side:
# y ~ x shows that y depends on x. If both x and y come from the same
# df you can specify a data arg set equal to the df so that you 
# don't have to keep typing df$col:
boxplot(iris$Sepal.Length~iris$Species)
boxplot(iris$Sepal.Length~iris$Species, col = c("red", "blue", "green"))

# 3.) Histogram:
hist(iris$Sepal.Length, col = "deepskyblue3")
# Plot all points below histogram:
rug(iris$Sepal.Length)
# Change the number of breaks or bars:
hist(iris$Sepal.Length, col = "firebrick4", breaks = 100)
# Add overlaying vertical line at 12 with a width of 2:
abline(v = 4.8, lwd = 2)
# Add overlaying vertical line in magenta at the median with width of 4:
abline(v = median(iris$Sepal.Length), col = "magenta", lwd = 4)
# Plot multiple histograms in one plot:
# First specify how you want plots displayed and margins:
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
# Subset:
setosa <- subset(my_data, Species == "setosa")
# Plot:
hist(setosa$Sepal.Length, col = "lightpink2")
hist(subset(my_data,Species == "versicolor")$Sepal.Length, col = "green")
hist(subset(my_data,Species == "virginica")$Sepal.Length, col = "lightgoldenrod2")

# 4.) Barplot:
barplot(table(iris$Sepal.Length), col = "wheat", main = "Flowers")
#Re-set par(mfrow)
par(mfrow=c(1,1))
barplot(table(iris$Sepal.Length), col = "wheat", main = "Flowers")

# 5.) Scatterplot:
with(my_data, plot(Sepal.Length, Sepal.Width))

plot(my_data$Sepal.Length, my_data$Sepal.Width, col = my_data$Species)

plot(my_data$Sepal.Length, my_data$Sepal.Width, col = my_data$Species, 
     xlab = "Length", ylab="Width")

#add legend
legend("topright", inset=.02, title="Species",
       c("setosa", "versicolor", "virginica"), fill=c("black", "red", "green"),
       horiz=TRUE, cex=0.8)




#################################### Lattice Plotting System ####################################


# Lattice is implemented using two packages. The first is called, not surprisingly, lattice, and it
# contains code for producing Trellis graphics. Some of the functions in this package are the higher
# level functions which you, the user, would call. These include xyplot, bwplot, and levelplot.

# The Lattice Package:
#   - Contains code for producing Trellis graphics
#   - xyplot, bwplot, levelplot
#   - Lattice package builds on the Grid package
#   - All plotting and annotating done in one function call

# Functions:
#   - xyplot: scatterplots 
xyplot(y ~ x | f * g, data)
#       - f and g are conditioning variables (they're optional)
#       - the * indicates an interaction between the two variables

# 1) Lattice functions generally take a formula for their first argument, usually of the form y ~ x.
# This indicates that y depends on x, so in a scatterplot y would be plotted on the y-axis and x on the x-axis.

# 2) The second argument is the data frame or list from which the variables in the formula should be looked up.  If no data frame or list is passed, then the parent frame is used. If no other arguments are passed, the default values are used.


# graph_type	  description	                formula examples
# barchart	    bar chart	                  x~A or A~x
# bwplot	      boxplot	                    x~A or A~x
# cloud	        3D scatterplot	            z~x*y|A
# contourplot	  3D contour plot	            z~x*y
# densityplot	  kernal density plot	        ~x|A*B
# dotplot	      dotplot	                    ~x|A
# histogram	    histogram	                  ~x
# levelplot	    3D level plot	              z~y*x
# parallel	    parallel coordinates plot	  data frame
# splom	        scatterplot matrix	        data frame
# stripplot	    strip plots	                A~x or x~A
# xyplot	      scatterplot	                y~x|A
# wireframe	    3D wireframe graph	        z~y*x



#### Lattice vs. Base Plotting ####
#   - Base plots directly into graphics device (screen, PDF, PNG, etc.)
#   - Lattice retuns an object of class TRELLIS:
#       - print methods plot the data to a grphics device
#       - Lattice functions return "plot objects" that can be stored (but it's
#           usually better to just save the code and data)
#       - On the commmand line, trellis objects are auto-printed so it appears
#           that the function is plotting the data



#### Panel Functions ####
#   - Panel functions control what happens inside each panel of the plot
#   - Lattice package comes with default panel functions, but you can supply your own
#       to customize
#   - Panel funcs receive x, y coords of data points in their panel

xyplot(y ~ x | f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...) # First call default panel func for xyplot
  panel.abline(h = median(y), lty = 2)
})

names(p) # Returns all named properties of plot


# Lattice Examples
iris <- iris
head(iris)

# Default plot
xyplot(Sepal.Length ~ Petal.Length, data = my_data)

# Color by groups
xyplot(Sepal.Length ~ Petal.Length, group = Species, 
       data = my_data, auto.key = TRUE)

# Show points ("p"), grids ("g") and smoothing line
# Change xlab and ylab
xyplot(Sepal.Length ~ Petal.Length, data = my_data,
       type = c("p", "g", "smooth"),
       xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

xyplot(Sepal.Length ~ Petal.Length | Species, 
       group = Species, data = my_data,
       type = c("p", "smooth"),
       scales = "free")

# Basic 3D scatter plot
cloud(Sepal.Length ~ Sepal.Length * Petal.Width, 
      data = iris)

# Color by groups; auto.key = TRUE to show legend
cloud(Sepal.Length ~ Sepal.Length * Petal.Width, 
      group = Species, data = iris,
      auto.key = TRUE)

#
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

# Basic box plot
bwplot(len ~ dose,  data = ToothGrowth,
       xlab = "Dose", ylab = "Length")

# Violin plot using panel = panel.violin
bwplot(len ~ dose,  data = ToothGrowth,
       panel = panel.violin,
       xlab = "Dose", ylab = "Length")

# Basic dot plot
dotplot(len ~ dose,  data = ToothGrowth,
        xlab = "Dose", ylab = "Length")

# Basic stip plot
stripplot(len ~ dose,  data = ToothGrowth,
          jitter.data = TRUE, pch = 19,
          xlab = "Dose", ylab = "Length")


# Box plot
bwplot(len ~ supp | dose,  data = ToothGrowth,
       layout = c(3, 1),
       xlab = "Dose", ylab = "Length")

# Violin plot
bwplot(len ~ supp | dose,  data = ToothGrowth,
       layout = c(3, 1), panel = panel.violin,
       xlab = "Dose", ylab = "Length")

# Dot plot
dotplot(len ~ supp | dose,  data = ToothGrowth,
        layout = c(3, 1),
        xlab = "Dose", ylab = "Length")

# Strip plot
stripplot(len ~ supp | dose,  data = ToothGrowth,
          layout = c(3, 1), jitter.data = TRUE,
          xlab = "Dose", ylab = "Length")

#Density Plot
densityplot(~ len, data = ToothGrowth,
            plot.points = FALSE)

#Histogram
histogram(~ len, data = ToothGrowth,
          breaks = 20)

#Density Plot
a <- densityplot(~ len, groups = dose, data = ToothGrowth,
plot.points = FALSE, auto.key = TRUE)

names(a)




#################################### ggplot2 ####################################

# The concept behind ggplot2 divides plot into three different fundamental parts: Plot = data + Aesthetics + Geometry.

# The principal components of every plot can be defined as follow:
# data is a data frame
# Aesthetics is used to indicate x and y variables. It can also be used to control the color, the size or the shape of points, the height of bars, etc...
# Geometry defines the type of graphics (histogram, box plot, line plot, density plot, dot plot, ..)
# Facets allow you to split a plot into a matrix of panels
# Scales... self-explanatory

# There are two major functions in ggplot2 package: qplot() and ggplot() functions.
# qplot() stands for quick plot, which can be used to produce easily simple plots.
# ggplot() function is more flexible and robust than qplot for building a plot piece by piece.

# Grammar of graphics: a statistical graphic is a MAPPING from data to AESTHETIC
#   attributes (color, shape, size), of GEOMETRIC objects (points, lines, bars).
#   It may also contain statistic transformations of the data and is drawn on
#   a specific coordinate system. 

# The ggplot2 package is composed of a set of independent components that can be composed in many different
# ways. ... you can create new graphics that are precisely tailored for your problem." These components include
# aesthetics which are attributes such as colour, shape, and size, and geometric objects or geoms such as
# points, lines, and bars.

# The ggplot2 package has 2 workhorse functions. The more basic workhorse function is qplot, (think quick plot),
# which works like the plot function in the base graphics system. It can produce many types of plots (scatter,histograms, box and whisker) while hiding tedious details from the user. Similar to lattice functions, it
# looks for data in a data frame or parent environment. The more advanced workhorse function in the package is ggplot, which is more flexible and can be customized
# for doing things qplot cannot do.

# Basic components of ggplot2:
# 1.) A data frame
# 2.) Aesthetic mappings: how data are mapped to color, size
# 3.) Geoms: geometric objects like points, lines, shapes
# 4.) Facets: for conditional plots
# 5.) Stats: statistical transformations like binning, quartiles, smoothing
# 6.) Scales: what scale an aesthetic map uses (EX. male = red, female = blue)
# 7.) Coordinate system

# Plots are built in layers:
#   - Plot the data
#   - Overlay a summary
#   - Metadata and annotation


# 1) Setup
# First, you need to tell ggplot what dataset to use. 
# This is done using the ggplot(df) function, where df is a dataframe that contains all features needed to make the plot. 
#This is the most basic step. Unlike base graphics, ggplot doesn't take vectors as arguments.

#Examples of establishing dataset (However, no plot will be printed until you add the geom layers.)
ggplot(diamonds)  # if only the dataset is known.
ggplot(diamonds, aes(x=carat))  # if only X-axis is known. The Y-axis can be specified in respective geoms.
ggplot(diamonds, aes(x=carat, y=price))  # if both X and Y axes are fixed for all layers.
ggplot(diamonds, aes(x=carat, color=cut))  # Each category of the 'cut' variable will now have a distinct  color, once a geom is added.


# 2) Layers
# The layers in ggplot2 are also called 'geoms'. 
# Once the base setup is done, you can append the geoms one on top of the other.
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth() # Adding scatterplot geom (layer1) and smoothing geom (layer2).
# We have added two layers (geoms) to this plot - the geom_point() and geom_smooth(). 
# Since the X axis Y axis and the color were defined in ggplot() setup itself, these two layers inherited those aesthetics. 
# Alternatively, you can specify those aesthetics inside the geom layer also as shown below.
ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) + geom_smooth(aes(x=carat, y=price)) # Remove color from geom_smooth
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(aes(color=cut)) + geom_smooth()  # same but simpler


# 3) Labels
gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + labs(title="Scatterplot", x="Carat", y="Price")  # add axis lables and plot title.
print(gg)


# 4) Facets
# What if you want one chart for one cut?
gg + facet_wrap( ~ cut, ncol=3)  # columns defined by 'cut'


#New data
# Setup
options(scipen=999)  # turn off scientific notation like 1e+06
data("midwest", package = "ggplot2")  # load the data
# midwest <- read.csv("http://goo.gl/G1K41K") # alt source 
# Init Ggplot
ggplot(midwest, aes(x=area, y=poptotal))  # area and poptotal are columns in 'midwest'


# 5) Scatterplot
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()
#add regression line
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(g)


# 6) Adjust axis and titles
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
# Add Title and Labels
g1 + labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
# or
g1 + ggtitle("Area Vs Population", subtitle="From midwest dataset") + xlab("Area") + ylab("Population")


# 7) Change the Color and Size of Points
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
plot(gg)


# 8) Histogram
# load package and data
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")


# 9) Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")
# ggMarginal(g, type = "density", fill="transparent")


# 10) Correl
data(mtcars)
corr <- round(cor(mtcars), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)


# 11) Diverging bars
theme_set(theme_bw())  

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()


# 12) Dot + Box Plot
theme_set(theme_bw())

# plot
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot + Dot plot", 
       subtitle="City Mileage vs Class: Each dot represents 1 row in source data",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


# 13) pie chart
theme_set(theme_classic())

# Source: Frequency table
df <- as.data.frame(table(mpg$class))
colnames(df) <- c("class", "freq")
pie <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)

# Source: Categorical variable.
# mpg$class
pie <- ggplot(mpg, aes(x = "", fill = factor(class))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)


# 14) bar chart
# From on a categorical column variable
g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")

#################################### Pros/Cons of each ####################################

#Base
## "Artist's palette" model
## Start with blank canvas and build up from there
## Start with plot function (or similar)
##  Use annotation functions to add/modify (text, lines, points, axis)

## Pros:
## Convenient, mirrors how we think of building plots and analyzing data

## Cons:
## Can't go back once plot has started (i.e. to adjust margins);
## need to plan in advance
## Difficult to "translate" to others once a new plot has been created (no graphical "language"). Plot is just a series of R commands

#Lattice
## Plots are created with a single function call (xyplot, bwplot, etc.)

## Pros:
## Most useful for conditioning types of plots: Looking at how y changes with x across levels of z
## Thinks like margins/spacing set automatically because entire plot is specified at once
## Good for putting many many plots on a screen

## Cons:
##  Sometimes awkward to specify an entire plot in a single function call
## Annotation in plot is not intuitive
## Use of panel functions and subscripts difficult to wield and requires intense preparation
## Cannot "add" to the plot once it's created


# ggplot2
## Pros:
## Split the difference between base and lattice
## Automatically deals with spacing, text, titles but also allows you to annotate by "adding"
## Superficial similarity to lattice but generally easier/more intuitive to use
## Default mode makes many choices for you (but you can customize!)

## Cons:
## "magic" evaluation model (i.e., very different from base R and other packages, perhaps harder to understand); 
## Probably even harder to customize (beyond what the package allows) than lattic
## no 3-D perspective plots corresponding to persp() [base], wireframe()/cloud() [lattice];
## lattice contains some functionality, like banking (aspect ratio control), that is not in ggplot;
## speed; ggplot is slow compared to lattice, especially for faceting.





#################################### Arts ####################################
# generate pairs of x-y values
x = seq(-50, 50, by = 1)
y = -(x^2)
# set graphic parameters
op = par(bg = 'black', mar = rep(0.5, 4))
# Plot 
plot(y, x, type = 'n')
lines(y, x, lwd = 2*runif(1), col = hsv(0.08, 1, 1, alpha = runif(1, 0.5, 0.9)))
for (i in seq(10, 2500, 10))
{
  lines(y-i, x, lwd = 2*runif(1), col = hsv(0.08, 1, 1, alpha = runif(1, 0.5, 0.9)))
}
for (i in seq(500, 600, 10))
{
  lines(y - i, x, lwd = 2*runif(1), col = hsv(0, 1, 1, alpha = runif(1, 0.5, 0.9)))
}
for (i in seq(2000, 2300, 10))
{
  lines(y - i, x, lwd = 2*runif(1), col = hsv(0, 1, 1, alpha = runif(1, 0.5, 0.9)))
}
for (i in seq(100, 150, 10))
{
  lines(y - i, x, lwd = 2*runif(1), col = hsv(0, 1, 1, alpha = runif(1, 0.5, 0.9)))
}






moxbuller = function(n) {   
  u = runif(n)   
  v = runif(n)   
  x = cos(2*pi*u)*sqrt(-2*log(v))  
  y = sin(2*pi*v)*sqrt(-2*log(u))
  r = list(x=x, y=y)
  return(r) 
}
r = moxbuller(50000) 
par(bg="black") 
par(mar=c(0,0,0,0)) 
plot(r$x,r$y, pch=".", col="blue", cex=1.2)
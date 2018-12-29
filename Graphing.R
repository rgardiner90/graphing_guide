G# Graphing
?plot

# COLORS:
# By the way, a chart of R's color palette can be found at
# http://research.stowers-institute.org/efg/R/Color/Chart/
# including a downloadable PDF version. This chart uses
# numbers for colors instead of names. To get the names, use
colors() [18]  # Which will output [1] "beige"
colors()[c(552, 254, 26)]  # Gives [1] "red" "green" "blue"

# BAR CHARTS (CATEGORICAL DATA)

# Load data
require("datasets")
?chickwts
data("chickwts")
View(chickwts)

# Simplest method
plot(chickwts$feed)

# "barplot" offers more control but must prepare data: R doesn't create bar charts direction from the 
# categorical variables; instead we first must create a table that has the frequencies for each level
# of the variable
feeds <- table(chickwts$feed)
feeds
barplot(feeds) # identical to the earlier plot
?barplot

# to put the bars in descending order:
barplot(feeds[order(feeds, decreasing = TRUE)])

# customize the chart
par(oma = c(1, 1, 1, 1)) # sets outside margins: bottom, left, top, right
par(mar = c(4, 5, 2, 1)) # sets plot margins
barplot(feeds[order(feeds)],
        horiz = TRUE,
        las = 1, # las gives orientation of axis labels
        col = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA, # no borders on bars,
        main = "Frequencies of Different Feeds\nin chickwts Dataset", # \n = line break,
        xlab = "Number of Chicks")


?par


     # BAR PLOTS FOR GROUPED MEANS

# load data
?InsectSprays
spray <- InsectSprays
View(spray)

# to plot means, first get the means for the groups
means <- aggregate(spray$count ~ spray$spray, FUN = "mean") # FUN means function type
means

plot(means) # get lines for means, but very basic.  Much better to get a barplot

# to get a barplot, need to extract means and reorganize
mean.data <- t(means[-1]) # new dataset that removes first colum and then transposes (t) second column to rows
mean.data
colnames(mean.data) <- means[, 1] # in the mean.data dataset, because we did transpose, we don't have 
# names. Colnames assigns columns and we are saying on the right side to use the rows in the first 
#column (which has the spray names)
mean.data

#basic barplot
barplot(mean.data)

# modified barplot
barplot(mean.data,
        col = "lightblue",
        main = "Effectiveness of Insect Spray",
        xlab = "Spray Used",
        ylab = "Insect Count")


     # CLUSTERED BAR CHARTS FOR FREQUENCIES
# load data
?warpbreaks
data("warpbreaks")

# this doesn't work with data as is:
barplot(breaks ~ wool*tension, data = warpbreaks) # data is structured wrong

# so we need to use tapply
?tapply
data <- tapply(warpbreaks$breaks,
               list(warpbreaks$wool,
                    warpbreaks$tension),
               mean)
View(data)

barplot(data,
        beside = TRUE, # this means we want things to be grouped
        col = c("steelblue3", "thistle3"),
        bor = NA, # turning off borders
        main = "Mean Number of Warp Breaks \nby Tension and Wool",
        xlab = "Tension",
        ylab = "Mean Number of Breaks")
# for legend, "locator(1)" is interactive and lets you lock where you wnat ot put the legen.
# you can also do it with coordinates.
legend(locator(1), # after you run this, look at the plot section and it will say that "locator is active
       rownames(data),
       fill = c("steelblue3", "thistle3"))



# PIE CHARTS (CATEGORICAL VARIABLE)
# using the chickwts dataset, so reload if necessary

# Create a table with frequencies 
feeds <- table(chickwts$feed) # same as before
feeds

# make basic pie chart
pie(feeds)
?pie

# Modified pie charts
pie(feeds[order(feeds, decreasing = TRUE)],
    init.angle = 90, # starts as 12 o'clock instead of 3
    clockwise = TRUE, # default is FALSE
    col = c("seashell", "cadetblue2", "lightpink", "lightcyan", "plum1", "papayawhip"),
    main = "Pie Chart for Feeds from Chickwts")

# !!!!! WHY PIE CHARTS ARE TERRIBLE:

# Three data sets
pie.a <- c(22, 14, 18, 20, 14, 12)
pie.b <- c(20, 18, 16, 18, 16, 12)
pie.c <- c(12, 14, 20, 18, 14, 22)
# Changing graphical parameters for a minute
oldpar <- par()   # Stores old graphical parameters
par(mfrow    = c(1, 3),  # Num. rows/cols
    cex.main = 3)   #  Main title 3x bigger
colors <- c("grey98", "grey90", "lightskyblue", "lightgreen", "grey98", "grey90")
?colors

# Three pie charts side by side
# Is the green slice or blue slice bigger?
pie(pie.a, main = "Pie A", col = colors)
pie(pie.b, main = "Pie B", col = colors)
pie(pie.c, main = "Pie C", col = colors)

# Three bar charts side by side
# Is the green bar or blue bar bigger?
barplot(pie.a, main = "Bar A", col = colors)
barplot(pie.b, main = "Bar B", col = colors)
barplot(pie.c, main = "Bar C", col = colors)

# CONCLUSION
# From R help on pie charts:
?pie
# Pie charts are a very bad way of displaying information.
# The eye is good at judging linear measures and bad at
# judging relative areas. A bar chart or dot chart is a
# preferable way of displaying this type of data.
# 
# Cleveland (1985), page 264: “Data that can be shown by
# pie charts always can be shown by a dot chart. This means
# that judgements of position along a common scale can be
# made instead of the less accurate angle judgements.”
# This statement is based on the empirical investigations
# of Cleveland and McGill as well as investigations by
# perceptual psychologists.

par(oldpar)  # Restore old graphical parameters



# HISTOGRAM (quantitative variables)

# load data
require("datasets")
?lynx
data(lynx)

# made default histogram
hist(lynx)
?hist

# modified histogram
hist(lynx,
     breaks = 11, # "suggests" 11 bins
#           breaks = seq(0, 7000, by = 100),
#           breaks = c(0, 100, 300, 500, 3000, 3500, 7000),
     freq = FALSE,
     col = "thistle1", # Or use: col = colors() [626]
     main = "Histogram of Annual Canadian Lynx Trappings\n1821-1934",
     xlab = "Number of Lynx Trapped")
# IF freq = FALSE, this will draw a normal distribution
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4",
      lwd = 2,
      add = TRUE)
abline(h=0, col = "gray60")
?curve


# DENSITY PLOT WITH VERTICAL LINE FOR MEAN
data(lynx)
require(lattice)

density <- density(lynx, from = 0)
plot(density, main = "Lynx Dataset")
abline(v = mean(lynx), col = "red") # this is the vertical line



# BOXPLOTS (QUANTITATIVE VARIABLES)
# load dataset
require("datasets")
# Lawyers' Ratings of State Judges in the US Superior Court (c. 1977)
?USJudgeRatings
USJudgeRatings  # View data
data(USJudgeRatings)  # Load into workspace
View(USJudgeRatings)
# At least two errors in data file:
# 1. Data appears to be on 1-10 or 0-10 scale but Callahan
#    has a 10.6 on CONT. 8.6 seems more likely.
# 2. Santaniello's last name is misspelled
# Best to fix errors in spreadsheet and reimport (I will just ignore it for this tutorial)

# basic boxplot
boxplot(USJudgeRatings$RTEN)
?boxplot

# modified boxplot
boxplot(USJudgeRatings$RTEN,
        horizontal = TRUE,
        las = 1, # Make all labels horizontal
        notch = TRUE, # NOtches for CI for median (I don't like it that much)
        ylim = c(0,11), # specify range on y-axis
        col = "slategray3",
        boxwex = 0.5, # width of box as proportion or original
        whisklty = 1, # Whisker line type; 1 = solid line
        staplelty = 0, # staple (line at end) type; 0 = none
        outpch = 16, # symbols for outliers; 16 = filled circle
        outcol = "slategray3", # color for outliers
        main = "Lawyers' Ratings of State Judges in tehe\nUS Superior Court (c. 1977)",
        xlab = "Lawyers' Ratings")
axis(2, at = 1, labels = "Retention") # telling R that I want the label to be Retention 
#    That doesn't make much sense here, but I wanted to use it

boxplot(USJudgeRatings, # notice that I didn't specify any variable within the dataset
        horizontal = TRUE,
        las = 1,  # Make all labels horizontal
        notch = TRUE,  # Notches for CI for median
        ylim = c(0, 11),  # Specify range on Y axis
        col = "slategray3",   # R's named colors (n = 657)
        boxwex = 0.5,  # Width of box as proportion of original
        whisklty = 1,  # Whisker line type; 1 = solid line
        staplelty = 0,  # Staple (line at end) type; 0 = none
        outpch = 16,  # Symbols for outliers; 16 = filled circle
        outcol = "slategray3",  # Color for outliers
        main = "Lawyers' Ratings of State Judges in the\nUS Superior Court (c. 1977)",
        xlab = "Lawyers' Ratings")
box()

     # GROUPED BOXPLOTS
# load data
require(MASS)
?painters
data("painters")
View(painters)

# draw boxplots of outcome (Expression) by group (school)
# basic version
boxplot(painters$Expression ~ painters$School)

# modified version
require(RColorBrewer)
boxplot(painters$Expression ~ painters$School,
        col = brewer.pal(8, "Pastel2"),
        names = c("Renais",
                  "Mannerist",
                  "Seicento",
                  "Venetian",
                  "Lombard",
                  "16th C.",
                  "17th C.",
                  "French"),
        boxwex = 0.5, # width of box
        whisklty = 1, # whisker line type =1 solid line
        staplelty = 0, # staple type: 0 = none
        outpch = 16, # outlier symbol, 16 = filled circle
        outcol = brewer.pal(8, "Pastel2"), # color of outlier cirlce
        main = "Expression Ratings of Painters by School\nFrom \"painters\" Dataset in \"Mass\" Package",
        xlab = "Painter's School",
        ylab = "Expression Ratings")



# SCATTERPLOTS
# load data
?cars
data(cars)
View(cars)

# basic scatterplot
plot(cars)

# modified scatterplot
plot(cars,
     pch = 16, # filled circles
     col = "gray",
     main = "Speed vs. Stopping Distance for Cars in 1920s\nFrom \"cars\" Dataset",
     xlab = "Speed (MPH)",
     ylab = "Distance (feet)")
# add a linear regression line
abline(lm(cars$dist ~ cars$ speed), # lm = linear model, dist is the DV with speed being IV
       col = "red",
       lwd = 2)
# locally weighted scatterplot smoothing (ada LOWESS), this one follows the pattern closer
lines(lowess(cars$speed, cars$dist),
      col = "blue",
      lwd = 2) # you will see that the lm is also pretty close


# Line plots using the plot command
plot(cars, 
     type = "l", # telling R we want a line plot instead of scatterplot
     main = "Speed vs. Stopping Distance for Cars in 1920s\nFrom \"cars\" Dataset",
     xlab = "Speed (MPH)",
     ylab = "Distance (feet)")

# Scatterplot using the "car" package for a similar scatterplot ("Companion to Applied Regression")
# install.packages("pbkrtest")
# install.packages("car")
require(pbkrtest)
require(car)
help(package = "car")
# "scatterplot" has marginal boxplots, smoothers, and quantile regression intervals.
scatterplot(cars$dist ~ cars$speed,
            pch = 16,
            col = "darkblue",
            main = "Speed vs. Stopping Distance for Cars in 1920s\nFrom \"cars\" Dataset",
            xlab = "Speed (MPH)",
            ylab = "Distance (feet)")
# this one has CIs and also shows boxplots for each variable



#     SCATTERPLOTS FOR GROUPED DATA
# load data
?iris
data(iris)
View(iris)

# load "car" package again if necessary
require(car)

# single scatterplot with groups marked
# function can be called 'scatterplot" or "sp"
scatterplot(Sepal.Width ~ Sepal.Length | Species, # "| Species" is the one where we break it down by species
            data = iris,
            xlab = "Sepal Width",
            ylab = "Sepal Length",
            main = "Iris Data",
            labels = row.names(iris),
            legend.coords = "topright")
?scatterplot


#     SCATTERPLOT MATRICES
# load data (if necessary)
?iris
data(iris)
View(iris)

# basic scatterplot matrix
pairs(iris[1:4]) # chart with first 4 variables (ignores species)

# modified scatterplot matrices

# create palette with RColorBrewer
# install.packages("RColorBrewer")
require(RColorBrewer)
display.brewer.pal(3, "Pastel1") # 3 colors from "Pastel1"

# put histograms on the diagonal (from "pairs" help)
# this is too complicated to explain, but this can work in almost any circumstance
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}

pairs(iris[1:4],
      panel = panel.smooth, # optional smoother
      main = "Scatterplot Matrix for Iris Dat using pairs Function",
      diag.panel = panel.hist, # calling the function from above
      pch = 16, # filled circles
      col = brewer.pal(3, "Pastel1")[unclass(iris$Species)]) # color by species

# Similar with "car" package
# Gives kernal density and rugplot for each variable
library(car)
scatterplotMatrix(~Petal.Length + Petal.Width + Sepal.Length + Sepal.Width | Species,
                  data = iris,
                  col = brewer.pal(3, "Dark2"),
                  main = "Scatterplot Matrix for Iris Data using \"car\" Package")


#     CREATING 3-D SCATTERPLOTS
# load data if necessary
data(iris)
View(iris)


#static 3D scatterplot
#install and load the "scatterplot3d" package
install.packages("scatterplot3d")
require(scatterplot3d)

#Basic static 3D scatterplot
scatterplot3d(iris[1:3]) #first 3 variables

#modified static 3D scatterplot
#coloring, vertical lines and Regression Plane
s3d <-scatterplot3d(iris[1:3],
                    pch = 16,
                    highlight.3d = TRUE,
                    type = "h",
                    main = "3D SCatterplot")
plane <- lm(iris$Petal.Length ~ iris$Sepal.Length + iris$Sepal.Width)
s3d$plane3d(plane)


#Create a spinning 3D scatterplot
#Install and load the "rgl" package ("3Dvisualization device system (OpenGL)")
#NOTE: This will cause RStudio to crach when graphics window is closed.  
#Instead, run this in the standard console version of R!!!!!

#export this to standard R
data(iris)
install.packages("rgl")
require("rgl")
require("RColorBrewer")
plot3d(iris$Petal.Length,
       iris$Petal.Width,
       iris$Sepal.Length,
       xlab = "Petal.Length",
       ylab = "Petal.Width",
       zlab = "Sepal.length",
       col = brewer.pal(3, "Dark2")[unclass(iris$Species)],
       size = 8)



# DENSITY PLOT ALL MONOGAN BOOK
pres.energy<-read.csv("http://j.mp/PRESenergy")
densityplot(~Energy, data=pres.energy, xlab = "Television Stories", col = "black")

# DOTPLOTS ALL FROM MONOGAN'S BOOK
library(lattice)
healthFinance <- read_dta("~/Google Drive/Random/Technology Learning/R/R Manual/Data/healthFinance.dta")
#for a dotplot (see page numbers 47-48 for explaination of dotplot) we can use this syntax
dotplot(partratebusness~supplybusiness, data=healthFinance, col="black",ylab="Lobby Participation Rate (Rank Order)", xlab="Number of Health Establishments")



# OVERLAYING PLOTS:

# load dataset
require(datasets)
?swiss
data(swiss)
str(swiss)
View(swiss)
fertility <- swiss$Fertility

# Plot 1: Histogram
h <- hist(fertility,
          prob = TRUE,
          ylim = c(0, 0.04),
          xlim = c(30,100),
          breaks = 11,
          col = "#E5E5E5",
          border = 0,
          main = "Fertility for 47 French-Speaking\nSwiss Provinces, c. 1888")
# Plot 2: normal curve (if prob = TRUE)
curve(dnorm(x, mean = mean(fertility), sd = sd(fertility)),
      col = "red",
      lwd = 3,
      add = TRUE)
# Plot 3 & 4: Kernal density lines (if prob = TRUE)
lines(density(fertility), col = "blue")
lines(density(fertility, adjust = 3), col = "darkgreen")
#Plot 5: Rug (that is, lineplot under histogram)
rug(fertility, col = "red")




# R FOR DATA SCIENTISTS SECTION
library(ggplot2)
mpg

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(title = "Fuel efficiency generally decreases with engine")
# basic plot showing negative relationship
# between engine size and mpg

ggplot(data = mpg) # shows an empty plot, need to add layers

str(mpg) # shows how many rows and columns
?mpg

ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl)) 

ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv)) # x is nominal data

# AESTHETIC MAPPING (visual property of the objects in your plot)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) # similar to our basic
# but now adding visual property of "class" as a color

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) # this one does it but with size
# not advised for discrete variables

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) # transparency of dots

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)) # shapes
# this one took out some obsefvations because ggplot can only do 6 discrete values

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue") 
# you can manually manage your aesthetics like the one above, note it is outside the aes()

# notice that we have actually 234 obs, but only 126 showed up because program corrected for "overplotting" 
    # not very helpful when want to see where mass of data lies (shows trends though).  Can overcome by "jitter"
    # jittering ads small amount of random noise to each point which spreads them out
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue", position = "jitter")


# FACETS (subplots that display one subset of the data)
# this is particularly good with categorical data

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2) # the variable in facet_wrap should be discrete 
# this shows the same information as above, but on different plots by car class

# if you want to use facets with multiple variables, use facet_grid. 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(drv ~ cyl) # variables should be separated by "~"
# drv = front wheel drive, rear wheel drive, and 4wd

# exercises with facets: notice the difference in the two graphs
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ drv)

#GEOMETIC (GEOM) SHAPES
# the Geom of point (a scatterplot)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# same chart but a line plot
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# another line but this time with multiple lines (by drive train)
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# to add multiple geom functions
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
# the same plot but with more efficient code
ggplot(data = mpg, mapping = aes(x =displ, y = hwy)) +
  geom_point() +
  geom_smooth()
# similar to last 2, but now has color in scatterplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) +
  geom_smooth(se = FALSE) # No standard error bands

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_line()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE) + 
  geom_point() + 
  geom_smooth(se = FALSE)


# statitstical transformations:

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) # the graph creates a variable called "count"
?geom_bar # look at computed variables

# to change it from the default stat of "count" to proportion
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))


# Colors with ggplot
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, colour = cut))
# Using "fill" instead of "colour" (said to be more useful)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))
# notice when we change the fill to actually be determined by clarity (a var) instead of cut (another var)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity)) # note that the coloring is stacked within each cut
# the position of the "fill" can be done with either "identity" (not very helpful here), "dodge" or "fill" 

# with fill
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") # stacks bar to same height, easier for comparison
    # across groups

# with dodge
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") # I like this one, makes it easier to compare
    # individual values


# coordinate systems in gpplot
library(ggplot2)
data(mpg)

# the coord_flip will flip the axes
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()
# there is also coord_quickmap which sets up aspect ratio for maps (spatial data)
# and there is coord_polar() which uses polar coordinates 



# EXPLORATORY DATA ANALYSIS
library(tidyverse)

# bar plot (categorical data)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) # visualizes the count of diamonds by cut

# can also be calculated with dplyr:: count()
diamonds %>%
  count(cut)

# histogram (continuous data)
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# can also be calculated with dplyr:: count()
diamonds %>%
  count(cut_width(carat, 0.5))

# this one is a line graph which shows carat and cut
ggplot(data = diamonds, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwith = 0.1)



# TYPICAL VALUES

# when you are doing exploratory data analysis, ask yourself the following:
## Which values are the most common? Why?
## Which values are rare? Why? Does that match your expectations?
## Can you see any unusual patterns? What might explain them?

# notice how there are two common areas
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)


# UNCOMMON VALUES

# these can either be data entry errors or truly outliers
# sometimes difficult to see when we have so many observations (like in the diamond hist)
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# so it is better to limit our y axis (we can now see the outliers)
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# BOXPLOTS
ggplot(diamonds) +
  geom_boxplot(mapping = aes(x= cut, y = price))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = class, y = hwy))
# in the above graph there is no natural order so it's nice to reorder (see below)
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))
# lastly, if you have long variable names, you can flip the chart to allow for that
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip() +
  xlab("Vehicle Class") +
  ylab("MPG for Highway")
?geom_boxplot

# jitter plots: They allow you to see the distribution along with the boxplot.  Very helpful
ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  geom_jitter()

# practice problems:
library(tidyverse)

diamonds
summary(diamonds$x)
summary(diamonds$y)
summary(diamonds$z)

head(diamonds)

hist(diamonds$x)
hist(diamonds$y) # can't see the outliers
hist(diamonds$z) # can't see outliers

hist(diamonds$y[diamonds$y >10])
hist(diamonds$z[diamonds$z >6])

ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(carat))
hist(diamonds$carat[diamonds$carat == .99 | diamonds$carat == 1])



# COVARIATION BETWEEN TWO CATEGORICAL VARIABLES
# The bigger the circle the bigger the covariation
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

# You can also just do a simple count and then visualize it with geom_tile() and fill aes
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))


# COVARIATION BETWEEN TWO CONTINUOUS VARIABLES

# the obvious one is a scatterplot
# install.packages("tidyverse")
library(tidyverse)

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# scatterplots get a little useless when you have a huge dataset
# try to make it more transparent with the alpha asethetic

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/100)

# another solution is to use bins, see comentary below from book:
# But using transparency can be challenging for very large datasets. 
# Another solution is to use bin. Previously you used geom_histogram() 
# and geom_freqpoly() to bin in one dimension. Now you'll learn how
# to use geom_bin2d() and geom_hex() to bin in two dimensions.
# geom_bin2d() and geom_hex() divide the coordinate plane into 2d 
# bins and then use a fill color to display how many points fall
# into each bin. geom_bin2d() creates rectangular bins. geom_hex()
#creates hexagonal bins. You will need to install the hexbin package 
# to use geom_hex().
ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = diamonds) +
  geom_hex(mapping= aes(x = carat, y = price))



# GGPLOT 2 CALLS (MORE ADVANCED)
# You can rewrite:
ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(carat))
# as:
ggplot(diamonds, aes(carat)) +
  geom_freqpoly()

# Sometimes we can end a pipeline data transformation into a plot.  Notice that %>% transitions to +
diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill = n)) +
  geom_tile()



# COMMUNICATING YOUR GRAPHICS NEATLY
# look at the labs() function on how to relabel axes, title, subtitle, and caption
library(tidyverse)
data(cars)

# adding labels
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class)) +
  labs(title = "Fuel efficiency generally decreases with engine",
       subtitle = "Two seaters (sports cars) are an exception because of their light weight",
       caption = "Data from fueleconomy.gov",
       x = "Engine displacement (L)",
       y = "Fuel Efficiency",
       colour = "Car Type")

# let's say you want to annotate certain points.  You can use that with geom_label()
# you generally do this by creating a tibble of the labels you want (here it is 
# based off of a calculated field of best in class cars)

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

# the geom_label() provides the information we need.  The nudge_y lets us off center
# it a little from the dot to ensure that we can still see the point
ggplot(data = mpg, aes(displ, hwy)) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class)) +
  labs(title = "Fuel efficiency generally decreases with engine",
       x = "Engine displacement (L)",
       y = "Fuel Efficiency",
       colour = "Car Type") +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)
# the problem with this chart is that at the top left, we have labels that overlap.
# we need to use the ggrepel add on package that will help with this:
library(ggrepel)
ggplot(data = mpg, aes(displ, hwy)) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class)) +
  labs(title = "Fuel efficiency generally decreases with engine",
       x = "Engine displacement (L)",
       y = "Fuel Efficiency",
       colour = "Car Type") +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.7)

## adding other features to annotate plot:
library(tidyverse)
data(mpg)
summary(mpg$hwy) # looking for the mean hwy to create a line

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(title = "Fuel efficiency generally decreases with engine") +
  geom_hline(aes(yintercept = 23.44), size = 1, colour = "red") 
# can also do heom_vline to create a vertical line




# making multiple graphs with facet_wrap!! Very helpful
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~cyl)
### you can make multiple graphs and save them as objects: p1 <- ggplot(......)
### then you can use grid.arrange(.....) to put them together! http://lightonphiri.org/blog/ggplot2-multiple-plots-in-one-graph-using-gridextra



## GGPAIRS: really good for quick analysis of relationships:
library(GGally)
set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

rm(list = ls()) # clean up

# Ordination plots 
# For ESCI 503
# R. Bunn
# Mar 8 2022

#################
# code to add ellipses to ordination plots to better recognize groups
#################

# Packages
library(vegan)
library(ape)
library(mvabund)
library(tidyverse)

# Use spider data from mvabund
data(spider)
spidy <- spider$abund
spidy.env <- spider$x

# Cluster to identify groups
my.hclust <- hclust(vegdist(spidy), "ward.D2")
my.cut <- cutree(my.hclust, k = 4)

# Run an ordination
## pcoa
my.pcoa <- pcoa(vegdist(spidy))

## could run a nmds
#set.seed(2)
#my.nmds <- metaMDS(spidy, k = 2)


# Prepare data for plotting
## df of sites/sample units
plot.sites <- data.frame(
  Axis1 = my.pcoa$vector[,1],
  Axis2 = my.pcoa$vector[,2],
  Cluster = as.factor(my.cut)
)


## Ellipses plot
p.ellipse <- ggplot(plot.sites, 
                   aes(x = Axis1, y = Axis2, 
                       color = Cluster)) +
  # add axes at 0,0
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") + 
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") + 
  # add sample units
  geom_point(size = 3,
             alpha = 0.4) + 
  # draw ellipses
  # NOTES:
  # my best understanding is that these are 'data ellipses' not confidence intervals
  # in other words
  # the ellipse comes from the confidence level of a probability contours 
  # superimposed over the scatterplot of the data
  # since this is just a visualization, we can use them if they look good ;)
  stat_ellipse(type = "norm", # can also choose "t"
               level = .95) + # can set different levels to capture different contours
  # general plot aesthetics
  coord_fixed(xlim = c(-1,.6),
              ylim = c(-.6,.6)) + 
  labs(x = "Axis 1", y = "Axis 2") +
  theme_classic()

p.ellipse  


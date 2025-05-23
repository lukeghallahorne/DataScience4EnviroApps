# Ordination plots 
# For ESCI 503
# R. Bunn
# Mar 8 2022

#################
# code to add centroids to ordination plots to better recognize groups
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

## df of centers of each cluster
plot.center <- plot.sites %>% 
  group_by(Cluster) %>% 
  summarise(
    Axis1.center = mean(Axis1), 
    Axis2.center = mean(Axis2)
    )


## Centroids plot
p.cent <- ggplot(plot.sites, 
                   aes(x = Axis1, y = Axis2, 
                       color = Cluster)) +
  # add axes at 0,0
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") + 
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") + 
  # add sample units
  geom_point(size = 2, alpha = 0.4) + 
  # add centroids
  geom_point(data = plot.center, 
             aes(x = Axis1.center, y = Axis2.center, 
                 colour = Cluster),
             shape = 8,
             size = 8,
             show.legend = TRUE) + 
  # general plot aesthetics
  coord_fixed(xlim = c(-.6,.3),
              ylim = c(-.42,.4)) + 
  labs(x = "Axis 1", y = "Axis 2") +
  theme_classic()

p.cent  




#Install packages
#devtools::install_github("YuLab-SMU/ggtree",force=TRUE)
#It took me a while to install it (I had to update a lot of stuff)

library(ggtree) #v3.5.3.002
library(ggplot2)

#Load phylogenetic data from europe
#trial
bee.tree100 = readRDS("Data/Europe_data/bee.tree100.rds")

# generate some random values for each tip label in the data
d1 <- data.frame(id=bee.tree100$tip.label, val=rnorm(length(bee.tree100$tip.label), sd=3))

# Make a second plot with the original, naming the new plot "dot", 
# using the data you just created, with a point geom.
p2 <- facet_plot(p, panel="dot", data=d1, geom=geom_point, aes(x=val), color='red3')

# Make some more data with another random value.
d2 <- data.frame(id=bee.tree100$tip.label, value = abs(rnorm(length(bee.tree100$tip.label), mean=100, sd=50)))

# Now add to that second plot, this time using the new d2 data above, 
# This time showing a bar segment, size 3, colored blue.
p3 <- facet_plot(p2, panel='bar', data=d2, geom=geom_segment, 
                 aes(x=0, xend=value, y=y, yend=y), size=3, color='blue4') 

# Show all three plots with a scale
p3 + theme_tree2()

#Edits...
p <- ggtree(bee.tree100) %<+% d2 + xlim(-0, 0.8)
p=p + geom_tiplab(offset = 0.2,size=2, justify = "left") +
geom_tippoint(aes(size = value,colour=value))+
scale_size_continuous(range = c(0, 2.5))+
scale_colour_viridis_c()

geospiza_raw <- data.frame(id=bee.tree100$tip.label, value = abs(rnorm(length(bee.tree100$tip.label), mean=100, sd=50)))
geospiza_raw$value2 <- geospiza_raw$value*0.5
geospiza_raw=geospiza_raw %>% tibble::column_to_rownames(var="id")

gheatmap(p, data=geospiza_raw, colnames_angle=45, width=0.3) + hexpand(.02) + vexpand(.0, -1) 



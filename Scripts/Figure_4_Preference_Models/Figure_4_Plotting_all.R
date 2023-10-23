


#All----

#Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

#Load data for plotting
long_data <- read_csv("Data/Europe_USA/data_preference_residuals_europe_usa_qualitative.csv")

model1 = read_csv("Data/Europe_USA/model_output_preference_residuals_europe_usa_qualitative.csv")

model2 = read_csv("Data/Europe_USA/model_output_preference_brain_weight_europe_usa_qualitative.csv")

model3 = read_csv("Data/Europe_USA/model_output_preference_it_europe_usa_qualitative.csv")


#Relevel for plotting
long_data = long_data %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model1 = model1 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model2 = model2 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model3 = model3 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 

#colorBlindness::displayAllColors(viridis::viridis(4))

all_1 = ggplot(model1, aes(x = residuals, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__,  fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab("Habitat occupancy") +
xlab("Relative brain size") +
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High")) 
#+ facet_wrap(~ Habitat)


all_2 = ggplot(model2, aes(x = Brain.weight, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__,  fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab(NULL) +
xlab("Brain weight (mg)") +
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High"))



all_3 = ggplot(model3, aes(x = IT, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab(NULL) +
xlab("Intertegular distance (mm)") +
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High"))


library(patchwork)

all_1 + (all_2/all_3) + plot_layout(guides = "collect") &
theme(legend.position = 'bottom', panel.border=element_rect(size=1),
      axis.title=element_text(face="bold"),
      legend.box.background = element_rect(colour = "black",size=1))


#Europe----

#Load data for plotting
long_data <- read_csv("Data/Europe_data/data_preference_residuals_europe_qualitative.csv")

model1 = read_csv("Data/Europe_data/model_output_preference_residuals_europe_qualitative.csv")

model2 = read_csv("Data/Europe_data/model_output_preference_brain_weight_europe_qualitative.csv")

model3 = read_csv("Data/Europe_data/model_output_preference_it_europe_qualitative.csv")


#Relevel for plotting
long_data = long_data %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model1 = model1 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model2 = model2 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model3 = model3 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 

#colorBlindness::displayAllColors(viridis::viridis(4))


#Check eu occurrences 
europe <- read.table("Data/Europe_data/all_above_50_europe.csv.gz",  header=T, quote="\"", sep=",")
nrow(europe)

eu_1 = ggplot(model1, aes(x = residuals, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab("Habitat occupancy") +
xlab("Relative brain size") +
ggtitle("D") +  
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High")) +
theme(legend.position="none", panel.border=element_rect(size=1),
axis.title=element_text(face="bold"),
legend.box.background = element_rect(colour = "black",size=1),
plot.title = element_text(face="bold"))

eu_2 = ggplot(model2, aes(x = Brain.weight, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab(NULL) +
xlab("Brain weight (mg)") +
ggtitle("E") +  
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High"))+
theme(legend.position="none", panel.border=element_rect(size=1),
axis.title=element_text(face="bold"),
legend.box.background = element_rect(colour = "black",size=1),
plot.title = element_text(face="bold"))


eu_3 = ggplot(model3, aes(x = IT, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab(NULL) +
xlab("Intertegular distance (mm)") +
ggtitle("F") +  
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High"))+
theme(legend.position="none", panel.border=element_rect(size=1),
axis.title=element_text(face="bold"),
legend.box.background = element_rect(colour = "black",size=1),
plot.title = element_text(face="bold"))

library(patchwork)

eu_1 + (eu_2/eu_3) + plot_layout(guides = "collect") &
theme(legend.position = 'bottom', panel.border=element_rect(size=1),
      axis.title=element_text(face="bold"),
      legend.box.background = element_rect(colour = "black",size=1))


#USA----

#Load data for plotting
long_data <- read_csv("Data/USA_data/data_preference_residuals_usa_qualitative.csv")

model1 = read_csv("Data/USA_data/model_output_preference_residuals_usa_qualitative.csv")

model2 = read_csv("Data/USA_data/model_output_preference_brain_weight_usa_qualitative.csv")

model3 = read_csv("Data/USA_data/model_output_preference_it_usa_qualitative.csv")


#Relevel for plotting
long_data = long_data %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model1 = model1 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model2 = model2 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 
#Relevel for plotting
model3 = model3 %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 

#colorBlindness::displayAllColors(viridis::viridis(4))

usa_1 = ggplot(model1, aes(x = residuals, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab("Habitat occupancy") +
xlab("Relative brain size") +
ggtitle("A") +  
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High"))+
theme(legend.position="none", panel.border=element_rect(size=1),
axis.title=element_text(face="bold"),
legend.box.background = element_rect(colour = "black",size=1),
plot.title = element_text(face="bold"))


usa_2 = ggplot(model2, aes(x = Brain.weight, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab(NULL) +
xlab("Brain weight (mg)") +
ggtitle("B") +  
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High"))+
theme(legend.position="none", panel.border=element_rect(size=1),
axis.title=element_text(face="bold"),
legend.box.background = element_rect(colour = "black",size=1),
plot.title = element_text(face="bold"))


usa_3 = ggplot(model3, aes(x = IT, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Habitat),color=NA, linetype = "dashed",alpha = 0.2)+
theme_bw() +
ylab(NULL) +
xlab("Intertegular distance (mm)") +
ggtitle("C") +  
scale_colour_viridis_d() +
scale_fill_viridis_d() +
scale_y_continuous(breaks = seq(0, 1, by = 1),labels = c("Low", "High"))+
theme(legend.position="none", panel.border=element_rect(size=1),
axis.title=element_text(face="bold"),
legend.box.background = element_rect(colour = "black",size=1),
plot.title = element_text(face="bold"))


usa_1 + (usa_2/usa_3) + plot_layout(guides = "collect") &
theme(legend.position = 'bottom', panel.border=element_rect(size=1),
      axis.title=element_text(face="bold"),
      legend.box.background = element_rect(colour = "black",size=1))

#Europe-USA panel----

library(cowplot)

eu <- read.table("Data/Europe_data/all_above_50_europe.csv.gz",  header=T, quote="\"", sep=",")
usa <- read.table("Data/Usa_data/all_above_50_usa.csv.gz",  header=T, quote="\"", sep=",")

#Europe
#Plot row 
plot_row1 <- plot_grid(eu_1, eu_2, eu_3, ncol = 3) 

# now add the title
title1 <- ggdraw() + 
  draw_label(
    paste("Europe occurrences (N =", format(nrow(eu), big.mark = ",")
 ,")"),
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

panel1 = plot_grid(
  title1, plot_row1,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 0.3)
)


#USA
#Plot row 
plot_row2 <- plot_grid(usa_1, usa_2, usa_3, ncol = 3)
#Get legend
legend2 <- get_legend(
  usa_3 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
    legend.box.background = element_rect(colour = "black",size=1))
)


# now add the title
title2 <- ggdraw() + 
  draw_label(
paste("United States occurrences (N =", format(nrow(usa), big.mark = ",") ,")"),    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

panel2 = plot_grid(
  title2, plot_row2,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 0.3)
)

plot_all = plot_grid(panel2, panel1, legend2, ncol=1, rel_heights = c(1, 1, 0.4))

saveRDS(plot_all, "Data/Processing/plot_all.rds")

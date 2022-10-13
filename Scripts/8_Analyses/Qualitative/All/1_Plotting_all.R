
#Load data for plotting
long_data <- read_csv("Data/Europe_USA/data_preference_residuals_europe_usa_qualitative.csv")

model1 = read_csv("Data/Europe_USA/model_output_preference_residuals_europe_usa_qualitative.csv")

model2 = read_csv("Data/Europe_USA/model_output_preference_brain_weight_europe_usa_qualitative.csv")

model3 = read_csv("Data/Europe_USA/model_output_preference_it_europe_usa_qualitative.csv")


colorBlindness::displayAllColors(viridis::viridis(4))

p1 = ggplot(model1, aes(x = residuals, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
#geom_ribbon(aes(ymin = lower__, ymax = upper__, color=Habitat), linetype = "dashed",alpha = 0)+
theme_bw() +
ylab("Habitat preference") +
xlab("Residuals") +
scale_color_manual(name="Habitat",
                   values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
                   labels = c("Agricultural", "Natural", "Semi-developed", "Urban"))



p2 = ggplot(model2, aes(x = Brain.weight, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
#geom_ribbon(aes(ymin = lower__, ymax = upper__, color=Habitat), linetype = "dashed",alpha = 0)+
theme_bw() +
ylab(NULL) +
xlab("Brain weight (mg)") +
scale_color_manual(name="Habitat",
                   values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
                   labels = c("Agricultural", "Natural", "Semi-developed", "Urban"))



p3 = ggplot(model3, aes(x = IT, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21, size=2.5) +
geom_line(aes(color=Habitat),size=0.7) +
#geom_ribbon(aes(ymin = lower__, ymax = upper__, color=Habitat), linetype = "dashed",alpha = 0)+
theme_bw() +
ylab(NULL) +
xlab("Intertegular distance (mm)") +
scale_color_manual(name="Habitat",
               values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
               labels = c("Agricultural", "Natural", "Semi-developed", "Urban"))


library(patchwork)

p1 + (p2/p3) + plot_layout(guides = "collect") &
theme(legend.position = 'bottom', panel.border=element_rect(size=1),
      axis.title=element_text(face="bold"),
      legend.box.background = element_rect(colour = "black",size=1))


#####################################
#### Regression Discontinuity Graphic 
#####################################

# Creating a graphic to plot a regression discontinuity


ESSSE$treatment <- factor(ESSSE$Treatment, labels = c("Control", "Treatment"))

trust_plot <- ggplot(ESSSE |> filter(Runner > -30), aes(x = Date, y = Trust, group = treatment)) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  geom_vline(xintercept=as.Date(attack_date), linetype='dashed', color='black', size=0.5) +
  theme_minimal() +
  labs(y = "Trust in the Police") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=6)) 

print(trust_plot)

obligation_plot <- ggplot(ESSSE |> filter(Runner > -30), aes(x = Date, y = Obligation, group = treatment)) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  geom_vline(xintercept=as.Date(attack_date), linetype='dashed', color='black', size=0.5) +
  theme_minimal() +
  labs(y = "Obloigation to Obey") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=6)) 

print(obligation_plot)

moralalignment_plot <- ggplot(ESSSE |> filter(Runner > -30), aes(x = Date, y = MoralAlignment, group = treatment)) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  geom_vline(xintercept=as.Date(attack_date), linetype='dashed', color='black', size=0.5) +
  theme_minimal() +
  labs(y = "Moral Alignment") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=6)) 

print(moralalignment_plot)

lawfullness_plot <- ggplot(ESSSE |> filter(Runner > -30), aes(x = Date, y = Lawfullness, group = treatment)) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  geom_vline(xintercept=as.Date(attack_date), linetype='dashed', color='black', size=0.5) +
  theme_minimal() +
  labs(y = "Moral Alignment") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=6)) 

print(lawfullness_plot)

proceduralfairness_plot <- ggplot(ESSSE |> filter(Runner > -30), aes(x = Date, y = ProceduralFairness, group = treatment)) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  geom_vline(xintercept=as.Date(attack_date), linetype='dashed', color='black', size=0.5) +  theme_minimal() +
  labs(y = "Procedural Fairness") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=6)) 

print(proceduralfairness_plot)

effectivness_plot <- ggplot(ESSSE |> filter(Runner > -30), aes(x = Date, y = Effectivness, group = treatment)) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  geom_vline(xintercept=as.Date(attack_date), linetype='dashed', color='black', size=0.5) +  theme_minimal() +
  labs(y = "Police Effectivness") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=6)) 


print(effectivness_plot)


rdd_graphic <- plot_grid(
  trust_plot,
  obligation_plot,
  moralalignment_plot,
  lawfullness_plot,
  proceduralfairness_plot,
  effectivness_plot,
  nrow = 6,
  ncol =1,
  align="v")

print(rdd_graphic)



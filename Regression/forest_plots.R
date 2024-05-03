################################################################################
### Forest Plot 
################################################################################


## Forest plot of Naive Model


# Estimate models

Estimates_Naive <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfullness", "Lawfullness", "ProceduralFairness", "Effectivness"),
                               bandwidth = c(5, 10, 15, 20, 25, 30, 35)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate("Treatment", response = .x), 
                     data = ESSSE |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "Treatment")


# Plot Models using ggplot


ggplot(Estimates_Naive, aes(x = variable, y = estimate,
                            group = bandwidth, label = as.character(bandwidth))) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high, color = as.factor(bandwidth)),
    position = position_dodge(width = .75),
    size = 0.5,
    shape = 15
  ) + 
  labs(y = "Bandwith Effect Size",
       title =  "'Naive' Estimation Models for Sweden",
       subtitle = "Effect of the Stocholm Bombing 2010") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_x_discrete(
    labels = c("Trust", "Obligation", "MoralAlignment", "Lawfullness", "ProceduralFairness", "Effectivness"),
    name = "Variable"
  ) +
  scale_color_discrete(name = "Bandwidth (Days)")  # Add color-coded legend

ggsave("./Analysis/Figures/Forest_ESSSE_Naive.png")





## Forest plot with covariate adjustment

# Estimate Models

Estimates_Covariate_Adjustment <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfullness", "ProceduralFairness", "Effectivness"),
                                              bandwidth = c(5, 10, 15, 20, 25, 30, 35)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate(c("Treatment", "agea", "gndr", "eduyrs", "log(hinctnta)", "blgetmg"), response = .x),
                     data = ESSSE |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "Treatment")

# Plot Graphics using ggplot 

ggplot(Estimates_Covariate_Adjustment, aes(x = variable, y = estimate,
                                           group = bandwidth, label = as.character(bandwidth))) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high, color = as.factor(bandwidth)),
    position = position_dodge(width = 0.75),
    size = 0.5,
    shape = 15
  ) + 
  labs(y = "Bandwith Effect Size",
       title =  "Covariate Adjusted Models for Sweden",
       subtitle = "Effect of the Stocholm Bombing 2010") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_x_discrete(
    labels = c("Trust", "Obligation", "MoralAlignment", "Lawfullness", "ProceduralFairness", "Effectivness"),
    name = "Variable"
  ) +
  scale_color_discrete(name = "Bandwidth (Days)")  # Add color-coded legend

ggsave("./Analysis/Figures/Forest_ESSSE_Covariate_Adjusted.png")


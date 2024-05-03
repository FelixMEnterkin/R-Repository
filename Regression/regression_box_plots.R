# Regression Plots using full Bandwith

# Creating a box for each variable that plots the effect size and confidence interval

a <- plot_summs(OLS_Trust_base,
                OLS_Trust_full_controls,
                coefs = c("Treatment" = "Treatment"),   colors = "Rainbow", point.shape = FALSE, inner_ci_level = .9) +
scale_color_grey() + theme_bw() + theme(legend.position = "none", axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank()
)

b <-plot_summs(OLS_Obligation_base,
               OLS_Obligation_full_controls,
               coefs = c("Treatment" = "Treatment"),   colors = "Rainbow", point.shape = FALSE, inner_ci_level = .9) +
scale_color_grey() + theme_bw() + theme(legend.position = "none", axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank()
)

c <-plot_summs(OLS_MoralAlignment_base,
               OLS_MoralAlignment_full_controls,
               coefs = c("Treatment" = "Treatment"),   colors = "Rainbow", point.shape = FALSE, inner_ci_level = .9) +
scale_color_grey() + theme_bw() + theme(legend.position = "none", axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank()
)

d <-plot_summs(OLS_Lawfullness_base,
               OLS_Lawfullness_full_controls,
               coefs = c("treatment" = "Treatment"),   colors = "Rainbow", point.shape = FALSE, inner_ci_level = .9) +
scale_color_grey() + theme_bw() + theme(legend.position = "none", axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank()
)

e <-plot_summs(OLS_ProceduralFairness_base,
               OLS_ProceduralFairness_full_controls,
               coefs = c("treatment" = "Treatment"),   colors = "Rainbow", point.shape = FALSE, inner_ci_level = .9) +
scale_color_grey() + theme_bw() + theme(legend.position = "none", axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank() 
)
f <-plot_summs(OLS_Effectivness_base,
               OLS_Effectivness_full_controls,
               coefs = c("treatment" = "Treatment"),   colors = "Rainbow", point.shape = FALSE, inner_ci_level = .9) +
scale_color_grey() + theme_bw() + theme(legend.position = "none", axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank() 
)

a <- a + labs(title = "Trust in the Police") 
b <- b + labs(title = "Obligation to Obey")
c <- c + labs(title = "Moral Alignment")
d <- d + labs(title = "Lawfullness")
e <- e + labs(title = "Procedural Fairness")
f <- f + labs(title = "Police Effectivness")

regression_dot_plot <- plot_grid(a,
                          b,
                          c,
                          d,
                          e,
                          f,
                          nrow = 2,
                          ncol =3,
                          align="h")
regression_dot_plot


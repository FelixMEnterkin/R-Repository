### histogram for interview frequency 

# this shows any potential non-responce pattern  to the survey itself as oppose
#   to to particul survey items 

sweden_attack = data.frame(date=as.Date(attack_date), event="Stockholm Bombing")


histo <- ggplot(ESSSE, aes(x=Date)) +
  theme(text = element_text(
    family = "Times New Roman",
    face = "bold",
    size = 10
  )) +
  theme_classic() +
  geom_histogram(position="identity", bins = 41) +
  geom_vline(data=sweden_attack, mapping=aes(xintercept=date), color="red") +
  theme(axis.title.y = element_text(colour = "white"),
        axis.title.x=element_blank(),
        axis.ticks.y = element_line(colour = "white"),
        text = element_text(size = 6)) +
  labs(Y = "Frequency of interviews per week",
       title='Histogram Representing the average number of interviews per day',)

print(histo)

ggsave("./data/figures/Histogram_ESSSE.png")

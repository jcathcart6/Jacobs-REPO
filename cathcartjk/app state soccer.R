shots <- App_state_shots
goals <- filter(shots, Result == "G")



ggplot(shots, aes(x = factor(Player))) + 
  geom_bar() + 
  labs(x = "Player Number", y = "Total Shots") +
  ggtitle("Shots Per Player")


ggplot(shots, aes(x = Type)) +
  geom_bar() +
  facet_wrap(~Player)


ggplot(shots, aes(x = Type)) +
  geom_bar()


ggplot(shots, aes (x = Result)) +
  geom_bar()


ggplot(shots, aes (x = Result)) +
  geom_bar() +
  facet_grid(~ Opposition)

ggplot(goals, aes(x = Type)) +
  geom_bar() +
  facet_wrap(~How)


ggplot(shots, aes(x = Result)) +
  geom_bar() +
  facet_wrap(~Player)

ggplot(shots, aes(x = ))


# How many playing card suits can Pete correctly identify?
# Is there evidence that he is not just randomly guessing?
# This is a series of 100 Bernoulli trials with p = 1/4.

# First, what is P(X >= 28) when X ~ BIN(100, 1/4)?

pmfX     <- data.frame(x = 0:100, p = dbinom(0:100, 100, 1/4))
shading1 <- ifelse(pmfX$x >= 28, "red", "blue")

library(ggplot2)
ggplot(pmfX, aes(x = x, y = p)) +
  geom_bar(stat = "identity", width = 1, fill = shading1, col = "gray") +
  labs(title = "Probability Distribution of X = Cards Guessed Correctly", 
       subtitle = "area corresponding to 28 or more correct guesses is shaded in red", 
       y = "p(x)")

prob28 <- pbinom(27, 100, 1/4, lower.tail = FALSE)
sprintf("The probability of getting 28 or more correct when guessing randomly is is %1.3f.", prob28)

# Second, what is P(X >= 44) when X ~ BIN(100, 1/4)?

pmfX    <- data.frame(x = 0:100, p = dbinom(0:100, 100, 1/4))
shading2 <- ifelse(pmfX$x >= 44, "red", "blue")

library(ggplot2)
ggplot(pmfX, aes(x = x, y = p)) +
  geom_bar(stat = "identity", width = 1, fill = shading2, col = "gray") +
  labs(title = "Probability Distribution of X = Cards Guessed Correctly", 
       subtitle = "the vertical red line shows x = 44", 
       y = "p(x)") +
  geom_vline(xintercept = 44, color = "red")

prob44 <- pbinom(43, 100, 1/4, lower.tail = FALSE)
sprintf("The probability of getting 44 or more correct when guessing randomly is is %1.6f.", prob44)
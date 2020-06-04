## gganimate test
install.packages('gganimate')
install.packages('gifski')
install.packages('png')
library(png)
library(gifski)
library(gganimate)
library(ggplot2)
v1 <- rnorm(100, mean = 10, sd = 4)
v2 <- rnorm(100, mean = 10, sd = 4)
v3 <- rnorm(100, mean = 10, sd = 6)

data.ani <- as.data.frame(cbind(v1,v2,v3))
data.ani$stage <- "stage_1"

v1 <- rnorm(100, mean = 10, sd = 4)
v2 <- rnorm(100, mean = 10, sd = 4)
v3 <- rnorm(100, mean = 10, sd = 6)

data.ani2 <- as.data.frame(cbind(v1,v2,v3))
data.ani2$stage <- "stage_2"

v1 <- rnorm(100, mean = 10, sd = 4)
v2 <- rnorm(100, mean = 10, sd = 4)
v3 <- rnorm(100, mean = 10, sd = 6)

data.ani3 <- as.data.frame(cbind(v1,v2,v3))
data.ani3$stage <- "stage_3"

data.ani <- rbind(data.ani, data.ani2, data.ani3)
data.ani$stage2 <- data.ani$stage

test.ani <- ggplot(data.ani, aes(x = v1, y = v2, size = v3)) +
  geom_point(alpha = 0.6, aes(colour = stage2, group = 1L)) +
  scale_colour_manual(values = c("#8bc4fc", "#055eb5", "#ffadcf")) +
  theme_void() +
  theme(legend.position = "none") +
  transition_states(stage, transition_length = 2, state_length = 0) + ease_aes('linear') 
#303030

anim_save("test_ani.gif", test.ani, width = 1000, height = 300)
anim_save("test_ani_lab.gif", test.ani, width = 1000, height = 100, bg = "transparent")


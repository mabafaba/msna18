test %>% gather

test %>% head
test<-data.frame(data$livelihoods.pin,
      data$nfi.pin,
data$food.pin,
data$access.pin.index,
data$health.pin)

adj<-as.matrix(test) %*% as.matrix(t(test))


ggplot(test, aes(x = from, y = to, fill = group)) +
  geom_raster() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none")
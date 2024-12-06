# ggradar <img align="right" src="https://raw.githubusercontent.com/alexander-pastukhov/ggradar/refs/heads/main/ggradar.svg" alt="Logo" height="138" style="float:right; height:138px;">
Collections of geoms compatible with ggplot2 for polar coordinates. Includes point, line, ribbon, and area geoms.


## Installation

```r
library("devtools")
install_github("alexander-pastukhov/ggradat", dependencies=TRUE)
```

## Usage


```r
example_df <- data.frame(x = factor(1:8),
                         y = c(0.8, 0.4, 1.0, 0.6, 0.4, 0.3, 0.9, 0.2)) |>
  mutate(ymin = y - runif(n(), 0, 0.2),
         ymax = y + runif(n(), 0, 0.2))
```

```{r}
ggplot2::ggplot(example_df, ggplot2::aes(x = x, y = y)) +
  geom_radar_ribbon(aes(ymin = ymin, ymax = ymax), fill = "#b3dee5ff", alpha = 0.5) +
  geom_radar_line(color = "#faac22ff") +
  geom_radar_point(color = "#faac22ff", size = 5) +
  ggplot2::coord_polar() +
  ggplot2::ylim(0, 1.2) +
  scale_x_discrete(name = NULL, labels = NULL) +
  scale_y_continuous(name = NULL, labels = NULL)
```

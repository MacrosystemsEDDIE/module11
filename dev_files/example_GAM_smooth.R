library("mgcv")
library("gratia")
library("dplyr")

konz_std <- read_csv("./app/data/konz_std.csv")

konz_doy <- konz_std %>%
  mutate(doy = yday(datetime)) %>%
  filter(variable == "nee")
ggplot(data = konz_doy, aes(x = doy, y = observation)) +
  geom_point() +
  theme_classic()

df <- data.frame(y = konz_doy$observation, x = konz_doy$doy)
gam <- mgcv::gam(formula = y ~ s(x, bs = "cs", k=5), family = gaussian(),
          data = df, method = "REML")
bs <- basis(gam)

# let's weight the basis functions (simulating model coefs)
set.seed(1)
betas <- data.frame(bf = factor(1:4), beta = as.numeric(unname(coef(gam)[-1])))
coef(gam)

# we need to merge the weights for each basis function with the basis object
bs <- bs |>
  left_join(betas, by = join_by(".bf" == "bf")) |>
  mutate(value_w = .value * beta * -1 + as.numeric(unname(coef(gam)[1]))) 

# now we want to sum the weighted basis functions for each value of `x`
spl <- bs |>
  group_by(x) |>
  summarise(spline = sum(value_w))

pred <- mgcv::predict.gam(gam, df)

pred_df <- data.frame(doy = df$x,
                      pred = pred)

# now plot
  ggplot() +
    geom_point(data = df, aes(x = x, y = y), color = "gray", alpha = 0.5)+
    geom_line(data = bs, aes(x = x, y = value_w, colour = .bf, group = .bf), show.legend = FALSE) +
    #geom_line(aes(x = x, y = spline), data = spl, linewidth = 1.5,
              #inherit.aes = FALSE) +
    geom_line(data = pred_df, aes(x = doy, y = pred), linewidth = 1.5)+
    labs(y = "net ecosystem exchange (gC/m2/day)", x = "doy")+
    theme_classic()


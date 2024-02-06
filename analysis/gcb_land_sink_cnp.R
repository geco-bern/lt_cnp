# load("/Volumes/Backup Plus/The Alchmist/ZCo-CN-Beni/TRENDY/CNP land sink.RData")
# save.image("/Volumes/Backup Plus/The Alchmist/ZCo-CN-Beni/TRENDY/CNP land sink.RData")

library(tidyverse)
library(patchwork)
library(cowplot)
library(RColorBrewer)
library(here)

df_gcb <- read.csv(here('data/GCB23.csv'))
df_type <- read.csv(here('data/model_type.csv'))
df_ts <- read.csv(here('data/TS23.csv'))

df_gcb <- df_gcb |>
  mutate(s_land_impl = fossil.emissions.excluding.carbonation + land.use.change.emissions - cement.carbonation.sink - ocean.sink - atmospheric.growth)

# df_gcb |>
#   pivot_longer(cols = c(land.sink, s_land_impl), values_to = "s_land", names_to = "source") |>
#   ggplot(aes(x = Year, y = s_land, color = source)) +
#   geom_line()

# combine
df <- left_join(df_ts, df_gcb, by = "Year")

# Define decades
df <- df |>
  mutate(decade = ifelse(Year > 1979 & Year < 1990,
                         1980,
                         ifelse(Year > 1989 & Year < 2000, 1990,
                                ifelse(Year > 1999 & Year < 2010, 2000,
                                       ifelse(Year > 2009, 2010, NA)))
  ))

# data frame in long format
df_models <- df |>
  select(-GCB, -Multi.model.mean, -Model.Spread..sd.) |>
  pivot_longer(cols = c("CABLE.POP",
                        "CLASSIC",
                        "CLM5.0",
                        "DLEM",
                        "EDv3",
                        "ELM",
                        "IBIS",
                        "ISAM",
                        "ISBA.CTRIP",
                        "JSBACH",
                        "JULES.ES",
                        "LPJ.GUESS",
                        "LPJwsl",
                        "LPJml",
                        "LPX.Bern",
                        "OCNv2",
                        "ORCHIDEE.v3",
                        "SDGVM",
                        "VISIT",
                        "YIBs"),
               names_to = "model",
               values_to = "sland"
  )

# aggregate (mean) by decade and model
df_decades <- df_models |>
  drop_na() |>
  group_by(decade, model) |>
  summarise(sland = mean(sland)) |>
  left_join(
    df_type,
    by = "model"
  )

# add info on model type to un-aggregated (containing all years) data frame
df_models <- df_models |>
  left_join(
    df_type,
    by = "model"
  )

# aggregate observations (implied land sink) by decade
df_decades_obs <- df |>
  drop_na() |>
  group_by(decade) |>
  summarise(sland = mean(s_land_impl))

## Exploratory plot
ggplot() +
  geom_boxplot(data = df_decades, aes(as.factor(decade), sland, fill = type)) +
  geom_jitter(data = df_decades, aes(as.factor(decade), sland, color = type), width = 0.1) +
  geom_point(data = df_decades_obs, aes(as.factor(decade), sland), color = "black", size = 5)

## Publication figure
# re-arrange data for plotting
tmp <- df_models |>

  # aggregate by model type (mean)
  group_by(Year, type) |>
  summarise(sland = mean(sland)) |>

  # add aggregated by model type (min)
  left_join(
    df_models |>
      group_by(Year, type) |>
      summarise(sland_min = min(sland))
  ) |>

  # add aggregated by model type (max)
  left_join(
    df_models |>
      group_by(Year, type) |>
      summarise(sland_max = max(sland))
  ) |>

  # add implied residual sink as "Obs."
  bind_rows(
    df |>
      select(Year, sland = s_land_impl) |>
      mutate(type = "Obs.")
  )

# make model type a factor
vec_types <- unique(tmp$type)
tmp <- tmp |>
  mutate(type = factor(type,
                       levels = (c("Obs.",
                                   vec_types[-which(vec_types == "Obs.")]))))

# create colour palette distinguishing models
colourCount = length(unique(tmp$type))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
set.seed(1982)

# time series
gg1 <- tmp |>
  ggplot(aes(Year, sland, color = type)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_ribbon(aes(x = Year, ymin = sland_min, ymax = sland_max, fill = type, alpha=0.6), color = NA) +
  geom_line(linewidth = 0.8) +
  theme_classic() +
  scale_color_manual(values = c("black", "tomato", "royalblue", 'yellow'),
                     name = "") +
  scale_fill_manual(values = c("black", "tomato", "royalblue", 'yellow'),
                    name = "") +
  labs(x = "Year", y = expression(paste("Sink (g C m"^2, "yr"^-1, ")")))

get_trend <- function(adf, year_start){
  adf <- adf |>
    filter(Year >= year_start)
  linmod <- lm(sland ~ Year, data = adf)
  sinktrend <- coef(linmod)["Year"]
  return(sinktrend)
}

get_iav <- function(adf, year_start){
  adf <- adf |>
    filter(Year >= year_start)
  linmod <- lm(sland ~ Year, data = adf)
  iav <- sd(linmod$residuals)
  return(iav)
}

get_sland_mean <- function(adf, use_years){
  adf |>
    filter(Year %in% use_years) |>
    summarise(sland = mean(sland)) |>
    pull(sland)
}

df_models_trend <- df_models |>
  bind_rows(
    df |>
      select(Year, sland = s_land_impl) |>
      mutate(model = "obs")
  ) |>
  group_by(model) |>
  nest() |>
  mutate(trend = purrr::map_dbl(data, ~get_trend(., year_start = 1959)),
         iav = purrr::map_dbl(data, ~get_iav(., year_start = 1959)),
         sland_mean = purrr::map_dbl(data, ~get_sland_mean(., use_years = 2011:2022))) |>
  left_join(
    df_type,
    by = "model"
  ) |>
  mutate(type = ifelse(is.na(type), "obs", type)) |>
  mutate(model = ifelse(model == "obs", "Observations", model)) |>
  mutate(type = ifelse(type == "obs", "Obs.", type))

colourCount = length(unique(df_models_trend$model))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
set.seed(1986)

# make long
df_models_trend_long <- df_models_trend |>
  pivot_longer(cols = c(sland_mean, trend, iav),
               names_to = "metric",
               values_to = "value")

# plot with facet
mylabels = c(
  sland_mean = "Mean 2011-2022",
  trend = "Trend 1959-2022"
)

gg2 <- df_models_trend_long |>
  filter(metric != "iav") |>
  mutate(type = factor(type, levels = c("CNP","CN", "C", "Obs.")),
         model = factor(model,
                        levels = (c("Observations",
                                    df_models_trend$model[-which(df_models_trend$model == "Observations")])))) |>
  ggplot(aes(x = type, y = value)) +
  geom_boxplot(fill = "azure3", outlier.color = "white") +
  geom_jitter(aes(color = model), width = 0.2, size = 3) +
  theme_classic() +
  scale_color_manual(values = c("black", sample(getPalette(colourCount)))) +
  labs(x = "Model type and observations",
       y = expression(paste("(g C m"^2, "yr"^-1, ")"))) +
  facet_wrap(~metric,
             scales = "free",
             labeller = labeller(metric = mylabels)) +
  theme(
    strip.text = element_text(
      size = 12, hjust = 0
    ),
    strip.background = element_rect(
      color = NA
    ),
    legend.title = element_blank()
  )

cowplot::plot_grid(gg1, gg2, nrow = 2, rel_heights = c(0.8, 1), labels = c("a", "b", "c"))

# gg3 <- df_models_trend_long |>
#   filter(metric == "trend") |>
#   mutate(type = factor(type, levels = c("CNP", "CN", "C", "Obs.")),
#          model = factor(model,
#                         levels = (c("Observations",
#                                     df_models_trend$model[-which(df_models_trend$model == "Observations")])))) |>
#   ggplot(aes(x = type, y = value)) +
#   geom_boxplot(fill = "azure3", outlier.color = "white") +
#   geom_jitter(aes(color = model), width = 0.2, size = 3) +
#   theme_classic() +
#   scale_color_manual(values = c("black", sample(getPalette(colourCount)))) +
#   labs(x = "Model type and observations",
#        y = expression(paste("Sink (g C m"^2, "yr"^-1, ")")),
#        title = "Trend 1959-2022") +
#   theme(
#     legend.title = element_blank()
#   )

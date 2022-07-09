library(tidyverse)
library(scales)

# List all files in the data folder.
file_source <- fs::dir_ls('data')

# map_dfr in conjunction with read_csv reads all
# files and appends them into one dataframe.
file_source |>
  map_dfr(read_csv, .id = 'filename') ->
  appended_files

appended_files |>
  mutate(
    # Extract the city name from the file name
    city = str_to_title(str_match(filename, r'(([a-z]+)_craigslist)')[,2]),
    city = recode(
      .x = city,
      'Newyork' = 'NYC',
      'Sfbay' = 'San Francisco Bay',
      'Washingtondc' = 'DC'
      )
    ) |>
  select(-filename) |>
  relocate(city, .after = date_time) |>
  drop_na(price, beds, sqft) |>
  # Removing questionable data
  filter(
    sqft <= 6000,
    between(price, 800, 20000)
  ) |>
  # Drop repeated / spammed posts
  distinct(title, .keep_all = TRUE) ->
  apts

# Filter out apartments costing more than this.
# There should be few data points above this threshold
# and most / all of them should be ultra-luxury
price_threshold <- 6000

# Taking a sample of 300 points from each city.
# Otherwise, there are too many data points for a scatter plot.
set.seed(123)
sample_data <- apts |>
  filter(price <= price_threshold) |>
  group_by(city) |>
  slice_sample(n = 400) |>
  ungroup()

# Summary of sample by city under the price threshold
sample_data |>
  group_by(city) |>
  summarise(
    across(where(is.numeric), .fns = list('mean' = mean, 'median' = median)),
    n_listings = n()
  ) ->
  sample_summary

# Linear regression slope and intercept by city
sample_data |>
  nest_by(city) |>
  mutate(
    city_lm = list(lm(data$price ~ data$sqft)),
    y_int = pluck(city_lm, 'coefficients')[1],
    slope = pluck(city_lm, 'coefficients')[2],
    equation = str_glue('y = {round(slope, 2)}x + {round(y_int, 2)}'),
    r_squared = cor(data$sqft, data$price)^2
  ) |>
  ungroup() |>
  select(-data, -city_lm) ->
  cities_lm

sample_data |>
  mutate(
    beds = factor(if_else(
      beds >= 4,
      '4 +', as.character(beds)
      ),
      levels = c('1', '2', '3', '4 +')
    ),
    city = as_factor(city)
  )|>
  ggplot(aes(x = sqft, y = price)) +
  geom_jitter(aes(color = beds, alpha = 0.5)) +
  facet_wrap(~ fct_relevel(
    city,
    'Boston', 'Chicago', 'NYC',
    'Austin', 'Denver', 'DC',
    'Portland', 'San Francisco Bay', 'Seattle'
    )
  ) +
  stat_smooth(method = 'lm', se = FALSE, color = '#504b99') +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(
    labels = label_dollar(),
    breaks = seq(1000, price_threshold, 1000)
  ) +
  geom_hline(
    data = sample_summary,
    aes(
      yintercept = price_median,
      linetype = 'Median Price'),
    color = 'black') +
  geom_vline(
    data = sample_summary,
    aes(
      xintercept = sqft_median,
      linetype = 'Median Size'),
    color = 'blue',
    show.legend = FALSE
  ) +
  scale_linetype_manual(name = '', values = c('dashed', 'dotted')) +
  theme_bw() +
  labs(
    title = 'Craigslist Apartment Listings of US Cities',
    subtitle = expression('Price Per Month v. ft'^2),
    x = expression('ft'^2),
    y = '',
    color = 'Beds'
  ) +
  guides(alpha = 'none') +
  scale_color_brewer(palette = 'Dark2')

library(tidyverse)
library(scales)

# List all files in the Craigslist folder.
file_source <- fs::dir_ls('data')

# map_dfr in conjunction with read_csv reads all
# files and appends them into one dataframe.
file_source |>
  map_dfr(read_csv, .id = 'filename') ->
  appended_files

appended_files |>
  mutate(
    city = str_to_title(str_match(filename, r'(([a-z]+)_craigslist)')[,2]),
    city = recode(
      .x = city,
      'Newyork' = 'NYC',
      'Sfbay' = 'San Francisco Bay',
      'Washingtondc' = 'DC'
    )) |>
  select(-filename, neighborhood = town) |>
  relocate(city, .after = date_time) |>
  drop_na(price, beds, sqft) |>
  filter(
    sqft <= 6000,
    between(price, 800, 30000)
  ) |>
  distinct(title, .keep_all = TRUE) ->
  apts

apts |>
  group_by(city) |>
  summarise(
    across(where(is.numeric), .fns = list('mean' = mean, 'median' = median)),
    n_listings = n()
  ) |>
  arrange(desc(price_median)) ->
  city_summary

apts |>
  filter(price <= 6000) |>
  group_by(city) |>
  summarise(
    across(where(is.numeric), .fns = list('mean' = mean, 'median' = median)),
    n_listings = n()
  ) ->
  non_lux_summary

apts |>
  mutate(
    beds = factor(if_else(
      beds >= 5,
      '5 +', as.character(beds)
      ),
      levels = c('1', '2', '3', '4', '5 +')
    )
  )|>
  filter(price <= 6000) |>
  ggplot(aes(x = sqft, y = price)) +
  geom_point() +
  facet_wrap(~ city) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(
    labels = label_dollar(),
    breaks = seq(1000, 10000, 1000)
  ) +
  geom_hline(
    data = non_lux_summary,
    aes(yintercept = price_median),
    color = 'green',
    linetype = 'dashed') +
  geom_vline(
    data = non_lux_summary,
    aes(xintercept = sqft_median),
    color = 'blue',
    linetype = 'dashed'
  ) +
  geom_jitter(aes(color = beds, alpha = 0.6)) +
  theme_bw() +
  labs(
    title = 'Craigslist Apartment Listings of US Cities',
    subtitle = 'Price v. Square Feet',
    x = expression('ft'^2),
    y = '$ Per Month',
    color = 'Beds'
  )
